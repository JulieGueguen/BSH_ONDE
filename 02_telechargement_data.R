# --------------------------------------------
#
# Auteur : Julie Guéguen
#
# Date de creation : -
# 
# Date de modification : 14/06/24
#
# Nom du script : 02_telechargement_data.R
#
# Description : Ce script permet de telecharger les données via hubeau.
#
# ------------------------------------
# Note : Ce script est basé sur le projet PRR_onde
# https://github.com/richaben/PRR_ONDE
#
## Remarque : Le tableau des données est telecharger à chaque lancement. 
# Cela permet de prendre en compte plus facilement les changements dans les données
# ajout de campagnes ou corrections dans les données.
# ------------------------------------


'%>%' <- dplyr::'%>%'

source("_config.R")

load(paste0(doss_engt_onde_hist, "to_update.rda"))

## Note : Est ce que l'on pourrait :
# - Si pas de telechargement prealble alors telecharger l'ensemble des donnees depuis 2012
# - Si on a deja des donnees, alors ne telecharger que les stations manquantes ? Ou au moions juste l'annee qui
# fait defaut ?

### Quelles sont les donnees a mettre a jour ?
# if(to_update){
#   annee_min <- switch(observation_maj,
#                          "tout" = 2012,
#                          "dpt" = 2012, # on telecharge toutes les donnees du nouveau dept
#                          "obs" = {
#                            #nouvelles donnees
#                            # dans le cas ou il y a plusieurs annees qui manque on prend l'anneee
#                           # la plus ancienne et au pire on telecharge 
#                            min(unique(lubridate::year(setdiff(new_data$new, old_data$old))))
#                          })
# }

### Utilisation de l'API Hubeau ----

if (to_update) {
  # dataframe avec les dernieres dates d'observations en fonction des departements 
  # choisis dans _config
  print("Dernieres observations realisees")
  dernieres_obs <- purrr::map_df(
    .x = conf_dep,
    .f = function(d) {
      data.frame(
        code_departement = d,
        date_observation = readLines(
          paste0(
            "https://hubeau.eaufrance.fr/api/v1/ecoulement/observations?format=json&code_departement=",
            d, "&size=1&fields=date_observation&sort=desc"
          )
        ) %>%
          stringr::str_extract(pattern = "\\d{4}-\\d{2}-\\d{2}")
      )

    }
  )
  
  #### infos campagnes
  # de 2012 a sys.date
  print("Campagnes")
  campagnes <- purrr::map_df(.x = conf_dep,
                      function(x) hubeau::get_ecoulement_campagnes(
                        list(
                          code_departement = x,
                          date_campagne_min = "2012-01-01", #paste0(annee_min, "-01-01"),
                          date_campagne_max = date_jour
                        )
                      )) %>% 
    dplyr::mutate(code_campagne = as.character(code_campagne)) %>% 
    dplyr::distinct()
  
  #### infos stations
  
  # vecteur des colonnes que l'on peut recuperer via hubeau pour les stations
  print("Stations")
  param_stations <- 
    hubeau::list_params(api = "ecoulement", endpoint = "stations") %>% 
    toString() %>% 
    gsub(pattern = " ",replacement = "") %>% 
    paste0(",etat_station")
  
  # telechargement des stations
  stations <- purrr::map_df(.x = conf_dep,
                     function(x) hubeau::get_ecoulement_stations(
                       list(code_departement = x,
                            fields = param_stations)
                     )) %>% 
    dplyr::distinct()
  
  #### infos observations
  
  # vecteur des colonnes disponible dans le tableau des observations
  # pouvant être telechargés via hubeau
  print("Parametres")
  param_obs <- 
    hubeau::list_params(api = "ecoulement", endpoint = "observations") %>% 
    toString() %>% 
    gsub(pattern = " ",replacement = "")
  
  # Donneees des observations de 2012 a sys.date
  # remarque : Pourquoi tout retelecherger a chaque fois si on a deja les données 
  # des campagnes precedentes ?
  observations <- purrr::map_df(.x = conf_dep,
                         function(x) hubeau::get_ecoulement_observations(
                           list(code_departement = x,
                                date_observation_min = "2012-01-01", # paste0(annee_min, "-01-01"),
                                date_observation_max = date_jour,
                                fields = param_obs)
                         )) %>% 
    dplyr::mutate(code_campagne = as.character(code_campagne)) %>% 
    dplyr::distinct()
  
  ### Assemblage des données stations, observations, campagnes ----
  print("Données onde")
  onde_df <- observations %>% 
    dplyr::left_join(campagnes) %>% 
    dplyr::left_join(stations) %>% 
    dplyr::mutate(
      date_campagne = lubridate::as_date(date_campagne, format = "%Y-%m-%d")
      ) %>% 
    dplyr::mutate(
      Annee = lubridate::year(date_campagne),
      libelle_ecoulement = dplyr::if_else(
             condition = is.na(libelle_ecoulement),
             true = "Donnée manquante",
             false = libelle_ecoulement
           )
      ) %>% 
    dplyr::arrange(code_station,code_departement,desc(Annee))
  
  ## nettoyage
  rm(observation_maj, annee_min)
  
  ### Ecriture/Sauvegarde des données ----
  print("Enregistrement")
  ## données Onde 2012 - sys.date
  write.csv(onde_df, paste0(doss_engt_onde_hist, "onde.csv"))
  
  # vecteur des dernieres dates de prelevement
  write.csv2(dernieres_obs, paste0(doss_engt_onde_hist, "dernieres_obs.csv"))
  
  save(to_update, date_jour, date_jour_heure, file = paste0(doss_engt_onde_hist, "to_update.rda"))
  
  print("Fin de telechargement des données")
  
  rm(list = ls())
  
}

