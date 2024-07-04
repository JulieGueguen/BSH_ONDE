# --------------------------------------------
#
# Auteur : Julie Guéguen
#
# Date de creation : -
# 
# Date de modification : 14/06/24
#
# Nom du script : 01_verifier_maj.R
#
# Description : Ce script permet de verifier si nos données sont a jour.
#
# ------------------------------------
# Note : Ce script est basé sur le projet PRR_onde
# https://github.com/richaben/PRR_ONDE
#
## Remarque : Dans un contexte de creation du bsh et comment 
# on gere les dossiers, le dataframe onde et derniere_obs.csv soient deposé a la racine
# ------------------------------------


'%>%' <- dplyr::'%>%'

source("_config.R")

# si on a aucun document alors forcement on met a jour
if (!file.exists(paste0(doss_engt_onde_hist, "onde.csv")) | 
    !file.exists(paste0(doss_engt_onde_hist, "dernieres_obs.csv")) |
    !file.exists(paste0(doss_mois,"/data/donnees_pour_graphiques.rda")) |
    !file.exists(paste0(doss_mois,"/output/graphiques.rda"))|
    !file.exists(paste0(doss_mois,"/data/donnees_generales.rda"))) { 
  #|
    # !file.exists(paste0(doss_mois,"/data/donnees_cartes.rda"))) {
  
  to_update <- TRUE
  observation_maj <- "tout" # aucune observations n'est trouvé
  
} else {
    
    # on lit les dernieres observations qu'on avait
    old_data <- read.csv2(paste0(doss_engt_onde_hist,"dernieres_obs.csv"), colClasses = "character") %>% 
      dplyr::mutate(date_observation = lubridate::as_date(date_observation, format = "%Y-%m-%d")) %>% 
      dplyr::rename(old = date_observation)
    
    new_data <- purrr::map_df(
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
    ) %>% 
      dplyr::rename(new = date_observation)
    
    # si jamais on a de nouveaux departements
    if (any(! old_data$code_departement %in% new_data$code_departement) | # un ancien dpt n'est pas de les nvx
        any(! new_data$code_departement %in% old_data$code_departement)){ # un nouveau dpt n'est pas dans les anciens
      
      to_update <- TRUE
      observation_maj <- "dpt"
      
    } else {
      # on verifie que les dates soit toutes egales,
      # s'il y en a des differentes alors on met a jour.
      to_update <- dplyr::left_join(
        x = old_data,
        y = new_data,
        by = "code_departement"
      ) %>% 
        dplyr::select(old, new) %>% 
        t() %>% 
        duplicated() %>% 
        any() %>% 
        '!'()
      observation_maj <- "obs"
    }
}

save(to_update, date_jour, date_jour_heure, observation_maj, file = paste0(doss_engt_onde_hist, "to_update.rda")) #  old_data, new_data
  
print(switch(as.character(to_update), `TRUE` = "Mise-à-jour requise", `FALSE` = "Pas de mise-à-jour requise"))

