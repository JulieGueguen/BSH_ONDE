# --------------------------------------------
#
# Auteur : Julie Guéguen
#
# Date de creation : -
# 
# Date de modification : 08/07/24
#
# Nom du script : 03_preparer_data.R
#
# Description : Ce script permet de mettre en forme les données. Et de creer de 
# nouveaux tableaux pour les analyses et graphiques
#
# ------------------------------------
# Note : Ce script est basé sur :
# - le projet PRR_onde
# https://github.com/richaben/PRR_ONDE
# 
# - le projet ondetools
# https://github.com/PascalIrz/ondetools
#
# ------------------------------------


'%>%' <- dplyr::'%>%'

source("_config.R")

load(paste0(doss_engt_onde_hist,"to_update.rda"))
# to_update <- TRUE
load("./data/raw_data/masks.Rdata")

if (to_update | mois_campagneAVoir != mois_campagne_jour) {
  
  # donnees onde depuis 2012 a maintenant
  onde_df <- read.csv(
    file = paste0(doss_engt_onde_hist, "onde.csv"),
    colClasses = "character"
    ) %>% 
    dplyr::filter(!is.na(code_station)) %>%
    dplyr::mutate(
      date_campagne = lubridate::as_date(date_campagne, format = "%Y-%m-%d")
      ) %>%
    dplyr::group_by(code_station) %>%
    dplyr::mutate(onde_plus = sum(libelle_type_campagne == "usuelle") == 0) %>%
    dplyr::ungroup()
  
  # couleurs légendes
  mes_couleurs_3mod <- c(
    "Ecoulement visible" = "#4575b4",
    "Ecoulement non visible" = "#fe9929",
    "Assec" = "#d73027",
    "Observation impossible" = "grey50",
    "Donnée manquante" = "grey90"
      )
  
  # légende
  mes_couleurs_4mod <- c(
    #"Ecoulement\nvisible" = "#0570b0",
    "Ecoulement\nvisible\nacceptable" = "#4575b4",
    "Ecoulement\nvisible\nfaible" = "#bdd7e7",
    "Ecoulement\nnon visible" = "#fe9929",
    "Assec" = "#d73027",
    "Observation\nimpossible" = "grey50",
    "Donnée\nmanquante" = "grey90"
      )
  
  
  ############################
  # Corriger les circonscriptions de bassin (cf fichier "bassin_stationsOnde_corrQgis.csv)
  
  onde_df <- onde_df %>%
    mutate(libelle_bassin = case_when(
      code_station == "H1130001" ~ "Seine-Normandie", # erreur -> base "Rhin-Meuse" Qgis SN
      code_station == "B2200001" ~ "Rhin-Meuse", # erreur -> base "Seine-Normandie" Qgis Rhin-Meuse
      code_station == "U0045006" ~ "Rhône-Méditerranée", # erreur -> base "Rhin-Meuse" Qgis RMC
      .default = libelle_bassin))
  
  ############################
  # Finalement, on va reconstruire le tableau des stations
  
  info_stations <- onde_df %>%
    select("code_station","libelle_station","code_departement",       
           "libelle_departement","code_commune","libelle_commune","code_region",              
           "libelle_region","code_bassin","libelle_bassin",
           "code_cours_eau", "etat_station", "onde_plus",
           "libelle_cours_eau", "longitude", "latitude") %>%
    distinct()
    
  ############################
  # Remarque : Dans le cadre des comparaisons inter-annuelle, on doit garder les
  # stations inactives. Par contre, on ne peut pas completer avec la fonction
  # automatique car les anciennes stations sont rajoutées pour les periodes recentes.
  # Car ce n'est pas ça que comme ça que c'était fait avant (est ce que c'est mieux ?). 
  # On considere que le reseau est "stable" en terme de nombre de stations (319 stations)
  # et pas 331 comme cela donnerais si on complete avec complete()
  
  # Remarque 2 : Dans le cas du mois de mai 2012 et septembre 2013, il manque des stations mais
  # pour autant, ce n'est pas indiqué comme des observations manquantes !
  
  # Remarque 3 : Si on ne fait pas le rajout automatique, alors ce n'est plus reproductible !
  
  # Remarque 4: En 2013, il y avait un reseau avec 307 stations "actives" (2024) et non pas 319 comme en 2024, par exemple.
  # elles etaient forcement actives en 2013 !
  # Est ce que pour autant, cela veut dire que les nouvelles stations pour les annees 
  # precedentes, sont "manquantes", certaines c'est qu'elles n'existaient pas encore.
  
  
  # tableau des stations hors onde+ pour toutes les annees
  onde_usuelle_all <- onde_df %>% 
    dplyr::select(-c(libelle_reseau, code_type_campagne)) %>% 
    dplyr::filter(!onde_plus) %>% 
    dplyr::mutate(
      Annee = as.numeric(Annee),
      Mois = format(as.Date(date_campagne), "%m")
    ) %>% 
    dplyr::mutate(
      lib_ecoul3mod = dplyr::case_when(
        libelle_ecoulement == 'Ecoulement visible faible' ~ 'Ecoulement visible',
        libelle_ecoulement == 'Ecoulement visible acceptable' ~ 'Ecoulement visible',
        TRUE ~ libelle_ecoulement
      ),
      lib_ecoul4mod = dplyr::case_when(
        libelle_ecoulement == 'Ecoulement visible' ~ 'Ecoulement visible acceptable',
        TRUE ~ libelle_ecoulement
      )
    ) %>% 
    dplyr::select(
      code_station, libelle_station, code_campagne,
      libelle_bassin, libelle_cours_eau,
      date_campagne, Annee, Mois, 
      lib_ecoul3mod, lib_ecoul4mod,
      libelle_type_campagne,
      longitude, latitude,
      code_departement,
      etat_station
    ) %>% 
    (function(df_temp) {
      dplyr::bind_rows(
        df_temp %>% 
          dplyr::filter(
            libelle_type_campagne == "usuelle"
          ) %>% 
          dplyr::filter(
            date_campagne <= Sys.Date()
          )
      ) %>% 
        dplyr::arrange(
          Annee, Mois, dplyr::desc(libelle_type_campagne)
        ) %>% 
        dplyr::mutate(Mois_campagne = lubridate::ym(paste0(Annee, Mois, sep="-")))
    })
  
  zaza <- onde_usuelle_all %>%
    dplyr::select(Mois, Annee) %>%
    dplyr::group_by(Mois, Annee) %>%
    dplyr::mutate(nbStation = dplyr::n()) %>%
    dplyr::distinct()
  
  table(zaza)
  
  # on ajoute les stations manquantes qui ne sont pas affichees en tant que telles :
  # rajouter les stations qui sont prises en 2012 et 2013
  
  stations2012 <- onde_usuelle_all %>%
    dplyr::filter(Annee == 2012) %>%
    dplyr::select(code_station) %>%
    dplyr::distinct() %>%
    dplyr::pull()
  
  stations2013 <- onde_usuelle_all %>%
    dplyr::filter(Annee == 2013) %>%
    dplyr::select(code_station) %>%
    dplyr::distinct() %>%
    dplyr::pull()
  
  onde_usuelle_manquantes <- onde_usuelle_all %>%
    ## on ajoute les dates/stations qui "manque" donc code_campagne = NA et lib_ecoul3mod = Donnees manquantes
    tidyr::complete(
      tidyr::nesting(code_station, libelle_station, longitude, latitude, code_departement, etat_station),
      Annee, Mois,
      fill = list(
        libelle_type_campagne = "usuelle",
        lib_ecoul3mod = "Donnée manquante",
        lib_ecoul4mod = "Donnée manquante"
      )
    ) %>%
    dplyr::mutate(
      Mois_campagne = dplyr::if_else(
        is.na(Mois_campagne), 
        lubridate::ym(paste0(Annee, Mois, sep="-")),
        Mois_campagne
      )
    ) %>%
    # TODO : Choisir une methode plus reproductible !!
    # les donnees qui nous interessent de garder sont :
    # - les stations code_campagne == NA et les stations prelevees en 2012  pour 2012 et les stations prelevees en 2013 pour 2013
    dplyr::filter(((is.na(code_campagne) & Mois_campagne == "2013-09-01" & code_station %in% stations2013 )) |
                    ((is.na(code_campagne) & Mois_campagne == "2012-05-01" & code_station %in% stations2012)))
  
  onde_usuelle_all <- dplyr::bind_rows(onde_usuelle_all, onde_usuelle_manquantes) %>%
    dplyr::mutate(Mois = factor(Mois, levels = c("05","06","07","08","09"))) %>%
    dplyr::mutate(libelle_mois = dplyr::case_when(Mois == 5 ~ "mai",
                            Mois == 6 ~ "juin",
                            Mois == 7 ~ "juillet",
                            Mois == 8 ~ "aout",
                            Mois == 9 ~ "septembre",
                            .default = Mois)) %>% # comme ca si mois hors on le voit
    dplyr::mutate(libelle_mois = factor(libelle_mois, levels = c("mai","juin","juillet","aout","septembre")))
  
  zaza2 <- onde_usuelle_all %>%
    dplyr::select(Mois, Annee) %>%
    dplyr::group_by(Mois, Annee) %>%
    dplyr::mutate(nbStation = dplyr::n()) %>%
    dplyr::distinct()
  
  table(zaza2)
  
  ## remarque : Si on selectionne seulement les stations "actives" alors
  # on reviens au point de depart.
  ############################
  
  
  ###########################
  ## selection des données usuelles sur toutes les annees Onde (2012 - now)
  # sur les mois entre mai et septembre
  
  onde_mois_usuel <- onde_usuelle_all %>% 
    dplyr::filter(
      libelle_type_campagne == "usuelle",
      Mois %in% c("05", "06", "07", "08", "09")
    )
  
  onde_mois_complementaire <- onde_usuelle_all %>% 
    dplyr::filter(libelle_type_campagne == "complementaire")
  
  ## calculs assecs toute annees periode ete sur campagnes usuelles
  assecs_mois_usuel <- onde_mois_usuel %>%
    dplyr::group_by(code_station, libelle_station) %>% # par station
    dplyr::summarise(
      n_donnees = dplyr::n(),
      n_assecs = length(lib_ecoul3mod[lib_ecoul3mod=='Assec']),
      .groups = "drop"
    ) %>%
    dplyr::mutate(
      pourcentage_assecs = round(n_assecs / n_donnees * 100, digits = 2),
      taille_point = sqrt(pourcentage_assecs)
    )
  
  ## calcul des indices. Basé sur la fonction calculer_indice_onde.R
  # du package ondetools
  # basé sur les ecoulement departementaux (4 classes) mais ne change pas 
  # si on prend national (3 classes) car on ne detecte que les debut des type d'écoulement
  # INDICE ONDE= (5*N2+10*N1)/N où  N : nombre total de stations et N1 : écoulement visible
  # N2 : écoulement non visible
  
#' Title
#'
#' @param post_df : Tableau obtenu avec premiere mise en forme des donnees
#'
#' @return
#' @export
#'
#' @examples
  fun_indice_postdf <- function(post_df, ...){
    post_df %>%
      dplyr::group_by(code_departement, Annee, Mois_campagne, ...) %>%
      dplyr::summarise(
        nb_sta = length(unique(code_station)),
        nb_ecoul_cont = sum(grepl(
          "Ecoulement visib.", lib_ecoul4mod
        )),
        nb_ecoul_interr = sum(grepl("Ecoulement non", lib_ecoul4mod)),
        nb_NA = sum(
          grepl(
            "Observation impossible|Donn\u00e9e manquante",
            lib_ecoul4mod
          )
        ),
        indice = dplyr::if_else(nb_NA == 0, round(((5 * nb_ecoul_interr + 10 * nb_ecoul_cont) / nb_sta), 2), NA)
      ) %>%
      dplyr::ungroup()
  }
  
  ## indice par code_campagne, code_departement, date_campagne, Annee
  # indice_onde_mois_usuel <- onde_mois_usuel %>%
  #   fun_indice_postdf()
  
  ## indice par code_campagne, code_departement, date_campagne, Annee et Mois
  indice_onde_mois_usuel_mois <- onde_mois_usuel %>%
    fun_indice_postdf(Mois)


  indice_onde_mois_complem_mois <- onde_mois_complementaire %>%
    fun_indice_postdf(Mois)
  
  ###############################################
  ## selection sous tableau des dernieres campagnes (DC)
  ###############################################
  
  selection_dernieres_campagnes <- function(df) {
    df %>% 
      dplyr::group_by(libelle_station) %>% 
      dplyr::slice(which.max(Mois_campagne)) %>% 
      dplyr::arrange(libelle_type_campagne, libelle_station, Mois_campagne) %>% 
      dplyr::ungroup() %>% 
      dplyr::mutate(
        Couleur_3mod = dplyr::recode(
          stringr::str_wrap(lib_ecoul3mod,12), !!!mes_couleurs_3mod),
        Couleur_4mod = dplyr::recode(
          stringr::str_wrap(lib_ecoul4mod,12), !!!mes_couleurs_4mod)
      ) %>% 
      dplyr::mutate(
        label_point_3mod = glue::glue('{libelle_station}: {lib_ecoul3mod} ({date_campagne})'),
        label_point_4mod = glue::glue('{libelle_station}: {lib_ecoul4mod} ({date_campagne})')
      )
  }
  
  # derniere campagne usuelle
  onde_DC_usuelles <- onde_mois_usuel %>% 
    # selection_dernieres_campagnes()
    dplyr::filter(Mois_campagne == mois_campagneAVoir)

  # Tableau 1
  # indices par annee et departement lies a la derniere campagne usuelle
  # indice_onde_DC_usuelles <- onde_DC_usuelles %>%
  #   fun_indice_postdf()
  
  indice_onde_DC_usuelles <- indice_onde_mois_usuel_mois %>%
    dplyr::filter(Mois_campagne == mois_campagneAVoir)

  
  #####################################
  # Mise en forme des tableaux pour les graphiques bilan
  
  prep_data_bilan <- function(df, mod, mod_levels, ...) {
    df %>% 
      dplyr::group_by(Mois, Annee, ..., {{mod}}) %>% 
      dplyr::summarise(NB = dplyr::n(), .groups = "drop_last") %>% 
      dplyr::mutate(frq = NB / sum(NB) *100) %>% 
      dplyr::arrange(Mois, ...) %>% 
      dplyr::mutate(
        dplyr::across(
          {{mod}},
          function(x) {
            factor(x, levels = mod_levels, ordered = TRUE)
          }
          )
      ) %>% 
    dplyr::mutate(Label = ifelse(is.na(NB),"",glue::glue("{NB}"))) %>% 
    dplyr::mutate(Label_p = ifelse(is.na(frq),"",glue::glue("{round(frq,0)}%"))) 
  }
  
  df_usuel_categ_obs_3mod <- onde_mois_usuel %>% 
    prep_data_bilan(
      mod = lib_ecoul3mod,
      mod_levels = c("Ecoulement visible",
                     "Ecoulement non visible",
                     "Assec",
                     "Observation impossible",
                     "Donnée manquante"),
      code_departement # group_by departement
    )

  df_usuel_categ_obs_3mod_region <- onde_mois_usuel %>% 
    prep_data_bilan(
      mod = lib_ecoul3mod,
      mod_levels = c("Ecoulement visible",
                     "Ecoulement non visible",
                     "Assec",
                     "Observation impossible",
                     "Donnée manquante")
    )
  
## attention, les sommes des frequences ne font pas 100 (exple 2012 et 2014)
  # du aux arrondis ?
  
  df_usuel_categ_obs_4mod <- onde_mois_usuel %>% 
    prep_data_bilan(
      mod = lib_ecoul4mod,
      mod_levels = c("Ecoulement visible acceptable",
                     "Ecoulement visible faible",
                     "Ecoulement non visible",
                     "Assec",
                     "Observation impossible",
                     "Donnée manquante"),
      code_departement
    )

  df_usuel_categ_obs_4mod_region <- onde_mois_usuel %>% 
    prep_data_bilan(
      mod = lib_ecoul4mod,
      mod_levels = c("Ecoulement visible acceptable",
                     "Ecoulement visible faible",
                     "Ecoulement non visible",
                     "Assec",
                     "Observation impossible",
                     "Donnée manquante")
    )
  
  ## Récurrence assecs
  prep_data_recurrence <- function(df, ...) {
    df %>% 
      dplyr::distinct(..., code_station, libelle_station, Annee, Mois, lib_ecoul3mod) %>% 
      dplyr::mutate(mois_num = as.numeric(Mois)) %>% 
      dplyr::as_tibble() %>%
      dplyr::arrange(code_station, Annee, Mois) %>% 
      dplyr::group_by(
        code_station,Annee,
        ID = data.table::rleid(code_station,lib_ecoul3mod == 'Assec')
        ) %>%
      dplyr::mutate(
        mois_assec_consec = ifelse(
          lib_ecoul3mod == 'Assec', 
          dplyr::row_number(), 0L
          )
        ) %>% 
      dplyr::group_by(..., Annee,code_station) %>% 
      dplyr::summarise(
        max_nb_mois_assec  = max(mois_assec_consec),
        .groups = "drop"
        ) %>% 
      dplyr::group_by(..., Annee, max_nb_mois_assec) %>%
      dplyr::summarise(nb_station = dplyr::n(), .groups = "drop_last") %>% 
      dplyr::mutate(pct = prop.table(nb_station)) %>% 
      dplyr::mutate(label = ifelse(max_nb_mois_assec == '1' | max_nb_mois_assec == '0',
                                   paste0(max_nb_mois_assec, " mois"),
                                   paste0(max_nb_mois_assec, " mois cons\u00e9cutifs"))) %>% 
      dplyr::ungroup()
  }
  
  order_fac_levels <- function(df) {
    df %>% 
      dplyr::mutate(
        max_nb_mois_assec = factor(max_nb_mois_assec,ordered = TRUE) %>% 
      forcats::fct_rev()
        )
  }
  
  duree_assecs_df_usuel <- onde_mois_usuel %>% 
    prep_data_recurrence() %>% 
    order_fac_levels()
  
  duree_assecs_df_usuel_dep <- onde_mois_usuel %>% 
    dplyr::group_by(code_departement) %>% 
    dplyr::group_split(.keep = TRUE) %>% 
    purrr::map_df(
      prep_data_recurrence,
      code_departement
    ) %>% 
    order_fac_levels()
  
  #####################################
  ## Donnees Propluvia
  ## rem : Comment est géré ce fichier ??
  load(file = './data/raw_data/propluvia_zone.Rdata')
  
  # propluvia_dpt <- propluvia_zone %>% 
  #   dplyr::filter(type == 'SUP') %>% 
  #   dplyr::filter(dpt %in% conf_dep)
  
  ###############################################
  ## selection sous tableau de l'annee en cours
  ###############################################
  
  # onde_mois_usuel_anneeChoix <- onde_mois_usuel %>%
  #   filter(Annee == anneeAVoir)
  # 
  # table(onde_mois_usuel_anneeChoix$lib_ecoul3mod)
  
  ###################################
  ## Dernières campagnes
  
  date_derniere_campagne_usuelle <- 
    unique(onde_DC_usuelles$Mois_campagne) %>% 
    max() %>% 
    format("%m/%Y") 

  
  ## pour la figure 2 : tableau pour barplot des ecoulements pour l'année en cours
  
  data_barplot_ecoul_AnneeRecente <- onde_mois_usuel %>%
    # dplyr::filter(Annee == max(Annee)) %>% # annee en cours ou la plus recente
    dplyr::filter(Annee == anneeAVoir) %>% # annee en cours ou la plus recente
    prep_data_bilan(
      mod = lib_ecoul3mod,
      mod_levels = c("Ecoulement visible",
                     "Ecoulement non visible",
                     "Assec",
                     "Observation impossible",
                     "Donnée manquante")
    )
  
  ####################
  #### Comparaison inter-annuelle hist + courante
  #####################

  ## figure 3 : ecoulement pour le mois choisi sur toutes les annees disponibles
  
  data_barplot_ecoul_interAnnee <- df_usuel_categ_obs_3mod_region %>%
    dplyr::filter(Mois == moisAVoir) %>% 
    # dplyr::filter(Mois = max(Mois))
    dplyr::ungroup()
  
  ## figure 4 : indice pour le mois choisi sur toutes les annees disponibles
  
  data_plot_indice_interAnnee <- indice_onde_mois_usuel_mois %>%
    dplyr::filter(Mois == moisAVoir) %>%
    dplyr::ungroup() %>%
  # il faut mettre en date pour pouvoir faire le graphique apres.
  dplyr::mutate(Annee = lubridate::year(as.Date(as.character(Annee), format = "%Y")))
  
  
  write.csv2(indice_onde_mois_usuel_mois,
       file = paste0(doss_engt_onde_hist, "data_hist_indice_onde.csv"), row.names = FALSE)
  
  save(
    data_barplot_ecoul_AnneeRecente, # fig 2 : ecoulement par mois pour annee + recente
    data_barplot_ecoul_interAnnee, # fig 3 : ecoulement mois choisi par annee
    data_plot_indice_interAnnee, # fig 4 : indice mois choisi par dpt et par annee
    mes_couleurs_3mod,
    mes_couleurs_4mod,
    duree_assecs_df_usuel,
    duree_assecs_df_usuel_dep,
    file = paste0(doss_mois,"/data/donnees_pour_graphiques.rda")
    ) 
  
  save(
    info_stations,
    onde_DC_usuelles,
    # propluvia_dpt,
    indice_onde_mois_usuel_mois,
    indice_onde_DC_usuelles, # tab1 : notes d'indice pour le mois choisi par dpt
    assecs_mois_usuel,
    onde_mois_usuel,
    indice_onde_mois_complem_mois,
    onde_mois_complementaire,
    df_usuel_categ_obs_4mod,
    df_usuel_categ_obs_3mod,
    df_usuel_categ_obs_4mod_region,
    df_usuel_categ_obs_3mod_region,
    file = paste0(doss_mois,"/data/donnees_generales.rda")
  )
  
  print("Fin de la creation des données")
}

rm(list=ls())
