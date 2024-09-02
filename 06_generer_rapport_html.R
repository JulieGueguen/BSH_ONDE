# --------------------------------------------
#
# Auteur : Julie Guéguen
#
# Date de creation : -
# 
# Date de modification : 19/08/24
#
# Nom du script : .R
#
# Description : 
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


source("_config.R")

# load(paste0(doss_mois,"/output/graphiques.rda"))
# load(paste0(doss_mois,"/data/donnees_generales.rda"))

rmarkdown::render("./assets/template_BSH3.qmd",
                  output_file = paste0("BSH_",anneeAVoir,moisAVoir,"_a_completer",".docx"),
                  output_dir = doss_mois,
                  params = list(
                    annee_campagne = anneeAVoir,
                    mois_campagne = lab_moisAVoir,
                    region_dr =  "Grand-Est",
                    doss_mois = dossier
                  ),
                  quiet = TRUE)

# donnees_onde <- read.csv(
#   file = paste0(doss_engt_onde_hist, "onde.csv"),
#   colClasses = "character"
# ) %>%
#   dplyr::filter(!is.na(code_station)) %>%
#   dplyr::mutate(
#     date_campagne = lubridate::as_date(date_campagne, format = "%Y-%m-%d")
#   ) %>%
#   dplyr::group_by(code_station) %>%
#   dplyr::mutate(onde_plus = sum(libelle_type_campagne == "usuelle") == 0) %>%
#   dplyr::ungroup()
# 
# rmarkdown::render("./assets/skeleton_DR.Rmd",
#                   output_file = paste0("BSH_",anneeAVoir,moisAVoir,"_a_completer_V2",".docx"),
#                   output_dir = doss_mois,
#                   params = list(
#                     doss_engt_onde_hist = doss_engt_onde_hist,
#                     annee_mois = "2024_06",
#                     onde_df2 = donnees_onde,
#                     code_region = conf_reg,
#                     codes_dpt = conf_dep,
#                     annee_rapport = anneeAVoir,
#                     mois_rapport = moisAVoir,
#                     region_dr = "Grand-Est",
#                     type_rapport = "usuelle"
#                   ),
#                   quiet = FALSE)

# produire_rapport_mensuel_dpt(onde_df = donnees_onde,
#                              code_departement = c('14', '27', '76'),
#                              annee_mois = "2024_05",
#                              region_dr = 'Grand-Est',
#                              complementaire = F,
#                              dossier_sortie = "./OUTPUT")
