# --------------------------------------------
#
# Auteur : Julie Guéguen
#
# Date de creation : -
# 
# Date de modification : 30/07/2026
#
# Nom du script : 06_generer_rapport_html.R
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

quarto::quarto_render("./assets/template_BSH3.qmd",
                      output_file = paste0("BSH_",anneeAVoir, moisAVoir,"_a_completer",".docx"),
                      output_format = "docx",
                      execute_dir = doss_mois,
                      execute_params = list(
                        annee_campagne = anneeAVoir,
                        mois_campagne = moisAVoir,
                        region_dr =  "Grand-Est",
                        doss_mois = dossier,
                        conf_dep = conf_dep
                      ),
                      quiet = FALSE)

# rem : quarto_render n'a pas de parametre output_dir. On va donc redeplacer le fichier.
fs::file_move(paste0("./assets/BSH_",anneeAVoir, moisAVoir,"_a_completer",".docx"), doss_mois)
# il faut aussi deplacer le dossier associé a l'html, attention, il y a des dossiers imbriqués !!
# move the file to the output path
current_folder <- "./assets/template_BSH3_files/"
new_folder <- here::here(doss_mois,"/template_BSH3_files/")
fs::dir_copy(current_folder, new_folder,overwrite = TRUE)
fs::dir_delete(current_folder)
