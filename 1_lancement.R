# --------------------------------------------
#
# Auteur : Julie Guéguen
#
# Date de creation : 28/06/24
# 
# Date de modification : 02/09/24
#
# Nom du script : 1_lancement.R
#
# Description : Fichier permettant de lancer l'integralité de la chaine
#
# ------------------------------------

# fichier de configuration des departements et creation de la structure
source("_config.R")

## installation des packages
source("00_installation_packages.R")

## verification de la necessite de mise a jour
source("01_verifier_maj.R")

## telechargement des données si necessaire
source("02_telechargement_data.R")

## mise en forme des données
source("03_preparer_data.R")

## creation des graphiques
source("04_preparer_graphiques.R")

## Creation du rapport pour pre-analyse
source("_config.R") # on remet car il y a des rm dans les scripts precedents
quarto::quarto_render("./05_quarto_preanalyse_onde.qmd",
                  output_file = paste0("05_quarto_preanalyse_onde_",anneeAVoir,moisAVoir,".html"),
                  quiet = TRUE)

# rem : quarto_render n'a pas de parametre output_dir. On va donc redeplacer le fichier.
fs::file_move(paste0("05_quarto_preanalyse_onde_",anneeAVoir,moisAVoir,".html"), doss_mois)
# il faut aussi deplacer le dossier associé a l'html, attention, il y a des dossiers imbriqués !!
# move the file to the output path
current_folder <- "./05_quarto_preanalyse_onde_files/"
new_folder <- paste0(doss_mois,"/05_quarto_preanalyse_onde_files/")
fs::dir_copy(current_folder, new_folder)
fs::dir_delete(current_folder)


## Creation du rapport BSH 
source("06_generer_rapport_html.R")





