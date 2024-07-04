# --------------------------------------------
#
# Auteur : Julie Guéguen
#
# Date de creation : 28/06/24
# 
# Date de modification : 
#
# Nom du script : 1_lancement.R
#
# Description : Fichier permettant de lancer l'integralité de la chaine
#
# ------------------------------------

# fichier de configuration des departements et creation de la sctructure
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

## Creation du rapport detaillé pour pre-analyse
rmarkdown::render("./quarto_preanalyse_onde.qmd",
                  output_file = paste0("quarto_preanalyse_onde_",anneeAVoir,moisAVoir,".html"),
                  output_dir = doss_mois,
                  quiet = TRUE)







