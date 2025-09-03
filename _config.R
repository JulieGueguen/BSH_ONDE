# --------------------------------------------
#
# Auteur : Julie Guéguen
#
# Date de creation : 13/06/24
# 
# Date de modification : 02/07/25
#
# Nom du script : _config.R
#
# Description : Fichier permettant de regrouper les parametrages pour le bulletin
#
# ------------------------------------
# Note : Ce script est basé sur le projet PRR_onde
# https://github.com/richaben/PRR_ONDE
# ------------------------------------

if (!require("tidyverse", character.only = TRUE)) {
  install.packages("tidyverse")
} else {
  require("tidyverse")
}

##########################
## configuration manuelle

moisAVoir <-  "08" 
lab_moisAVoir <-  "Aout"
anneeAVoir <- "2025"

## configuration variables donnees

conf_titre <- "Bulletin de Surveillance Hydrologique (BSH) pour la région Grand-Est"

conf_auteur <- "Julie Guéguen"

conf_dep <- c("08","10","51","52","54","55","57","67","68","88")

## remarque on peut aussi passer directement par le code region
conf_reg <- 44  # Grand-Est

# conf_libelle_type_campagne <- "usuelle"
# conf_selection_mois <- c("05", "06", "07", "08", "09")

# pour forcer la mise à jour
forcer <- TRUE # FALSE
# attention, si on ne force pas la mise à jour et qu'il y a une modification des données, il faudrait que
# la date de mise à jour soit indiquée

##########################
## configuration auto


### gestion des differentes dates utilisées

if(!(exists("moisAVoir") & (exists("lab_moisAVoir")) & (exists("anneeAVoir")))) {
  ## Dans le cas où on regarde les données du mois de lancement de l'analyse. Rarement le cas...
  ## On lance l'analyse sur le mois precedent ! Donc on retire 1 mois !
  moisAVoir <-  paste0("0",lubridate::month(Sys.Date()) - 1)
  
  ## On lance l'analyse sur le mois precedent ! Donc on retire 1 mois !
  lab_moisAVoir <- Sys.Date() %>%
                    lubridate::rollbackward(period("1 month")) %>%
                    lubridate::month(., label = TRUE) %>%
                    as.character()
  
  anneeAVoir <- lubridate::year(Sys.Date()) # "2024"
}

# date de la creation du rapport
date_jour <- as.character(format(Sys.time(),"%Y-%m-%d"))
date_jour_heure <- as.character(format(Sys.time(),"%Y-%m-%d_%Hh%m"))

# mois qui nous interesse
mois_campagneAVoir <- lubridate::ym(paste(anneeAVoir,moisAVoir,sep="-"))
mois_campagne_jour <- lubridate::ym(paste( lubridate::year(Sys.Date()),
                                           lubridate::month(Sys.Date()),sep="-"))
## creation des dossiers

doss_annee <- here::here("./..", anneeAVoir)
dir.create(doss_annee)

doss_mois <- here::here(doss_annee, lab_moisAVoir)
dir.create(doss_mois)

dir.create(here::here(doss_mois, "data"))
dir.create(here::here(doss_mois, "output"))
dir.create(here::here(doss_mois, "www"))

dossier <- here::here(paste0("./../",anneeAVoir,"/",lab_moisAVoir,"/"))

# dossier d'enregistrement de la compilation des données onde
doss_engt_onde_hist <- "./../"
