# --------------------------------------------
#
# Auteur : Julie Guéguen
#
# Date de creation : 07/06/24
# 
# Date de modification : 10/06/24
#
# Nom du script : analyses.R
#
# Description : 
#
# 
# ------------------------------------
# Note : 
# ------------------------------------

##########
# Library ----
##########

require(tidyr)
require(dplyr)
require(ggplot2)
require(lubridate)


##########
# Lecture des fichiers ----
##########

source("_config.R")

load(paste0(doss_mois,"/output/graphiques.rda"))
load(paste0(doss_mois,"/data/donnees_generales.rda"))

##########
# Analyses ----
##########


######## Ecoulement ----

couleur_ecoulement <- c("Assec" = "#d3313f", "Ecoulement visible" = "#269ed2",
                        "Ecoulement non visible" = "#ef8b36", "Observations manquantes" = "#b6b7b8")

####################
#### Campagne actuelle

# verif annee
unique(onde_DC_usuelles$Annee)

# nombre de stations prelevees = 319 ?
dim(onde_DC_usuelles)

if(nrow(onde_DC_usuelles) != 319){
  print("Attention peut être un probleme")  
}


# Quelle semaine de prelevement ?
unique(lubridate::week(lubridate::ymd(onde_DC_usuelles$date_campagne)))

table(onde_DC_usuelles$date_campagne)

### Nombre de station par bassin

table(onde_DC_usuelles$libelle_bassin)

table(onde_DC_usuelles$libelle_bassin, onde_DC_usuelles$code_departement )

table(onde_DC_usuelles$date_campagne, onde_DC_usuelles$code_departement)

## departement manquant

dpt_manquants <- setdiff(conf_dep, unique(onde_DC_usuelles$code_departement))

if(length(dpt_manquants) != 0){
  print(paste0("Il manque les données pour les departements suivant : ", paste0(dpt_manquants, collapse =", ")))
}

# calculer delta entre dept

onde_DC_usuelles %>%
  dplyr::select(code_departement, date_campagne) %>%
  dplyr::group_by(code_departement) %>%
  dplyr::summarise(maxi_date = max(date_campagne),
                   mini_date = min(date_campagne), delta_date = max(date_campagne) - min(date_campagne)) %>%
  dplyr::ungroup()

### Nombre de typologie d'écoulement

table(onde_DC_usuelles$lib_ecoul3mod)

table(onde_DC_usuelles$lib_ecoul3mod, onde_DC_usuelles$code_departement)

table(onde_DC_usuelles$lib_ecoul3mod, onde_DC_usuelles$libelle_bassin)

table(onde_DC_usuelles$lib_ecoul4mod, onde_DC_usuelles$libelle_bassin)

## commentaire

# table(onde_DC_usuelles$commentaire_sur_la_campagne, onde_DC_usuelles$code_departement )

## stations assec

onde_DC_usuelles %>%
  dplyr::filter(lib_ecoul3mod == "Assec") %>%
  dplyr::select(libelle_cours_eau, code_departement)

## barplot par mois de la situation

data_barplot <- onde_mois_usuel %>%
  dplyr::filter(Annee == max(Annee)) %>%
  dplyr::select(lib_ecoul3mod, Mois, code_station ) %>%
  dplyr::group_by(Mois) %>%
  dplyr::count(lib_ecoul3mod, .drop = FALSE)

ggplot(data_barplot, aes(fill= lib_ecoul3mod, 
                         x = Mois, y = n)) +
  geom_bar(position = position_dodge(), stat = "identity") +
  geom_text(aes(label = ifelse(n > 0, n, "")), vjust = -1, color = "black",
            position = position_dodge(0.9), size=3.5) +
  scale_fill_manual(values = couleur_ecoulement, drop = FALSE, name = "") +
  ylim(c(0, max(data_barplot$n)+2)) +
  labs(x = "Mois des campagnes usuelles", y = "Nombre de stations",
       caption = "Données Onde, au 07/06/24",
       title = paste0("Situation des typologies nationales d'écoulement pour la région Grand Est - Campagne ", anneeAVoir)) +
  theme_light() +
  theme(text = element_text(size = 12),
        axis.text = element_text(size = 12)) +
  scale_x_discrete(labels = paste0(c("Mai", "Juin", "Juillet", "Août", "Septembre"), "\n 2024"))

plot_ecoul_anneeAVoir


bilan_cond_reg_typo_nat

bilan_cond_reg_typo_dep

assecs_consecutifs_reg


####################
#### Comparaison inter-annuelle hist + courante

## ecoulement

zaza <- onde_DC_usuelles %>%
  dplyr::select(Mois, Annee) %>%
  dplyr::group_by(Mois, Annee) %>%
  dplyr::mutate(nbStation = dplyr::n()) %>%
  dplyr::distinct()

zaza

## <!> par construction <!>
zaza <- onde_mois_usuel %>%
  dplyr::select(Mois, Annee) %>%
  dplyr::group_by(Mois, Annee) %>%
  dplyr::mutate(nbStation = dplyr::n()) %>%
  dplyr::distinct()

table(zaza$nbStation, zaza$Annee)

## remarque : le nombre de stations change en fonction des années. De plus, des fois on a pas le même 
# nombre de stations entre les mois d'une meme Annee

plot_ecoul_moisAVoir



######## Indice ----

plot_indice_moisAVoir

# indice_onde_mois_usuel_mois

