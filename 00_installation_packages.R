# --------------------------------------------
#
# Auteur : Julie Guéguen
#
# Date de creation : 13/06/24
# 
# Date de modification : 14/06/24
#
# Nom du script : 00_installation_packages.R
#
# Description : Script permettant d"installer les packages servant à créer le BSH
#
# ------------------------------------
# Note : Ce script est basé sur le projet PRR_onde
# https://github.com/richaben/PRR_ONDE
# ------------------------------------

## Dépendances 

# installer pak
if(system.file(package = "pak") == ""){ # le package n'est pas installé
  install.packages("pak")
}

pkg_gh <- c("richaben/ondetools", "inrae/hubeau")
pkg_cran <- c("tidyverse", "purrr", "sf", "mapview", "leaflet", "leaflet.extras", "ggrepel",
              "glue", "forcats", "scales", "data.table", "lubridate", "stringr", "tidyr", 
              "ggplot2", "knitr", "rmarkdown", "htmltools", "leafem", "png", "webp")

try(pak::pkg_install(c(pkg_gh, pkg_cran)))

sapply(
  X = pkg_cran,
  FUN = function(package) {
    if (!require(package, character.only = TRUE)) {
      install.packages(package)
    } else {
      require(package)
    }
  }
)


