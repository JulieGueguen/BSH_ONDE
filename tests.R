df_usuel_categ_obs_4mod %>%
  filter(code_departement == 55 & Annee == 2025)

onde_DC_usuelles %>%
  filter(code_departement == 55 & Annee == 2025) %>%
  select(libelle_station, lib_ecoul4mod, lib_ecoul3mod)

# de 3 preparer_data
zaza2 <- onde_df2  %>%
  filter(code_departement == 55 & Annee == 2025 & libelle_type_campagne == "usuelle")

table(zaza2$libelle_ecoulement)

zaza2 %>% View(title = "apres")

## de 02_telechargement_data.R 
zaza1 <- onde_df  %>%
  filter(code_departement == 55 & Annee == 2025 & libelle_type_campagne == "usuelle")

table(zaza1$libelle_ecoulement)

zaza1 %>% View(title = "avant")
