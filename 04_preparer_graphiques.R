# --------------------------------------------
#
# Auteur : Julie Guéguen
#
# Date de creation : -
# 
# Date de modification : 06/08/24 - print fin script
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


'%>%' <- dplyr::'%>%'

source("_config.R")

load(paste0(doss_engt_onde_hist, "to_update.rda"))

load(paste0(doss_mois,"/data/donnees_pour_graphiques.rda"))

load(paste0(doss_mois,"/data/donnees_generales.rda"))

if (to_update | mois_campagneAVoir != mois_campagne_jour) {

  ## Cartes
  ### Préparation données
  
  
  ## Conditions d'écoulement lors des campagnes usuelles de l'année en cours
#' Title
#'
#' @param data_bilan 
#' @param lib_ecoulement 
#' @param regional 
#' @param modalites 
#'
#' @return
#' @export
#'
#' @examples
  plot_bilan_prop <- function(data_bilan, lib_ecoulement, regional = FALSE, modalites = ggplot2::waiver()) {
    data_bilan %>% 
      
      ggplot2::ggplot(
        mapping = ggplot2::aes(
          y = frq, 
          x = forcats::fct_rev(factor(Mois)),
          fill= forcats::fct_rev({{lib_ecoulement}}), 
          label=Label_p
        )
      ) +
      ggplot2::geom_bar(
        position = "stack", 
        stat = "identity",
        # alpha = 0.7, 
        colour = 'black', 
        width = 0.7, 
        linewidth = 0.01
      ) +
      {
        if(regional == FALSE)
          ggplot2::facet_grid(~code_departement)
      } +
      ggrepel::geom_text_repel(
        size = 3,
        color = "black", 
        fontface = 'bold.italic',
        position = ggplot2::position_stack(vjust = 0.5)
      ) +
      ggplot2::coord_flip() +
      ggplot2::ylab("Pourcentage (%)") +
      ggplot2::xlab("Mois") +
      ggplot2::scale_fill_manual(
        name = "Situation stations",
        values = c("Donnée manquante" = "grey90",
                   "Observation impossible" = "grey50",
                   "Assec" = "#d73027",
                   "Ecoulement non visible" = "#fe9929",
                   "Ecoulement visible faible" = "#bdd7e7",
                   "Ecoulement visible acceptable" = "#4575b4",
                   "Ecoulement visible" = "#4575b4"
        ),
        breaks = modalites,
        drop = FALSE
      ) +
      ggplot2::theme_bw() +
      ggplot2::theme(
        title = ggplot2::element_text(size = 11, face = "bold"), 
        legend.text = ggplot2::element_text(size = 11),
        legend.title = ggplot2::element_text(size = 11, face = 'bold'),
        axis.text.y = ggplot2::element_text(size = 11, colour = 'black'),
        axis.text.x = ggplot2::element_text(size = 11, colour = 'black'),
        strip.text.x = ggplot2::element_text(size = 11, color = "black", face = "bold"),
        strip.background = ggplot2::element_rect(
          color="black", fill="grey80", linewidth = 1, linetype="solid"
        ),
        panel.grid.major = ggplot2::element_line(colour = NA),
        panel.grid.minor = ggplot2::element_line(colour = NA),
        legend.position = "bottom",
        plot.background = ggplot2::element_blank(),
      ) +
      ggplot2::guides(
        fill = ggplot2::guide_legend(nrow = 2, byrow = FALSE)
      )
  }
  
  # bilan des ecoulements selon 3 modalites
  bilan_cond_reg_typo_nat <- plot_bilan_prop(
    df_usuel_categ_obs_3mod_region %>% 
      dplyr::filter(Annee == anneeAVoir) %>%
      dplyr::mutate(lib_ecoul3mod = forcats::fct_rev(lib_ecoul3mod)),
    lib_ecoulement = lib_ecoul3mod, 
    regional = TRUE,
    modalites = c("Donnée manquante", "Observation impossible", "Assec", "Ecoulement non visible", "Ecoulement visible")
  )
  
  # bilan des ecoulements selon les 4 modalites
  bilan_cond_reg_typo_dep <- plot_bilan_prop(
    df_usuel_categ_obs_4mod_region %>% 
      dplyr::filter(Annee == anneeAVoir) %>%
      dplyr::mutate(lib_ecoul4mod = forcats::fct_rev(lib_ecoul4mod)), 
    lib_ecoulement = lib_ecoul4mod, 
    regional = TRUE,
    modalites = c("Donnée manquante", "Observation impossible", "Assec", "Ecoulement non visible", "Ecoulement visible faible", "Ecoulement visible acceptable")
  )
  
  ## Des assecs qui se suivent
#' Title
#'
#' @param df_assecs 
#' @param round_prec 
#'
#' @return
#' @export
#'
#' @examples
  plot_assecs_consecutifs <- function(df_assecs, round_prec = 1) {
    df_assecs %>%
      dplyr::ungroup() %>% 
      dplyr::filter(label != '0 mois') %>% 
      dplyr::mutate(label = factor(
        label, 
        levels = c(
          paste0(5:2, " mois consécutifs"), "1 mois"
        )
      )
      ) %>% 
      ggplot2::ggplot(
        mapping = ggplot2::aes(x = as.factor(Annee), y = pct, fill = label)
      ) + 
      ggplot2::geom_col(width = .95) +
      ggplot2::geom_text(
        mapping = ggplot2::aes(y = pct, label = nb_station), 
        fontface ="italic",
        size = 3.5,
        position = ggplot2::position_stack(vjust = 0.5),
        show.legend = FALSE
      ) +
      ggplot2::scale_fill_manual(
        values = c(
          "1 mois" = "#FFFFB2FF",
          "2 mois consécutifs" = "#FECC5CFF",
          "3 mois consécutifs" = "#FD8D3CFF",
          "4 mois consécutifs" = "#F03B20FF",
          "5 mois consécutifs" = "#BD0026FF"
        ),
        drop = FALSE
      ) +
      ggplot2::scale_y_continuous(labels = scales::percent_format(round_prec)) +
      ggplot2::scale_x_discrete(limits = factor(sort(unique(df_assecs$Annee)))) +
      ggplot2::ggtitle("Proportions et nombre de stations concernées") +
      ggplot2::theme_bw() +
      ggplot2::theme(
        title = ggplot2::element_text(size = 11,face = 'bold'), 
        axis.text.x = ggplot2::element_text(size = 11, angle = 45,hjust = 1),
        axis.text.y = ggplot2::element_text(size=11),
        panel.grid.major.x = ggplot2::element_blank(),
        panel.grid.minor = ggplot2::element_blank()
      ) +
      ggplot2::ylab(NULL) + 
      ggplot2::xlab(NULL)
  }
  
  # assec consecutifs au niveau regional
  assecs_consecutifs_reg <- plot_assecs_consecutifs(duree_assecs_df_usuel)
  
  # assec consecutifs au niveau departemental
  assecs_consecutifs_dep <- conf_dep %>% 
    purrr::map(
      function(d) {
        plot_assecs_consecutifs(
          duree_assecs_df_usuel_dep %>% 
            dplyr::filter(code_departement == d),
          round_prec = .1
        )
      }
    ) %>% 
    purrr::set_names(conf_dep)
  
  
  #### figure 2 : Situation des écoulements pour le GE campagne annee recente (mai a septembre)
  
  plot_ecoul_anneeAVoir <- ggplot2::ggplot(data_barplot_ecoul_AnneeRecente, 
                                           ggplot2::aes(fill= lib_ecoul3mod, x = Mois, y = NB)) +
    ggplot2::geom_bar(position = ggplot2::position_dodge(), stat = "identity") +
    ggplot2::geom_text(ggplot2::aes(label = ifelse(NB > 0, NB, "")), vjust = -1, color = "black",
                       position = ggplot2::position_dodge(0.9), size=3.5) +
    ggplot2::scale_fill_manual(values = mes_couleurs_3mod, drop = TRUE, name = "") +
    ggplot2::ylim(c(0, max(data_barplot_ecoul_AnneeRecente$NB) + 5)) +
    ggplot2::labs(x = "Mois des campagnes usuelles", y = "Nombre de stations",
                  caption = paste0("Données Onde, au ", Sys.Date()),
                  title = paste0("Situation des typologies nationales d'écoulement pour la région Grand Est - Campagne ", 
                                 unique(data_barplot_ecoul_AnneeRecente$Annee))) +
    ggplot2::theme_light() +
    ggplot2::theme(text = ggplot2::element_text(size = 12),
                   axis.text = ggplot2::element_text(size = 12)) +
    ggplot2::scale_x_discrete(labels = paste0(c("Mai", "Juin", "Juillet", "Août", "Septembre"), "\n 2024"))
  
  #### figure 3 : Situation des ecoulements pour le GE campagne du mois choisi
  # pour toutes les annees (2012- now)
  
  
  plot_ecoul_moisAVoir <- ggplot2::ggplot(data = data_barplot_ecoul_interAnnee, 
                                          ggplot2::aes(x = Annee, y = NB, fill = lib_ecoul3mod)) +
    ggplot2::geom_bar(position = ggplot2::position_stack(), stat = "identity") + # , color = "white"
    ggplot2::scale_fill_manual(values = mes_couleurs_3mod, drop = FALSE, name = "") +
    ggplot2::labs(x = "Années des campagnes", y = "Nombre de stations",
                  caption = paste0("Données Onde, au ", Sys.Date()),
                  title = paste0("Situation des typologies nationales d'écoulement pour la région Grand Est (",lab_moisAVoir," 2012 - 2024)")) +
    ggplot2::scale_x_continuous(breaks  = c(2012 : 2024), labels = paste0(lab_moisAVoir," \n", c(2012 : 2024))) + # 
    ggplot2::theme_light() +
    ggplot2::theme(text = ggplot2::element_text(size = 12),
                   axis.text = ggplot2::element_text(size = 12))
  
  
  #### Figure 4 : Indice onde suivant les departements campagne de l'anneeAVoir pour toutes
  ## les annees disponibles
  
  
  # New facet label names for dose variable
  dose.labs <- c("Ardennes", "Aube", "Marne",
                 "Haute-Marne", "Meurthe-et-Moselle", 
                 "Meuse", "Moselle", "Bas-Rhin",
                 "Haut-Rhin", "Vosges")
  names(dose.labs) <- c("08", "10" ,"51", "52", "54", "55", "57", "67", "68", "88")
  
  # ancien tableau 2
  
  plot_indice_moisAVoir <- ggplot2::ggplot(data_plot_indice_interAnnee, 
                                           ggplot2::aes(x = Annee, y = indice)) +
    ggplot2::geom_path(color = "grey") +
    ggplot2::geom_point(shape = 21, color = "black", size = 2,
                        ggplot2::aes(fill = ifelse(indice == 10 , "10", "B"))) +
    ggplot2::facet_wrap(~code_departement, ncol = 3, 
                        labeller = ggplot2::labeller(code_departement = dose.labs)) +
    # ggplot2::scale_x_date(breaks = "1 year", date_labels = "%Y") + 
    ggplot2::scale_x_continuous(breaks  = c(2012 : 2024), labels = c(2012 : 2024)) + 
    ggplot2::scale_y_continuous(breaks = c(0,2,4,6,8,10),limits = c(0,10)) +
    ggplot2::scale_fill_manual(values = c("10" = "#0077B6", "B" = "#48CAE4"), name = "", labels = c("Indice = 10", "Indice < 10")) +
    ggplot2::labs(x = paste0("Années de campagnes ONDE (mois de ", lab_moisAVoir,")"), y = "Valeurs d'indice",
                  caption = paste0("Données Onde, au ", Sys.Date()),
                  title = paste0("Notes d'indice pour les départements de la région Grand Est (", lab_moisAVoir," 2012 - 2024)")) +
    ggplot2::theme_bw() +
    ggplot2::theme(text = ggplot2::element_text(size = 12),
                   axis.text = ggplot2::element_text(size = 12),
                   axis.text.x = ggplot2::element_text(angle = 90, vjust = 0.5),
                   strip.text = ggplot2::element_text(size = 12, color = "black", face = "bold"),
                   strip.background = ggplot2::element_rect(fill = "white"),
                   legend.box = "vertical",
                   legend.position = c(0.5, 0.05))
  
  
  ## Sauvegarde
  save(
    bilan_cond_reg_typo_nat,
    bilan_cond_reg_typo_dep,
    assecs_consecutifs_reg,
    plot_ecoul_anneeAVoir,
    plot_ecoul_moisAVoir,
    plot_indice_moisAVoir,
    file = paste0(doss_mois, "/output/graphiques.rda")
  )
  
  print("Fin de la creation des graphiques")
}

rm(list= ls())
