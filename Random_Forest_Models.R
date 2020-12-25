

  
{ # Model all years   ----
  RF_Importance_All_Years <- readr::read_csv("App/Tidy_Data/Species_Importance_All_Years.csv")
  Top_30 <- RF_Importance_All_Years %>% 
    head(30) %>% 
    droplevels()
  
  Target_Colors <- c("Index Value" = "dodgerblue2", "Categorical" = "darkgoldenrod3",
                     "Targeted" = "mediumvioletred", "Non-targeted" = "mediumseagreen")
  
  Accuracy <- 
    ggplot(
      Top_30, aes(x = MeanDecreaseAccuracy, color = Targeted,
                  y = reorder(CommonName, MeanDecreaseAccuracy))) +
    geom_point() +
    geom_segment(
      size = 1, 
      aes(x = min(MeanDecreaseAccuracy) - .5, xend = MeanDecreaseAccuracy, 
          y = CommonName, yend = CommonName)) +
    labs(x = "Mean Decrease in % Accuracy", y = NULL, 
         color = NULL, linetype = NULL) +
    scale_x_continuous(expand = expansion(mult = c(0,.1)), 
                       limits = c(min(Top_30$MeanDecreaseAccuracy) - .5, NA)) +
    scale_color_manual(values = Target_Colors) +
    theme_classic() +
    theme(axis.text = element_text(size = 12),
          axis.title = element_text(size = 12),
          legend.text = element_text(size = 12))
  
  Gini <- ggplot(Top_30, aes(x = MeanDecreaseGini, color = Targeted, 
                             y = reorder(CommonName, MeanDecreaseGini))) +
    geom_point() +
    geom_segment(size = 1,
                 aes(x = min(MeanDecreaseGini) - .5, xend = MeanDecreaseGini,
                     y = CommonName, yend = CommonName)) +
    labs(x = "Mean Decrease in Gini Index", y = NULL, 
         color = NULL, linetype = NULL) +
    scale_x_continuous(expand = expansion(mult = c(0,.1)), 
                       limits = c(min(Top_30$MeanDecreaseGini) - .5, NA)) +
    scale_color_manual(values = Target_Colors) +
    theme_classic() +
    theme(axis.text = element_text(size = 12),
          axis.title = element_text(size = 12),
          legend.text = element_text(size = 12))
  ggarrange(Accuracy, Gini, ncol = 2, align = "h", common.legend = TRUE, legend = "bottom")
  
  
}

{ # Model > 2004   ----
  
  RF_Importance_2005 <- readr::read_csv("App/Tidy_Data/Species_Importance_2005.csv")
  
  Top_30 <- RF_Importance_2005 %>% 
    head(30) %>% 
    droplevels()
  
  Target_Colors <- c("Index Value" = "dodgerblue2", "Categorical" = "darkgoldenrod3",
                     "Targeted" = "mediumvioletred", "Non-targeted" = "mediumseagreen")
  
  Accuracy <- 
    ggplot(
      Top_30, aes(x = MeanDecreaseAccuracy, color = Targeted,
                  y = reorder(CommonName, MeanDecreaseAccuracy))) +
    geom_point() +
    geom_segment(
      size = 1, 
      aes(x = min(MeanDecreaseAccuracy) - .5, xend = MeanDecreaseAccuracy, 
          y = CommonName, yend = CommonName)) +
    labs(x = "Mean Decrease in % Accuracy", y = NULL, 
         color = NULL, linetype = NULL) +
    scale_x_continuous(expand = expansion(mult = c(0,.1)), 
                       limits = c(min(Top_30$MeanDecreaseAccuracy) - .5, NA)) +
    scale_color_manual(values = Target_Colors) +
    theme_classic() +
    theme(axis.text = element_text(size = 12),
          axis.title = element_text(size = 12),
          legend.text = element_text(size = 12))
  
  Gini <- 
    ggplot(
      Top_30, aes(x = MeanDecreaseGini, color = Targeted, 
                  y = reorder(CommonName, MeanDecreaseGini))) +
    geom_point() +
    geom_segment(size = 1,
                 aes(x = min(MeanDecreaseGini) - .5, xend = MeanDecreaseGini,
                     y = CommonName, yend = CommonName)) +
    labs(x = "Mean Decrease in Gini Index", y = NULL, 
         color = NULL, linetype = NULL) +
    scale_x_continuous(expand = expansion(mult = c(0,.1)), 
                       limits = c(min(Top_30$MeanDecreaseGini) - .5, NA)) +
    scale_color_manual(values = Target_Colors) +
    theme_classic() +
    theme(axis.text = element_text(size = 12),
          axis.title = element_text(size = 12),
          legend.text = element_text(size = 12))
  ggarrange(Accuracy, Gini, ncol = 2, align = "h", common.legend = TRUE, legend = "bottom")
}

