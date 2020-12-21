


{ # Data  ----
  
  { # Inverts and Algae Only (all years)  ----
    Mixed_Data_Fish_Density <- readr::read_csv("App/Tidy_Data/Mixed_Data_Fish_Density.csv") %>%
      dplyr::mutate(SurveyYear = factor(SurveyYear),
                    IslandName = factor(IslandName),
                    ReserveStatus = factor(ReserveStatus)) %>% 
      dplyr::select(-SiteNumber, -SiteName, -IslandCode, -SiteCode) 
  }
  
  { # With fish Biomass (> 2004)  ----
    Mixed_Data_Fish_Biomass <- readr::read_csv("App/Tidy_Data/Mixed_Data_Fish_Biomass.csv") %>%
      dplyr::mutate(SurveyYear = factor(SurveyYear),
                    IslandName = factor(IslandName),
                    ReserveStatus = factor(ReserveStatus)) %>% 
      dplyr::select(-SiteNumber, -SiteName, 
                    -IslandCode, -SiteCode) 
  }
  
}

      
{ # Model all years   ----
  set.seed(100)
  train <- sample(nrow(Random_Kelp_Forest), 
                  0.7 * nrow(Random_Kelp_Forest), replace = FALSE)
  
  TrainSet <- Random_Kelp_Forest[train,]
  ValidSet <- Random_Kelp_Forest[-train,]
  
  RF_Reserve_Model <- randomForest::randomForest(
    data = Random_Kelp_Forest,
    ReserveStatus ~ ., ntree = 3000, mtry = 8,
    importance = TRUE, proximity = TRUE, keep.forest = TRUE)
  
  varImpPlot(RF_Reserve_Model)
  
  RF_Importance <- randomForest::importance(RF_Reserve_Model) %>%
    as.data.frame() %>%
    tibble::rownames_to_column("Common_Name") %>%
    dplyr::arrange(desc(MeanDecreaseAccuracy)) 
  # %>% 
  #   dplyr::left_join(Mixed_Data_xRef) %>% 
  #   dplyr::mutate(CommonName = factor(CommonName),
  #                 ScientificName = factor(ScientificName))
  Top_30 <- RF_Importance %>% 
    head(30) %>% 
    droplevels()
  
  # Target_Colors <- c("Calculated Value" = "dodgerblue2", "Categorical" = "darkgoldenrod3",
  #                    "Targeted" = "mediumvioletred", "Non-targeted" = "mediumseagreen")
  
  Accuracy <- ggplot(Top_30, aes(x = MeanDecreaseAccuracy, 
                                 y = reorder(Common_Name, MeanDecreaseAccuracy))) +
    geom_point() +
    geom_segment(size = 1, 
                 aes(x = min(MeanDecreaseAccuracy) - .5, xend = MeanDecreaseAccuracy,
                     y = Common_Name, yend = Common_Name)) +
    labs(x = "Mean Decrease in % Accuracy", y = NULL, 
         color = NULL, linetype = NULL) +
    scale_x_continuous(expand = expansion(mult = c(0,.1)), 
                       limits = c(min(Top_30$MeanDecreaseAccuracy) - .5, NA)) +
    scale_color_manual(values = Target_Colors) +
    theme_classic() +
    theme(axis.text = element_text(size = 12),
          axis.title = element_text(size = 12),
          legend.text = element_text(size = 12))
  
  Gini <- ggplot(Top_30, aes(x = MeanDecreaseGini, 
                             y = reorder(Common_Name, MeanDecreaseGini))) +
    geom_point() +
    geom_segment(size = 1,
                 aes(x = min(MeanDecreaseGini) - .5, xend = MeanDecreaseGini,
                     y = Common_Name, yend = Common_Name)) +
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
  
  set.seed(100)
  train <- sample(nrow(Mixed_Data_Fish_Biomass), 
                  0.7 * nrow(Mixed_Data_Fish_Biomass), replace = FALSE)
  
  TrainSet <- Mixed_Data_Fish_Biomass[train,]
  ValidSet <- Mixed_Data_Fish_Biomass[-train,]
  
  RF_Reserve_Model <- randomForest::randomForest(
    data = Mixed_Data_Fish_Biomass,
    ReserveStatus ~ ., ntree = 3000, mtry = 8,
    importance = TRUE, proximity = TRUE, keep.forest = TRUE)
  
  varImpPlot(RF_Reserve_Model)
  
  RF_Importance <- randomForest::importance(RF_Reserve_Model) %>%
    as.data.frame() %>%
    tibble::rownames_to_column("Common_Name") %>%
    dplyr::arrange(desc(MeanDecreaseAccuracy)) 
  # %>% 
  #   dplyr::left_join(Mixed_Data_xRef) %>% 
  #   dplyr::mutate(CommonName = factor(CommonName),
  #                 ScientificName = factor(ScientificName))
  Top_30 <- RF_Importance %>% 
    head(30) %>% 
    droplevels()
  
  # Target_Colors <- c("Calculated Value" = "dodgerblue2", "Categorical" = "darkgoldenrod3",
  #                    "Targeted" = "mediumvioletred", "Non-targeted" = "mediumseagreen")
  
  Accuracy <- ggplot(Top_30, aes(x = MeanDecreaseAccuracy, 
                                 y = reorder(Common_Name, MeanDecreaseAccuracy))) +
    geom_point() +
    geom_segment(size = 1, 
                 aes(x = min(MeanDecreaseAccuracy) - .5, xend = MeanDecreaseAccuracy,
                     y = Common_Name, yend = Common_Name)) +
    labs(x = "Mean Decrease in % Accuracy", y = NULL, 
         color = NULL, linetype = NULL) +
    scale_x_continuous(expand = expansion(mult = c(0,.1)), 
                       limits = c(min(Top_30$MeanDecreaseAccuracy) - .5, NA)) +
    scale_color_manual(values = Target_Colors) +
    theme_classic() +
    theme(axis.text = element_text(size = 12),
          axis.title = element_text(size = 12),
          legend.text = element_text(size = 12))
  
  Gini <- ggplot(Top_30, aes(x = MeanDecreaseGini, 
                             y = reorder(Common_Name, MeanDecreaseGini))) +
    geom_point() +
    geom_segment(size = 1,
                 aes(x = min(MeanDecreaseGini) - .5, xend = MeanDecreaseGini,
                     y = Common_Name, yend = Common_Name)) +
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


{ # 3-D nMDS Plot  ----
  library(plotly)
  
  Mixed_Data_Fish_Biomass <- readr::read_csv("App/Tidy_Data/Mixed_Data_Fish_Biomass.csv") 
  
  Random_Kelp_Forest <- Mixed_Data_Fish_Biomass%>%
    dplyr::mutate(SurveyYear = factor(SurveyYear),
                  IslandName = factor(IslandName),
                  ReserveStatus = factor(ReserveStatus)) %>% 
    dplyr::select(-SiteNumber, -SiteName, 
                  -IslandCode, -SiteCode) 
  
  RF_Reserve_Model <- randomForest::randomForest(
    data = Random_Kelp_Forest,
    ReserveStatus ~ ., ntree = 3000, mtry = 8,
    importance = TRUE, proximity = TRUE, keep.forest = TRUE)
  
  nMDS_3D <- randomForest::MDSplot(
    RF_Reserve_Model, fac = Random_Kelp_Forest$ReserveStatus,
    k = 3, palette = rep(1, 2),
    pch = as.numeric(Random_Kelp_Forest$ReserveStatus))
  
  nMDS_3D <- unlist(nMDS_3D$points) %>%
    as.data.frame() %>%
    cbind(dplyr::select(
      Mixed_Data_Fish_Biomass, SiteCode, SiteName, IslandName, ReserveStatus, SurveyYear)) %>% 
    readr::write_csv("App/Tidy_Data/nMDS_3D.csv")
  
  fig <-
    plotly::plot_ly(nMDS_3D, x = ~`Dim 1`, y = ~`Dim 2`, z = ~`Dim 3`,
                    frame = ~SurveyYear, text = ~SiteName, hoverinfo = "text",
                    color = ~ReserveStatus, colors = Island_Colors) %>%
    plotly::add_markers(symbol = ~ReserveStatus,
                        symbols = c('Inside' = "triangle", 'Outside' = "square")) %>%
    plotly::add_text(text = ~SiteCode) %>%
    plotly::layout(scene = list(xaxis = list(title = 'Dim 1'),
                                yaxis = list(title = 'Dim 2'),
                                zaxis = list(title = 'Dim 3'))) %>%
    plotly::animation_opts(1500, easing = "linear")
  fig
  
}
