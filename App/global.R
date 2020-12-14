
{ # Library   ----
  
  library(shiny)
  library(shinydashboard)
  library(shinyWidgets)
  library(tidyverse)
  library(tidymodels)
  library(ggpubr)
  library(glue)
  library(lubridate)
  library(rmarkdown)
  library(zoo)
  library(MASS)
  library(vegan)
  library(labdsv)
  library(lme4)
  library(car)
  library(knitr)
  library(tinytex)
  library(Cairo)
  library(ggnewscale)
  library(randomForest)
  library(pdp)
  library(broom)
  library(scales)
  
}

source('R/modules.R')

{ # Species and Trophic Levels   ----
  
  Species_Info <- readr::read_csv("Meta_Data/Species_Complete.csv")
  
  # Mixed_Data_xRef <- readr::read_csv("Meta_Data/Mixed_Data_xref.csv")
  
  Benthic_Biomass_Species <- c(
    "Crassedoma giganteum", "rock scallop",
    "Haliotis rufescens", "red abalone",
    "Kelletia kelletii", "Kellet's whelk",           
    "Lithopoma gibberosa", "red turban snail", 
    "Lytechinus anamesus", "white sea urchin", 
    "Megastraea undosa", "wavy turban snail",             
    "Megathura crenulata", "giant keyhole limpet", 
    "Patiria miniata", "bat star",
    "Pisaster giganteus", "giant-spined sea star",            
    "Pycnopodia helianthoides", "sunflower star",
    "Strongylocentrotus franciscanus", "red sea urchin", 
    "Strongylocentrotus purpuratus", "purple sea urchin",
    "Tethya aurantia", "orange puffball sponge", 
    "Tegula regina", "queen tegula", 
    "Macrocystis pyrifera", "giant kelp",
    "Muricea californica", "California golden gorgonian",
    "Muricea fruticosa", "brown gorgonian",
    "Lophogorgia chilensis", "red gorgonian")
  
  Potential_Biomass_Additions <- c(
    "Astrangia lajollaensis", "Corynactis californicus",
    "Phragmatopoma californica", "Serpulorbis squamiger",
    "Diaperoecia californica")
  
  Fish_Biomass_Species <- c(
    "Caulolatilus princeps", "ocean whitefish",
    "Chromis punctipinnis", "blacksmith",
    "Embiotoca jacksoni", "black surfperch",
    "Embiotoca lateralis", "striped surfperch",
    "Girella nigricans", "opaleye",
    "Halichoeres semicinctus", "rock wrasse, female", "rock wrasse, male",     
    "Hypsypops rubicundus", "garibaldi",
    "Medialuna californiensis", "halfmoon",
    "Ophiodon elongatus", "lingcod",
    "Oxyjulis californica", "senorita",
    "Paralabrax clathratus", "kelp bass",
    "Rhacochilus toxotes", "rubberlip surfperch",
    "Rhacochilus vacca", "pile perch",
    "Scorpaena guttata", "California scorpionfish",
    "Scorpaenichthys marmoratus", "cabezon",
    "Sebastes atrovirens", "kelp rockfish",
    "Sebastes chrysomelas", "black and yellow rockfish",
    "Sebastes mystinus", "blue rockfish",  
    "Sebastes serranoides", "olive rockfish",
    "Sebastes serriceps", "treefish",
    "Semicossyphus pulcher", "California sheephead, male", "California sheephead, female")
  
  BenthicBiomassColor <- c(
    'Lithopoma gibberosa' = "deeppink", 
    'Megastraea undosa' = "grey40", 
    'Patiria miniata' = "orange2", 
    'Strongylocentrotus franciscanus' = "red2", 
    'Strongylocentrotus purpuratus' = "darkorchid2", 
    'Tegula regina' = "yellow", 
    'Pisaster giganteus' = "deepskyblue2", 
    'Crassedoma giganteum' = "gold", 
    'Haliotis rufescens' = "firebrick1",             
    'Kelletia kelletii' = "black",
    'Lytechinus anamesus' = "grey80", 
    'Megathura crenulata' = "aquamarine2", 
    'Pycnopodia helianthoides' = "aquamarine2",       
    'Tethya aurantia' = "gold1", 
    'Macrocystis pyrifera' = "forestgreen", 
    'Lophogorgia chilensis' = "red",       
    'Muricea californica' = "gold4", 
    'Muricea fruticosa' = "brown")
  
  oneM_Biomass_Species <- c(
    "Megastraea undosa",  #  All years
    "Lithopoma gibberosa",  #  All years
    "Tegula regina", # 2006 to present (when they were added as a species)
    "Patiria miniata",  #  All years               
    "Pisaster giganteus", # 1982 - 1995 then 5 m to 2013
    "Strongylocentrotus franciscanus",  #  All years
    "Strongylocentrotus purpuratus")  #  All years
  fiveM_Biomass_Species <- c("Pisaster giganteus") # 1996 - 2013 then to Band Transects
  bands_Biomass_Species <- c(
    "Tethya aurantia", #  All years 
    "Haliotis rufescens",  #  All years
    "Kelletia kelletii",  #  All years
    "Megathura crenulata",  #  All years
    "Crassedoma giganteum", #  All years
    "Pisaster giganteus",  #  2014 - present 
    "Pycnopodia helianthoides",  #  All years
    "Lytechinus anamesus")  #  All years
  
  SpeciesColor <- c(as.character(Species_Info$Color))
  names(SpeciesColor) <- c(Species_Info$CommonName)
  SpeciesColor <- SpeciesColor[!is.na(SpeciesColor)]
}

{ # Island and Site Information   -----
  Site_Info <- readr::read_csv("Meta_Data/Site_Info.csv")
  
  Island_Colors <- c("San Miguel" = "darkmagenta", "SM" = "darkmagenta", 
                     "Santa Rosa" = "dodgerblue4", "SR" = "dodgerblue4", 
                     "Santa Cruz" = "forestgreen", "SC" = "forestgreen", 
                     "Anacapa" = "darkorange", "AN" = "darkorange", 
                     "Santa Barbara" = "firebrick2","SB" = "firebrick2", 
                     "Inside" = "green", "Outside" = "red") 
  
  Island_Levels <- c(
    "San Miguel", "San Miguel Island", 
    "Santa Rosa", "Santa Rosa Island", 
    "Santa Cruz", "Santa Cruz Island", 
    "Anacapa", "Anacapa Island",  
    "Santa Barbara", "Santa Barbara Island")
  
  MPA_Levels <- c("Santa Rosa", "Santa Cruz", "Anacapa",  "Santa Barbara")
  
  SiteLevels <- c(
    # San Miguel
    "Wyckoff Ledge", "Miracle Mile", # Out
    "Hare Rock", # In
    # Santa Rosa
    "Johnson's Lee North", "Johnson's Lee South", "Rodes Reef", "Cluster Point", # Out
    "Trancion Canyon", "Chickasaw", "South Point", # In
    # Santa Cruz
    "Fry's Harbor", "Pelican Bay", "Yellow Banks", "Devil's Peak Member", "Pedro Reef", "Little Scorpion", # Out
    "Gull Island South", "Scorpion Anchorage", "Potato Pasture", "Cavern Point",  # In
    # Anacapa
    "Admiral's Reef", "East Fish Camp", "Lighthouse",  # Out
    "Cathedral Cove" , "Landing Cove", "Black Sea Bass Reef", "Keyhole",  # In
    # Santa Barbara
    "Arch Point", "Cat Canyon", "Webster's Arch",  # Out
    "SE Sea Lion Rookery", "Graveyard Canyon", "Southeast Reef") # In
  
  SiteColor <- as.character(Site_Info$Color)
  names(SiteColor) <- Site_Info$SiteName
  
  SiteLine <- Site_Info$LineType
  names(SiteLine) <- Site_Info$SiteName
}

{ # SST Indicies  ----
  SST_Anomaly_Index <- readr::read_csv("Tidy_Data/SST_Anomaly_Index.csv")
}

{ # Dive meta data  ----
  Dive_Meta_Data <- readr::read_csv("Meta_Data/Dive_Totals.csv")
  
  Total_Dives <- sum(Dive_Meta_Data$Dives)
  
  Divers <- mean(Dive_Meta_Data$Divers)
  
  Dive_Time <- sum(Dive_Meta_Data$Dive_Hours)
  
  Vessel_Time <- sum(Dive_Meta_Data$Vessel_Days)
  
}

{ # Custom Functions   ----
  is.even <- function(x) x %% 2 == 0
  is.odd <- function(x) x %% 2 != 0 
  
  boot_ratio <- function (data, indices) {
    sample = data[indices, ]
    ratio = mean(sample$Mean_Biomass[sample$ReserveStatus == "Inside"])/
      mean(sample$Mean_Biomass[sample$ReserveStatus == "Outside"])
    return(ratio)
  }
  timeseries_top_theme <- function () {
    ggpubr::theme_classic2() +
      ggplot2::theme(text = element_text(color="black", family ="Cambria"),
                     plot.caption = element_text(size = 11),
                     legend.justification = c(0, 0.5),
                     legend.key.width = unit(.75, "cm"),
                     legend.title = element_text(size = 12, color = "black"),
                     legend.text = element_text(size = 11, color = "black"),
                     axis.title = element_text(size = 12, color="black"),
                     axis.text.y = element_text(size = 12, color="black"),
                     axis.text.x = element_blank(),
                     panel.grid.major= element_line())
  }
  timeseries_bottom_theme <- function (){
    ggpubr::theme_classic2() +
      ggplot2::theme(text = element_text(color="black", family ="Cambria"),
                     plot.caption = element_text(size = 13),
                     legend.justification = c(0, 0.5),
                     legend.key.width = unit(.75, "cm"),
                     legend.title = element_text(size = 12, color = "black"),
                     legend.text = element_text(size = 11, color = "black"),
                     legend.margin = unit(0.1, "cm"),
                     axis.title = element_text(size = 12, color = "black"),
                     axis.text.y = element_text(size = 12, color = "black"),
                     axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1, size = 11, color="black"),
                     axis.line.x = element_blank(),
                     panel.grid.major= element_line())
  }
  nMDS_theme <- function () {
    theme_bw() + 
      theme(plot.title = element_text(size = 10, hjust = 0.5),
            axis.text = element_blank(), 
            axis.ticks = element_blank(), 
            axis.title = element_blank(), 
            panel.background = element_blank(), 
            panel.grid.major = element_blank(),  
            panel.grid.minor = element_blank(),  
            plot.background = element_blank(),
            plot.caption = element_text(size=9, hjust = 0),
            aspect.ratio=1) 
  }
  Ratio_theme <- function () {
    theme_classic2() +
      theme(plot.title = element_text(hjust = 0.5, size = 16),
            panel.grid.major = element_line(),
            legend.position = "none",
            legend.justification = c(0.5,0.5),
            legend.background = element_rect(size = unit(5, "cm")),
            legend.title = element_text(size = 14, color = "black"),
            legend.text = element_text(size = 13, colour = "black"),
            axis.title = element_blank(),
            axis.text = element_text(size = 12),
            strip.text = element_text(size = 12, colour = "black", angle = 90))
  }
  Biomass_Summary_theme <- function () {
    ggplot2::theme_classic() +
      ggplot2::theme(plot.title = element_text(hjust = 0.5, size = 18),
                     plot.subtitle = element_text(hjust = 0.5, size = 16),
                     plot.caption = element_text(hjust = 0),
                     panel.border = element_rect(fill = FALSE),
                     legend.position = "bottom",
                     legend.justification = c(0.5, 0.5),
                     legend.title = element_text(size = 12, color = "black"),
                     legend.text = element_text(size = 9, color = "black"),
                     axis.title = element_text(size = 16),
                     axis.text.y = element_text(size = 12), 
                     axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1, size = 12),
                     strip.text = element_text(size = 10, colour = "black"))
  }
  Original_16_top_theme <- function () {
    ggpubr::theme_classic2() +
      ggplot2::theme(text = element_text(color="black", family ="Cambria"),
                     legend.position = "right",
                     legend.justification = c(0, 0.5),
                     legend.key.width = unit(.75, "cm"),
                     legend.title = element_text(size = 12, color = "black"),
                     legend.text = element_text(size = 11, color = "black"),
                     axis.title = element_text(hjust = .5, size = 12),
                     axis.text.y = element_text(size = 12, color="black"),
                     axis.text.x = element_blank(),
                     panel.grid.major= element_line())
  }
  Original_16_bottom_theme <- function () {
    ggpubr::theme_classic2() +
      ggplot2::theme(text = element_text(color="black", family ="Cambria"),
                     legend.position = "right",
                     legend.justification = c(0,0.5),
                     legend.key.width = unit(.75, "cm"),
                     legend.background = element_rect(size = unit(5, "cm")),
                     legend.title = element_text(size = 12, color = "black"),
                     legend.text = element_text(size = 11, colour = "black"),
                     panel.grid.major = element_line(),
                     axis.title = element_text(hjust = .5, size = 12),
                     axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1, size = 12, color="black"),
                     axis.text.y = element_text(size = 12, color="black"),
                     axis.line.x = element_blank(),
                     strip.text = element_text(size = 10, colour = "black", angle = 90))
  }
}















