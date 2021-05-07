
{ # Library   ----
  
  library(shiny)
  library(shinydashboard)
  library(shinyWidgets)
  library(plotly)
  library(tidyverse)
  library(ggpubr)
  library(glue)
  library(lubridate)
  library(scales)
  library(sf)
  library(leaflet)
  library(DT)
  library(pdp)
  library(randomForest)
  library(wordcloud)
  library(arrow)
  library(cachem)
  library(shinycssloaders)
  # library(ggnewscale)
  # library(iml)
  # library(tidymodels)
  # library(rmarkdown)
  # library(zoo)
  # library(MASS)
  # library(vegan)
  # library(labdsv)
  # library(lme4)
  # library(car)
  
}

source('modules.R')
# shinyOptions(cache = cachem::cache_disk("./myapp-cache"))

{ # Species and Trophic Levels   ----
  
  Species_Info <- readr::read_csv("Meta_Data/Species_Complete.csv")
  
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
  
  VFT_Species <- c(
    "blacksmith", 
    "black_surfperch", 
    "striped_surfperch", 
    "opaleye",                     
    "garibaldi", 
    "senorita",
    "kelp_bass", 
    "pile_perch",                 
    "kelp_rockfish", 
    "blue_rockfish", 
    "olive_rockfish", 
    "California_sheephead_female",
    "California_sheephead_male", 
    "rock_wrasse_female", 
    "rock_wrasse_male")
  
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
  
  
  
  
  Target_Colors <- c("Calculated Value" = "dodgerblue2", 
                     "Categorical" = "darkgoldenrod3",
                     "Targeted" = "mediumvioletred", 
                     "Non-targeted" = "mediumseagreen",
                     'Mixed' = "darkorchid")
  Target_Shapes <- c("Targeted" = 10, 
                     "Non-targeted" = 5, 
                     'Mixed' = 9)
  
  Protocols <- c("Species List" = "species",
                 "1 m² Quadrats" = "1m", 
                 "5 m² Quadrats" = "5m",  
                 "Band Transects" = "bands", 
                 "Random Point Contacts" = "rpcs",
                 "Natural Habitat Size Frequencies" = "nhsf",
                 "Artificial Recruitment Modules" = "arms", 
                 "Roving Diver Fish Count" = "rdfc", 
                 "Visual Fish Transect" = "vft", 
                 "Fish Size Frequencies" = "fsf", 
                 "Video Taped Transects" = "vtt",
                 "Temperature Loggers" = "temp")
}

{ # Sites   -----
  Site_Info <- readr::read_csv("Meta_Data/Site_Info.csv")
  
  site_data <- Site_Info %>%
    dplyr::mutate(Island = IslandName) %>% 
    dplyr::select(SiteNumber, Island, IslandName, SiteCode, SiteName, Reference, ReserveStatus, ARMs, ReserveYear,
                  Latitude, Longitude, MeanDepth, Rock, Cobble, Sand) %>%
    dplyr::rename(`Site #` = SiteNumber, `Site Code` = SiteCode, `Site` = SiteName,
                  Reference = Reference, `Reserve Status` = ReserveStatus, `Mean Depth` = MeanDepth, 
                  `Rock (%)` = Rock, `Cobble (%)` = Cobble, `Sand (%)` =  Sand) %>% 
    dplyr::mutate(Island = gsub(" Island", "", Island)) 
  
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
  
  SiteColor <- c(as.character(Site_Info$Color), as.character(Site_Info$Color))
  names(SiteColor) <- c(Site_Info$SiteName, Site_Info$SiteCode)
  
  SiteLine <- c(Site_Info$LineType, Site_Info$LineType)
  names(SiteLine) <- c(Site_Info$SiteName, Site_Info$SiteCode)
  
}

{ # Islands and MPAs   -----
  
  Island_Colors <- 
    c("San Miguel" = "darkmagenta", "San Miguel Island" = "darkmagenta", "SM" = "darkmagenta", 
      "Santa Rosa" = "dodgerblue4", "Santa Rosa Island" = "dodgerblue4", "SR" = "dodgerblue4", 
      "Santa Cruz" = "forestgreen", "Santa Cruz Island" = "forestgreen", "SC" = "forestgreen", 
      "Anacapa" = "darkorange", "Anacapa Island" = "darkorange", "AN" = "darkorange", 
      "Santa Barbara" = "firebrick2", "Santa Barbara Island" = "firebrick2", "SB" = "firebrick2", 
      "Inside" = "green", "Outside" = "red", "1978" = "green", "2003" = "dodgerblue2") 
  
  Island_Levels_Short <- c(
    "San Miguel", 
    "Santa Rosa", 
    "Santa Cruz", 
    "Anacapa", 
    "Santa Barbara")
  Island_Levels_Long <- c(
    "San Miguel Island", 
    "Santa Rosa Island", 
    "Santa Cruz Island", 
    "Anacapa Island",  
    "Santa Barbara Island")
  
  MPA_Levels_Short <- c("Santa Rosa", 
                        "Santa Cruz", 
                        "Anacapa",  
                        "Santa Barbara")
  MPA_Levels_Long <- c("Santa Rosa Island",
                       "Santa Cruz Island", 
                       "Anacapa Island",  
                       "Santa Barbara Island")
  
  
}

{ # SST Indicies  ----
  SST_Anomaly_Index <- arrow::read_feather("Tidy_Data/SST_Anomaly_Index.feather")
}

{ # Dive meta data  ----
  Dive_Meta_Data <- readr::read_csv("Meta_Data/Dive_Totals.csv")
  
  Total_Dives <- sum(Dive_Meta_Data$Dives)
  
  Divers <- mean(Dive_Meta_Data$Divers)
  
  Dive_Time <- sum(Dive_Meta_Data$Dive_Hours)
  
  Vessel_Time <- sum(Dive_Meta_Data$Vessel_Days)
  
}

{ # Plot Themes   ----
  
  map_bubble_theme <- function() {
    ggplot2::theme_void() +
      ggplot2::theme(legend.position = "right", 
                     plot.title = element_text(hjust = 0.5),
                     plot.subtitle = element_text(hjust = 0.5))
  }
  
  all_sites_theme <- function () {
    ggpubr::theme_classic2() +
      ggplot2::theme(
        legend.position = "right",
        panel.grid.major = element_line(),
        legend.justification = c(0, 0.5),
        legend.key.width = unit(.75, "cm"),
        legend.background = element_rect(size = unit(5, "cm")),
        legend.title = element_text(size = 12, color = "black"),
        legend.text = element_text(size = 10, colour = "black"),
        legend.spacing.y = unit(.01, 'cm'),
        legend.margin = ggplot2::margin(unit(0.1, "cm")),
        axis.title = element_text(hjust = .5, size = 12),
        axis.text.x = element_blank(),
        axis.text.y = element_text(size = 12),
        strip.text = element_text(size = 10, colour = "black", angle = 90))
  }
  
  timeseries_top_theme <- function () {
    ggpubr::theme_classic2() +
      ggplot2::theme(
        text = element_text(color = "black"),
        plot.caption = element_text(size = 11),
        legend.justification = c(0, 0.5),
        legend.key.width = unit(.75, "cm"),
        legend.title = element_text(size = 12, color = "black"),
        legend.text = element_text(size = 10, color = "black"),
        axis.title = element_text(size = 12, color="black"),
        axis.text.y = element_text(size = 12, color="black"),
        axis.text.x = element_blank(),
        panel.grid.major= element_line())
  }
  
  timeseries_bottom_theme <- function (){
    ggpubr::theme_classic2() +
      ggplot2::theme(
        text = element_text(color="black"),
        plot.caption = element_text(size = 13),
        legend.justification = c(0, 0.5),
        legend.key.width = unit(.75, "cm"),
        legend.title = element_text(size = 12, color = "black"),
        legend.text = element_text(size = 11, color = "black"),
        legend.margin = ggplot2::margin(unit(0.1, "cm")),
        axis.title = element_text(size = 12, color = "black"),
        axis.text.y = element_text(size = 12, color = "black"),
        axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1, size = 11, color="black"),
        axis.line.x = element_blank(),
        panel.grid.major= element_line())
  }
  
  nMDS_theme <- function () {
    ggplot2::theme_bw() + 
      ggplot2::theme(
        plot.title = element_text(size = 16, hjust = 0.5),
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
  
  Ratio_Wide_theme <- function () {
    ggpubr::theme_classic2() +
      ggplot2::theme(
        plot.title = element_text(hjust = 0.5, size = 16),
        panel.grid.major = element_line(),
        legend.position = "none",
        legend.justification = c(0.5,0.5),
        legend.background = element_rect(size = unit(5, "cm")),
        legend.title = element_text(size = 14, color = "black"),
        legend.text = element_text(size = 13, colour = "black"),
        axis.title = element_blank(),
        axis.text.y = element_text(size = 12),
        axis.text.x = element_text(size = 12, vjust = 1, hjust = 1, angle = 45),
        strip.text = element_text(size = 12, colour = "black", angle = 90))
  }
  
  Ratio_Long_theme <- function () {
    ggpubr::theme_classic2() +
      ggplot2::theme(
        plot.title = element_text(hjust = 0.5, size = 16),
        plot.subtitle = element_text(hjust = 0.5, size = 16),
        legend.position = "right",
        panel.grid.major = element_line(),
        legend.justification = c(0,0.5),
        legend.key.width = unit(.75, "cm"),
        legend.background = element_rect(size = unit(5, "cm")),
        legend.title = element_text(size = 14, color = "black"),
        legend.text = element_text(size = 13, colour = "black"),
        axis.title = element_text(hjust = .5, size = 18),
        axis.text = element_text(size = 12),
        strip.text = element_text(size = 14, colour = "black", angle = 90))
  }
  
  Biomass_Summary_theme <- function () {
    ggpubr::theme_classic() +
      ggplot2::theme(
        plot.title = element_text(hjust = 0.5, size = 18),
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
      ggplot2::theme(
        text = element_text(color="black"),
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
      ggplot2::theme(
        text = element_text(color="black"),
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
  
  Boxplot_theme <- function() {
    theme_classic() +
      theme(plot.title = element_text(size = 16, face = "italic"),
            plot.subtitle = element_text(size = 14),
            axis.title = element_text(size = 12),
            axis.text.y = element_text(size = 12),
            axis.text.x = element_text(size = 12, angle = 45, hjust = 1, vjust = 1),
            strip.text = element_text(size = 12, angle = 90),
            legend.position = "bottom",
            legend.background = element_rect(),
            legend.title = element_text(size = 12),
            legend.text = element_text(size = 10),
            plot.caption = element_text(size = 10, hjust = 0),
            axis.line.x = element_blank())
  }
  
}

{ # Maps Data   ----
  CINP <- sf::st_read(dsn = "GIS_Data/CINP_Islands.gpkg", layer = 'CINP_Islands')
  mpa <- sf::st_read(dsn = "GIS_Data/CA_MPA.gpkg", layer = "CA_MPA")
  GPS_Transects <- sf::st_read(dsn = "GIS_Data/KFM_Transects.gpkg", layer = "KFM_Transects")
  NPS_boundary <- sf::st_read(dsn = "GIS_Data/CINP_Boundary.gpkg", layer = "CINP_Boundary")
  CINMS_boundary <- sf::st_read(dsn = "GIS_Data/CINMS_Boundary.gpkg", layer = "CINMS_Boundary")
  Buoys_List <- readr::read_csv("GIS_Data/Buoy_Stations.csv")
}

{ # Biodiversity Data    ----
  Diversity <- arrow::read_feather("Tidy_Data/Diversity.feather")
}

{ # Mixed Data   ----
  Mixed_2005 <- arrow::read_feather("Tidy_Data/Mixed_Data_2005.feather") %>%
    dplyr::mutate(SurveyYear = factor(SurveyYear), 
                  IslandName = factor(IslandName),
                  ReserveStatus = factor(ReserveStatus)) %>% 
    dplyr::select(-SiteNumber, -SiteName,  -IslandCode, -SiteCode) 
  
  Mixed_All <- arrow::read_feather("Tidy_Data/Mixed_Data_All.feather") %>%
    dplyr::mutate(SurveyYear = factor(SurveyYear),
                  IslandName = factor(IslandName),
                  ReserveStatus = factor(ReserveStatus)) %>% 
    dplyr::select(-SiteNumber, -SiteName,  -IslandCode, -SiteCode) 
  
}

{ # Random Forest Models   ----
  RF_Reserve_Model_All <- base::readRDS("Models/RF_Reserve_Model_All.rds")
  RF_Reserve_Model_2005 <- base::readRDS("Models/RF_Reserve_Model_2005.rds")
  RF_Island_Model_All <- base::readRDS("Models/RF_Island_Model_All.rds")
  RF_Island_Model_2005 <- base::readRDS("Models/RF_Island_Model_2005.rds")
}

{ # Community Similarity Data   ----
  nMDS <- arrow::read_feather('Tidy_Data/nMDS.feather')
}

{ # Important Species Data  ---- 
  RF_Importance <- arrow::read_feather("Tidy_Data/RF_Importance.feather")
}

{ # Density and Biomass Data    ----
  Density <- arrow::read_feather("Tidy_Data/Density.feather")
  Biomass <- arrow::read_feather("Tidy_Data/Biomass.feather")
  
  All_Ratios <- arrow::read_feather("Tidy_Data/Ratios.feather")
}

{ # Sizes  ----
  Benthic_Sizes <- arrow::read_feather("Tidy_Data/Benthic_Sizes.feather")
  Fish_Sizes <- arrow::read_feather("Tidy_Data/Fish_Sizes.feather")
  ARM_Sizes <- arrow::read_feather("Tidy_Data/ARMs.feather")
  ARM_par_Sizes <- arrow::read_feather("Tidy_Data/ARMs_par.feather")
}

{ # Report Text   -----
  Text <- arrow::read_feather("Tidy_Data/Text.feather")
  Acronyms <- dplyr::arrange(data.table::fread("Meta_Data/Acronyms.csv", encoding = "Latin-1"))
}

{ # NPS Tags   ------
  NPS_Blog_tagList <- tagList(  
    tags$h4(
      "Our World Underwater Scholarship Society's (OWUSS) NPS Intern Blogs:",
      tags$br(),
      tags$a(href="http://blog.owuscholarship.org/2011/11/channel-islands-national-park/", 
             "2011 - Naomi Blinick", target = "_blank"),
      tags$br(),
      tags$a(href="http://blog.owuscholarship.org/2012/09/channel-islands-national-park-a-paradise-hidden-in-plain-sight/", 
             "2012 - Tim White", target = "_blank"),
      tags$br(),
      tags$a(href="http://blog.owuscholarship.org/2013/08/kelp-forest-monitoring-in-the-channel-islands/", 
             "2013 - Julia Mason", target = "_blank"),
      tags$br(),
      tags$a(href="http://blog.owuscholarship.org/2014/08/exploring-the-mysteries-of-the-channel-islands/", 
             "2014 - Yasmeen Smalley", target = "_blank"),
      tags$br(),
      tags$a(href="http://blog.owuscholarship.org/2015/07/cruising-monitoring-at-the-channel-islands/", 
             "2015 - Michael Spector", target = "_blank"),
      tags$br(),
      tags$a(href="http://blog.owuscholarship.org/2016/10/ventura-channel-islands-national-park/", 
             "2016 - Garret Fundakowski", target = "_blank"),
      tags$br(),
      tags$a(href="https://blog.owuscholarship.org/2018/02/a-kelp-forest-homecoming-at-channel-islands-national-park/", 
             "2017 - Shaun Wolfe", target = "_blank"),
      tags$br(),
      tags$a(href="http://blog.owuscholarship.org/2018/06/channel-islands-national-parks-exploring-the-majestic-kelp-forests/", 
             "2018 - Shannon Brown", target = "_blank"),
      tags$br(),
      tags$a(href="http://blog.owuscholarship.org/2019/10/exploring-submerged-forests-at-channel-islands-national-park/", 
             "2019 - Michael Langhans", target = "_blank"),
      tags$br(), tags$br(),
      "Channel Islands National Park's lectures and videos:",
      tags$br(),
      tags$a(href="https://www.youtube.com/watch?v=86foiSxQmVU", "KFM Lecture Part 1", target = "_blank"),
      tags$br(),
      tags$a(href="https://www.youtube.com/watch?v=pv9N2xDu0y8", "KFM Lecture Part 2", target = "_blank"),
      tags$br(),
      tags$a(href="https://www.nps.gov/im/medn/index.htm", "Inventory & Monitoring Network Video", target = "_blank"),
      tags$br(),
      tags$a(href="https://www.nps.gov/chis/planyourvisit/channel-islands-live-nps.htm", "Channel Islands Live (CHIL)", target = "_blank")
    )
  )
}




