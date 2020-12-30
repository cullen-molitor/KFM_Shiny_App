
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
  # library(tidymodels)
  # library(rmarkdown)
  # library(zoo)
  # library(MASS)
  # library(vegan)
  # library(labdsv)
  # library(lme4)
  # library(car)
  # library(ggnewscale)
  
}

source('R/modules.R')

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
  
  
  
  
  Target_Colors <- c("Calculated Value" = "dodgerblue2", "Categorical" = "darkgoldenrod3",
                     "Targeted" = "mediumvioletred", "Non-targeted" = "mediumseagreen")
  
  Protocols <- c("1 m² Quadrats" = "1m", 
                 "5 m² Quadrats" = "5m",  
                 "Band Transects" = "bands", 
                 "Random Point Contacts" = "rpcs",
                 "Natural Habitat Size Frequencies" = "nhsf",
                 "Artificial Recruitment Modules" = "arms", 
                 "Roving Diver Fish Count" = "rdfc", 
                 "Visual Fish Transect" = "vft", 
                 "Fish Size Frequencies" = "fsf", 
                 "Video Taped Transects" = "vtt",
                 "Temperature Loggers" = "temp",
                 "Species List" = "species")
}

{ # Island and Site Information   -----
  Site_Info <- readr::read_csv("Meta_Data/Site_Info.csv")
  
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
  all_sites_theme <- function () {
    ggpubr::theme_classic2() +
      ggplot2::theme(
        # plot.title = element_text(hjust = 0.5, size = 18),
        # plot.subtitle = element_text(hjust = 0.5, size = 16),
        legend.position = "right",
        panel.grid.major = element_line(),
        legend.justification = c(0,0.5),
        legend.key.width = unit(.75, "cm"),
        legend.background = element_rect(size = unit(5, "cm")),
        legend.title = element_text(size = 12, color = "black"),
        legend.text = element_text(size = 10, colour = "black"),
        axis.title = element_text(hjust = .5, size = 18),
        axis.text.x = element_blank(),
        axis.text.y = element_text(size = 10),
        strip.text = element_text(size = 10, colour = "black", angle = 90))
  }
  timeseries_top_theme <- function () {
    ggpubr::theme_classic2() +
      ggplot2::theme(text = element_text(color="black"),
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
      ggplot2::theme(text = element_text(color="black"),
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
    theme_bw() + 
      theme(plot.title = element_text(size = 16, hjust = 0.5),
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
      ggplot2::theme(text = element_text(color="black"),
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
      ggplot2::theme(text = element_text(color="black"),
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

{ # Maps Data   ----
  
  CINP <- st_read("GIS_Data/california_islands.shp") %>%
    st_as_sf() %>%
    mutate(geometry = st_transform(geometry, "+proj=longlat +ellps=WGS84 +datum=WGS84")) %>%
    dplyr::filter(COUNTY_ID %in% c(520, 530, 519, 527, 528, 556, 557)) %>%
    dplyr::mutate(
      IslandName = case_when(
        COUNTY_ID == 520 ~ "San Miguel Island",
        COUNTY_ID == 530 ~ "Santa Rosa Island",
        COUNTY_ID == 519 ~ "Santa Cruz Island",
        COUNTY_ID == 527 | COUNTY_ID == 528 ~ "Anacapa Island",
        COUNTY_ID == 556 | COUNTY_ID == 557 ~ "Santa Barbara Island"))
  
  mpa <- st_read("GIS_Data/California_Marine_Protected_Areas.shp")
  
  mpa <- st_as_sf(mpa)
  
  marine <- mpa %>%
    filter(Type == "SMR" |
             Type == "FMCA" |
             Type == "SMCA" |
             Type == "FMR") %>%
    mutate(Color = ifelse(Type == "SMR", "red",
                          ifelse(Type == "SMCA", "blue",
                                 ifelse(Type == "FMR", "orange", "purple"))))
  
  TransectSites <- Site_Info %>% 
    filter(SiteNumber != 1 & SiteNumber != 5 & SiteNumber != 11) 
  
  transects <- st_read("GIS_Data/KFM_Transects_SmoothLine5.shp")  %>%
    st_as_sf() %>%
    mutate(geometry = st_transform(geometry, "+proj=longlat +ellps=WGS84 +datum=WGS84"))
  
  Transect_Endpoints <- readr::read_csv("GIS_Data/transect_0_100.csv")
  
  NPS_boundary <- st_read("GIS_Data/nps_boundary.shp") %>%
    st_as_sf()
  
  CINMS_boundary <- st_read("GIS_Data/cinms_py.shp") %>%
    st_as_sf()
  
  Buoys_List <- readr::read_csv("GIS_Data/Buoy_Stations.csv")
    
}

{ # Biodiversity Data    ----
  Diversity <- readr::read_csv("Tidy_Data/Diversity.csv")
}

{ # Mixed Data   ----
  Mixed_2005 <- readr::read_csv("Tidy_Data/Mixed_Data_Fish_Biomass.csv") %>%
    dplyr::mutate(SurveyYear = factor(SurveyYear),
                  IslandName = factor(IslandName),
                  ReserveStatus = factor(ReserveStatus)) %>% 
    dplyr::select(-SiteNumber, -SiteName, 
                  -IslandCode, -SiteCode) 
  RF_Reserve_Model_2005 <- base::readRDS("Models/RF_Reserve_Model_2005.rds")
  
  Mixed_All <- readr::read_csv("Tidy_Data/Mixed_Data_Fish_Density.csv") %>% 
    dplyr::filter(SiteCode != "MM" | SurveyYear > 2004) %>%
    dplyr::mutate(SurveyYear = factor(SurveyYear),
                  IslandName = factor(IslandName),
                  ReserveStatus = factor(ReserveStatus)) %>% 
    dplyr::select(-SiteNumber, -SiteName, 
                  -IslandCode, -SiteCode) 
  RF_Reserve_Model_All <- base::readRDS("Models/RF_Reserve_Model_All_Years.rds")
  
}

{ # Community Similarity Data   ----
  nMDS_2D_2005 <- readr::read_csv("Tidy_Data/nMDS_2d_2005.csv")
  nMDS_2D_All <- readr::read_csv("Tidy_Data/nMDS_2d_All.csv")
  
  nMDS_3D_2005 <- readr::read_csv("Tidy_Data/nMDS_3D_2005.csv")
  nMDS_3D_All <- readr::read_csv("Tidy_Data/nMDS_3D_All.csv") 
}

{ # Important Species Data  ---- 
  RF_Importance_All <- 
    readr::read_csv("Tidy_Data/Species_Importance_All_Years.csv")
  RF_Importance_2005 <-
    readr::read_csv("Tidy_Data/Species_Importance_2005.csv")
  
  rf_species_all <- c(as.character(RF_Importance_All$Common_Name))
  names(rf_species_all) <- c(RF_Importance_All$CommonName)
  
  
  rf_species_2005 <- c(as.character(RF_Importance_2005$Common_Name))
  names(rf_species_2005) <- c(RF_Importance_2005$CommonName)
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
      tags$a(href="https://www.nps.gov/im/medn/index.htm", "Mediterranean Coast Inventory & Monitoring Network Video", target = "_blank")
    )
  )
}






