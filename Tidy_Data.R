#
#
#
#
#     Use for Manipulating data frames 
#
#
#
#
#

Export_END_Year <- 2019

{ # Library   ----
 
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

{ # Metadata    ----
  
  Species_Info <- readr::read_csv("App/Meta_Data/Species_Complete.csv")
  
  Mixed_Data_xRef_Biomass <- readr::read_csv("App/Meta_Data/Mixed_Data_xref_Fish_Biomass.csv")
  
  Mixed_Data_xRef_Density <- readr::read_csv("App/Meta_Data/Mixed_Data_xref_Fish_Density.csv")
  
  Site_Info <- readr::read_csv("App/Meta_Data/Site_Info.csv")
  
  Fish_Biomass_Species <- c(
    "Caulolatilus princeps", "ocean whitefish", "ocean_whitefish",
    "Chromis punctipinnis", "blacksmith", 
    "Embiotoca jacksoni", "black surfperch", "black_surfperch",
    "Embiotoca lateralis", "striped surfperch", "striped_surfperch",
    "Girella nigricans", "opaleye", 
    "Halichoeres semicinctus", "rock wrasse, female", "rock wrasse, male",   
                              "rock_wrasse_female", "rock_wrasse_male",  
    "Hypsypops rubicundus", "garibaldi",
    "Medialuna californiensis", "halfmoon", 
    "Ophiodon elongatus", "lingcod", 
    "Oxyjulis californica", "senorita",
    "Paralabrax clathratus", "kelp bass", "kelp_bass",
    "Rhacochilus toxotes", "rubberlip surfperch", "rubberlip_surfperch",
    "Rhacochilus vacca", "pile perch", "pile_perch",
    "Scorpaena guttata", "California scorpionfish", "California_scorpionfish",
    "Scorpaenichthys marmoratus", "cabezon",
    "Sebastes atrovirens", "kelp rockfish", "kelp_rockfish",
    "Sebastes chrysomelas", "black and yellow rockfish", "black_and_yellow_rockfish",
    "Sebastes mystinus", "blue rockfish", "blue_rockfish",  
    "Sebastes serranoides", "olive rockfish", "olive_rockfish",
    "Sebastes serriceps", "treefish", 
    "Semicossyphus pulcher", "California sheephead, male", "California sheephead, female",
                             "California_sheephead_male", "California_sheephead_female")
  
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

}

{ # SST Anomaly Index (ONI and PDO)   ----
  
  { # Oceanic Nino Index  ----
    oni <- read.table( # Read in  ONI to be added to all data
      "https://origin.cpc.ncep.noaa.gov/products/analysis_monitoring/ensostuff/detrend.nino34.ascii.txt",
      header = T) %>%
      dplyr::mutate(Date = as.Date(ISOdate(YR, MON, 1)),
                    DateStart = as.Date(ISOdate(YR, MON, 1)),
                    DateEnd = ceiling_date(DateStart, "month")) %>%
      dplyr::rename(ONI_ANOM = ANOM,
                    Month = MON,
                    SurveyYear = YR) %>% 
      dplyr::select(SurveyYear, Month, Date, DateStart, DateEnd, ONI_ANOM) 
    
  }
  
  { # PDO  ----
    pdo <- read.table(
      "https://www.cpc.ncep.noaa.gov/products/GODAS/PDO/pdo_h300_pac_current.txt",
      header = T)  %>%
      dplyr::mutate(Date = as.Date(ISOdate(Year, Month, 1)),
                    DateStart = as.Date(ISOdate(Year, Month, 1)),
                    DateEnd = ceiling_date(DateStart, "month")) %>%
      dplyr::rename(PDO_ANOM = PDO,
                    SurveyYear = Year) %>% 
      dplyr::select(SurveyYear, Month, Date, DateStart, DateEnd, PDO_ANOM)
    
  }
  
  { # Full Index  ----
    SST_Anomaly_Index <- dplyr::left_join(oni, pdo) %>% 
      readr::write_csv("App/Tidy_Data/SST_Anomaly_Index.csv")
  }
  
}

{ # Benthic Density   ----
  
  { # 1 m Density     ----
    One_M_Density <- readr::read_csv(
      glue("Raw_Data/KFM_1mQuadrat_RawData_1982-{Export_END_Year}.txt"),   
      col_types = cols(CountA = col_number(), CountB = col_number())) %>%
      dplyr::filter(IslandCode != "CL",
                    ScientificName != "Lithopoma gibberosa" | SurveyYear > 2002) %>%
      dplyr::left_join(Site_Info) %>%
      tidyr::separate(SurveyDate, c('Date','Time'),' ') %>%
      dplyr::mutate(Date = lubridate::mdy(Date)) %>%
      tidyr::pivot_longer(cols = c(CountA, CountB), values_to = "Count") %>% 
      dplyr::filter(!is.na(Count), !is.na(CommonName),
                    !CommonName %in% c(
                      "giant kelp stipes > 1m",
                      'giant kelp, all',
                      "California sea palm, all",
                      "oar weed, all",
                      "Southern sea palm, all"),
                    !ScientificName %in% c(
                      "Undaria pinnatifida",
                      "Dictyoneuropsis reticulata/Agarum fimbriatum",
                      "Haliotis rufescens",
                      "Crassedoma giganteum",
                      "Kelletia kelletii",
                      "Oxylebius pictus",
                      "Pycnopodia helianthoides",
                      "Lytechinus anamesus",
                      "Sargassum horneri")) %>% 
      dplyr::group_by(SiteNumber, IslandCode, IslandName, SiteCode, SiteName, Species, ScientificName,
                      CommonName, SurveyYear, Date, ReserveStatus, MeanDepth, Reference) %>%
      dplyr::summarise(Count_To_Reuse = Count,
                       Area_Surveyed = ifelse(SurveyYear %in% 1985:1994, n() * 2, n()),
                       Total_Count = base::sum(Count),
                       Mean_Density = base::round(Total_Count / Area_Surveyed, 4), 
                       SD = base::round(stats::sd(Count), 4),
                       SE = base::round(SD / base::sqrt(Area_Surveyed), 4),
                       Survey_Type = "One_Meter") %>% 
      dplyr::ungroup() %>%  
      dplyr::distinct(SiteNumber, IslandCode, IslandName, SiteCode, SiteName, ScientificName, SurveyYear, 
                      Total_Count, Mean_Density, SD, SE, Area_Surveyed, .keep_all = TRUE)  %>% 
      dplyr::filter(ScientificName != "Pisaster giganteus" | SurveyYear < 1996,
                    ScientificName != "Macrocystis pyrifera" | SurveyYear < 1996) %>%
      dplyr::arrange(SiteNumber, SurveyYear, ScientificName) %>%
      dplyr::select(SiteNumber, IslandCode, IslandName, SiteCode, SiteName, SurveyYear, Date,
                    Species, ScientificName, CommonName, Mean_Density, SD, SE, 
                    Area_Surveyed, MeanDepth, Survey_Type, ReserveStatus, Reference) 
  } 
  
  { # 5 m Density    ----
    
    Macro_Combo <- 
      readr::read_csv(
        glue::glue("Raw_Data/KFM_5mQuadrat_RawData_1996-{Export_END_Year}.txt")) %>%
      dplyr::filter(Species == 2002.25 | Species == 2002.75) %>%
      dplyr::mutate(CommonName = "giant kelp, adult (>1m)",
                    Species = 2002) %>% 
      dplyr::group_by(SiteNumber, SurveyYear, QuadratNumber) %>% 
      dplyr::mutate(Count = sum(Count)) %>% 
      dplyr::distinct(.keep_all = TRUE) %>% 
      dplyr::ungroup()
    
    Sargassum_Combo <- 
      readr::read_csv(
        glue::glue("Raw_Data/KFM_5mQuadrat_RawData_1996-{Export_END_Year}.txt")) %>%   
      dplyr::filter(Species == 2016.00 | Species == 2016.50) %>%
      dplyr::mutate(CommonName = "devil weed (all)",
                    Species = 2017) %>% 
      dplyr::group_by(SiteNumber, SurveyYear, QuadratNumber) %>% 
      dplyr::mutate(Count = sum(Count)) %>% 
      dplyr::distinct(.keep_all = TRUE) %>%
      dplyr::ungroup()
    
    Five_M_Density <- 
      readr::read_csv(
        glue::glue("Raw_Data/KFM_5mQuadrat_RawData_1996-{Export_END_Year}.txt")) %>%
      base::rbind(Macro_Combo, Sargassum_Combo) %>% 
      tidyr::separate(SurveyDate, c('Date','Time'),' ') %>%
      dplyr::filter(IslandCode != "CL") %>%
      dplyr::left_join(Site_Info) %>%
      dplyr::mutate(Date = lubridate::mdy(Date)) %>%
      dplyr::filter(!is.na(Count), !is.na(CommonName)) %>% 
      dplyr::group_by(SiteNumber, IslandCode, IslandName, SiteCode, SiteName, Species, ScientificName, CommonName,
                      SurveyYear, ReserveStatus, MeanDepth, Reference) %>%
      dplyr::summarise(Date = Date, # GI in 2003 was surveyed on 2 dates, cant group by
                       Area_Surveyed = dplyr::n() * 5,
                       Total_Count = base::sum(Count),
                       Mean_Density = base::round(Total_Count / Area_Surveyed, 4), 
                       SD = base::round(stats::sd(Count), 4),
                       SE = base::round(SD / base::sqrt(Area_Surveyed), 4),
                       Survey_Type = "Five_Meter") %>% 
      dplyr::ungroup() %>%
      dplyr::distinct(SiteNumber, IslandCode, IslandName, SiteCode, SiteName, CommonName,
                      ScientificName, SurveyYear, Total_Count, Mean_Density, SD, SE, Area_Surveyed, .keep_all = TRUE)  %>% 
      dplyr::filter(ScientificName != "Pisaster giganteus" | SurveyYear < 2014,
                    ScientificName != "Pisaster ochraceus" | SurveyYear < 2014,
                    ScientificName != "Undaria pinnatifida") %>%
      dplyr::arrange(SiteNumber, SurveyYear, ScientificName) %>% 
      dplyr::select(SiteNumber, IslandCode, IslandName, SiteCode, SiteName, SurveyYear, Date,
                    Species, ScientificName, CommonName, Mean_Density, SD, SE, 
                    Area_Surveyed, MeanDepth, Survey_Type, ReserveStatus, Reference) 
    
  }
  
  { # Bands Density    ----
    Bands_Density <- readr::read_csv(
      glue("Raw_Data/KFM_BandTransect_RawData_1982-{Export_END_Year}.txt"),   
      col_types = cols(CountA = col_number(), CountB = col_number())) %>%
      dplyr::filter(IslandCode != "CL") %>%
      dplyr::left_join(Site_Info) %>%
      tidyr::separate(SurveyDate, c('Date','Time'),' ') %>%
      dplyr::mutate(Date = lubridate::mdy(Date)) %>%
      tidyr::pivot_longer(cols = c(CountA, CountB), values_to = "Count") %>% 
      dplyr::filter(!is.na(Count), 
                    !is.na(CommonName),
                    ScientificName != "Sargassum horneri") %>% 
      dplyr::group_by(SiteNumber, ScientificName, CommonName, SurveyYear, TransectNumber) %>%
      dplyr::mutate(Count = sum(Count, na.rm = TRUE)) %>%
      dplyr::ungroup() %>%
      dplyr::distinct(SiteNumber, ScientificName, CommonName, SurveyYear, TransectNumber, .keep_all = TRUE)  %>% 
      dplyr::group_by(SiteNumber, IslandCode, IslandName, SiteCode, SiteName, Species, ScientificName,
                      CommonName, SurveyYear, ReserveStatus, MeanDepth, Reference) %>%
      dplyr::summarise(Date = Date,
                       Area_Surveyed = ifelse(SurveyYear %in% 1983:1984, n() * 40, n() * 60),
                       Total_Count = base::sum(Count),
                       Mean_Density = base::round(Total_Count / Area_Surveyed, 4), 
                       SD = base::round(stats::sd(Count), 4),
                       SE = base::round(SD / base::sqrt(Area_Surveyed), 4),
                       Survey_Type = "Bands") %>% 
      dplyr::ungroup() %>% 
      dplyr::distinct(IslandCode, IslandName, SiteCode, SiteName, ScientificName,
                      SurveyYear,  Mean_Density, ReserveStatus, Reference, .keep_all = TRUE) %>%
      dplyr::select(SiteNumber, IslandCode, IslandName, SiteCode, SiteName, SurveyYear, Date,
                    Species, ScientificName, CommonName, Mean_Density, SD, SE, 
                    Area_Surveyed, MeanDepth, Survey_Type, ReserveStatus, Reference) 
    
  }
  
}

{ # Fish Density   ----
  
  { # RDFC  ----
    RDFC_Density <- 
      readr::read_csv(
        glue("Raw_Data/KFM_RovingDiverFishCount_RawData_1982-{Export_END_Year}.txt"),
        locale(encoding = "ISO-8859-1"), col_names = TRUE,
        col_types = cols(Count = col_double())) %>%
      tidyr::separate(SurveyDate, c('Date','Time'),' ') %>%
      dplyr::mutate(Date = lubridate::mdy(Date),
                    Count = as.double(Count)) %>% 
      dplyr::left_join(Site_Info) %>%
      dplyr::group_by(SiteCode, SurveyYear) %>% 
      dplyr::filter(IslandCode != "CL",
                    Date == base::max(Date), SurveyYear > 2003, 
                    ExperienceLevel == "E") %>%
      dplyr::ungroup() %>% 
      dplyr::mutate(CommonName = gsub('ñ', 'n', CommonName),
                    Abundance = gsub("c", "C", Abundance),
                    Abundance = gsub("f", "F", Abundance),
                    Abundance = gsub("s", "S", Abundance),
                    Abundance = gsub("m", "M", Abundance),
                    Abundance = gsub("^$", NA, Abundance),
                    Abundance = gsub("-", NA, Abundance),
                    Score = ifelse(is.na(Score), 0, Score),
                    Abundance = ifelse(is.na(Abundance), "N", Abundance),
                    Count = ifelse(Score == 0, 0, Count)) %>% 
      dplyr::filter(!CommonName %in% c(
        "black surfperch, all", "blacksmith, all", "blue rockfish, all", "kelp bass, all", 
        "kelp rockfish, all", "olive rockfish, all",  "opaleye, all", "pile perch, all", 
        "senorita, all", "striped surfperch, all")) %>% 
      dplyr::group_by(SiteNumber, IslandCode, IslandName, SiteCode, SiteName, SurveyYear, Date, 
                      ScientificName, CommonName, ReserveStatus, Reference) %>%
      dplyr::summarise(Count = mean(Count, na.rm = TRUE)) %>% 
      dplyr::mutate(Count = ifelse(Count > 0 & Count < 1, 1, Count)) %>% 
      dplyr::ungroup() %>% 
      readr::write_csv("App/Tidy_Data/RDFC_Count.csv")
    
    

  }
  
  { # VFT ----
    VFT_Density <- 
      readr::read_csv(
        glue("Raw_Data/KFM_VisualFishTransect_RawData_1985-{Export_END_Year}.txt"),
        locale(encoding = "ISO-8859-1"), col_names = TRUE, col_types = NULL) %>% 
      tidyr::separate(SurveyDate, c('Date','Time'),' ') %>%
      dplyr::mutate(Date = as.Date(Date, format = '%m/%d/%Y'),
                    CommonName = gsub('ñ', 'n', CommonName)) %>%
      dplyr::left_join(Site_Info) %>%
      dplyr::rename(Count = CountA) %>%
      dplyr::group_by(SiteNumber, SurveyYear) %>%
      dplyr::filter(Date == max(Date), 
                    IslandCode != "CL") %>%
      dplyr::ungroup() %>% 
      dplyr::group_by(SiteNumber, SurveyYear, Species) %>% 
      tidyr::pivot_wider(names_from = "Transect_Number", names_prefix = "T", values_from = "Count") %>%
      dplyr::mutate(T1 = ifelse(SurveyYear < 1997, T1, T1 + T2),
                    T2 = ifelse(SurveyYear < 1997, T2, T3 + T4)) %>% 
      dplyr::select(-T3, -T4) %>%
      tidyr::pivot_longer(cols = c(T1, T2), values_to = "Count", names_to = "Transect_Number") %>%
      dplyr::ungroup() %>% 
      dplyr::group_by(SiteNumber, IslandCode, IslandName, SiteCode, SiteName, SurveyYear, 
                      CommonName, ScientificName, ReserveStatus, Reference, ReserveYear) %>%
      dplyr::summarise(Mean_Density = round(sum(Count, na.rm = TRUE) / 600, 4)) %>% 
      dplyr::ungroup() %>%  
      dplyr::distinct(SiteNumber, IslandCode, IslandName, SiteCode, SiteName, SurveyYear, 
                      CommonName, ScientificName, Mean_Density, ReserveStatus, Reference) %>%
      readr::write_csv("App/Tidy_Data/VFT_Density.csv") 
  }
  
}

{ # RPC % Cover   ----
  RPC_Cover <- readr::read_csv(
    glue::glue("Raw_Data/KFM_RandomPointContact_RawData_1982-{Export_END_Year}.txt"), 
    col_types = cols(CountA = col_number(), CountB = col_number(), 
                     CountC = col_number(), CountD = col_number())) %>%
    tidyr::pivot_longer(cols = c(CountA, CountB, CountC, CountD), values_to = "Count") %>%
    tidyr::separate(SurveyDate, c('Date','Time'),' ') %>%
    dplyr::filter(IslandCode != "CL",
                  !ScientificName %in% c(
                    "Macrocystis, Pterygophora, and Eisenia combined",
                    "Leucetta losangelensis",
                    "Hydrozoa", "Balanus",
                    "Sargassum muticum",
                    "Polymastia pachymastia",
                    "Spirobranchus spinosus")) %>%
    dplyr::mutate(Date = lubridate::mdy(Date),  
      ScientificName = dplyr::case_when(
        CommonName == "encrusting coralline algae" ~ "encrusting coralline algae",
        CommonName == "articulated coralline algae" ~ "articulated coralline algae",
        TRUE ~ ScientificName)) %>% 
    dplyr::left_join(Site_Info) %>% 
    dplyr::group_by(SiteNumber, IslandCode, IslandName, SiteCode, SiteName, SurveyYear, 
                    Species, ScientificName, CommonName, ReserveStatus, Reference) %>%
    dplyr::filter(!is.na(Count), !is.na(CommonName), Date == base::max(Date)) %>% 
    dplyr::summarise(
      Date = Date,
      Area_Surveyed = 
        ifelse(SurveyYear == 1982, 500, 
               ifelse(SurveyYear == 1983, 400,
                      ifelse(SurveyYear == 1984, 500,
                             ifelse(SurveyYear > 1984 & SurveyYear <= 1995, 1000, 600)))),
      Total_Count = sum(Count),
      Percent_Cover = 
        ifelse(SurveyYear == 1982, round((Total_Count / Area_Surveyed), 4), 
               ifelse(SurveyYear == 1983, round((Total_Count / Area_Surveyed), 4),
                      ifelse(SurveyYear == 1984, round((Total_Count / Area_Surveyed), 4),
                             ifelse(SurveyYear > 1984 & SurveyYear <= 1995, round((Total_Count / Area_Surveyed), 4), 
                                    round((Total_Count / Area_Surveyed), 4))))) * 100,
      SD = round(sd(Count), 4),
      SE = round(SD / sqrt(Area_Surveyed), 4)) %>% 
    dplyr::ungroup() %>%  
    dplyr::distinct(SiteCode, ScientificName, CommonName, SurveyYear, Percent_Cover, .keep_all = TRUE)  %>%
    readr::write_csv("App/Tidy_Data/RPC_Cover.csv")
  
}

{ # Diversity  ----
  
  { # Shannon's Index   ---- 
    Benthic_Density <- base::rbind(One_M_Density, Five_M_Density, Bands_Density) %>% 
      readr::write_csv("App/Tidy_Data/Benthic_Density.csv") 
    
    Benthic_Counts <- Benthic_Density %>% 
      dplyr::filter(!CommonName %in% c(
        "giant kelp, juvenile (<1m)",
        "giant kelp, subadult (>1m and no haptera above the primary dichotomy)",
        "giant kelp, adult (>1m and haptera above the primary dichotomy)",
        "Sargassum horneri, adult (>50cm or recepticles present)",
        "Sargassum horneri, juvenile (<50cm and no recepticles)")) %>% 
      dplyr::mutate(Count = Mean_Density * 2000) %>% 
      dplyr::group_by(SiteNumber, IslandCode, IslandName, SiteCode, SiteName, 
                      ScientificName, SurveyYear, ReserveStatus, Reference) %>% 
      dplyr::summarise(Count = sum(Count)) %>% 
      dplyr::ungroup()
  
    Fish_Counts <- RDFC_Density %>%
      dplyr::select(SiteNumber, IslandCode, IslandName, SiteCode, SiteName, 
                    ScientificName, SurveyYear, Count, ReserveStatus, Reference) %>% 
      dplyr::filter(!ScientificName %in% 
                      c('Alloclinus holderi', 'Coryphopterus nicholsi', 
                        'Lythrypnus dalli', 'Sebastes')) 

    All_Counts <- rbind(Benthic_Counts, Fish_Counts) %>%
      tidyr::pivot_wider(names_from = ScientificName, values_fn = sum,
                         values_from = Count, values_fill = 0)

    ShannonIndex <- All_Counts %>%
      dplyr::select(-SiteNumber, -IslandCode, -IslandName, -SiteCode, -SiteName, 
                    -SurveyYear, -ReserveStatus, -Reference) %>%
      vegan::diversity()
    
    Diversity_Shannon <- All_Counts %>% 
      dplyr::select(SiteNumber, IslandCode, IslandName, SiteCode, 
                    SiteName, SurveyYear, ReserveStatus, Reference) %>%
      base::cbind("shannon" = ShannonIndex) %>%
      dplyr::mutate(Date = base::as.Date(base::ISOdate(SurveyYear, 7, 1)))
    # %>% 
      # readr::write_csv("App/Tidy_Data/Diversity_Shannon.csv")
  }
  
  { # Simpson's Index  ----
    RPC_Cover_Wide <- RPC_Cover %>%
      dplyr::select(-SE, -SD, -CommonName, -Date, -Species,
                    -Area_Surveyed, -Total_Count) %>%
      tidyr::pivot_wider(names_from = ScientificName, values_fn = sum,
                         values_from = Percent_Cover, values_fill = 0)
    
    SimpsonIndex <- RPC_Cover_Wide %>%
      dplyr::select(-SiteNumber, -IslandCode, -IslandName, -SiteCode, -SiteName,
                    -SurveyYear, -ReserveStatus, -Reference) %>%
      vegan::diversity(index = "simpson")

    Diversity_Simpson <- RPC_Cover_Wide %>% 
      dplyr::select(SiteNumber, IslandCode, IslandName, SiteCode, 
                    SiteName, SurveyYear, ReserveStatus, Reference) %>%
      base::cbind("simpson" = SimpsonIndex) %>%
      dplyr::mutate(Date = base::as.Date(base::ISOdate(SurveyYear, 7, 1))) 
    # %>% 
      # readr::write_csv("App/Tidy_Data/Diversity_Simpson.csv")
  }
  
  { # Diversity Complete   ----
    Diversity <- Diversity_Shannon  %>% 
      dplyr::left_join(Diversity_Simpson) %>% 
      dplyr::left_join(Site_Info %>% 
                         dplyr::select(SiteNumber, ReserveYear)) %>% 
      readr::write_csv("App/Tidy_Data/Diversity.csv")
  }
  
 # Miracle Mile only is missing RPCs for 2001, 2002, and 2004. 
  
}

{ # Size Frequencies & Biomass  ----
  
  { # Conversion Coefficients  ----
    Benthic_Biomass_Coversions <- 
      readr::read_csv("App/Meta_Data/Benthic_Biomass_Equations.csv")
    
    # Fish_Biomass_Coversions <- readr::read_csv("App/Meta_Data/Kramp_growth_parameter.csv") %>% 
    #   dplyr::filter(ScientificName %in% Fish_Biomass_Species)  %>%
    #   dplyr::arrange(ScientificName) %>%
    #   dplyr::select(-CommonName, -WL_Reference, -WL_L_units_conversion_reference)  %>% 
    #   dplyr::left_join(dplyr::select(Species_Info, ScientificName, CommonName)) %>% 
    #   dplyr::mutate(CommonName = gsub(", juvenile", "", CommonName),
    #                 CommonName = gsub(", subadult", "", CommonName),
    #                 CommonName = gsub(", adult", "", CommonName),
    #                 CommonName = gsub(", male", "", CommonName),
    #                 CommonName = gsub(", female", "", CommonName)) %>% 
    #   dplyr::distinct(ScientificName, .keep_all = TRUE) %>%
    #   tidyr::drop_na(WL_a) %>% 
    #   dplyr::mutate(
    #     WL_a_corrected = ifelse(WL_L_units == "mm", WL_a * 10 ^ WL_b, WL_a)) %>% 
    #   dplyr::select(ScientificName, WL_a_corrected, WL_b)  %>%
    #   readr::write_csv("App/Meta_Data/Fish_Biomass_Coversions.csv")
    
    Fish_Biomass_Coversions <- 
      readr::read_csv("App/Meta_Data/Fish_Biomass_Coversions.csv")
  }
  
  { # Kelp Sizes  ----
    Kelp_Sizes <- 
      read_csv(
        glue::glue("Raw_Data/KFM_Macrocystis_RawData_1984-{Export_END_Year}.txt"),
        col_types = cols(PermanentObserverNumber = col_double())) %>%  
      tidyr::separate(SurveyDate, c('Date','Time'),' ') %>%
      dplyr::filter(IslandCode != "CL") %>% 
      dplyr::mutate(Date = lubridate::mdy(Date)) %>% 
      dplyr::left_join(Site_Info) %>%  
      dplyr::rename(Size = Stipe_Count) %>% 
      dplyr::mutate(CommonName = "giant kelp, adult (>1m)",
                    Biomass = Size * 8.477333) %>% 
      dplyr::select(SiteNumber, IslandCode, IslandName, SiteCode, SiteName, SurveyYear, Date,
                    ScientificName, CommonName, Size, Biomass, ReserveStatus, Reference)
  }
  
  { # Gorgonian Sizes    ----
    Gorgonian_Sizes <- 
      readr::read_csv(
        glue::glue("Raw_Data/KFM_Gorgonians_RawData_1984-2014_ NHSF_ 2015-{Export_END_Year}.txt")) %>% 
      tidyr::separate(SurveyDate, c('Date','Time'),' ')  %>%
      dplyr::filter(IslandCode != "CL") %>%
      dplyr::mutate(Date = lubridate::mdy(Date)) %>% 
      dplyr::left_join(Site_Info) %>%  
      dplyr::rename(Size = Width_cm) %>%
      tidyr::uncount(weights = Count) %>%
    dplyr::mutate(
      Biomass = case_when(
        ScientificName ==  "Lophogorgia chilensis" ~ (.018 * 10 ^ 1.529) * Size ^ 1.529,
        ScientificName ==  "Muricea californica" ~ (.002 * 10 ^ 1.529) * Size ^ 2.001,
        ScientificName ==  "Muricea fruticosa" ~ (.002 * 10 ^ 1.529) * Size ^ 2.001)) %>% 
      dplyr::select(SiteNumber, IslandCode, IslandName, SiteCode, SiteName, SurveyYear, Date,
                    ScientificName, CommonName, Size, Biomass, ReserveStatus, Reference)
  }
  
  { # Invertebrate Sizes   ----
    Invert_Sizes <- 
      readr::read_csv(
        glue::glue("Raw_Data/KFM_NaturalHabitatSizeFrequency_RawData_1985-{Export_END_Year}.txt")) %>% 
      tidyr::separate(SurveyDate, c('Date','Time'),' ') %>%
      dplyr::filter(IslandCode != "CL") %>%
      dplyr::mutate(Date = lubridate::mdy(Date)) %>%
      dplyr::left_join(Site_Info) %>%
      dplyr::left_join(
        dplyr::select(
          Benthic_Biomass_Coversions, ScientificName, CommonName, a, b)) %>% 
      tidyr::uncount(weights = NoOfInd) %>%  
      dplyr::rename(Size = Size_mm) %>%
      dplyr::mutate(
        Biomass = case_when(
        ScientificName ==  "Macrocystis pyrifera" ~  Size * 8.477333,
        TRUE ~ a * Size ^ b)) %>% 
      dplyr::select(SiteNumber, IslandCode, IslandName, SiteCode, SiteName, SurveyYear, Date,
                    ScientificName, CommonName, Size, Biomass, ReserveStatus, Reference)
   
    
  }
  
  { # Benthic Sizes  ----
    Benthic_Sizes <- base::rbind(Kelp_Sizes, Gorgonian_Sizes, Invert_Sizes) %>% 
      readr::write_csv("App/Tidy_Data/Benthic_Sizes.csv")
  }
  
  { # Benthic Biomass Long  ----
    Benthic_Biomass <- Benthic_Density %>% 
      dplyr::filter(CommonName %in% unique(Benthic_Sizes$CommonName)) %>%
      dplyr::select(SiteNumber, IslandCode, IslandName, SiteCode, SiteName, SurveyYear,        
                    ScientificName, CommonName, Mean_Density, Survey_Type, ReserveStatus, Reference) %>%
      dplyr::full_join(Benthic_Sizes) %>%
      dplyr::filter(!ScientificName %in% c(
        "Haliotis assimilis",
        "Cypraea spadicea",
        # "Parastichopus parvimensis", # This can potential be commented out but they were only measured in early years
        "Centrostephanus coronatus",
        "Stylaster californicus")) %>% 
      dplyr::group_by(SiteCode, ScientificName, SurveyYear) %>% 
      dplyr::mutate(
        Biomass = case_when(
          is.na(Biomass) & Mean_Density == 0 ~ 0,
          is.na(Biomass) & is.na(Mean_Density) ~ 0,
          TRUE ~ Biomass),
        Mean_Density = case_when(
          Biomass > 0 & Mean_Density == 0 ~ n()/2000,
          Biomass > 0 & is.na(Mean_Density) ~ n()/2000,
          Biomass == 0 & is.na(Mean_Density) ~ 0,
          TRUE ~ Mean_Density)) %>% 
      dplyr::ungroup() %>% 
      dplyr::group_by(SiteNumber, IslandCode, IslandName, SiteCode, SiteName, SurveyYear,
                      ScientificName, CommonName, Mean_Density, Survey_Type, ReserveStatus, Reference) %>% 
      dplyr::summarise(Mean_Biomass = sum(1/n() * Biomass * Mean_Density)) %>% 
      dplyr::ungroup() 
    
    Benthic_Regression <- Benthic_Biomass %>%
      group_by(CommonName, ReserveStatus) %>%
      dplyr::do(broom::tidy(lm(Mean_Biomass ~ Mean_Density, ., na.action = na.exclude))) %>% 
      dplyr::select(-statistic, -p.value, -std.error) %>% 
      tidyr::pivot_wider(names_from = term, values_from = estimate) %>% 
      dplyr::rename(yint = "(Intercept)", b = Mean_Density) %>% 
      dplyr::mutate(yint = ifelse(yint < 0, 0, yint))
    
    Benthic_Mean_Biomass <- Benthic_Biomass %>% 
      left_join(Benthic_Regression) %>% 
      dplyr::left_join(dplyr::distinct(Species_Info, ScientificName, CommonNameSimple)) %>% 
      dplyr::mutate(
        CommonName = CommonNameSimple,
        Mean_Biomass = dplyr::case_when(
          is.na(Mean_Biomass) ~ yint + Mean_Density * b,
          TRUE ~ Mean_Biomass)) %>% 
      dplyr::mutate(Date = base::as.Date(base::ISOdate(SurveyYear, 7, 1))) %>%
      dplyr::select(SiteNumber, IslandCode, IslandName, SiteCode, SiteName,
                    SurveyYear, Date, ScientificName, CommonName, Mean_Density, 
                    Mean_Biomass, Survey_Type, ReserveStatus, Reference) %>% 
      readr::write_csv("App/Tidy_Data/Benthic_Biomass.csv")
  }
  
  { # Fish Density for Biomass   ----
    RDFC_Biomass <-  
      readr::read_csv(
        glue("Raw_Data/KFM_RovingDiverFishCount_RawData_1982-{Export_END_Year}.txt"),
        locale(encoding = "ISO-8859-1"), col_names = TRUE,
        col_types = cols(Count = col_double())) %>%
      tidyr::separate(SurveyDate, c('Date','Time'),' ') %>%
      dplyr::mutate(Date = lubridate::mdy(Date),
                    Count = as.double(Count)) %>% 
      dplyr::left_join(Site_Info) %>%
      dplyr::group_by(SiteCode, SurveyYear) %>% 
      dplyr::filter(
        IslandCode != "CL",
        Date == base::max(Date), SurveyYear > 2003, 
        ExperienceLevel == "E",
        ScientificName %in% Fish_Biomass_Species,
        !CommonName %in% c(
          "black surfperch, all", "blacksmith, all", 
          "blue rockfish, all", "kelp bass, all", 
          "kelp rockfish, all", "olive rockfish, all", 
          "opaleye, all", "pile perch, all", 
          "señorita, all", "striped surfperch, all")) %>%
      dplyr::ungroup() %>% 
      dplyr::mutate(
        CommonName = gsub("ñ", "n", CommonName),
        Abundance = gsub("c", "C", Abundance),
        Abundance = gsub("f", "F", Abundance),
        Abundance = gsub("s", "S", Abundance),
        Abundance = gsub("m", "M", Abundance),
        Abundance = gsub("^$", NA, Abundance),
        Abundance = gsub("-", NA, Abundance),
        Score = ifelse(is.na(Score), 0, Score),
        Abundance = ifelse(is.na(Abundance), "N", Abundance),
        Count = ifelse(Score == 0, 0, Count),
        CommonName = base::factor(CommonName),
        CommonName = forcats::fct_collapse(
          CommonName, 
          "California sheephead, female" = c(
            "California sheephead, female", "California sheephead, juvenile"),
          "rock wrasse" = c(
            "rock wrasse, male", "rock wrasse, female", "rock wrasse, juvenile")),
        CommonName = base::as.character(CommonName),
        CommonName = base::gsub(", adult", "", CommonName),
        CommonName = base::gsub(", subadult", "", CommonName),
        CommonName = base::gsub(", juvenile", "", CommonName)) %>%
      dplyr::group_by(SiteNumber, IslandCode, IslandName, SiteCode, SiteName, SurveyYear, Date,
                      ScientificName, CommonName, ReserveStatus, Reference, PermanentObserverNumber) %>%
      dplyr::summarise(Count = sum(Count, na.rm = TRUE)) %>%
      dplyr::group_by(SiteNumber, IslandCode, IslandName, SiteCode, SiteName, SurveyYear, Date, 
                      ScientificName, CommonName, ReserveStatus, Reference) %>%
      dplyr::summarise(Count = mean(Count, na.rm = TRUE)) %>% 
      dplyr::mutate(Count = ifelse(Count > 0 & Count < 1, 1, Count)) %>% 
      dplyr::ungroup() 
  }
  
  { # Fish Sizes  ----
    Fish_Sizes <- 
      data.table::fread(glue::glue(
        "Raw_Data/KFM_FishSizeFrequency_RawData_2007-{Export_END_Year}.txt")) %>%
      dplyr::left_join(Site_Info) %>%
      tidyr::separate(SurveyDate, c('Date','Time'),' ') %>%
      dplyr::mutate(
        CommonName = gsub('ñ', 'n', CommonName),
        CommonName = gsub(", all", "", CommonName),
        CommonName = factor(CommonName),
        CommonName = forcats::fct_collapse(
          CommonName, 
          "California sheephead, female" = c(
            "California sheephead, female", "California sheephead, juvenile"),
          "rock wrasse, female" = c(
            "rock wrasse, female", "rock wrasse, juvenile")),
        Date = lubridate::mdy(Date)) %>% 
      dplyr::rename(Size_cm = TotalLength_cm) %>%
      tidyr::uncount(weights = Count, .remove = TRUE) %>%
      dplyr::select(SiteNumber, IslandCode, SiteCode, IslandName, SiteName, SurveyYear, 
                    ScientificName, CommonName, Size_cm, ReserveStatus, Reference) %>% 
      readr::write_csv("App/Tidy_Data/Fish_Sizes.csv")
     
    Fish_Biomass <- Fish_Sizes %>% 
      dplyr::filter(ScientificName %in% Fish_Biomass_Species) %>%
      dplyr::mutate(
        CommonName = forcats::fct_collapse(
          CommonName, "rock wrasse" = c(
            "rock wrasse, male", "rock wrasse, female"))) %>% 
      dplyr::left_join(Fish_Biomass_Coversions) %>%
      dplyr::mutate(
        Biomass = case_when(
          ScientificName == "Embiotoca jacksoni" ~ WL_a_corrected * (0.799 * Size_cm - 0.407) ^ WL_b,
          ScientificName == "Girella nigricans" ~ WL_a_corrected * (0.851 * Size_cm) ^ WL_b,
          ScientificName == "Hypsypops rubicundus" ~ WL_a_corrected * (0.79 * Size_cm + 0.42) ^ WL_b,
          ScientificName == "Medialuna californiensis" ~ WL_a_corrected * (0.92 * Size_cm) ^ WL_b,
          ScientificName == "Scorpaenichthys marmoratus" ~ WL_a_corrected * Size_cm ^ WL_b * 1000,
          ScientificName == "Ophiodon elongatus" ~ WL_a_corrected * Size_cm ^ WL_b * 1000,
          TRUE ~ WL_a_corrected * Size_cm ^ WL_b))%>%
      dplyr::full_join(RDFC_Biomass) %>%
      dplyr::group_by(SiteNumber, SurveyYear, CommonName) %>%
      dplyr::mutate(
        Count = case_when(
          Biomass > 0 & is.na(Count) ~ as.double(dplyr::n()),
          Biomass == 0 & is.na(Count) ~ 0,
          TRUE ~ Count)) %>%
      dplyr::ungroup() %>%
      dplyr::group_by(SiteNumber, IslandCode, SiteCode,  IslandName, SiteName, SurveyYear, 
                      ScientificName, CommonName, Count, ReserveStatus, Reference) %>% 
      dplyr::summarise(Mean_Biomass = sum(Biomass)) %>%
      dplyr::ungroup() %>% 
      tidyr::complete(nesting(SiteNumber, SiteCode, SiteName, IslandName,
                              IslandCode, ReserveStatus, Reference),
                      nesting(ScientificName, CommonName), SurveyYear,
                      fill = list(Mean_Biomass = NA, Count = NA)) %>%
      dplyr::group_by(SiteNumber, IslandCode, SiteCode,  IslandName, SiteName, SurveyYear, 
                      ScientificName, CommonName, ReserveStatus, Reference) %>% 
      dplyr::mutate(
        Mean_Biomass = case_when(
          Count == 0 & is.na(Mean_Biomass) ~ 0,
          is.na(Count) & is.na(Mean_Biomass) ~ 0,
          TRUE ~ Mean_Biomass),
        Count = case_when(
          Mean_Biomass == 0 & is.na(Count) ~ 0,
          TRUE ~ Count)) %>%
      dplyr::ungroup()
    
    
  }
  
  { # Fish Biomass Long ----- 
    
    Fish_Regression <- Fish_Biomass %>%
      dplyr::group_by(CommonName, ReserveStatus) %>%
      dplyr::do(broom::tidy(lm(Mean_Biomass ~ Count, ., na.action = na.exclude))) %>% 
      dplyr::select(-statistic, -p.value, -std.error) %>%
      tidyr::pivot_wider(names_from = term, values_from = estimate) %>%
      dplyr::rename(yint = "(Intercept)", b = Count) %>%
      dplyr::mutate(yint = ifelse(yint < 0, 0, yint))
    
    Fish_Mean_Biomass <- Fish_Biomass %>%
      left_join(Fish_Regression) %>%
      dplyr::mutate(
        Mean_Biomass = dplyr::case_when(
          is.na(Mean_Biomass) ~ yint + Count * b,
          Count == 0 ~ 0,
          TRUE ~ Mean_Biomass)) %>%
      dplyr::mutate(Date = base::as.Date(base::ISOdate(SurveyYear, 7, 1))) %>%
      dplyr::select(SiteNumber, IslandCode, IslandName, SiteCode, SiteName,
                    SurveyYear, Date, ScientificName, CommonName, Count,
                    Mean_Biomass, ReserveStatus, Reference) %>%
      readr::write_csv("App/Tidy_Data/Fish_Biomass.csv")
    
  }
  
  { # Fish Biomass Wide   ----
    Fish_Biomass_Wide <- Fish_Mean_Biomass %>%
      dplyr::select(-Count) %>%
      dplyr::select(SiteNumber, IslandCode, IslandName, SiteCode, SiteName,
                    SurveyYear, CommonName, Mean_Biomass, ReserveStatus, Reference) %>%
      tidyr::pivot_wider(names_from = CommonName, values_from = Mean_Biomass, values_fill = 0) %>%
      dplyr::rename_with(~ base::gsub(",", "", .)) %>%
      dplyr::rename_with(~ base::gsub(" ", "_", .))
    
    # readr::write_csv("App/Tidy_Data/Benthic_Biomass_Wide.csv") 
  } 
  
}

{ # Mixed Data (% Cover, Count, Biomass) for Random Forest Model  ----
  
  { # RDFC Counts for Mixed Data ---- 
    
    RDFC_Wide <- RDFC_Density %>% 
      dplyr::filter(SurveyYear > 2004,
                    !ScientificName %in% Fish_Biomass_Species,
                    !CommonName %in% c( 
                      "blackeye goby", "blue-banded goby",
                      "island kelpfish", "copper rockfish, all",
                      "goby spp.", "kelp greenling, adult",
                      "rockfish spp.", "rockfish spp., adult")) %>% 
      dplyr::mutate(
        CommonName = factor(CommonName),
        CommonName = forcats::fct_collapse(
          CommonName,
          "baitfish spp." = c("baitfish unidentified", "Pacific sardine", "northern anchovy"),
          "kelp greenling" = c("kelp greenling, male", "kelp greenling, female",  "kelp greenling, juvenile"),
          "Rockfish YOY" = c(
            "black and yellow/gopher rockfish, juvenile", 
            "kelp/gopher/copper/black and yellow rockfish, juvenile",
            "olive/yellowtail rockfish, juvenile",
            "rockfish spp., juvenile",
            "rosy rockfish, juvenile",
            "squarespot rockfish, juvenile",
            "stripetail rockfish, juvenile",
            "splitnose rockfish, juvenile",
            "bocaccio, juvenile",
            "brown rockfish, juvenile", 
            "calico rockfish, juvenile", 
            "canary rockfish, juvenile",
            "grass rockfish, juvenile", 
            "halfbanded rockfish, juvenile"), 
          "sculpin spp." = c(
            "lavender sculpin",
            "sailfin sculpin",
            "snubnose sculpin",
            "sculpin spp.",
            "spotfin sculpin",
            "scalyhead sculpin"),
          "surfperch spp." = c(
            "surfperch spp., adult",
            "surfperch spp., juvenile",
            "sharpnose surfperch",
            "sharpnose/white surfperch",
            "white surfperch"),
          "c_o turbot" = c("c-o turbot")),
        CommonName = as.character(CommonName),
        CommonName = gsub(", juvenile", "", CommonName),
        CommonName = gsub(", adult", "", CommonName)) %>%
      dplyr::group_by(SiteNumber, IslandCode, IslandName, SiteCode, SiteName, SurveyYear, 
                      CommonName, ReserveStatus, Reference) %>%
      dplyr::summarise(Mean_Density = sum(Count)) %>%
      dplyr::ungroup() %>%
      dplyr::select(SiteNumber, IslandCode, IslandName, SiteCode, SiteName, SurveyYear,
                    CommonName, Mean_Density, ReserveStatus, Reference) %>%
      tidyr::pivot_wider(names_from = CommonName, values_from = Mean_Density, values_fill = 0) %>% 
      dplyr::rename_with(~ base::gsub(",", "", .)) %>% 
      dplyr::rename_with(~ base::gsub(" ", "_", .)) %>% 
      dplyr::rename_with(~ base::gsub("-", "_", .)) %>% 
      dplyr::rename_with(~ base::gsub("'", "", .)) 
  }
  
  { # Counts and Biomass  ----
    
    VFT_Counts <- VFT_Density %>% 
      dplyr::mutate(
        CommonName = gsub(
          "California sheephead, juvenile", "California sheephead, female", CommonName),
        CommonName = gsub(
          "rock wrasse, juvenile", "rock wrasse, female", CommonName),
        CommonName = gsub(", juvenile", "", CommonName),
        CommonName = gsub(", subadult", "", CommonName),
        CommonName = gsub(", adult", "", CommonName)) %>% 
      dplyr::group_by(SiteNumber, IslandCode, IslandName, SiteCode, SiteName, SurveyYear, 
                      ScientificName, CommonName, ReserveStatus, Reference) %>% 
      dplyr::summarise(Mean_Density = sum(Mean_Density * 2000))
    
    Counts <- Benthic_Counts %>%
      dplyr::left_join(dplyr::distinct(Species_Info, ScientificName, CommonNameSimple)) %>% 
      dplyr::mutate(CommonName = CommonNameSimple,
                    Mean_Density = Count) %>%  
      dplyr::select(-CommonNameSimple, -Count) %>%
      base::rbind(VFT_Counts) %>% 
      dplyr::filter(!CommonName %in% Benthic_Mean_Biomass$CommonName)
    
  }
  
  { # Mixed Data Wide  -----
    
    All_Mixed_Data_Wide <- RPC_Cover %>% 
      dplyr::filter(
        !ScientificName %in% c(
          "Macrocystis pyrifera", "Eisenia arborea",
          "Pterygophora californica", "Laminaria farlowii",
          "Sargassum horneri", "Bare Substrate",
          "Rock", "Cobble", "Sand")) %>% 
      dplyr::left_join(dplyr::distinct(Species_Info, ScientificName, CommonNameSimple)) %>% 
      dplyr::mutate(CommonName = CommonNameSimple,
                    Mean_Density = Percent_Cover) %>% 
      dplyr::select(SiteNumber, IslandCode, IslandName, SiteCode, SiteName, SurveyYear, 
                    ScientificName, CommonName, Mean_Density, ReserveStatus, Reference) %>% 
      base::rbind(Counts, Benthic_Mean_Biomass %>% 
                    dplyr::select(-Date, -Mean_Density, -Survey_Type) %>% 
                    dplyr::mutate(Mean_Density = Mean_Biomass) %>% 
                    dplyr::select(-Mean_Biomass)) %>% 
      dplyr::select(SiteNumber, IslandCode, IslandName, SiteCode, SiteName, SurveyYear, 
                    CommonName, Mean_Density, ReserveStatus, Reference) %>% 
      tidyr::pivot_wider(names_from = CommonName, values_from = Mean_Density, values_fill = 0) %>%
      dplyr::left_join(dplyr::select(Diversity_Shannon, -Date)) %>%
      dplyr::left_join(dplyr::select(Diversity_Simpson, -Date)) %>% 
      dplyr::mutate(
        ReserveStatus = case_when(
          SurveyYear < 2003 & SiteCode == "LC" ~ "Inside",
          SurveyYear < 2003 & SiteCode == "CC" ~ "Inside",
          SurveyYear < 2003 ~ "Outside",
          TRUE ~ ReserveStatus)) %>% 
      dplyr::rename_with(~ base::gsub(",", "", .)) %>% 
      dplyr::rename_with(~ base::gsub(" ", "_", .)) %>% 
      dplyr::rename_with(~ base::gsub("-", "_", .)) %>% 
      dplyr::rename_with(~ base::gsub("'", "", .)) %>%
      readr::write_csv("App/Tidy_Data/Mixed_Data_Fish_Density.csv") %>% 
      dplyr::select(-all_of(VFT_Species)) %>% 
      dplyr::filter(SurveyYear > 2004) %>%
      dplyr::left_join(RDFC_Wide) %>% 
      dplyr::left_join(Fish_Biomass_Wide) %>% 
      base::replace(is.na(.), 0) %>%
      readr::write_csv("App/Tidy_Data/Mixed_Data_Fish_Biomass.csv")
      
    # which(is.na(All_Mixed_Data_Wide), arr.ind=TRUE)
  }
  
}

{ # Mixed Data nMDS Dimensions    ----
  
  { # All Years nMDS Dimensions    -----
    
    Mixed_Data_Fish_Density <- readr::read_csv("App/Tidy_Data/Mixed_Data_Fish_Density.csv") %>% 
      dplyr::filter(SiteCode != "MM" | SurveyYear > 2004)
    
    RKF_All_Years <- Mixed_Data_Fish_Density  %>% 
      dplyr::mutate(SurveyYear = factor(SurveyYear),
                    IslandName = factor(IslandName),
                    ReserveStatus = factor(ReserveStatus)) %>%
      dplyr::select(-SiteNumber, -SiteName, 
                    -IslandCode, -SiteCode)
    
    which(is.na(RKF_All_Years), arr.ind=TRUE)
    
    RF_Reserve_Model_All_Years <- randomForest::randomForest(
      data = RKF_All_Years,
      ReserveStatus ~ ., ntree = 3000, mtry = 8,
      importance = TRUE, proximity = TRUE, keep.forest = TRUE)
    
    nMDS_3D_ay <- randomForest::MDSplot(
      RF_Reserve_Model_All_Years, fac = RKF_All_Years$ReserveStatus,
      k = 3, palette = rep(1, 2),
      pch = as.numeric(RKF_All_Years$ReserveStatus))
    
    nMDS_3D_all_years <- unlist(nMDS_3D_ay$points) %>%
      as.data.frame() %>%
      cbind(dplyr::select(
        Mixed_Data_Fish_Density, SiteCode, SiteName, IslandName, ReserveStatus, SurveyYear)) %>% 
      readr::write_csv("App/Tidy_Data/nMDS_3D_all_years.csv")
    
  }
  
  { # > 2004 nMDS Dimensions    -----
    
    Mixed_Data_Fish_Biomass <- readr::read_csv("App/Tidy_Data/Mixed_Data_Fish_Biomass.csv") 
    
    RKF_2005 <- Mixed_Data_Fish_Biomass%>%
      dplyr::mutate(SurveyYear = factor(SurveyYear),
                    IslandName = factor(IslandName),
                    ReserveStatus = factor(ReserveStatus)) %>% 
      dplyr::select(-SiteNumber, -SiteName, 
                    -IslandCode, -SiteCode) 
    
    RF_Reserve_Model_2005 <- randomForest::randomForest(
      data = RKF_2005,
      ReserveStatus ~ ., ntree = 3000, mtry = 8,
      importance = TRUE, proximity = TRUE, keep.forest = TRUE)
    
    nMDS_3D_2005 <- randomForest::MDSplot(
      RF_Reserve_Model_2005, fac = RKF_2005$ReserveStatus,
      k = 3, palette = rep(1, 2),
      pch = as.numeric(RKF_2005$ReserveStatus))
    
    nMDS_3D_2005_now <- unlist(nMDS_3D_2005$points) %>%
      as.data.frame() %>%
      cbind(dplyr::select(
        Mixed_Data_Fish_Biomass, SiteCode, SiteName, IslandName, ReserveStatus, SurveyYear)) %>% 
      readr::write_csv("App/Tidy_Data/nMDS_3D_2005_now.csv")
  }
  
}

{ # Random Forest Important Species    ----
  
  { # All Years RF Important Species   -----
    RF_Importance_All_Years <- randomForest::importance(RF_Reserve_Model_All_Years) %>%
      as.data.frame() %>%
      tibble::rownames_to_column("Common_Name") %>%
      dplyr::arrange(desc(MeanDecreaseAccuracy)) %>%
      dplyr::left_join(Mixed_Data_xRef_Density) %>% 
      readr::write_csv("App/Tidy_Data/Species_Importance_All_Years.csv")
  }
  
  { # > 2004 RF Model Important Species -----
    RF_Importance_2005 <- randomForest::importance(RF_Reserve_Model_2005) %>%
      as.data.frame() %>%
      tibble::rownames_to_column("Common_Name") %>%
      dplyr::arrange(desc(MeanDecreaseAccuracy)) %>% 
      dplyr::left_join(Mixed_Data_xRef_Biomass) %>% 
      readr::write_csv("App/Tidy_Data/Species_Importance_2005.csv")
    # %>%
    #   dplyr::mutate(CommonName = factor(CommonName),
    #                 ScientificName = factor(ScientificName))
  }
  
}

{ # Temperature RAW to Tidy   ----
  
  # temp_Raw <- readr::read_csv( # have not yet used but could...
  #   glue("Raw_Data/Temperature_RawData_1994-{Export_END_Year}.txt")) %>%
  #   dplyr::filter(IslandCode != "CL", Site_Number < 38, !base::is.na(Temp_C)) %>%
  #   dplyr::select(-Date, -Time) %>%
  #   tidyr::separate(DateTime, c('Date','Time'),' ') %>%
  #   dplyr::group_by(Date, Site_Number) %>%
  #   dplyr::mutate(Date = lubridate::mdy(Date),
  #                 Month = lubridate::month(Date, label = TRUE),
  #                 Temp_Daily_Min = base::min(Temp_C),
  #                 Temp_Daily_Max = base::max(Temp_C),
  #                 Temp_Daily_Mean = base::mean(Temp_C)) %>%
  #   dplyr::ungroup() %>%
  #   dplyr::group_by(Site_Number, Year, Month) %>%
  #   dplyr::mutate(Include = ifelse(is.even(match(Month, month.abb)) & n() < 24, FALSE,
  #                                  ifelse(is.odd(match(Month, month.abb)) & n() < 25, FALSE, TRUE)))%>%
  #   dplyr::filter(Include == TRUE) %>%
  #   dplyr::mutate(Temp_Monthly_Mean = base::mean(Temp_C)) %>%
  #   dplyr::ungroup() %>%
  #   dplyr::distinct(Site_Number, Date, .keep_all = TRUE) %>%
  #   dplyr::left_join(Site_Info) %>%
  #   dplyr::select(SiteNumber, IslandCode, IslandName, SiteCode, SiteName,
  #                 Year, Month, Date, Temp_Daily_Mean, Temp_Daily_Min, Temp_Daily_Max, Temp_Monthly_Mean, MeanDepth) %>%
  #   readr::write_csv("App/Tidy_Data/Temp_Raw_Tidy.csv")
  
}



