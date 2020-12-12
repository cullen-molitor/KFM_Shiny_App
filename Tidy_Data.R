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
  
  Mixed_Data_xRef <- readr::read_csv("App/Meta_Data/Mixed_Data_xref.csv")
  
  Site_Info <- readr::read_csv("App/Meta_Data/Site_Info.csv")

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
      dplyr::mutate(Date = as.Date(Date, format='%m/%d/%Y')) %>%
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
      dplyr::mutate(Date = as.Date(Date, format='%m/%d/%Y')) %>%
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
      dplyr::mutate(Date = as.Date(Date, format='%m/%d/%Y')) %>%
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
      dplyr::summarise(Abundance = Abundance,
                       Score = Score,
                       Count = mean(Count, na.rm = TRUE)) %>% 
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
      dplyr::group_by(IslandCode, IslandName, SiteCode, SiteName, CommonName, ScientificName,
                      SurveyYear, ReserveStatus, Reference, ReserveYear) %>%
      dplyr::summarise(Mean_Density = round(sum(Count, na.rm = TRUE) / 600, 4)) %>% 
      dplyr::ungroup() %>%  
      dplyr::distinct(IslandCode, IslandName, SiteCode, SiteName, SurveyYear, 
                      CommonName, ScientificName, Mean_Density, ReserveStatus) %>%
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
    dplyr::filter(!ScientificName %in% c(
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
                    Species, ScientificName, CommonName, ReserveStatus) %>%
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
      dplyr::select(SiteNumber, IslandCode, IslandName, SiteCode, SiteName, 
                      ScientificName, SurveyYear, Count, ReserveStatus)
  
    Fish_Counts <- RDFC_Density %>%
      dplyr::select(SiteNumber, IslandCode, IslandName, SiteCode, SiteName, 
                    ScientificName, SurveyYear, Count, ReserveStatus) %>% 
      dplyr::filter(!ScientificName %in% 
                      c('Alloclinus holderi', 'Coryphopterus nicholsi', 
                        'Lythrypnus dalli', 'Sebastes')) 

    All_Counts <- rbind(Benthic_Counts, Fish_Counts) %>%
      tidyr::pivot_wider(names_from = ScientificName, values_fn = sum,
                         values_from = Count, values_fill = 0)

    ShannonIndex <- All_Counts %>%
      dplyr::select(-IslandCode, -IslandName, -SiteCode, -SiteName, 
                    -SurveyYear, -ReserveStatus) %>%
      vegan::diversity()
    
    Diversity_Shannon <- All_Counts %>% 
      dplyr::select(IslandCode, IslandName, SiteCode, 
                    SiteName, SurveyYear, ReserveStatus) %>%
      base::cbind("Shannon_Index" = ShannonIndex) %>%
      dplyr::mutate(Date = base::as.Date(base::ISOdate(SurveyYear, 7, 1))) %>% 
      readr::write_csv("App/Tidy_Data/Diversity_Shannon.csv")
  }
  
  { # Simpson's Index  ----
    RPC_Cover_Wide <- RPC_Cover %>%
      dplyr::select(-SE, -SD, -CommonName, -Date, -Species,
                    -Area_Surveyed, -Total_Count) %>%
      tidyr::pivot_wider(names_from = ScientificName, values_fn = sum,
                         values_from = Percent_Cover, values_fill = 0)
    
    SimpsonIndex <- RPC_Cover_Wide %>%
      dplyr::select(-IslandCode, -IslandName, -SiteCode, -SiteName,
                    -SurveyYear, -ReserveStatus) %>%
      vegan::diversity(index = "simpson")

    Diversity_Simpson <- RPC_Cover_Wide %>% 
      dplyr::select(IslandCode, IslandName,
                    SiteCode, SiteName, SurveyYear, ReserveStatus) %>%
      base::cbind("Simpson_Index" = SimpsonIndex) %>%
      dplyr::mutate(Date = base::as.Date(base::ISOdate(SurveyYear, 7, 1))) %>% 
      readr::write_csv("App/Tidy_Data/Diversity_Simpson.csv")
  }
  
}

{ # Size Frequencies & Biomass  ----
  
  { # Conversion Coefficients  ----
    Benthic_Biomass_Coversions <- 
      readr::read_csv("App/Meta_Data/Benthic_Biomass_Equations.csv")
    Fish_Biomass_Coversions <- 
      readr::read_csv("App/Meta_Data/Kramp_growth_parameter.csv")
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
  
# Worked up to here 10-12-2020
  
  { # Fish Sizes  ----
    
  }
  
  { # Benthic Biomass  ----
    Benthic_Biomass <- Benthic_Density %>% 
      dplyr::filter(CommonName %in% unique(Benthic_Sizes$CommonName)) %>%
      dplyr::select(SiteNumber, IslandCode, IslandName, SiteCode, SiteName, SurveyYear,        
                     ScientificName, CommonName, Mean_Density, Survey_Type, ReserveStatus, Reference) %>%
      dplyr::full_join(Benthic_Sizes) 
    # Need to Do regression and fill in missing values now 10-12-2020
  }
  
}





{ # Benthic Biomass Tables (Wide for models, Long for plots)   ----
  
  { # Gorgonian Biomass  ----
    
    bands_Gorgo_Data <- read_csv(
      glue("Raw_Data/KFM_BandTransect_RawData_1982-{Export_END_Year}.txt",),   
      col_types = cols(CountA = col_number(), CountB = col_number())) %>%
      dplyr::left_join(Site_Info) %>%
      dplyr::filter(ScientificName %in% c("Lophogorgia chilensis", "Muricea californica", "Muricea fruticosa"),
                    Reference == TRUE, SiteCode != "KH", SurveyYear > 2004) %>%
      tidyr::pivot_longer(cols = c(CountA, CountB), values_to = "Count") %>% 
      dplyr::group_by(SiteNumber, ScientificName, CommonName, SurveyYear, TransectNumber) %>%
      dplyr::mutate(Count = sum(Count, na.rm = TRUE)) %>%
      dplyr::ungroup() %>%
      dplyr::group_by(IslandCode, IslandName, SiteCode, SiteName, ScientificName, 
                      CommonName, SurveyYear, ReserveStatus, Reference) %>%
      dplyr::summarise(Count_To_Reuse = Count,
                       Area_Surveyed = ifelse(SurveyYear %in% 1983:1984, n() * 40, n() * 60),
                       Total_Count = base::sum(Count),
                       Mean_Density = base::round(Total_Count / Area_Surveyed, 4)) %>% 
      dplyr::ungroup() %>% 
      dplyr::distinct(IslandCode, IslandName, SiteCode, SiteName, ScientificName, CommonName,
                      SurveyYear, Mean_Density, ReserveStatus, Reference)
    
    Gorgo <- readr::read_csv(
      glue("Raw_Data/KFM_Gorgonians_RawData_1984-2014_ NHSF_ 2015-{Export_END_Year}.txt")) %>% 
      tidyr::separate(SurveyDate, c('Date','Time'),' ') %>%
      dplyr::mutate(Date = lubridate::mdy(Date)) %>% 
      dplyr::left_join(Site_Info) %>%
      tidyr::uncount(weights = Count) %>% 
      dplyr::mutate(
        Biomass = case_when(
          ScientificName ==  "Lophogorgia chilensis" ~ (.018 * 10 ^ 1.529) * Width_cm ^ 1.529,
          ScientificName ==  "Muricea californica" ~ (.002 * 10 ^ 1.529) * Width_cm ^ 2.001,
          ScientificName ==  "Muricea fruticosa" ~ (.002 * 10 ^ 1.529) * Width_cm ^ 2.001)) %>% 
      dplyr::filter(Reference == TRUE, SiteCode != "KH", SurveyYear > 2004) %>% 
      dplyr::full_join(bands_Gorgo_Data) %>%
      dplyr::group_by(SiteCode, ScientificName, SurveyYear) %>% 
      dplyr::mutate(
        Biomass = case_when(
          is.na(Biomass) & Mean_Density == 0 ~ 0,
          TRUE ~ Biomass),
        Mean_Density = case_when(
          Biomass > 0 & Mean_Density == 0 ~ n()/2000,
          TRUE ~ Mean_Density)) %>% 
      dplyr::ungroup() %>% 
      dplyr::select(IslandCode, IslandName, SiteCode, SiteName, ScientificName, CommonName,
                    SurveyYear, Biomass, Mean_Density, ReserveStatus, Reference)
    
    Gorgo_Regression_Tidy <- Gorgo %>%
      group_by(CommonName, IslandName) %>%
      do(broom::tidy(lm(Biomass ~ 0 + Mean_Density, ., na.action = na.exclude)))
    
    Gorgo_Regression <- Gorgo %>%
      group_by(CommonName, IslandName) %>%
      do(broom::glance(lm(Biomass ~ 0 + Mean_Density, ., na.action = na.exclude)))  %>% 
      dplyr::select(-statistic, -p.value) %>% 
      dplyr::full_join(Gorgo_Regression_Tidy) %>% 
      readr::write_csv("Meta_Data/Gorgonian_Regression.csv")
    
    Gorgo_Biomass_Long <- Gorgo %>% 
      left_join(Gorgo_Regression) %>% 
      dplyr::mutate(
        Biomass = dplyr::case_when(
          is.na(Biomass) ~ Mean_Density * estimate,
          TRUE ~ Biomass)) %>% 
      dplyr::select(IslandCode, SiteCode,  IslandName, SiteName, SurveyYear, 
                    ScientificName, CommonName, Biomass, Mean_Density, 
                    ReserveStatus, Reference) %>% 
      dplyr::group_by(IslandCode, IslandName, SiteCode, SiteName, ScientificName,
                      CommonName, SurveyYear, ReserveStatus, Reference) %>%
      dplyr::summarise(Mean_Biomass = sum(1/n() * Biomass * Mean_Density),
                       Mean_Density = Mean_Density) %>% 
      dplyr::distinct(IslandCode, IslandName, SiteCode, SiteName, ScientificName,
                      CommonName, SurveyYear, Mean_Biomass, Mean_Density, ReserveStatus, Reference)
    
  }
  
  { # 1 m Density     ----
    oneM_Biomass_Data <- readr::read_csv(
      glue("Raw_Data/KFM_1mQuadrat_RawData_1982-{Export_END_Year}.txt"),   
      col_types = cols(CountA = col_number(), CountB = col_number())) %>%
      dplyr::filter(IslandCode != "CL",
                    CommonName != "giant kelp, juvenile",
                    CommonName != "giant kelp stipes > 1m",
                    CommonName != "giant kelp, all",
                    ScientificName != "Lithopoma gibberosa" | SurveyYear > 2002) %>%
      dplyr::left_join(Site_Info) %>%
      tidyr::pivot_longer(cols = c(CountA, CountB), values_to = "Count") %>% 
      dplyr::filter(!is.na(Count), !is.na(CommonName)) %>% 
      dplyr::mutate(CommonName = factor(CommonName),
                    CommonName = forcats::fct_collapse(
                      CommonName, "giant kelp" = c("giant kelp, adult (>1m)", "giant kelp, juvenile (<1m)"))) %>% 
      dplyr::group_by(SiteNumber, IslandCode, IslandName, SiteCode, SiteName,  ScientificName,
                      CommonName, SurveyYear, ReserveStatus, MeanDepth, Reference) %>%
      dplyr::summarise(Area_Surveyed = ifelse(SurveyYear %in% 1985:1994, n() * 2, n()),
                       Total_Count = base::sum(Count),
                       Mean_Density = base::round(Total_Count / Area_Surveyed, 4)) %>% 
      dplyr::ungroup() %>%  
      dplyr::select(IslandCode, IslandName, SiteCode, SiteName, ScientificName,
                    CommonName, SurveyYear, Mean_Density, ReserveStatus, Reference) %>%
      dplyr::distinct(SiteCode, ScientificName, SurveyYear, Mean_Density, .keep_all = TRUE) 
    
    oneM_Biomass <- oneM_Biomass_Data %>% # separate because Macro Biomass below
      dplyr::filter(ScientificName %in% oneM_Biomass_Species, 
                    ScientificName != "Pisaster giganteus" | SurveyYear < 1996)
  }
  
  { # 5 m Density     ----
    
    fiveM_Biomass_Data <- readr::read_csv(
      glue("Raw_Data/KFM_5mQuadrat_RawData_1996-{Export_END_Year}.txt")) %>%
      dplyr::filter(IslandCode != "CL") %>%
      dplyr::left_join(Site_Info) %>%
      dplyr::filter(!is.na(Count), !is.na(CommonName)) %>% 
      dplyr::mutate(CommonName = factor(CommonName),
                    CommonName = forcats::fct_collapse(
                      CommonName, "giant kelp" = 
                        c("giant kelp, adult (>1m and haptera above the primary dichotomy)", 
                          "giant kelp, subadult (>1m and no haptera above the primary dichotomy)"))) %>% 
      dplyr::group_by(IslandCode, IslandName, SiteCode, SiteName, ScientificName,
                      CommonName, SurveyYear, ReserveStatus, Reference) %>%
      dplyr::summarise(Mean_Density = base::round(base::sum(Count) / 200, 4)) %>% 
      dplyr::ungroup() %>%  
      dplyr::select(IslandCode, IslandName, SiteCode, SiteName, ScientificName, 
                    CommonName, SurveyYear, Mean_Density, ReserveStatus, Reference) %>%
      dplyr::distinct(SiteCode, ScientificName, SurveyYear, Mean_Density, .keep_all = TRUE)  
    
    fiveM_Biomass <- fiveM_Biomass_Data %>% # separate because Macro Biomass below
      dplyr::filter(ScientificName == "Pisaster giganteus",
                    ScientificName != "Pisaster giganteus" | SurveyYear < 2014) 
    
  }
  
  { # Bands Density     ----
    
    bands_Biomass_Data <- read_csv(
      glue("Raw_Data/KFM_BandTransect_RawData_1982-{Export_END_Year}.txt",),   
      col_types = cols(CountA = col_number(), CountB = col_number())) %>%
      dplyr::filter(IslandCode != "CL", ScientificName %in% bands_Biomass_Species) %>%
      dplyr::filter(ScientificName != "Muricea californica",
                    ScientificName != "Muricea fruticosa", 
                    ScientificName != "Lophogorgia chilensis") %>%
      dplyr::left_join(Site_Info) %>%
      tidyr::pivot_longer(cols = c(CountA, CountB), values_to = "Count") %>% 
      dplyr::filter(!is.na(Count), !is.na(CommonName)) %>% 
      dplyr::group_by(SiteNumber, ScientificName, CommonName, SurveyYear, TransectNumber) %>%
      dplyr::mutate(Count = sum(Count, na.rm = TRUE)) %>%
      dplyr::ungroup() %>%
      dplyr::group_by(IslandCode, IslandName, SiteCode, SiteName, ScientificName,
                      CommonName,  SurveyYear, ReserveStatus, Reference) %>%
      dplyr::summarise(Area_Surveyed = ifelse(SurveyYear %in% 1983:1984, n() * 40, n() *60),
                       Mean_Density = base::round(base::sum(Count) / Area_Surveyed, 4)) %>% 
      dplyr::ungroup() %>% 
      dplyr::select(IslandCode, IslandName, SiteCode, SiteName, ScientificName, 
                    CommonName, SurveyYear, Mean_Density, ReserveStatus, Reference) %>%
      dplyr::distinct(SiteCode, ScientificName, SurveyYear, Mean_Density, .keep_all = TRUE)
    
  }
  
  { # Macro Stipe Density  ----
    oneM_Macro <- oneM_Biomass_Data %>% 
      filter(ScientificName == "Macrocystis pyrifera",
             SurveyYear %in% 1984:1995) 
    
    fiveM_Macro <- fiveM_Biomass_Data %>% 
      dplyr::filter(ScientificName == "Macrocystis pyrifera")
    
    Macro <- rbind(oneM_Macro, fiveM_Macro)
    
    stipedensity_FSC <- read_csv("App/Meta_Data/stipedensity_FSC_regressioncoefs.csv") %>%
      dplyr::filter(Month %in% season_months) %>%
      dplyr::summarise(slope = mean(slope))
    
    Kelp_Biomass <- read_csv(
      glue::glue("Raw_Data/KFM_Macrocystis_RawData_1984-{Export_END_Year}.txt"),
      col_types = cols(PermanentObserverNumber = col_double())) %>% 
      dplyr::select(-SurveyDate, -Species, -PermanentObserverNumber, -Marker, -Diameter_cm) %>% 
      dplyr::full_join(Macro) %>% 
      dplyr::filter(Reference == TRUE, SurveyYear > 2004, SiteCode != "KH") %>%  # commment to make original 16
      dplyr::mutate(
        Stipe_Count = dplyr::case_when(
          is.na(Stipe_Count) & Mean_Density == 0 ~ 0,
          TRUE ~ Stipe_Count),
        Stipe_Count = tidyr::replace_na(Stipe_Count, 0)) %>% 
      dplyr::group_by(IslandCode, IslandName, SiteCode, SiteName, SurveyYear, 
                      ScientificName, CommonName, Mean_Density, Reference, ReserveStatus) %>%
      dplyr::summarise(Stipe_Density = sum(Stipe_Count / sum(Stipe_Count) * Mean_Density, na.rm = TRUE)) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(Mean_Biomass = (Stipe_Density * stipedensity_FSC$slope)*1000) %>% # converts from kg to g here
      dplyr::arrange(SiteName, SurveyYear) %>% 
      dplyr::select(-Stipe_Density)
    
  }
  
  { # Invertebrate Biomass Raw  ----
    Invert_Biomass <- readr::read_csv(
      glue("Raw_Data/KFM_InvertebrateBiomass_1985-{Export_END_Year}.txt")) %>% 
      dplyr::left_join(Site_Info) %>%
      dplyr::filter(ScientificName %in% Benthic_Biomass_Species,
                    Reference == TRUE, SiteCode != "KH", SurveyYear > 2004, # Comment out to make original 16
                    ScientificName != "Macrocystis pyrifera",
                    ScientificName != "Muricea californica",
                    ScientificName != "Muricea fruticosa", 
                    ScientificName != "Lophogorgia chilensis") %>%
      dplyr::left_join(
        dplyr::select(Benthic_Biomass_Coversions, ScientificName, CommonName, a, b)) %>% 
      tidyr::uncount(weights = NoOfInd) %>% 
      dplyr::mutate(Biomass = a * Size_mm ^ b) %>% 
      dplyr::select(SiteNumber, IslandCode, IslandName, SiteCode, SiteName, SurveyYear,  
                    ScientificName, CommonName, Biomass, ReserveStatus, Reference)
  }
  
  { # Benthic Biomass Long  ----
    Benthic_Biomass <- base::rbind(
      oneM_Biomass, fiveM_Biomass, bands_Biomass_Data) %>% 
      dplyr::select(IslandCode, IslandName, SiteCode, SiteName, SurveyYear,  
                    ScientificName, CommonName, Mean_Density, ReserveStatus, Reference) %>% 
      dplyr::filter(Reference == TRUE, SiteCode != "KH", SurveyYear > 2004) %>% # Comment out to make original 16
      dplyr::full_join(Invert_Biomass)  %>%
      dplyr::group_by(SiteCode, ScientificName, SurveyYear) %>% 
      dplyr::mutate(
        Biomass = case_when(
          is.na(Biomass) & Mean_Density == 0 ~ 0,
          TRUE ~ Biomass),
        Mean_Density = case_when(
          Biomass > 0 & Mean_Density == 0 ~ n()/2000,
          TRUE ~ Mean_Density)) %>% 
      dplyr::ungroup() %>% 
      dplyr::select(IslandCode, IslandName, SiteCode, SiteName, ScientificName, CommonName,
                    SurveyYear, Biomass, Mean_Density, ReserveStatus, Reference)
    
    Benthic_Regression_Tidy <- Benthic_Biomass %>%
      group_by(CommonName, ReserveStatus) %>%
      do(broom::tidy(lm(Biomass ~ 0 + Mean_Density, ., na.action = na.exclude)))
    
    Benthic_Regression <- Benthic_Biomass %>%
      group_by(CommonName, ReserveStatus) %>%
      do(broom::glance(lm(Biomass ~ 0 + Mean_Density, ., na.action = na.exclude)))  %>% 
      dplyr::select(-statistic, -p.value) %>% 
      dplyr::full_join(Benthic_Regression_Tidy) %>% 
      readr::write_csv("Meta_Data/Benthic_Regression.csv")
    
    Benthic_Biomass_Long <- Benthic_Biomass %>% 
      left_join(Benthic_Regression) %>% 
      dplyr::mutate(
        Biomass = dplyr::case_when(
          is.na(Biomass) ~ Mean_Density * estimate,
          TRUE ~ Biomass)) %>% 
      dplyr::select(IslandCode, SiteCode,  IslandName, SiteName, SurveyYear, 
                    ScientificName, CommonName, Biomass, Mean_Density, 
                    ReserveStatus, Reference) %>% 
      dplyr::group_by(IslandCode, IslandName, SiteCode, SiteName, ScientificName,
                      CommonName, SurveyYear, ReserveStatus, Reference) %>%
      dplyr::summarise(Mean_Biomass = sum(1/n() * Biomass * Mean_Density),
                       Mean_Density = Mean_Density) %>% 
      dplyr::distinct(IslandCode, IslandName, SiteCode, SiteName, ScientificName,
                      CommonName, SurveyYear, Mean_Biomass, Mean_Density, ReserveStatus, Reference) %>% 
      base::rbind(Kelp_Biomass, Gorgo_Biomass_Long) %>%
      dplyr::mutate(Date = base::as.Date(base::ISOdate(SurveyYear, 1, 1)),
                    IslandName = gsub(" Island", "", IslandName)) %>%
      # dplyr::left_join(dplyr::select(Site_Info, SiteCode, SiteNumber, ReserveYear)) %>% # uncommment to make original 16
      # dplyr::filter(SiteNumber %in% 1:16) %>%
      # readr::write_csv("App/Tidy_Data/Original_16_Benthic_Biomass_Long.csv")
      readr::write_csv("App/Tidy_Data/Benthic_Biomass_Long.csv")
  }
  
  { # Benthic Biomass Wide   ----
    Benthic_Biomass_Wide <- Benthic_Biomass_Long %>% 
      dplyr::select(IslandCode, IslandName, SiteCode, SiteName, ScientificName, 
                    SurveyYear, Date, Mean_Biomass, ReserveStatus, Reference) %>% 
      dplyr::group_by(SiteCode, SiteName, IslandCode, IslandName, 
                      ScientificName, SurveyYear, Date, ReserveStatus) %>%
      dplyr::summarise(Mean_Biomass = base::sum(Mean_Biomass)) %>%
      dplyr::ungroup() %>%
      tidyr::pivot_wider(names_from = ScientificName, values_from = Mean_Biomass, values_fill = 0)  %>%
      dplyr::mutate(IslandName = gsub(" Island", "", IslandName)) %>%
      dplyr::left_join(annual_mean_oni, by = c("SurveyYear")) %>% 
      dplyr::rename_with(~ base::gsub(",", "", .)) %>% 
      dplyr::rename_with(~ base::gsub(" ", "_", .)) %>%
      readr::write_csv("App/Tidy_Data/Benthic_Biomass_Wide.csv") 
  }
  
}

{ # Fish Biomass Tables (Wide for models, Long for plots)   ----
  
  { # Fish Biomass Conversion Tables ----
    
    { # Correct Equations  ----
      Fish_Biomass_Coversions <- readr::read_csv("App/Meta_Data/Kramp_growth_parameter.csv") %>% 
        dplyr::filter(ScientificName %in% Fish_Biomass_Species)  %>% 
        dplyr::arrange(ScientificName) %>%
        dplyr::select(1, 3:12, -9, -10, -11)  %>% 
        dplyr::left_join(dplyr::select(Species_Info, ScientificName, CommonName)) %>% 
        dplyr::mutate(CommonName = gsub(", juvenile", "", CommonName),
                      CommonName = gsub(", subadult", "", CommonName),
                      CommonName = gsub(", adult", "", CommonName),
                      CommonName = gsub(", male", "", CommonName),
                      CommonName = gsub(", female", "", CommonName)) %>% 
        dplyr::distinct(ScientificName, .keep_all = TRUE) %>%
        dplyr::mutate(
          WL_a_corrected = case_when(
            WL_L_units == "mm" ~ WL_a * 10 ^ WL_b,
            WL_L_units == "cm" ~ WL_a),
          WL_W_units_corrected = "g",
          WL_L_units_corrected = "cm",
          WL_input_length_corrected = "TL",
          WL_a_conversion = case_when(
            WL_L_units == "mm" ~ paste(round(WL_a, 10), "* 10 ^", WL_b, sep = ""),
            WL_L_units == "cm" ~ paste(WL_a)),
          WL_L_units_conversion = case_when(
            ScientificName == "Embiotoca jacksoni" ~ "(0.799 * TL - 0.407)",
            ScientificName == "Girella nigricans" ~ "(0.851 * TL)",
            ScientificName == "Hypsypops rubicundus" ~ "(0.79 * TL + 0.42)",
            ScientificName == "Medialuna californiensis" ~ "(0.92 * TL)",
            TRUE ~ "TL"),
          WL_Equation = case_when(
            WL_W_units =="kg" ~ paste("W(g)=", round(WL_a_corrected, 6), "*", WL_L_units_conversion, 
                                      "^", WL_b, "*1000", sep = ""),
            WL_W_units =="g" ~ paste("W(g)=", round(WL_a_corrected, 6),"*", WL_L_units_conversion,
                                     "^", WL_b, sep = "")),
          WL_W_conversion = case_when(
            WL_W_units =="kg" ~ "g*1000",
            WL_W_units =="g" ~ "g")) %>% 
        dplyr::select(ScientificName, CommonName,
                      WL_a, WL_a_conversion, WL_a_corrected, WL_b, 
                      WL_W_units, WL_W_conversion, WL_W_units_corrected,
                      WL_L_units, WL_L_units_conversion, WL_L_units_corrected, 
                      WL_input_length, WL_input_length_corrected, WL_Equation,
                      WL_Reference, WL_L_units_conversion_reference) %>%
        readr::write_csv("Fish_Bio/Fish_Biomass_Coversions.csv")
    }
    
  }
  
  { # RDFC RAW and NASTY  ----
    RDFC_Biomass_Data <- data.table::fread(
      glue("Raw_Data/KFM_RovingDiverFishCount_RawData_1982-{Export_END_Year}.txt")) %>%
      tidyr::separate(SurveyDate, c('Date','Time'),' ') %>%
      dplyr::mutate(CommonName = gsub('ñ', 'n', CommonName),
                    Abundance = gsub("c", "C", Abundance),
                    Abundance = gsub("f", "F", Abundance),
                    Abundance = gsub("s", "S", Abundance),
                    Abundance = gsub("m", "M", Abundance),
                    Abundance = gsub("^$", NA, Abundance),
                    Abundance = gsub("-", NA, Abundance)) %>%
      dplyr::mutate(Date = lubridate::mdy(Date),
                    CommonName = factor(CommonName)) %>% 
      dplyr::group_by(SurveyYear, SiteNumber) %>% 
      dplyr::filter(Date == base::max(Date), 
                    IslandCode != "CL", 
                    SurveyYear > 2004, 
                    ExperienceLevel == "E",
                    ScientificName %in% Fish_Biomass_Species ) %>%  
      dplyr::ungroup() %>% 
      dplyr::left_join(Site_Info) %>%
      dplyr::filter(! CommonName %in% c(
        "black surfperch, adult", "black surfperch, juvenile",
        "blacksmith, adult", "blacksmith, juvenile",
        "blue rockfish, adult", "blue rockfish, juvenile",
        "kelp bass, adult", "kelp bass, juvenile",
        "kelp rockfish, adult", "kelp rockfish, juvenile",
        "olive rockfish, adult", "olive rockfish, juvenile",
        "opaleye, adult", "opaleye, juvenile",
        "pile perch, adult", "pile perch, juvenile",
        "senorita, adult", "senorita, juvenile",
        "striped surfperch, adult", "striped surfperch, juvenile")) %>% 
      dplyr::mutate(
        CommonName = forcats::fct_collapse(
          CommonName, 
          "California scorpionfish" = c("California scorpionfish, adult", "California scorpionfish, juvenile"),
          "California sheephead, female" = c("California sheephead, female", "California sheephead, juvenile"),
          "rock wrasse, female" = c("rock wrasse, female", "rock wrasse, juvenile"),
          "black and yellow rockfish" = c("black and yellow rockfish, adult"),
          "cabezon" = c("cabezon, adult","cabezon, juvenile"),
          "halfmoon" = c("halfmoon, adult", "halfmoon, juvenile"),
          "garibaldi" = c("garibaldi, adult", "garibaldi, subadult", "garibaldi, juvenile"),
          "lingcod" = c("lingcod, adult"),
          "ocean whitefish" = c("ocean whitefish, adult", "ocean whitefish, juvenile"),
          "treefish" = c("treefish, adult", "treefish, juvenile")),
        CommonName = gsub(", all", "", CommonName),
        CommonName = factor(CommonName)) %>%
      dplyr::group_by(SiteNumber, IslandCode, IslandName, SiteCode, SiteName, ScientificName,
                      CommonName, SurveyYear, Date, ReserveStatus, MeanDepth, Reference) %>%
      dplyr::summarise(Mean_Count = as.double(mean(Count))) %>%
      dplyr::ungroup()  %>%
      dplyr::select(SiteNumber, IslandCode, IslandName, SiteCode, SiteName, ScientificName,
                      CommonName, SurveyYear, Date, Mean_Count, ReserveStatus, MeanDepth, Reference)
    levels(RDFC_Biomass_Data$CommonName)
  }
  
  { # FSF to Biomass  ----
    FSF_Raw <- data.table::fread(glue::glue(
      "Raw_Data/KFM_FishSizeFrequency_RawData_2007-{Export_END_Year}.txt")) %>%
      dplyr::left_join(Site_Info) %>%
      tidyr::separate(SurveyDate, c('Date','Time'),' ') %>%
      dplyr::filter(ScientificName %in% Fish_Biomass_Species) %>%
      dplyr::mutate(
        CommonName = gsub('ñ', 'n', CommonName),
        CommonName = gsub(", all", "", CommonName),
        CommonName = factor(CommonName),
        CommonName = forcats::fct_collapse(
          CommonName, 
          "California sheephead, female" = c("California sheephead, female", "California sheephead, juvenile"),
          "rock wrasse, female" = c("rock wrasse, female", "rock wrasse, juvenile")),
        Count = as.double(Count),
        Date = as.Date(Date, format='%m/%d/%Y')) %>% 
      dplyr::rename(Size_cm = TotalLength_cm) %>%
      tidyr::uncount(weights = Count, .remove = TRUE)  %>%
      dplyr::left_join(dplyr::select(
        Fish_Biomass_Coversions, ScientificName, WL_a_corrected, WL_b)) %>% 
      dplyr::mutate(
        Fish_Biomass = case_when(
          ScientificName == "Embiotoca jacksoni" ~ WL_a_corrected * (0.799 * Size_cm - 0.407) ^ WL_b,
          ScientificName == "Girella nigricans" ~ WL_a_corrected * (0.851 * Size_cm) ^ WL_b, 
          ScientificName == "Hypsypops rubicundus" ~ WL_a_corrected * (0.79 * Size_cm + 0.42) ^ WL_b, 
          ScientificName == "Medialuna californiensis" ~ WL_a_corrected * (0.92 * Size_cm) ^ WL_b,
          ScientificName == "Scorpaenichthys marmoratus" ~ WL_a_corrected * Size_cm ^ WL_b * 1000, 
          ScientificName == "Ophiodon elongatus" ~ WL_a_corrected * Size_cm ^ WL_b * 1000,
          TRUE ~ WL_a_corrected * Size_cm ^ WL_b))%>%
      dplyr::full_join(RDFC_Biomass_Data) %>% 
      dplyr::filter(Reference == TRUE, SiteCode != "KH")  %>%  
      dplyr::group_by(SiteNumber, IslandCode, SiteCode,  IslandName, SiteName, SurveyYear,  
                      ScientificName, CommonName, ReserveStatus, Reference, MeanDepth) %>% 
      dplyr::mutate(
        Mean_Count = case_when(
          Fish_Biomass > 0 & is.na(Mean_Count) ~ as.double(length(Size_cm)),
          Fish_Biomass == 0 & is.na(Mean_Count) ~ as.double(0),
          TRUE ~ Mean_Count)) %>% 
      dplyr::summarise(Mean_Biomass = sum(Fish_Biomass),
                       Mean_Count = mean(Mean_Count)) %>%
      dplyr::ungroup() %>% 
      tidyr::complete(nesting(SiteNumber, SiteCode, SiteName, IslandName,
                              IslandCode, MeanDepth, ReserveStatus, Reference),
                      nesting(ScientificName, CommonName), SurveyYear,
                      fill = list(Mean_Biomass = NA, Mean_Count = NA)) %>%
      dplyr::group_by(SiteNumber, IslandCode, SiteCode,  IslandName, SiteName, SurveyYear, 
                      ScientificName, CommonName, ReserveStatus, Reference, MeanDepth) %>% 
      dplyr::mutate(
        Mean_Biomass = case_when(
          Mean_Count == 0 & is.na(Mean_Biomass) ~ 0,
          is.na(Mean_Count) & is.na(Mean_Biomass) ~ 0,
          TRUE ~ Mean_Biomass)) %>%
      dplyr::mutate(
        Mean_Count = case_when(
          Mean_Biomass == 0 & is.na(Mean_Count) ~ 0,
          TRUE ~ Mean_Count)) %>%
      dplyr::ungroup() 
    
  }
  
  { # Fish Biomass Long ----- 
    
    Fish_Regression_Tidy <- FSF_Raw %>%
      group_by(CommonName, IslandName) %>%
      do(broom::tidy(lm(Mean_Biomass ~ 0 + Mean_Count, ., na.action = na.exclude)))
    
    Fish_Regression <- FSF_Raw %>%
      group_by(CommonName, IslandName) %>%
      do(broom::glance(lm(Mean_Biomass ~ 0 + Mean_Count, ., na.action = na.exclude)))  %>% 
      dplyr::select(-statistic, -p.value) %>% 
      dplyr::full_join(Fish_Regression_Tidy) %>% 
      readr::write_csv("Meta_Data/Fish_Regression_Table.csv")
    
    Fish_Biomass_Long <- FSF_Raw %>% 
      left_join(Fish_Regression) %>% 
      dplyr::mutate(
        Mean_Biomass = dplyr::case_when(
          is.na(Mean_Biomass) ~ Mean_Count * estimate,
          TRUE ~ Mean_Biomass)) %>%
      dplyr::left_join(Fish_Trophic_Levels) %>% 
      dplyr::select(SiteNumber, IslandCode, SiteCode,  IslandName, SiteName, SurveyYear, 
                    ScientificName, CommonName, Mean_Biomass, Mean_Count, Targeted, Trophic_Full,
                    ReserveStatus, Reference, MeanDepth) %>%
      dplyr::mutate(Date = base::as.Date(base::ISOdate(SurveyYear, 1, 1)),
                    IslandName = gsub(" Island", "", IslandName)) %>% 
      readr::write_csv("App/Tidy_Data/Fish_Biomass_Long.csv")
  }
  
  { # Fish Biomass Wide   -----
    Fish_Biomass_Wide <- Fish_Biomass_Long %>%  
      dplyr::filter(Reference == TRUE, SurveyYear > 2004, SiteCode != "KH") %>%
      dplyr::group_by(IslandCode, IslandName, SiteCode, SiteName, 
                      CommonName, SurveyYear, ReserveStatus) %>%
      dplyr::summarise(Mean_Biomass = base::sum(Mean_Biomass)) %>%
      dplyr::ungroup() %>% 
      tidyr::pivot_wider(names_from = CommonName, values_from = Mean_Biomass, values_fill = 0) %>%
      dplyr::left_join(annual_mean_oni, by = c("SurveyYear")) %>% 
      dplyr::rename_with(~ base::gsub(",", "", .)) %>% 
      dplyr::rename_with(~ base::gsub(" ", "_", .)) %>%
      readr::write_csv("App/Tidy_Data/Fish_Biomass_Wide.csv") 
  }
  
}

{ # Mixed Data (% Cover, Count, Biomass) for Random Forest Model  ----
  
  # Run Benthic Count Block 
  # Run Both Biomass Blocks First
  
  { # RDFC Count Data  ---- 
    RDFC_Wide <- data.table::fread(
      glue("Raw_Data/KFM_RovingDiverFishCount_RawData_1982-{Export_END_Year}.txt")) %>%
      dplyr::left_join(Site_Info) %>%
      tidyr::separate(SurveyDate, c('Date','Time'),' ') %>%
      dplyr::mutate(Date = lubridate::mdy(Date),
                    CommonName = factor(CommonName)) %>% 
      dplyr::group_by(SurveyYear, SiteNumber) %>% 
      dplyr::filter(Date == base::max(Date), 
                    IslandCode != "CL", SurveyYear > 2004, 
                    ExperienceLevel == "E",) %>% 
      dplyr::ungroup() %>% 
      dplyr::mutate(CommonName = gsub('ñ', 'n', CommonName),
                    Abundance = gsub("c", "C", Abundance),
                    Abundance = gsub("f", "F", Abundance),
                    Abundance = gsub("s", "S", Abundance),
                    Abundance = gsub("m", "M", Abundance),
                    Abundance = gsub("^$", NA, Abundance),
                    Abundance = gsub("-", NA, Abundance)) %>%
      dplyr::filter(! CommonName %in% c( 
        "black surfperch, adult", "black surfperch, juvenile",
        "blacksmith, adult", "blacksmith, juvenile",
        "blue rockfish, all",
        "kelp bass, adult", "kelp bass, juvenile",
        "kelp rockfish, all",
        "olive rockfish, all",
        "opaleye, adult", "opaleye, juvenile",
        "pile perch, adult", "pile perch, juvenile",
        "senorita, adult", "senorita, juvenile",
        "striped surfperch, adult", "striped surfperch, juvenile",
        "copper rockfish, all",
        "goby spp.",
        "kelp greenling, adult",
        "rockfish spp.", "rockfish spp., adult")) %>% 
      dplyr::mutate(
        CommonName = forcats::fct_collapse(
          CommonName,
          "blue rockfish" = c("blue rockfish, adult"),
          "kelp rockfish" = c("kelp rockfish, adult"),
          "olive rockfish" = c("olive rockfish, adult"),
          "treefish" = c("treefish, adult"),
          "black and yellow rockfish" = c("black and yellow rockfish, adult"),
          "black rockfish" = c("black rockfish, adult"),
          "bocaccio" = c("bocaccio, adult"),
          "brown rockfish" = c("brown rockfish, adult"),
          "calico rockfish" = c("calico rockfish, adult"),
          "canary rockfish" = c("canary rockfish, adult"),
          "copper rockfish" = c("copper rockfish, adult"),
          "gopher rockfish" = c("gopher rockfish, adult"),
          "grass rockfish" = c("grass rockfish, adult"),
          "halfbanded rockfish" = c("halfbanded rockfish, adult"),
          "vermillion rockfish" = c("vermillion rockfish, adult"),
          "Rockfish YOY" = c("blue rockfish, juvenile",
                             "kelp rockfish, juvenile",
                             "olive rockfish, juvenile",
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
                             "copper rockfish, juvenile",
                             "gopher rockfish, juvenile", 
                             "grass rockfish, juvenile", 
                             "vermillion rockfish, juvenile",
                             "halfbanded rockfish, juvenile", 
                             "treefish, juvenile"), 
          "cabezon" = c("cabezon, adult","cabezon, juvenile"),
          "California scorpionfish" = c("California scorpionfish, adult",  "California scorpionfish, juvenile"),
          "California sheephead, female" = c("California sheephead, female",  "California sheephead, juvenile"),
          "halfmoon" = c("halfmoon, adult", "halfmoon, juvenile"),
          "garibaldi" = c("garibaldi, adult", "garibaldi, subadult", "garibaldi, juvenile"),
          "lingcod" = c("lingcod, adult"),
          "ocean whitefish" = c("ocean whitefish, adult", "ocean whitefish, juvenile"),
          "rock wrasse, female" = c("rock wrasse, female", "rock wrasse, juvenile"),
          "baitfish spp." = c("baitfish unidentified", "Pacific sardine", "northern anchovy"),
          "finescale triggerfish" = c("finescale triggerfish, adult",  "finescale triggerfish, juvenile"),
          "giant black sea bass" = c("giant black sea bass, adult",  "giant black sea bass, juvenile"),
          "giant kelpfish" = c("giant kelpfish, adult", "giant kelpfish, juvenile"),
          "kelp greenling" = c("kelp greenling, male", "kelp greenling, female",  "kelp greenling, juvenile"),
          "sculpin spp." = c("lavender sculpin",
                             "sailfin sculpin",
                             "snubnose sculpin",
                             "sculpin spp.",
                             "spotfin sculpin",
                             "scalyhead sculpin"),
          "surfperch spp." = c("surfperch spp., adult",
                               "surfperch spp., juvenile",
                               "sharpnose surfperch",
                               "sharpnose/white surfperch",
                               "white surfperch"),
          "top smelt" = c("top smelt, adult", "top smelt, juvenile"),
          "tubesnout" = c("tubesnout, adult", "tubesnout, juvenile"),
          "c_o turbot" = c("c-o turbot")),
        CommonName = gsub(", all", "", CommonName),
        CommonName = factor(CommonName)) %>%
      dplyr::group_by(SiteNumber, IslandCode, IslandName, SiteCode, SiteName, 
                      CommonName, SurveyYear, Date, ReserveStatus, MeanDepth, Reference) %>%
      dplyr::summarise(Mean_Count = as.double(mean(Count))) %>%
      dplyr::ungroup() %>%
      dplyr::select(SiteNumber, IslandCode, IslandName, SiteCode, SiteName, 
                    CommonName, SurveyYear, Date, Mean_Count, ReserveStatus, Reference) %>%
      dplyr::filter(Reference == TRUE, SurveyYear > 2004, SiteCode != "KH") %>%
      dplyr::select(IslandCode, IslandName, SiteCode, SiteName, 
                    CommonName, SurveyYear, Mean_Count, ReserveStatus) %>%
      tidyr::pivot_wider(names_from = CommonName, values_from = Mean_Count, values_fill = 0) %>% 
      dplyr::select(-'blackeye goby', -'blue-banded goby', -'island kelpfish')  %>% 
      replace(is.na(.), 0)
  }
  
  { # Counts and Biomass  ----
    Counts <- base::rbind(oneM_Count_Data, fiveM_Count_Data, bands_Count_Data) %>%
      dplyr::mutate(Mean_Density = Mean_Density * 2000) 
    Biomasses <- dplyr::full_join(Benthic_Biomass_Wide, Fish_Biomass_Wide) %>% 
      dplyr::select(-Mean_ONI_ANOM)  
    remove <- c('SiteCode', 'SiteName', 'IslandCode', 'IslandName', 
                'SurveyYear', 'ReserveStatus', 'Date', 'Mean_ONI_ANOM')
    Biomass_Species <- unique(factor(c(names(Benthic_Biomass_Wide), names(Fish_Biomass_Wide))))
    Biomass_Species <- droplevels(Biomass_Species[! Biomass_Species %in% remove])
    levels(Biomass_Species)
  }
  
  { # Mixed Data Wide  -----
    All_Mixed_Data_Wide <- readr::read_csv(
      glue("Raw_Data/KFM_RandomPointContact_RawData_1982-{Export_END_Year}.txt"), 
      col_types = cols(CountA = col_number(), CountB = col_number(), CountC = col_number(), CountD = col_number())) %>%
      dplyr::filter(IslandCode != "CL") %>%
      dplyr::left_join(Site_Info) %>%
      tidyr::separate(SurveyDate, c('Date','Time'),' ') %>%
      dplyr::mutate(Date = as.Date(Date, format='%m/%d/%Y'),
                    ScientificName = dplyr::case_when(
                      CommonName == "encrusting coralline algae" ~ "encrusting coralline algae",
                      CommonName == "articulated coralline algae" ~ "articulated coralline algae",
                      TRUE ~ ScientificName)) %>%
      tidyr::pivot_longer(cols = c(CountA, CountB, CountC, CountD), values_to = "Count") %>% 
      dplyr::filter(!is.na(Count), !is.na(CommonName),
                    ScientificName != "Macrocystis, Pterygophora, and Eisenia combined",
                    ScientificName != "Macrocystis pyrifera",
                    ScientificName != "Eisenia arborea",
                    ScientificName != "Pterygophora californica",
                    ScientificName != "Laminaria farlowii",
                    ScientificName != "Sargassum horneri",
                    ScientificName != "Leucetta losangelensis",
                    ScientificName != "Hydrozoa",
                    ScientificName != "Bare Substrate",
                    ScientificName != "Rock",
                    ScientificName != "Cobble",
                    ScientificName != "Sand",
                    ScientificName != "Balanus",
                    ScientificName != "Sargassum muticum",
                    ScientificName != "Polymastia pachymastia",
                    ScientificName != "Spirobranchus spinosus") %>% 
      dplyr::group_by(SiteNumber, IslandCode, IslandName, SiteCode, SiteName, Species, ScientificName,
                      CommonName, SurveyYear, Date, ReserveStatus, MeanDepth, Reference) %>%
      dplyr::summarise(
        Area_Surveyed = 
          ifelse(SurveyYear == 1982, 5, 
                 ifelse(SurveyYear == 1983, 4,
                        ifelse(SurveyYear == 1984, 5,
                               ifelse(SurveyYear > 1984 & SurveyYear <= 1995, 10, 6)))),
        Total_Count = sum(Count),
        Mean_Density = 
          ifelse(SurveyYear == 1982, round((Total_Count / Area_Surveyed), 4), 
                 ifelse(SurveyYear == 1983, round((Total_Count / Area_Surveyed), 4),
                        ifelse(SurveyYear == 1984, round((Total_Count / Area_Surveyed), 4),
                               ifelse(SurveyYear > 1984 & SurveyYear <= 1995, round((Total_Count / Area_Surveyed), 4), 
                                      round((Total_Count / Area_Surveyed), 4)))))) %>% 
      dplyr::ungroup() %>%  
      dplyr::distinct(SiteNumber, ScientificName, CommonName, SurveyYear, Mean_Density, .keep_all = TRUE)  %>% 
      dplyr::arrange(SiteNumber, SurveyYear, ScientificName) %>%
      dplyr::select(IslandCode, IslandName, SiteCode, SiteName, ScientificName,
                    SurveyYear, Mean_Density, ReserveStatus, Reference) %>% 
      base::rbind(oneM_Count_Data, fiveM_Count_Data, bands_Count_Data, Counts) %>% 
      dplyr::filter(Reference == TRUE, SurveyYear > 2004, SiteCode != "KH") %>% 
      dplyr::group_by(IslandCode, IslandName, SiteCode, SiteName, ScientificName, SurveyYear, ReserveStatus) %>%
      dplyr::summarise(Mean_Density = base::sum(Mean_Density)) %>%
      dplyr::ungroup() %>% 
      tidyr::pivot_wider(names_from = ScientificName, values_from = Mean_Density, values_fill = 0) %>%
      dplyr::full_join(RDFC_Wide) %>%
      dplyr::mutate(IslandName = gsub(" Island", "", IslandName),
                    IslandName = factor(IslandName, levels = MPA_Levels)) %>%   
      dplyr::rename_with(~ base::gsub(",", "", .)) %>% 
      dplyr::rename_with(~ base::gsub(" ", "_", .)) %>%
      dplyr::select(-tidyselect::all_of(Biomass_Species)) %>% 
      dplyr::full_join(Biomasses) %>% 
      dplyr::rename("island_kelpfish" = "Alloclinus_holderi",
                    "blackeye_goby" = "Coryphopterus_nicholsi",
                    "blue_banded_goby" = "Lythrypnus_dalli") %>% 
      readr::write_csv("App/Tidy_Data/All_Mixed_Data_Wide.csv") 
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



