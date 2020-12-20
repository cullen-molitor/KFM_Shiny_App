

library(tidyverse)
library(ggmap)
library(sf)
library(ggsn)

# This is a copy from another directory, need to change the file locations - Inputs and Outputs
# This was used to make the static satellite imagery

# setwd("D://Google_Maps")
register_google("")

{ # Data  ----
  Site_Info <- readr::read_csv("Site_Info.csv")
  
  mpa <- st_read("GIS/California_Marine_Protected_Areas.shp")
  
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
  
  transects <- st_read("GIS/KFM_Transects_SmoothLine5.shp")  %>%
    st_as_sf() %>%
    mutate(geometry = st_transform(geometry, "+proj=longlat +ellps=WGS84 +datum=WGS84"))
  
  
  
  NPS_boundary <- st_read("GIS/nps_boundary.shp") %>%
    st_as_sf()
  
  CINMS_boundary <- st_read("GIS/cinms_py.shp") %>%
    st_as_sf()
}

{ # Theme  ----
  map_theme <- function() {
    theme_classic() +
      theme(legend.position = "none",
            legend.title = element_text(),
            plot.title = element_text(hjust = 0.5, size = 36, face = "bold"),
            plot.subtitle = element_text(hjust = 0.5, size = 16),
            axis.title = element_text(size = 32, face = "bold"),
            axis.text.y = element_text(size = 22, face = "bold"),
            axis.text.x = element_text(size = 22, face = "bold")) 
  }
}

{ # Chis for insets  ----
  CHIS <- get_googlemap(c(-119.8, 33.90037),
                        zoom = 9,
                        maptype = "satellite",
                        style = c(labels = "off"))
  C <- ggmap(CHIS, extent = "device") +
    scale_fill_manual(values = c(
      'SMR' = "green", 'FMR' = "deepskyblue2", 
      'SMCA' = "blue2", 'FMCA' = "red2")) +
    scale_color_manual(values = c(
      'SMR' = "green", 'FMR' = "deepskyblue2", 
      'SMCA' = "blue2", 'FMCA' = "red2",
      'Inside' = "green", 'Outside' = "red2")) +
    labs(title = NULL, x = NULL, y = NULL) +
    theme_classic() +
    theme(legend.position = "none",
          legend.title = element_text(),
          axis.text = element_blank())
}

{ # CHIS Full   ----
  
  png(filename = "Maps/CHIS_Full.png",
      width = 1000, height = 1000, type = "cairo")
  
  print(ggmap(CHIS, extent = "device") +
    geom_sf(data = marine, aes(fill = Type, color = Type),
            inherit.aes = FALSE, alpha = .1) +
    geom_label(data = NULL, size = 7,
                  aes(x = -120.45, y = 34.125), label = "San Miguel Island") +
    geom_label(data = NULL, size = 7,
               aes(x = -120.12, y = 34.1), label = "Santa Rosa Island") +
    geom_label(data = NULL, size = 7,
               aes(x = -119.75, y = 34.125), label = "Santa Cruz Island") +
    geom_label(data = NULL, size = 7,
               aes(x = -119.4, y = 34.125), label = "Anacapa Island") +
    geom_label(data = NULL, size = 7,
               aes(x = -119.2, y = 33.55), label = "Santa Barbara Island") +
    geom_sf(data = NPS_boundary, inherit.aes = FALSE, 
            color = "forestgreen", size = 1, alpha = 0) +
    geom_point(data = Site_Info,
               aes(x = Longitude, y = Latitude, color = ReserveStatus),
               size = 2, inherit.aes = FALSE) +
    scale_fill_manual(values = c(
      'SMR' = "green", 'FMR' = "deepskyblue2", 
      'SMCA' = "blue2", 'FMCA' = "red2")) +
    scale_color_manual(values = c(
      'SMR' = "green", 'FMR' = "deepskyblue2", 
      'SMCA' = "blue2", 'FMCA' = "red2",
      'Inside' = "green", 'Outside' = "red2")) +
    labs(title = "Channel Islands National Park",
         x = "Longitude", y = "Latitude") +
    map_theme() +
    scalebar(x.min = -119.5,
             x.max = -119,
             y.min = 34.4,
             y.max = 33.25,
             dist = 10, dist_unit = "mi",
             st.bottom = FALSE, st.color = "white",
             transform = TRUE, model = "WGS84") 
  )
  dev.off()
}

{ # Islands   ----
  
  { # San Miguel  ----
    SMI <- get_googlemap(c(-120.3724, 34.0376),
                         zoom = 12,
                         maptype = "satellite",
                         style = c(labels = "off"))
    
    png(filename = "Maps/SM.png",
        width = 1000, height = 1000, type = "cairo")
    
    SP <-  C + 
      geom_point(data = NULL,
                 aes(x = -120.3724, y = 34.0376), color = "white", stroke = 1,
                 size = 10, inherit.aes = FALSE, shape = 0) 
    
    print(ggmap(SMI, extent = "device") +
      geom_sf(data = marine, aes(fill = Type, color = Type),
              inherit.aes = FALSE, alpha = .1) +
      geom_sf(data = transects, aes(color = Site_Code),
              inherit.aes = FALSE, alpha = .6, color = "lightblue", size = 1) +
      geom_sf(data = NPS_boundary, inherit.aes = FALSE, 
              color = "forestgreen", size = 1, alpha = 0) +
      geom_sf_label(data = marine, inherit.aes = FALSE, hjust = .5, vjust = .5, size = 6,
                    aes(label = NAME)) +
      geom_point(data = Site_Info,
                 aes(x = Longitude, y = Latitude, color = ReserveStatus),
                 size = 2, inherit.aes = FALSE) +
      geom_text(data = Site_Info, hjust = 0, vjust = 0.5, size = 6, color = "white",
                aes(x = Longitude + .001, y = Latitude, label = SiteCode)) +
      scale_fill_manual(values = c(
        'SMR' = "green", 'FMR' = "deepskyblue2", 
        'SMCA' = "blue2", 'FMCA' = "red2")) +
      scale_color_manual(values = c(
        'SMR' = "green", 'FMR' = "deepskyblue2", 
        'SMCA' = "blue2", 'FMCA' = "red2",
        'Inside' = "green", 'Outside' = "red2")) +
      labs(title = "San Miguel Island",
           x = "Longitude", y = "Latitude") +
      map_theme() +
      inset(ggplotGrob(SP), 
            xmin = -120.485, 
            xmax = -120.42, 
            ymin = 33.955, 
            ymax = 33.995) +
      scalebar(x.min = -120.35,
               x.max = -120.275,
               y.min = 33.955,
               y.max = 34.1,
               dist = 2, dist_unit = "mi",
               st.bottom = FALSE, st.color = "white",
               transform = TRUE, model = "WGS84") 
    )
    dev.off()
  }
  
  { # Santa Rosa  ----
    SRI <- get_googlemap(c(-120.0896, 33.9773),
                         zoom = 11,
                         maptype = "satellite",
                         style = c(labels = "off"))
    
    png(filename = "Maps/SR.png",
        width = 1000, height = 1000, type = "cairo")
    
    SP <-  C + 
      geom_point(data = NULL,
                 aes(x = -120.0896, y = 33.9773), color = "white", stroke = 1,
                 size = 16, inherit.aes = FALSE, shape = 0) 
    
    print(ggmap(SRI, extent = "device") +
      geom_sf(data = marine, aes(fill = Type, color = Type),
              inherit.aes = FALSE, alpha = .1) +
      geom_sf(data = transects, aes(color = Site_Code),
              inherit.aes = FALSE, alpha = .6, color = "lightblue", size = 1) +
      geom_sf(data = NPS_boundary, inherit.aes = FALSE, 
              color = "forestgreen", size = 1, alpha = 0) +
      geom_sf_label(data = marine, inherit.aes = FALSE, hjust = .5, vjust = 1, size = 6,
                    aes(label = NAME)) +
      geom_point(data = Site_Info,
                 aes(x = Longitude, y = Latitude, color = ReserveStatus),
                 size = 2, inherit.aes = FALSE) +
      geom_text(data = Site_Info, hjust = 0, vjust = 0.5, size = 6, color = "white",
                 aes(x = Longitude + .001, y = Latitude, label = SiteCode)) +
      scale_fill_manual(values = c(
        'SMR' = "green", 'FMR' = "deepskyblue2", 
        'SMCA' = "blue2", 'FMCA' = "red2")) +
      scale_color_manual(values = c(
        'SMR' = "green", 'FMR' = "deepskyblue2", 
        'SMCA' = "blue2", 'FMCA' = "red2",
        'Inside' = "green", 'Outside' = "red2")) +
      labs(title = "Santa Rosa Island",
           x = "Longitude", y = "Latitude") +
      map_theme() +
      inset(ggplotGrob(SP), 
            xmin = -120.3, 
            xmax = -120.18, 
            ymin = 33.81, 
            ymax = 33.9) +
      scalebar(x.min = -120,
               x.max = -119.9,
               y.min = 34.1,
               y.max = 33.805,
               dist = 2.5, dist_unit = "mi",
               st.bottom = FALSE, st.color = "white",
               transform = TRUE, model = "WGS84") 
    )
    dev.off()
  }
  
  { # Santa Cruz  ----
    SCI <- get_googlemap(c(-119.73, 34.0232),
                         zoom = 11,
                         maptype = "satellite",
                         style = c(labels = "off"))
    
    png(filename = "Maps/SC.png",
        width = 1000, height = 1000, type = "cairo")
    
    SP <-  C + 
      geom_point(data = NULL,
                 aes(x = -119.73, y = 34.0232), color = "white", stroke = 1,
                 size = 22, inherit.aes = FALSE, shape = 0) 
    
    print(ggmap(SCI, extent = "device") +
      geom_sf(data = marine, aes(fill = Type, color = Type),
              inherit.aes = FALSE, alpha = .1) +
      geom_sf(data = transects, aes(color = Site_Code),
              inherit.aes = FALSE, alpha = .6, color = "lightblue", size = 1) +
      geom_sf(data = NPS_boundary, inherit.aes = FALSE, 
              color = "forestgreen", size = 1, alpha = 0) +
      geom_sf_label(data = marine, inherit.aes = FALSE, hjust = 0, vjust = 1, size = 6,
                    aes(label = NAME)) +
      geom_point(data = Site_Info,
                 aes(x = Longitude, y = Latitude, color = ReserveStatus),
                 size = 2, inherit.aes = FALSE) +
      geom_text(data = Site_Info, hjust = 0.5, vjust = 0.5, size = 6, color = "white",
                aes(x = Longitude + .01, y = Latitude, label = SiteCode)) +
      scale_fill_manual(values = c(
        'SMR' = "green", 'FMR' = "deepskyblue2", 
        'SMCA' = "blue2", 'FMCA' = "red2")) +
      scale_color_manual(values = c(
        'SMR' = "green", 'FMR' = "deepskyblue2", 
        'SMCA' = "blue2", 'FMCA' = "red2",
        'Inside' = "green", 'Outside' = "red2")) +
      labs(title = "Santa Cruz Island",
           x = "Longitude", y = "Latitude") +
      map_theme() +
      inset(ggplotGrob(SP), 
            xmin = -119.945, 
            xmax = -119.85, 
            ymin = 33.853, 
            ymax = 33.94) +
      scalebar(x.min = -119.7,
               x.max = -119.55,
               y.min = 33.855,
               y.max = 34.15,
               dist = 3, dist_unit = "mi",
               st.bottom = FALSE, st.color = "white",
               transform = TRUE, model = "WGS84") 
    )
    dev.off()
  }
  
  { # Anacapa  ----
    ANI <- get_googlemap(c(-119.3996, 34.0044),
                         zoom = 13,
                         maptype = "satellite",
                         style = c(labels = "off"))
    
    png(filename = "Maps/AN.png",
        width = 1000, height = 1000, type = "cairo")
    
    SP <-  C + 
      geom_point(data = NULL,
                 aes(x = -119.3996, y = 34.0044), color = "white", stroke = 1,
                 size = 10, inherit.aes = FALSE, shape = 0) 
    
    print(ggmap(ANI, extent = "device") +
      geom_sf(data = marine, aes(fill = Type, color = Type),
              inherit.aes = FALSE, alpha = .1) +
      geom_sf(data = transects, aes(color = Site_Code),
              inherit.aes = FALSE, alpha = .6, color = "lightblue", size = 1) +
      geom_sf(data = NPS_boundary, inherit.aes = FALSE, 
              color = "forestgreen", size = 1, alpha = 0) +
      geom_sf_label(data = marine, inherit.aes = FALSE, hjust = .5, vjust = 1, size = 6,
                    aes(label = NAME)) +
      geom_point(data = Site_Info,
                 aes(x = Longitude, y = Latitude, color = ReserveStatus),
                 size = 2, inherit.aes = FALSE) +
      geom_text(data = Site_Info, hjust = 0, vjust = 0.5, size = 6, color = "white",
                aes(x = Longitude + .001, y = Latitude, label = SiteCode)) +
      scale_fill_manual(values = c(
        'SMR' = "green", 'FMR' = "deepskyblue2", 
        'SMCA' = "blue2", 'FMCA' = "red2")) +
      scale_color_manual(values = c(
        'SMR' = "green", 'FMR' = "deepskyblue2", 
        'SMCA' = "blue2", 'FMCA' = "red2",
        'Inside' = "green", 'Outside' = "red2")) +
      labs(title = "Anacapa Island",
           x = "Longitude", y = "Latitude") +
      map_theme() +
      inset(ggplotGrob(SP), 
            xmin = -119.455, 
            xmax = -119.42, 
            ymin = 33.9625, 
            ymax = 33.985) +
      scalebar(x.min = -119.4,
               x.max = -119.35,
               y.min = 33.9625,
               y.max = 34.02,
               dist = 1, dist_unit = "mi",
               st.bottom = FALSE, st.color = "white",
               transform = TRUE, model = "WGS84") 
    )
    dev.off()
  }
  
  { # Santa Barbara  ----
    SBI <- get_googlemap(c(-119.0373, 33.4756),
                         zoom = 14,
                         maptype = "satellite",
                         style = c(labels = "off"))
    
    png(filename = "Maps/SB.png",
        width = 1000, height = 1000, type = "cairo")
    
    SP <-  C + 
      geom_point(data = NULL,
                 aes(x = -119.0373, y = 33.4756), color = "white", stroke = 1,
                 size = 8, inherit.aes = FALSE, shape = 0) 
    
    print(ggmap(SBI, extent = "device") +
      geom_sf(data = marine, aes(fill = Type, color = Type),
              inherit.aes = FALSE, alpha = .1) +
      geom_sf(data = transects, aes(color = Site_Code),
              inherit.aes = FALSE, alpha = .6, color = "lightblue", size = 1) +
      geom_sf(data = NPS_boundary, inherit.aes = FALSE, 
              color = "forestgreen", size = 1, alpha = 0) +
      geom_sf_label(data = marine, inherit.aes = FALSE, hjust = .5, vjust = 1, size = 6,
                    aes(label = NAME)) +
      geom_point(data = Site_Info,
                 aes(x = Longitude, y = Latitude, color = ReserveStatus),
                 size = 2, inherit.aes = FALSE) +
      geom_text(data = Site_Info, hjust = 0, vjust = 0.5, size = 6, color = "white",
                aes(x = Longitude + .001, y = Latitude, label = SiteCode)) +
      scale_fill_manual(values = c(
        'SMR' = "green", 'FMR' = "deepskyblue2", 
        'SMCA' = "blue2", 'FMCA' = "red2")) +
      scale_color_manual(values = c(
        'SMR' = "green", 'FMR' = "deepskyblue2", 
        'SMCA' = "blue2", 'FMCA' = "red2",
        'Inside' = "green", 'Outside' = "red2")) +
      labs(title = "Santa Barbara Island",
           x = "Longitude", y = "Latitude") +
      map_theme() +
      inset(ggplotGrob(SP), 
            xmin = -119.065, 
            xmax = -119.048, 
            ymin = 33.455, 
            ymax = 33.4675) +
      scalebar(x.min = -119.03,
               x.max = -119.015,
               y.min = 33.455,
               y.max = 33.49,
               dist = .5, dist_unit = "mi",
               st.bottom = FALSE, st.color = "white",
               transform = TRUE, model = "WGS84") 
    )
    dev.off()
  }
  
}

{ # MPAs  ----
  
  { # South Point SMR   ----
    SouthPointSMR <- get_googlemap(c(-120.1357, 33.90037),
                                   zoom = 13,
                                   maptype = "satellite",
                                   style = c(labels = "off"))
    
    png(filename = "Maps/SPSMR.png",
        width = 1000, height = 1000, type = "cairo")
    
    SP <-  C + 
      geom_point(data = NULL,
                 aes(x = -120.14, y = 33.9), color = "white", stroke = 1,
                 size = 8, inherit.aes = FALSE, shape = 0) 
    
    print(ggmap(SouthPointSMR, extent = "device") +
      geom_sf(data = marine, aes(fill = Type, color = Type),
              inherit.aes = FALSE, alpha = .1) +
      geom_sf_label(data = marine, inherit.aes = FALSE, hjust = .5, vjust = .5, size = 6,
                    aes(label = FULLNAME)) +
      geom_sf(data = transects, aes(color = Site_Code),
              inherit.aes = FALSE, alpha = .6, color = "lightblue", size = 1) +
      geom_sf(data = NPS_boundary, inherit.aes = FALSE, 
              color = "forestgreen", size = 1, alpha = 0) +
      geom_point(data = Site_Info,
                 aes(x = Longitude, y = Latitude, color = ReserveStatus),
                 size = 2, inherit.aes = FALSE) +
      geom_label(data = Site_Info, hjust = .1, vjust = 1, size = 6,
                 aes(x = Longitude + .0001, y = Latitude - .001, label = SiteName)) +
      scale_fill_manual(values = c(
        'SMR' = "green", 'FMR' = "deepskyblue2", 
        'SMCA' = "blue2", 'FMCA' = "red2")) +
      scale_color_manual(values = c(
        'SMR' = "green", 'FMR' = "deepskyblue2", 
        'SMCA' = "blue2", 'FMCA' = "red2",
        'Inside' = "green", 'Outside' = "red2")) +
      labs(title = "South Point SMR at Santa Rosa Island",
           x = "Longitude", y = "Latitude") +
      map_theme() +
      inset(ggplotGrob(SP), xmin = -120.19, xmax = -120.16, ymin = 33.858, ymax = 33.882) +
      scalebar(x.min = -120.13,
               x.max = -120.09,
               y.min = 33.86,
               y.max = 33.94,
               dist = .5, dist_unit = "mi",
               st.bottom = FALSE, st.color = "white",
               transform = TRUE, model = "WGS84") 
    )
    dev.off()
  }
  
  { # Scorpion SMR   ----
    ScorpionSMR <- get_googlemap(c(-119.5669, 34.05428),
                                 zoom = 13,
                                 maptype = "satellite",
                                 style = c(labels = "off"))
    
    png(filename = "Maps/SSMR.png",
        width = 1000, height = 1000, type = "cairo")
    
    SC <- C +
      geom_point(data = NULL,
                 aes(x = -119.57, y = 34.045), color = "white", stroke = 1,
                 size = 8, inherit.aes = FALSE, shape = 0) 
    
    print(ggmap(ScorpionSMR, extent = "device") +
      geom_sf(data = marine, aes(fill = Type, color = Type),
              inherit.aes = FALSE, alpha = .1) +
      geom_sf_label(data = marine, inherit.aes = FALSE, hjust = .5, vjust = .5, size = 6,
                    aes(label = FULLNAME)) +
      geom_sf(data = transects, aes(color = Site_Code),
              inherit.aes = FALSE, alpha = .6, color = "lightblue", size = 1) +
      geom_sf(data = NPS_boundary, inherit.aes = FALSE, 
              color = "forestgreen", size = 1, alpha = 0) +
      geom_point(data = Site_Info,
                 aes(x = Longitude, y = Latitude, color = ReserveStatus),
                 size = 2, inherit.aes = FALSE) +
      geom_label(data = Site_Info, hjust = .1, vjust = 0, size = 6,
                 aes(x = Longitude + .001, y = Latitude + .001, label = SiteName)) +
      scale_fill_manual(values = c(
        'SMR' = "green", 'FMR' = "deepskyblue2", 
        'SMCA' = "blue2", 'FMCA' = "red2")) +
      scale_color_manual(values = c(
        'SMR' = "green", 'FMR' = "deepskyblue2", 
        'SMCA' = "blue2", 'FMCA' = "red2",
        'Inside' = "green", 'Outside' = "red2")) +
      labs(title = "Scorpion SMR at Santa Cruz Island",
           x = "Longitude",
           y = "Latitude") +
      map_theme() +
      inset(ggplotGrob(SC), 
            xmin = -119.62,
            xmax = -119.59, 
            ymin = 34.012, 
            ymax = 34.035) +
      scalebar(x.min = -119.56,
               x.max = -119.52,
               y.min = 34.015,
               y.max = 34.08,
               dist = .5, dist_unit = "mi",
               st.bottom = FALSE, st.color = "white",
               transform = TRUE, model = "WGS84") 
    )
    dev.off()
  }
  
  { # Anacapa Island SMR   ----
    AnacapaSMR <- get_googlemap(c(-119.40, 34.01260),
                                zoom = 13,
                                maptype = "satellite",
                                style = c(labels = "off"))
    
    png(filename = "Maps/ANISMR.png",
        width = 1000, height = 1000, type = "cairo")
    
    AN <- C +
      geom_point(data = NULL,
                 aes(x = -119.4, y = 34.01), color = "white", stroke = 1,
                 size = 8, inherit.aes = FALSE, shape = 0) 
    
    print(ggmap(AnacapaSMR, extent = "device") +
      geom_sf(data = marine, aes(fill = Type, color = Type),
              inherit.aes = FALSE, alpha = .1) +
      geom_sf_label(data = marine, inherit.aes = FALSE, hjust = .5, vjust = .5, size = 6,
                    aes(label = FULLNAME)) +
      geom_sf(data = transects, aes(color = Site_Code),
              inherit.aes = FALSE, alpha = .6, color = "lightblue", size = 1) +
      geom_sf(data = NPS_boundary, inherit.aes = FALSE, 
              color = "forestgreen", size = 1, alpha = 0) +
      geom_point(data = Site_Info,
                 aes(x = Longitude, y = Latitude, color = ReserveStatus),
                 size = 2, inherit.aes = FALSE) +
      geom_label(data = Site_Info, hjust = 1, vjust = 0, size = 4.5,
                 aes(x = Longitude + .001, y = Latitude + .0009, label = SiteName)) +
      scale_fill_manual(values = c(
        'SMR' = "green", 'FMR' = "deepskyblue2",
        'SMCA' = "blue2", 'FMCA' = "red2")) +
      scale_color_manual(values = c(
        'SMR' = "green", 'FMR' = "deepskyblue2", 
        'SMCA' = "blue2", 'FMCA' = "red2",
        'Inside' = "green", 'Outside' = "red2")) +
      labs(title = "Anacapa Island SMR",
           x = "Longitude",
           y = "Latitude") +
      map_theme() +
      inset(ggplotGrob(AN),
            xmin = -119.4525, 
            xmax = -119.4225, 
            ymin = 33.9675,
            ymax = 33.999) +
      scalebar(x.min = -119.39,
               x.max = -119.355,
               y.min = 34.04,
               y.max = 33.975,
               dist = 1, dist_unit = "mi",
               st.bottom = FALSE, st.color = "white",
               transform = TRUE, model = "WGS84") 
    )
    dev.off()
  }
  
  { # Santa Barbara Island SMR   ----
    SantaBarbaraSMR <- get_googlemap(c(-119.0392, 33.475),
                                     zoom = 14,
                                     maptype = "satellite",
                                     style = c(labels = "off"))
    
    png(filename = "Maps/SBISMR.png",
        width = 1000, height = 1000, type = "cairo")
    
    SB <- C + 
      geom_point(data = NULL,
                 aes(x = -119.04, y = 33.475), color = "white", stroke = 1,
                 size = 8, inherit.aes = FALSE, shape = 0) 
    
    print(ggmap(SantaBarbaraSMR, extent = "device") +
      geom_sf(data = marine, aes(fill = Type, color = Type),
              inherit.aes = FALSE, alpha = .1) +
      geom_sf_label(data = marine, inherit.aes = FALSE, hjust = .5, vjust = .5, size = 6,
                    aes(label = FULLNAME)) +
      geom_sf(data = transects, aes(color = Site_Code),
              inherit.aes = FALSE, alpha = .6, color = "lightblue", size = 1) +
      geom_sf(data = NPS_boundary, inherit.aes = FALSE, 
              color = "forestgreen", size = 1, alpha = 0) +
      geom_point(data = Site_Info,
                 aes(x = Longitude, y = Latitude, color = ReserveStatus),
                 size = 2, inherit.aes = FALSE) +
      geom_label(data = Site_Info, hjust = 0, vjust = 1, size = 6,
                 aes(x = Longitude + .001, y = Latitude + .0001, label = SiteName)) +
      scale_fill_manual(values = c(
        'SMR' = "green", 'FMR' = "deepskyblue2", 
        'SMCA' = "blue2", 'FMCA' = "red2")) +
      scale_color_manual(values = c(
        'SMR' = "green", 'FMR' = "deepskyblue2", 
        'SMCA' = "blue2", 'FMCA' = "red2",
        'Inside' = "green", 'Outside' = "red2")) +
      labs(title = "Santa Barbara Island SMR",
           x = "Longitude",
           y = "Latitude") +
      map_theme() +
      inset(ggplotGrob(SB), 
            xmin = -119.0725, xmax = -119.045, 
            ymin = 33.454, ymax = 33.465) +
      scalebar(x.min = -119.03,
               x.max = -119.015,
               y.min = 33.49,
               y.max = 33.455,
               dist = .25, dist_unit = "mi",
               st.bottom = FALSE, st.color = "white",
               transform = TRUE, model = "WGS84") 
    )
    dev.off()
  } 
  
}

{ # Site Loop   ----
  
  for (site in Site_Info$SiteCode) {
    Site <- Site_Info %>% 
      dplyr::filter(SiteCode == site)
    
    isl_zoom_level <- 
      if (Site$IslandCode == "SM") {12} 
    else if (Site$IslandCode == "SB") {14}  
    else if (Site$IslandCode == "AN") {13} else {11}
    
    site_zoom_level <- 
      if (Site$SiteCode == "BSBR") {15} 
    else if (Site$SiteCode == "YB") {15} 
    else if (Site$SiteCode == "RR") {15} 
    else if (Site$SiteCode == "CP") {15} else {16}
    
    xmn <- if (site_zoom_level == 15) 
      {Site$Longitude - .0129}
    else {Site$Longitude - .0065}
    
    xmx <- if (site_zoom_level == 15) 
      {Site$Longitude - .0035}
    else {Site$Longitude - .002}
    
    ymn <- if (site_zoom_level == 15) 
      {Site$Latitude - .0105}
    else {Site$Latitude - .001}
    
    ymx <- if (site_zoom_level == 15)
      {Site$Latitude - .0035}
    else {Site$Latitude - .006}
    
    x.mn <- if (site_zoom_level == 15) 
    {Site$Longitude}
    else {Site$Longitude}
    
    x.mx <- if (site_zoom_level == 15) 
    {Site$Longitude + .0105}
    else {Site$Longitude + .006}
    
    y.mn <- if (site_zoom_level == 15) 
    {Site$Latitude - .01}
    else {Site$Latitude - .005}
    
    y.mx <- if (site_zoom_level == 15)
    {Site$Latitude + .01}
    else {Site$Latitude + .004}
    
    Site_Map <- get_googlemap(
      c(Site$Longitude, Site$Latitude),
      scale = 2, 
      zoom = site_zoom_level,
      maptype = "satellite",
      style = c(labels = "off"))
    
    inseter <- get_googlemap(
      c(lon = Site$Island_Longitude, lat = Site$Island_Latitude),
      zoom = isl_zoom_level,
      maptype = "satellite",
      style = c(labels = "off"))
    
     sz <- if (isl_zoom_level == 11) {7}
     else if(isl_zoom_level == 12) {10}
     else if(isl_zoom_level == 13) {15}
     else {25}
    
    png(filename = glue::glue("Maps/{site}.png"),
        width = 1000, height = 1000, type = "cairo")
    
    St <- ggmap(inseter, extent = "device") +
      geom_sf(data = marine, aes(fill = Type, color = Type),
              inherit.aes = FALSE, alpha = .1) +
      geom_point(data = Site,
                 aes(x = Longitude, y = Latitude), color = "white", stroke = 1,
                 size = sz, inherit.aes = FALSE, shape = 0) +
      geom_point(data = Site_Info,
                 aes(x = Longitude, y = Latitude, color = ReserveStatus),
                 size = 2, inherit.aes = FALSE) +
      scale_fill_manual(values = c(
        'SMR' = "green", 'FMR' = "deepskyblue2", 
        'SMCA' = "blue2", 'FMCA' = "red2")) +
      scale_color_manual(values = c(
        'SMR' = "green", 'FMR' = "deepskyblue2", 
        'SMCA' = "blue2", 'FMCA' = "red2",
        'Inside' = "green", 'Outside' = "red2")) +
      labs(title = NULL, x = NULL, y = NULL) +
      theme_classic() +
      theme(legend.position = "none",
            legend.title = element_text(),
            axis.text = element_blank())
    print(
      ggmap(Site_Map, extent = "device") +
        geom_sf(data = transects, aes(color = Site_Code),
                inherit.aes = FALSE, alpha = .6, color = "lightblue", size = 1) +
        geom_sf(data = NPS_boundary, inherit.aes = FALSE, 
                color = "forestgreen", size = 1, alpha = 0) +
        geom_point(data = Site,
                   aes(x = Longitude, y = Latitude),
                   size = 2, inherit.aes = FALSE, color = "dodgerblue") +
        geom_point(data = Site,
                   aes(x = Start_Longitude, y = Start_Latitude),
                   size = 2, inherit.aes = FALSE, color = "green") +
        geom_text(data = Site, hjust = 0, vjust = 0, size = 6,
                  aes(x = Start_Longitude + .0001, y = Start_Latitude, label = Start_Label),
                  inherit.aes = FALSE, color = "white") +
        geom_point(data = Site,
                   aes(x = End_Longitude, y = End_Latitude),
                   size = 2, inherit.aes = FALSE, color = "red") +
        geom_text(data = Site, hjust = 1, vjust = 1, size = 6,
                  aes(x = End_Longitude - .0001, y = End_Latitude, label = End_Label),
                  inherit.aes = FALSE, color = "white") +
        scale_fill_manual(values = c(
          'SMR' = "green", 'FMR' = "deepskyblue2", 
          'SMCA' = "blue2", 'FMCA' = "red2")) +
        scale_color_manual(values = c(
          'SMR' = "green", 'FMR' = "deepskyblue2", 
          'SMCA' = "blue2", 'FMCA' = "red2",
          'Inside' = "green", 'Outside' = "red2")) +
        labs(title = paste(Site$SiteName, " - ", Site$IslandCode),
             x = "Longitude", y = "Latitude") +
        map_theme() +
        inset(ggplotGrob(St), xmin = xmn, xmax = xmx, ymin = ymn, ymax = ymx) +
        scalebar(x.min = x.mn,
                 x.max = x.mx,
                 y.min = y.mn,
                 y.max = y.mx,
                 dist = .1, dist_unit = "mi",
                 st.bottom = FALSE, st.color = "white",
                 transform = TRUE, model = "WGS84") 
      )
    dev.off()
  }
}

{ # Niño Region 3.4   ----
  ONI <- get_googlemap(c(-145, 0),
                       zoom = 2,
                       # maptype = "satellite",
                       style = c(labels = "off"))
  
  png(filename = "Maps/ONI.png",
      width = 1000, height = 600, type = "cairo")
  print(
  ggmap(ONI, extent = "device") + 
    labs(title = "Niño Region 3.4",
         subtitle = "± 5° Latitude, 170° W to 120° W",
         x = "Longitude", y = "Latitude") +
    map_theme() + 
    geom_rect(data = NULL, alpha = 0, color = "black",
              aes(xmin = -170, 
                  xmax = -120, 
                  ymin = -5, 
                  ymax = 5)) +
    geom_segment(data = NULL, aes(x = -Inf, xend = Inf, y = 0, yend = 0)) +
    ylim(-40, 40) +
    scalebar(x.min = -100,
             x.max = -50,
             y.min = -40,
             y.max = 40,
             dist = 1000, dist_unit = "mi",
             st.bottom = FALSE, st.color = "black",
             transform = TRUE, model = "WGS84")
  )
  dev.off()
}








