
# Define server logic
server <- function(input, output, session) {
  
  { # Protocols  -----
    protocol_Server(id = "protocol")
  }
  
  { # Species   ----
    foundation_Server(id = "kelp")
    foundation_Server(id = "p_urchin")
    foundation_Server(id = "r_urchin")
    foundation_Server(id = "r_abalone")
    foundation_Server(id = "lobsta")
    foundation_Server(id = "sheep")
    foundation_Server(id = "sunflower")
    foundation_Server(id = "giant")
    
    # Invasives
    foundation_Server(id = "sargassum")
    foundation_Server(id = "undaria")
    
    # Disease
    output$SSWD <- renderUI({tags$iframe(
      style = "height:650px; width:100%; scrolling=yes",
      src = "Handbook/Outside_Program_Guides/stars_disease_guide.pdf")
    })
    
    output$urchins <- renderUI({tags$iframe(
      style = "height:650px; width:100%; scrolling=yes",
      src = "Handbook/Outside_Program_Guides/urchin_disease_guide.pdf")
    })
    
    output$abalone <- renderImage({list(
      src = "www/Handbook/Outside_Program_Guides/healthyVshrunken.jpg", 
      width = "100%", height = "100%")}, delete = FALSE)
    
    # Species List
    # output$Species_List <- renderUI({tags$iframe(
    #   style = "height:650px; width:100%; scrolling=yes",
    #   src = "Handbook/Species_Guides/species_species_guide.pdf")
    # })
    species_guide_Server(id = "species")
  }
  
  
  Taxa_Server(id = "species")
  
  
  { # Maps   ----
    
    { # .... Leaflet Maps     ----
      
      output$Leaflet <- renderLeaflet({
        leaflet() %>%
          setView(lng = -119.7277, lat = 33.76416, zoom = 9) %>%
          addTiles(group = "OSM (default)") %>% 
          addProviderTiles(providers$Esri, group = "ESRI") %>%
          addProviderTiles(providers$Esri.OceanBasemap, group = "Ocean Base") %>%
          addProviderTiles(providers$Esri.WorldImagery, group = "Imagery") %>%
          addProviderTiles(providers$Esri.WorldTopoMap, group = "Topography") %>%
          addProviderTiles(providers$Esri.NatGeoWorldMap, group = "Nat. Geo.") %>%
          addPolygons(data = marine, color = marine$Color, weight = 1,
                      fillOpacity = 0.1, opacity = 0.25, label = marine$NAME, group = "MPA Boundaries")  %>%
          addPolygons(data = NPS_boundary, weight = 2, color = "green", fill = FALSE,
                      label = "Channel Islands National Park (CINP) Boundary", group = "CINP Boundary") %>%
          addPolygons(data = CINMS_boundary, weight = 2, color = "blue", fill = FALSE,
                      label = "Channel Islands National Marine Sanctuary (CINMS) Boundary", group = "CINMS Boundary") %>%
          addPolylines(data = transects, group = "Transects")  %>%
          addCircles(radius = 1, group = "Transect End Points", color = "green",
                     lng = Transect_Endpoints$Start_Long, lat = Transect_Endpoints$Start_Lat, 
                     label = Transect_Endpoints$Start_Label) %>%
          addCircles(radius = 1, group = "Transect End Points", color = "red",
                     lng = Transect_Endpoints$End_Long, lat = Transect_Endpoints$End_Lat, 
                     label = Transect_Endpoints$End_Label) %>%
          addMarkers(data = Site_Info, label = paste(Site_Info$IslandCode, Site_Info$SiteName), group = "Site Markers") %>% 
          addCircleMarkers(data = Buoys_List, label = Buoys_List$DC.description, group = "Buoy Stations") %>% 
          addLayersControl(
            baseGroups = c("OSM (default)", "ESRI", "Ocean Base", "Imagery", "Topography", "Nat. Geo."),
            overlayGroups = c("Site Markers", "Transects", "Transect End Points",
                              "MPA Boundaries", "CINP Boundary", "CINMS Boundary",  "Buoy Stations"),
            options = layersControlOptions(collapsed = TRUE)) %>%
          addMeasure(position = "bottomleft",
                     primaryLengthUnit = "meters",
                     primaryAreaUnit = "sqmeters",
                     activeColor = "#3D535D",
                     completedColor = "#7D4479")
      })
      
    }
    
    { # .... Satellite Site Maps  -----
      
      satMapCode <- reactive({
        if (input$Sat_Isl_Site == "Park") {
          return("CHIS")
        }
        else if (input$Sat_Isl_Site == "Island") {
          return(dplyr::filter(Site_Info, IslandName == input$Sat_Isl)$IslandCode[1])
        }
        else if (input$Sat_Isl_Site == "MPA") {
          return(dplyr::filter(Site_Info, Reference == TRUE, IslandName == input$Sat_MPA)$MPA_Code[1])
        } 
        else {
          return(Site_Selector_Server(id = 'Site_Sat')()$SiteCode)
        } 
      })
      
      output$satMap <- renderImage({
        list(
          src = glue("www/Maps/Satellite/{satMapCode()}.png"),
          contentType = "image/png",
          width = if (input$Sat_Isl_Site == "Park") {1000} else {750},
          height = if (input$Sat_Isl_Site == "Park") {772.72} else {750}
        )
      }, deleteFile = FALSE)
    }
    
    { # .... Bathymetry Maps   ----
      Bath_Site <- reactive(
        dplyr::filter(Site_Info, SiteName == input$Bath_Maps_Site)$SiteNumber)
      
      output$Bathymetry_Map <- renderImage({
        list(
          src = glue::glue("www/Maps/Bathymetry/{Bath_Site()}.png"),
          contentType = "image/png",
          width = 1000,
          height = 750
        )
      } , deleteFile = FALSE)
      
      
    }
    
    { # .... ARM Maps   ----
      
      ARM_Site <- reactive(
        dplyr::filter(Site_Info, SiteName == input$Arm_Maps_Site)$SiteNumber)
      
      output$ARM_Map <- renderImage({
        list(
          src = glue("www/Maps/ARMs/{ARM_Site()}.png"),
          contentType = "image/png",
          width = 1000,
          height = 750
        )
      }, deleteFile = FALSE)
      
    }
    
    { # .... Site Descriptions   ----
      
      output$SitePDF <- renderUI({
        tags$iframe(
          style = "height:700px; width:100%; scrolling=yes",
          src = glue::glue(
            "Handbook/Site_Descriptions/{ 
            Site_Selector_Server(id = 'Site_Descriptions')()$SiteNumber}.pdf"))
      })
    }
  }
  
  { # Biodiversity   ----
    diversity_Server(id = "richness") 
    diversity_Server(id = "shannon")
    diversity_Server(id = "simpson") 
  }
  
  { # Community Similarity  ----
    
    Two_D_data <- reactive({
      if (input$radio_2D_years == "All Years (Fewer Species)" 
          & input$radio_2D_color == "Reserve Status") {
        nMDS_2D_All %>% 
          dplyr::filter(SurveyYear == input$slider2d_all) %>% 
          dplyr::mutate(Color = ReserveStatus)
      } 
      else if (input$radio_2D_years == "All Years (Fewer Species)" 
               & input$radio_2D_color == "Island Name") {
        nMDS_2D_All %>% 
          dplyr::filter(SurveyYear == input$slider2d_all) %>% 
          dplyr::mutate(Color = IslandName)
      }
      else if (input$radio_2D_years == "Years > 2004 (All Species)" 
               & input$radio_2D_color == "Reserve Status") {
        nMDS_2D_2005 %>% 
          dplyr::filter(SurveyYear == input$slider2d_2005) %>% 
          dplyr::mutate(Color = ReserveStatus)
      }
      else if (input$radio_2D_years == "Years > 2004 (All Species)" 
               & input$radio_2D_color == "Island Name") {
        nMDS_2D_2005 %>% 
          dplyr::filter(SurveyYear == input$slider2d_2005) %>% 
          dplyr::mutate(Color = IslandName)
      }
    })
    
    output$Two_D <- renderPlot({
      ggplot(data = Two_D_data(),
             aes(x = NMDS1, y = NMDS2)) + 
        geom_point(size = 4, aes(shape = ReserveStatus, color = Color)) + 
        geom_text(size = 3, vjust = 2, aes(label = SiteCode)) +  
        # stat_ellipse(aes(color = IslandName), level = 0.95) +
        # stat_stars(aes(color = ReserveStatus)) +
        scale_colour_manual(values = Island_Colors) +
        coord_fixed() +
        scale_x_reverse() +
        # coord_flip() +
        labs(title = input$slider2d, 
             color = input$radio_2D_color, 
             shape = "Reserve Status") +
        nMDS_theme()
    })
    
    
    Three_D_data <- reactive({
      if (input$radio_3D_years == "All Years (Fewer Species)" 
          & input$radio_3D_color == "Reserve Status") {
        nMDS_3D_All %>%  
          dplyr::filter(SurveyYear == input$slider3d_all) %>% 
          dplyr::mutate(Color = ReserveStatus)
      } 
      else if (input$radio_3D_years == "All Years (Fewer Species)" 
               & input$radio_3D_color == "Island Name") {
        nMDS_3D_All %>% 
          dplyr::filter(SurveyYear == input$slider3d_all) %>% 
          dplyr::mutate(Color = IslandName)
      }
      else if (input$radio_3D_years == "Years > 2004 (All Species)" 
               & input$radio_3D_color == "Reserve Status") {
        nMDS_3D_2005 %>% 
          dplyr::filter(SurveyYear == input$slider3d_2005) %>% 
          dplyr::mutate(Color = ReserveStatus)
      }
      else if (input$radio_3D_years == "Years > 2004 (All Species)" 
               & input$radio_3D_color == "Island Name") {
        nMDS_3D_2005 %>% 
          dplyr::filter(SurveyYear == input$slider3d_2005) %>% 
          dplyr::mutate(Color = IslandName)
      }
    })
    
    output$Three_D <- renderPlotly({
      plotly::plot_ly(Three_D_data(), x = ~`Dim 1`, y = ~`Dim 2`, z = ~`Dim 3`,
                      # frame = ~SurveyYear, 
                      text = ~SiteName, hoverinfo = "text",
                      color = ~Color, colors = Island_Colors) %>%
        plotly::add_markers(symbol = ~ReserveStatus, 
                            symbols = c('Inside' = "cross-open", 'Outside' = "square")) %>%
        plotly::add_text(text = ~SiteCode, showlegend = FALSE) %>%
        plotly::layout(scene = list(xaxis = list(title = 'X'),
                                    yaxis = list(title = 'Y'),
                                    zaxis = list(title = 'Z'))) 
      # %>%
      #   plotly::animation_opts(1500, easing = "linear")
    })
    
  }
  
  { # Variable Importance  ----
    
    { # Random Forest Models ----
      VI_Server(id = "reserve")
      VI_Server(id = "island")
    }
    
    { # Indicator Species Analysis   ----
      
    }
    
  }
  
  { # Biomass and Density   ----
    Time_Server(id = "biomass")
    Time_Server(id = "density")
  }
 
} 




# add kelp and gorgonian species guide and protocol guide
# add shell size frequency guides







