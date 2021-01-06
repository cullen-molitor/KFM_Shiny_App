
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
    foundation_Server(id = "giant-spined")
    
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
      height = "100%")}, delete = FALSE)
    
    species_guide_Server(id = "species")
    
    Taxa_Server(id = "species")
  }
  
  { # Sampling Locations   ----
    
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
      
      site_data <- Site_Info %>%
        dplyr::mutate(Island = IslandName) %>% 
        dplyr::select(SiteNumber, Island, IslandName, SiteCode, SiteName, Reference, ReserveStatus, ARMs, ReserveYear,
                      Latitude, Longitude, MeanDepth, Rock, Cobble, Sand) %>%
        dplyr::rename(`Site #` = SiteNumber, 
                      `Site Code` = SiteCode,
                      `Site` = SiteName,
                      Reference = Reference,
                      `Reserve Status` = ReserveStatus, 
                      `Mean Depth` = MeanDepth, 
                      `Rock (%)` = Rock, 
                      `Cobble (%)` = Cobble, 
                      `Sand (%)` =  Sand) %>% 
        dplyr::mutate(Island = gsub(" Island", "", Island)) 
      
      site_table_data <- reactive({
        if (input$Sat_Isl_Site == 'Island') {
          site_data %>% 
            dplyr::filter(IslandName == input$Sat_Isl) %>% 
            dplyr::select(-IslandName)
        }
        else if (input$Sat_Isl_Site == 'MPA') {
          site_data %>%
            dplyr::filter(IslandName == input$Sat_MPA, Reference == TRUE) %>% 
            dplyr::select(-IslandName)
        }
        else if (input$Sat_Isl_Site == 'Site') {
          site_data %>% 
            dplyr::filter(Site == Site_Selector_Server(id = 'Site_Sat')()$SiteName) %>% 
            dplyr::select(-IslandName) 
        }
        
      }) 
      
      output$Site_Table <- renderDT({
        datatable(
          site_table_data(), rownames = FALSE,  
          options = list(searching = FALSE,  paging = FALSE,
                         ordering = TRUE, info = FALSE, scrollX = TRUE, 
                         initComplete = JS(
                           "function(settings, json) {",
                           "$(this.api().table().header()).css({'background-color': '#3c8dbc', 'color': '#fff'});}"))) %>%
          formatStyle(names(site_table_data()), color = "black", backgroundColor = 'white')
      })
      
      output$Park_Table <- renderDT({
        datatable(
          dplyr::select(site_data, -IslandName), rownames = FALSE, extensions = 'ColReorder',
          options = list(
            scrollY = "500px", scrollX = TRUE, paging = FALSE,
            ordering = TRUE, info = FALSE, dom = 'Bfrtip', colReorder = TRUE,
                         initComplete = JS(
                           "function(settings, json) {",
                           "$(this.api().table().header()).css({'background-color': '#3c8dbc', 'color': '#fff'});}"))) %>%
          formatStyle(names(dplyr::select(site_data, -IslandName)), color = "black", backgroundColor = 'white')
      })
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
      
      ARM_Site <- reactive(dplyr::filter(Site_Info, Isl_SiteName == input$Arm_Maps_Site)$SiteNumber)
      
      output$ARM_Map <- renderImage({
        list(src = glue("www/Maps/ARMs/{ARM_Site()}.png"),
             contentType = "image/png", height = '100%')
      }, deleteFile = FALSE)
      
    }
    
    { # .... Site Descriptions   ----
      Site_Desc_Site <- reactive(dplyr::filter(Site_Info, Isl_SiteName == input$Site_Description_Site)$SiteNumber)
      
      output$Site_Description <- renderImage({
        list(src = glue::glue(
            "www/Handbook/Site_Descriptions/{Site_Desc_Site()}.png"),
            contentType = "image/png", height = '100%')
      }, deleteFile = FALSE)
      
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
        nMDS %>% 
          dplyr::filter(SurveyYear == input$slider2d_all,
                        Type == '2D_All') %>% 
          dplyr::mutate(Color = ReserveStatus)
      } 
      else if (input$radio_2D_years == "All Years (Fewer Species)" 
               & input$radio_2D_color == "Island Name") {
        nMDS %>% 
          dplyr::filter(SurveyYear == input$slider2d_all,
                        Type == '2D_All') %>% 
          dplyr::mutate(Color = IslandName)
      }
      else if (input$radio_2D_years == "Years > 2004 (All Species)" 
               & input$radio_2D_color == "Reserve Status") {
        nMDS %>% 
          dplyr::filter(SurveyYear == input$slider2d_2005,
                        Type == '2D_2005') %>% 
          dplyr::mutate(Color = ReserveStatus)
      }
      else if (input$radio_2D_years == "Years > 2004 (All Species)" 
               & input$radio_2D_color == "Island Name") {
        nMDS %>% 
          dplyr::filter(SurveyYear == input$slider2d_2005,
                        Type == '2D_2005') %>% 
          dplyr::mutate(Color = IslandName)
      }
    })
    
    output$Two_D <- renderPlot({
      ggplot(data = Two_D_data(), aes(x = `Dim 1`, y = `Dim 2`)) + 
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
        nMDS %>%  
          dplyr::filter(SurveyYear == input$slider3d_all,
                        Type == '3D_All') %>% 
          dplyr::mutate(Color = ReserveStatus)
      } 
      else if (input$radio_3D_years == "All Years (Fewer Species)" 
               & input$radio_3D_color == "Island Name") {
        nMDS %>% 
          dplyr::filter(SurveyYear == input$slider3d_all,
                        Type == '3D_All') %>% 
          dplyr::mutate(Color = IslandName)
      }
      else if (input$radio_3D_years == "Years > 2004 (All Species)" 
               & input$radio_3D_color == "Reserve Status") {
        nMDS %>% 
          dplyr::filter(SurveyYear == input$slider3d_2005,
                        Type == '3D_2005') %>% 
          dplyr::mutate(Color = ReserveStatus)
      }
      else if (input$radio_3D_years == "Years > 2004 (All Species)" 
               & input$radio_3D_color == "Island Name") {
        nMDS %>% 
          dplyr::filter(SurveyYear == input$slider3d_2005,
                        Type == '3D_2005') %>% 
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
    
    {
      output$cucumba <- renderImage({list(
        src = "www/Photos/Indicator_Species/11007.jpg", 
        height = "100%")}, delete = FALSE)
      
      output$lobsta <- renderImage({list(
        src = "www/Photos/Indicator_Species/8001.jpg", 
        height = "100%")}, delete = FALSE)
      
      output$rose <- renderImage({list(
        src = "www/Photos/Indicator_Species/6002.jpg", 
        height = "100%")}, delete = FALSE)
      
      output$kelkel <- renderImage({list(
        src = "www/Photos/Indicator_Species/9006.jpg", 
        height = "100%")}, delete = FALSE)
      
    }
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
    
    Ratio_Server(id = 'biomass_ratio')
    Ratio_Server(id = 'density_ratio')
  }
 
  { # Reports   -----
    output$Annual_Report <- renderUI({ 
      tags$iframe(style="height:750px; width:100%; scrolling=yes", src = glue("Annual_Reports/{input$Report}.pdf"))
    })
    
    output$Handbook <- renderUI({ 
      tags$iframe(style="height:750px; width:100%; scrolling=yes", src = glue("Handbook/Full_Versions/{input$old_handy}.pdf"))
    })
    
    output$ReviewsOutput <- renderUI({ 
      tags$iframe(style="height:750px; width:100%; scrolling=yes", src = glue("Handbook/Reviews/{input$reviews}.pdf"))
    })
    
    output$CollaborativeOutput <- renderUI({ 
      tags$iframe(style="height:750px; width:100%; scrolling=yes", src = glue("Handbook/Collaborative_Reports/{input$collab}.pdf"))
    })
  }
  
} 




# add kelp and gorgonian species guide and protocol guide
# add shell size frequency guides







