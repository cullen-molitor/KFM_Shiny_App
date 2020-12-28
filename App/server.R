
# Define server logic
server <- function(input, output, session) {
  
  { # Protocols  -----
    protocol_Server(id = "protocol")
  }
  
  { # Species   ----
    foundation_Sever(id = "kelp")
    foundation_Sever(id = "p_urchin")
    foundation_Sever(id = "r_urchin")
    foundation_Sever(id = "r_abalone")
    foundation_Sever(id = "lobsta")
    foundation_Sever(id = "sheep")
    foundation_Sever(id = "sunflower")
    foundation_Sever(id = "giant")
    
    # Invasives
    foundation_Sever(id = "sargassum")
    foundation_Sever(id = "undaria")
    
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
  
  output$Three_D <- renderPlotly({ # Community Similarity   ----
    com_sim_data <- reactive({
      if (input$radio_3D_years == "All Years (Fewer Species)") {
        nMDS_3D_all_years
      } else {nMDS_3D_2005_now}
    })
    
    plotly::plot_ly(com_sim_data(), x = ~`Dim 1`, y = ~`Dim 2`, z = ~`Dim 3`,
                    frame = ~SurveyYear, text = ~SiteName, hoverinfo = "text", mode = 'markers') %>%
      plotly::add_markers(color = ~ReserveStatus,
                          colors = Island_Colors) %>% 
      plotly::add_markers(symbol = ~ReserveStatus,
                          symbols = c('Inside' = "cross-open", 'Outside' = "square")) %>%
      plotly::add_text(text = ~SiteCode) %>%
      plotly::layout(scene = list(xaxis = list(title = 'Dim 1'),
                                  yaxis = list(title = 'Dim 2'),
                                  zaxis = list(title = 'Dim 3'))) %>%
      plotly::animation_opts(frame = 1500, transition = 500, easing = "elastic")
    
  })
  
  Variable_Importance <- reactive({
      if (input$radio_ISA_years == "All Years (Fewer Species)") {
        RF_Importance_All_Years %>% 
          head(30) %>% 
          droplevels()
      } else {
        RF_Importance_2005 %>% 
          head(30) %>% 
          droplevels()
      }
    })
  
  output$ISA_plot <- renderPlot({
    
    Accuracy <- 
      ggplot(
        Variable_Importance(), aes(x = MeanDecreaseAccuracy, color = Targeted,
                    y = reorder(CommonName, MeanDecreaseAccuracy))) +
      geom_point() +
      geom_segment(
        size = 1, 
        aes(x = min(MeanDecreaseAccuracy) - .5, xend = MeanDecreaseAccuracy, 
            y = CommonName, yend = CommonName)) +
      labs(x = "Mean Decrease in % Accuracy", y = NULL, 
           color = NULL, linetype = NULL) +
      scale_x_continuous(expand = expansion(mult = c(0,.1)), 
                         limits = c(min(Variable_Importance()$MeanDecreaseAccuracy) - .5, NA)) +
      scale_color_manual(values = Target_Colors) +
      theme_classic() +
      theme(axis.text = element_text(size = 12),
            axis.title = element_text(size = 12),
            legend.text = element_text(size = 12))
    
    Gini <- ggplot(Variable_Importance(), aes(x = MeanDecreaseGini, color = Targeted, 
                               y = reorder(CommonName, MeanDecreaseGini))) +
      geom_point() +
      geom_segment(size = 1,
                   aes(x = min(MeanDecreaseGini) - .5, xend = MeanDecreaseGini,
                       y = CommonName, yend = CommonName)) +
      labs(x = "Mean Decrease in Gini Index", y = NULL, 
           color = NULL, linetype = NULL) +
      scale_x_continuous(expand = expansion(mult = c(0,.1)), 
                         limits = c(min(Variable_Importance()$MeanDecreaseGini) - .5, NA)) +
      scale_color_manual(values = Target_Colors) +
      theme_classic() +
      theme(axis.text = element_text(size = 12),
            axis.title = element_text(size = 12),
            legend.text = element_text(size = 12))
    ggarrange(Accuracy, Gini, ncol = 2, align = "h", common.legend = TRUE, legend = "bottom")
  })
  
} 




# add kelp and gorgonian species guide and protocol guide
# add shell size frequency guides







