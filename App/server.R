
# Define server logic
server <- function(input, output, session) {
  
  protocol_Server(id = "1m")
  protocol_Server(id = "5m")
  protocol_Server(id = "bands")
  protocol_Server(id = "rpcs")
  protocol_Server(id = "nhsf")
  protocol_Server(id = "arms")
  protocol_Server(id = "rdfc")
  protocol_Server(id = "vft")
  protocol_Server(id = "fsf")
  protocol_Server(id = "vtt")
  protocol_Server(id = "temp")
  protocol_Server(id = "species")
  
  { # ........ Map_Servers ........   ----
    
    { # Leaflet Maps     ----
      
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
    
    { # Satellite Site Maps  -----
      
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
    
    { # Bathymetry Maps   ----
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
    
    { # ARM Maps   ----
      
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
    
    { # Site Descriptions   ----
      
      output$SitePDF <- renderUI({
        tags$iframe(
          style = "height:700px; width:100%; scrolling=yes",
          src = glue::glue(
            "Handbook/Site_Descriptions/{ 
            Site_Selector_Server(id = 'Site_Descriptions')()$SiteNumber}.pdf"))
      })
    }
  }
  
  output$Shannon_Plot <- renderPlot({ # Biodiversity  ----
    
   
    Shannon_Split <- base::split(Shannon_Index, f = Shannon_Index$IslandName) 
    p1 <- ggplot(Shannon_Split$`San Miguel Island`,
                 aes(x = Date, y = Shannon_Index, color = SiteCode, linetype = SiteCode)) + 
      # geom_line(size = .5) +
      geom_smooth(size = .5, se = FALSE, method = 'loess', formula = 'y ~ x') +
      ggplot2::scale_x_date(date_labels = "%Y", date_breaks = "year", expand = c(0, 0),
                            limits = c(lubridate::ymd(min(Shannon_Index$Date)),
                                       lubridate::ymd(max(Shannon_Index$Date)))) +
      scale_y_continuous(limits = c(0, NA), 
        expand = expansion(mult = c(0, .1))) +
      labs(title = "Shannon-Weiner Diversity Index",  
           color = "Site Code", linetype = "Site Code",
           x = NULL, y = NULL) +
      facet_grid(rows = vars(IslandName), scales = "fixed") +
      scale_color_manual(values = SiteColor, guide = guide_legend(ncol = 2)) +
      scale_linetype_manual(values = SiteLine, guide = guide_legend(ncol = 2)) +
      all_sites_theme()
    p2 <- p1 %+% Shannon_Split$`Santa Rosa Island` +
      labs(title = NULL, subtitle = NULL)
    p3 <- p1 %+% Shannon_Split$`Santa Cruz Island` +
      labs(title = NULL, subtitle = NULL)
    p4 <- p1 %+% Shannon_Split$`Anacapa Island` +
      labs(title = NULL, subtitle = NULL)
    p5 <- p1 %+% Shannon_Split$`Santa Barbara Island`  +
      labs(title = NULL, subtitle = NULL, x = "Year") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1, size = 10))
    arrange_plot <- ggpubr::ggarrange(
      p1, p2, p3, p4, p5, ncol = 1, heights = c(.9, .75, .75, .75, 1),
      align = "v", common.legend = FALSE)
    annotate_plot <- ggpubr::annotate_figure(
      arrange_plot, left = text_grob("Index Value", color = "black", rot = 90, size = 16)
    )
    print(annotate_plot)
    
    
    
    
    
    # p1 <- ggplot2::ggplot(Shannon_Index, aes(x = Date, y = Shannon_Index, linetype = ReserveStatus)) +
    #   ggplot2::geom_smooth(size = 1, span = 0.75,
    #                        aes(color = ReserveStatus)) +
    #   ggplot2::scale_x_date(date_labels = "%Y", date_breaks = "year", expand = c(0, 0)) +
    #   ggplot2::scale_y_continuous(limits = c(0, NA), expand = c(0, 0), oob = squish) +
    #   ggplot2::scale_colour_manual(values = Island_Colors) +
    #   ggplot2::labs(title = "Shannon-Weiner Diversity Index",
    #                 x = NULL, y = NULL,
    #                 linetype = "Reserve Status",
    #                 color = "Reserve Status") +
    #   timeseries_top_theme()
    # 
    # p2 <- ggplot2::ggplot(Shannon_Index, aes(x = Date, y = Shannon_Index, color = IslandName)) +
    #   ggplot2::geom_smooth(size = 1, span = .75) +
    #   ggplot2::scale_x_date(date_labels = "%Y", date_breaks = "year", expand = c(0, 0)) +
    #   ggplot2::scale_y_continuous(limits = c(0, NA), expand = c(0, 0), oob = squish) +
    #   ggplot2::scale_colour_manual(values = Island_Colors) +
    #   ggplot2::labs(x = NULL, y = NULL,
    #                 color = "Island") +
    #   timeseries_top_theme()
    # 
    # p3 <- ggplot2::ggplot() +
    #   geom_rect(data = SST_Anomaly_Index,
    #             aes(xmin = DateStart, xmax = DateEnd, ymin = -Inf, ymax = 0, fill = ONI_ANOM)) +
    #   scale_fill_gradient2(high = "darkred", mid = "white", low = "navyblue", midpoint = 0,
    #                        guide = guide_colorbar(direction = "horizontal", title.position = "top",
    #                                               order = 3, barheight = unit(.2, "cm"))) +
    #   ggplot2::geom_smooth(data = Shannon_Index, method = 'loess', formula = 'y~x', size = 1, se = F, span = .75,
    #                        aes(x = Date, y = Shannon_Index, color = IslandName, linetype = ReserveStatus)) +
    #   ggplot2::geom_hline(aes(yintercept = 0)) +
    #   ggplot2::scale_x_date(date_labels = "%Y", date_breaks = "year", expand = c(0, 0),
    #                         limits = c(lubridate::ymd(min(Shannon_Index$Date)), 
    #                                    lubridate::ymd(max(Shannon_Index$Date)))) +
    #   ggplot2::scale_y_continuous(expand = expansion(mult = c(.1, 0)),
    #                               limits = c(0, NA), oob = squish) +
    #   ggplot2::guides(color = guide_legend(order = 1), 
    #                   linetype = guide_legend(order = 2, override.aes = list(col = 'black'))) +
    #   ggplot2::scale_colour_manual(values = Island_Colors) +
    #   ggplot2::labs(x = "Survey Year", y = NULL,
    #                 color = "Island",
    #                 fill = "Oceanic Niño Index",
    #                 linetype = "Reserve Status") +
    #   timeseries_bottom_theme() 
    # Diversity_Plot <-ggarrange(p1, p2, p3, ncol = 1, align = "v", heights = c(.8, .8, 1))
    # Diversity_annotated <- ggpubr::annotate_figure(
    #   Diversity_Plot,
    #   left = text_grob("Diversity Index Value", 
    #                    family ="Cambria", color = "black", rot = 90, size = 13))
    # print(Diversity_annotated)
    
    
  })
  
  output$Three_D <- renderPlotly({
    
      plotly::plot_ly(nMDS_3D, x = ~`Dim 1`, y = ~`Dim 2`, z = ~`Dim 3`,
                      frame = ~SurveyYear, text = ~SiteName, hoverinfo = "text",
                      color = ~ReserveStatus, colors = Island_Colors) %>%
      plotly::add_markers(symbol = ~ReserveStatus,
                          symbols = c('Inside' = "cross-open", 'Outside' = "square")) %>%
      plotly::add_text(text = ~SiteCode) %>%
      plotly::layout(scene = list(xaxis = list(title = 'Dim 1'),
                                  yaxis = list(title = 'Dim 2'),
                                  zaxis = list(title = 'Dim 3'))) %>%
      plotly::animation_opts(1500, easing = "linear")
    
  })
  
} 