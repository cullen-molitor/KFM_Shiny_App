
# Define server logic
server <- function(input, output, session) {
  
  { # About  ----
    
    { # Images   ----
      
      { # Disclaimer Pics  ----
        output$disc_pic_1 <- renderImage({list(
        src = 'www/Photos/Kelp_Forest_Scenes/Laurie_Montgomery/1 (2).jpg', 
        height = "100%")}, delete = FALSE)
      }
      
      { # App Basics Pics   ----
        
        output$basics_pic_1 <- renderImage({list(
          src = 'www/Photos/Kelp_Forest_Scenes/Brett_Seymour/1 (4).jpg', 
          height = "100%")}, delete = FALSE)
        
        output$basics_pic_2 <- renderImage({list(
          src = 'www/Photos/Kelp_Forest_Scenes/Kenan_Chan/1 (12).jpg', 
          height = "100%")}, delete = FALSE)
        
        output$basics_pic_3 <- renderImage({list(
          src = 'www/Photos/Kelp_Forest_Scenes/Kenan_Chan/1 (14).jpg', 
          height = "100%")}, delete = FALSE)
        
      }
      
      { # KFMP History Pics   -----
        
        output$history_pic_1 <- renderImage({list(
          src = 'www/Maps/Satellite/CHIS.png', 
          height = "100%")}, delete = FALSE)
        
        output$history_pic_2 <- renderImage({list(
          src = 'www/Photos/Kelp_Forest_Scenes/Brett_Seymour/1 (10).jpg', 
          height = "100%")}, delete = FALSE)
        
        output$history_pic_3 <- renderImage({list(
          src = 'www/Photos/Kelp_Forest_Scenes/Kelly_Moore/1 (1).jpg', 
          height = "100%")}, delete = FALSE)
      }
      
      { # Acknowledgments Pics   ----
        
        output$ack_pic_1 <- renderImage({list(
          src = 'www/Photos/Kelp_Forest_Scenes/Laurie_Montgomery/1 (3).jpg', 
          height = "100%")}, delete = FALSE)
        
        output$ack_pic_2 <- renderImage({list(
          src = 'www/Photos/Kelp_Forest_Scenes/Brett_Seymour/1 (11).jpg', 
          height = "100%")}, delete = FALSE)
        
        output$ack_pic_3 <- renderImage({list(
          src = 'www/Photos/Protocols/boating/boat (2).jpg', 
          height = "100%")}, delete = FALSE)
        
      }
      
      { # Acronyms Pics   ----
        
        output$acr_pic_1 <- renderImage({list(
          src = 'www/Photos/Kelp_Forest_Scenes/Brett_Seymour/1 (6).jpg', 
          height = "100%")}, delete = FALSE)
        
        output$acr_pic_2 <- renderImage({list(
          src = 'www/Photos/Protocols/rpcs/rpcs (1).jpg', 
          height = "100%")}, delete = FALSE)
        
      }
      
      { # Blog Pics   ----
        
        output$blog_pic_1 <- renderImage({list(
          src = 'www/Photos/Kelp_Forest_Scenes/Brett_Seymour/1 (5).jpg', 
          height = "100%")}, delete = FALSE)
        
      }
      
      { # FAQ Pics   ----
        
        output$faq_pic_1 <- renderImage({list(
          src = 'www/Photos/Kelp_Forest_Scenes/Kenan_Chan/1 (5).jpg', 
          height = "100%")}, delete = FALSE)
        
      }
      
    }
    
    { # Acronyms   ----
      output$Acro_Table <- renderDT({
        datatable(
          Acronyms, rownames = FALSE,  
          options = list(
            searching = FALSE,  paging = FALSE,
            ordering = TRUE, info = FALSE, scrollX = TRUE, 
            initComplete = JS(
              "function(settings, json) {",
              "$(this.api().table().header()).css({'background-color': '#3c8dbc', 'color': '#fff'});}"))) %>%
          formatStyle(names(Acronyms), color = "black", backgroundColor = 'white')
      })
    }
    
  }
  
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
    
    { # Images   ----
      output$site_image1 <- renderImage({list(
        src = 'www/Maps/Other/tempmap.jpg', 
        height = "100%")}, delete = FALSE)
      
      output$site_image2 <- renderImage({list(
        src = 'www/Photos/Protocols/site/1 (1).jpg', 
        height = "100%")}, delete = FALSE)
      
      output$site_image3 <- renderImage({list(
        src = "www/Photos/Protocols/boating/boat (1).jpg", 
        height = "100%")}, delete = FALSE)
      
      output$site_image4 <- renderImage({list(
        src = 'www/Photos/Protocols/boating/boat (4).jpg', 
        height = "100%")}, delete = FALSE)
      
      output$site_image5 <- renderImage({list(
        src = 'www/Photos/Protocols/boating/boat (7).jpg', 
        height = "100%")}, delete = FALSE)
      
      output$site_image6 <- renderImage({list(
        src = "www/Photos/Protocols/boating/boat (8).jpg", 
        height = "100%")}, delete = FALSE)
      
      output$site_image7 <- renderImage({list(
        src = "www/Photos/Protocols/boating/boat (6).jpg", 
        height = "100%")}, delete = FALSE)
    }
    
    { # .... Leaflet Maps     ----
      
      output$Leaflet <- renderLeaflet({
        leaflet() %>%
          setView(lng = -119.7277, lat = 33.76416, zoom = 9) %>%
          addProviderTiles(providers$Esri.OceanBasemap, group = "Ocean Base") %>%
          addTiles(group = "OSM") %>% 
          addProviderTiles(providers$Esri, group = "ESRI") %>%
          addProviderTiles(providers$Esri.WorldImagery, group = "Sat. Imagery") %>%
          addProviderTiles(providers$Esri.WorldTopoMap, group = "Topography") %>%
          addProviderTiles(providers$Esri.NatGeoWorldMap, group = "Nat. Geo.") %>%
          addPolygons(data = mpa, color = mpa$Color, weight = 1,
                      fillOpacity = 0.1, opacity = 0.25, label = mpa$NAME, group = "MPA Boundaries")  %>%
          addPolygons(data = NPS_boundary, weight = 2, color = "green", fill = FALSE,
                      label = "Channel Islands National Park (CINP) Boundary", group = "CINP Boundary") %>%
          addPolygons(data = CINMS_boundary, weight = 2, color = "blue", fill = FALSE,
                      label = "Channel Islands National Marine Sanctuary (CINMS) Boundary", group = "CINMS Boundary") %>%
          addPolylines(data = GPS_Transects, group = "Transects")  %>%
          addCircles(radius = 1, group = "Transect End Points", color = "green",
                     lng = Site_Info$Start_Longitude, lat = Site_Info$Start_Latitude, label = Site_Info$Start_Label) %>%
          addCircles(radius = 1, group = "Transect End Points", color = "red",
                     lng = Site_Info$End_Longitude, lat = Site_Info$End_Latitude, label = Site_Info$End_Label) %>%
          addMarkers(data = Site_Info, label = paste(Site_Info$IslandCode, Site_Info$SiteName), group = "Site Markers") %>% 
          addCircleMarkers(data = Buoys_List, label = Buoys_List$DC.description, group = "Buoy Stations") %>% 
          addLayersControl(
            baseGroups = c("Ocean Base", "OSM", "ESRI", "Sat. Imagery", "Topography", "Nat. Geo."),
            overlayGroups = c("Site Markers", "Transects", "Transect End Points",
                              "MPA Boundaries", "CINP Boundary", "CINMS Boundary",  "Buoy Stations"),
            options = layersControlOptions(collapsed = TRUE)) %>%
          addMeasure(position = "bottomleft", primaryLengthUnit = "meters", primaryAreaUnit = "sqmeters",
                     activeColor = "#3D535D", completedColor = "#7D4479")
      })
      
    }
    
    { # .... Static Imagery  -----
      
      Sat_Map_Site <- Site_Selector_Server(id = 'Site_Sat')
      
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
          return(Sat_Map_Site()$SiteCode)
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
      
      map_text_filename <- reactive({
        if (input$Sat_Isl_Site == 'Site') {"Text/Sites/gps_transects.md"}
        else if (input$Sat_Isl_Site == 'Park') {NULL}
        else {glue::glue("Text/Sites/{satMapCode()}.md")}
      })
      
      output$map_text <- renderUI({includeMarkdown(path = map_text_filename())})
      
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
            dplyr::filter(Site == Sat_Map_Site()$SiteName) %>% 
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
    
    { # Images -----
      output$diversity_pic1 <- renderImage({list(
        src = 'www/Photos/Kelp_Forest_Scenes/Kenan_Chan/1 (3).jpg', 
        height = "100%")}, delete = FALSE)
      
      output$diversity_pic2 <- renderImage({list(
        src = "www/Photos/Kelp_Forest_Scenes/Kenan_Chan/1 (15).jpg", 
        height = "100%")}, delete = FALSE)
      
      output$diversity_pic3 <- renderImage({list(
        src = 'www/Photos/Kelp_Forest_Scenes/Kenan_Chan/1 (6).jpg', 
        height = "100%")}, delete = FALSE)
      
      output$diversity_pic4 <- renderImage({list(
        src = "www/Photos/Kelp_Forest_Scenes/Kenan_Chan/1 (2).jpg", 
        height = "100%")}, delete = FALSE)
      
      output$diversity_pic5 <- renderImage({list(
        src = "www/Photos/Kelp_Forest_Scenes/Kenan_Chan/1 (1).jpg", 
        height = "100%")}, delete = FALSE)
      
    }
    
    diversity_Server(id = "richness") 
    diversity_Server(id = "shannon")
    diversity_Server(id = "simpson") 
  }
  
  { # Community Similarity  ----
    
    { # Images -----
      output$com_pic_1 <- renderImage({list(
        src = 'www/Photos/Kelp_Forest_Scenes/Kenan_Chan/1 (3).jpg', 
        height = "100%")}, delete = FALSE)
      
      output$com_pic_2 <- renderImage({list(
        src = "www/Photos/Kelp_Forest_Scenes/Kenan_Chan/1 (15).jpg", 
        height = "100%")}, delete = FALSE)
    }
    
    { # 2D  ----
      
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
          labs(title = Two_D_data()$SurveyYear, color = input$radio_2D_color, shape = "Reserve Status") +
          nMDS_theme()
      }) %>% 
        shiny::bindCache(Two_D_data(), cache = cachem::cache_disk("./cache/2d-cache"))
      
    }
    
    { # 3D  ----
      
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
          plotly::layout(title = list(text = paste(Three_D_data()$SurveyYear)),
                         scene = list(xaxis = list(title = 'X'),
                                      yaxis = list(title = 'Y'),
                                      zaxis = list(title = 'Z'))) 
        # %>%
        #   plotly::animation_opts(1500, easing = "linear")
      }) %>% 
        shiny::bindCache(Three_D_data(), cache = cachem::cache_disk("./cache/3d-cache"))
      
    }
    
  }
  
  { # Variable Importance  ----
    
    { # Images   -----
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
    
    { # Images  ----
      output$Biomass_pic_1 <- renderImage({list(
        src = "www/Photos/Kelp_Forest_Scenes/Kenan_Chan/1 (4).jpg", 
        height = "100%")}, delete = FALSE)
      
      output$Biomass_pic_2 <- renderImage({list(
        src = "www/Photos/Kelp_Forest_Scenes/Kenan_Chan/1 (10).jpg", 
        height = "100%")}, delete = FALSE)
      
      output$Biomass_pic_3 <- renderImage({list(
        src = "www/Photos/Kelp_Forest_Scenes/Shaun_Wolfe/1 (1).jpg", 
        height = "100%")}, delete = FALSE)
      
      output$Biomass_pic_4 <- renderImage({list(
        src = "www/Photos/Kelp_Forest_Scenes/Shaun_Wolfe/1 (3).jpg", 
        height = "100%")}, delete = FALSE)
      
      output$Biomass_pic_5 <- renderImage({list(
        src = "www/Photos/Kelp_Forest_Scenes/Shaun_Wolfe/1 (5).jpg", 
        height = "100%")}, delete = FALSE)
      
      
      output$Density_pic_1 <- renderImage({list(
        src = "www/Photos/Kelp_Forest_Scenes/Brett_Seymour/1 (3).jpg", 
        height = "100%")}, delete = FALSE)
      
      output$Density_pic_2 <- renderImage({list(
        src = "www/Photos/Kelp_Forest_Scenes/Brett_Seymour/1 (6).jpg",
        height = "100%")}, delete = FALSE)
      
      output$Density_pic_3 <- renderImage({list(
        src = "www/Photos/Kelp_Forest_Scenes/Brett_Seymour/1 (8).jpg", 
        height = "100%")}, delete = FALSE)
      
      output$Density_pic_4 <- renderImage({list(
        src = "www/Photos/Kelp_Forest_Scenes/Shaun_Wolfe/1 (4).jpg", 
        height = "100%")}, delete = FALSE)
      
    }
    
    { # Time Series   ----
      Time_Server(id = "biomass")
      Time_Server(id = "density")
    }
    
    { # Ratios   ----
      Ratio_Server(id = 'biomass_ratio')
      Ratio_Server(id = 'density_ratio')
    }
    
    { # Map Bubbles    ----
      bubbles_Server(id = "biomass_bubbles")
      bubbles_Server(id = "density_bubbles")
    }
    
  }
  
  { # Size Frequencies   ----
    
    { # Images  ----
      output$Size_pic_1 <- renderImage({list(
        src = "www/Photos/Protocols/nhsf/nhsf (1).jpg", 
        height = "100%")}, delete = FALSE)
      
      output$Size_pic_2 <- renderImage({list(
        src = "www/Photos/Protocols/nhsf/nhsf (4).jpg", 
        height = "100%")}, delete = FALSE)
      
    }
    
    { # Box Plots  ----
      
      Site <- Site_Selector_Server(id = "sizes")
      
      Size_Data <- reactive({
        if (input$size_category == "Invertebrates") {Benthic_Sizes %>% 
            dplyr::filter(ScientificName != "Macrocystis pyrifera", 
                          CommonName != "Coronado urchin", CommonName != "Chestnut Cowrie" & SurveyYear > 1990)}
        else if (input$size_category == "Algae") {Benthic_Sizes %>% dplyr::filter(ScientificName == "Macrocystis pyrifera")}
        else if (input$size_category == "Fish") {Fish_Sizes}
      })
      
      output$size_site_year <- renderUI({
        if (input$size_site_radio == "One Site") {
          Site_Selector_UI(id = "sizes")
        }
        else if (input$size_site_radio == "All Sites") {
          tagList(
            sliderInput(inputId = "size_year_slider", label = "Year:",
                        min = min(Size_Year_Species()$SurveyYear), 
                        max = max(Size_Year_Species()$SurveyYear), 
                        value = min(Size_Year_Species()$SurveyYear),
                        sep = "", step = 1, animate = TRUE),
            h5("Animation Note: Animals with many measurements take a long time to plot. ",
               "Plots are cached within a session. ",
               "Run the animation once and allow all plots to complete (watch year in top left corner). ",
               "Re-run to show smooth animation from cached plots.") 
          )
          
        }
      })
      
      Size_Year_Species <- reactive({Size_Data() %>% dplyr::filter(CommonName == input$size_species)})
      
      Site_Levels <- reactive({
        if (input$size_year_slider < 2001) {Site_Info %>% dplyr::filter(SiteNumber < 17) %>% dplyr::arrange(Longitude)}
        else if (input$size_year_slider > 2000 & input$size_year_slider < 2005) {
          Site_Info %>% dplyr::filter(SiteNumber < 22) %>% dplyr::arrange(Longitude)}
        else if (input$size_year_slider > 2004) {Site_Info %>% dplyr::arrange(Longitude)}
      })
      
      Size_Year_Data <- reactive({
        Size_Year_Species() %>% 
          dplyr::filter(SurveyYear == input$size_year_slider) %>% 
          dplyr::mutate(SiteCode = factor(SiteCode, levels = Site_Levels()$SiteCode)) 
      })
      
      Size_Site_Data <- reactive(Size_Data() %>% dplyr::filter(SiteName == Site()$SiteName))
      
      species_choice <- reactive({
        if (input$size_site_radio == "One Site") {levels(factor(Size_Site_Data()$CommonName))}
        else if (input$size_site_radio == "All Sites") {levels(factor(Size_Data()$CommonName))}
      })
      
      output$size_species_UI <- renderUI({
        selectInput(inputId = "size_species", label = "Species:", choices = species_choice())
        })
      
      Size_Site_Data_Subset <- reactive({Size_Site_Data() %>% dplyr::filter(CommonName == input$size_species)})
      
      output$size_site_plot <- renderPlot({
          ggplot2::ggplot() +
            ggplot2::geom_boxplot(data = Size_Site_Data_Subset(), width = 150,
                                  aes(x = Date, y = Size, group = SurveyYear, color = CommonName)) +
            ggplot2::geom_point(data = Size_Site_Data_Subset(), size = 1, color = "black",
                                aes(x = Date, y = Mean_Size, group = SurveyYear)) +
            ggplot2::geom_label(data = Size_Site_Data_Subset(), size = 3, hjust = .5, vjust = 0,
                                aes(x = Date, y = -Inf, label = Size_Site_Data_Subset()$Total_Count)) +
            ggplot2::geom_hline(yintercept = 0) +
            ggplot2::scale_y_continuous(limits = c(0, NA), expand = expansion(mult = c(0.1, 0))) +
            ggplot2::scale_x_date(date_labels = "%Y", breaks = unique(Size_Site_Data_Subset()$Date), expand = expansion(mult = c(0.01, 0.01)),
                                  limits = c(min(Size_Site_Data_Subset()$Date) - 150, max(Size_Site_Data_Subset()$Date) + 150)) +
            ggplot2::labs(title = Size_Site_Data_Subset()$ScientificName,
                          subtitle = glue("{Size_Site_Data_Subset()$IslandName} {Size_Site_Data_Subset()$SiteName}"), 
                          color = "Common Name", x = "Year", y = "Size Distribution") +
            ggplot2::scale_color_manual(values = SpeciesColor) +
            Boxplot_theme()
      }) %>% 
        shiny::bindCache(Size_Site_Data_Subset(), cache = cachem::cache_disk("./cache/sizes-cache"))
      
      output$size_year_plot <- renderPlot({
        ggplot2::ggplot() +
          ggplot2::geom_boxplot(data = Size_Year_Data(), aes(x = SiteCode, y = Size, group = SiteCode, color = CommonName)) +
          ggplot2::geom_point(data = Size_Year_Data(), size = 1, color = "black", aes(x = SiteCode, y = Mean_Size, group = SurveyYear)) +
          ggplot2::geom_label(data = Size_Year_Data(), size = 3, hjust = .5, vjust = 0, aes(x = SiteCode, y = -Inf, label = Size_Year_Data()$Total_Count)) +
          ggplot2::geom_hline(yintercept = 0) +
          ggplot2::scale_y_continuous(limits = c(0, NA), expand = expansion(mult = c(0.1, 0.01))) +
          ggplot2::scale_x_discrete(drop = FALSE) +
          ggplot2::labs(title = Size_Year_Data()$SurveyYear, color = "Common Name", x = NULL, y = "Size Distribution",
                        caption = "Sites arranged by longitude (west to east)") +
          ggplot2::scale_color_manual(values = SpeciesColor) +
          Boxplot_theme()
        
      }) %>% 
        shiny::bindCache(Size_Year_Data(), cache = cachem::cache_disk("./cache/sizes-cache"))
      
    }
    
    { # ARMs  ----
      
      ARM_Data <- reactive({
        ARM_Sizes 
        # %>% 
          # dplyr::filter(Size_mm == input$Size_Limit)
      })
      
      output$arm_site_year <- renderUI({
        if (input$arm_site_radio == "One Site") {
          selectInput(inputId = "ARM_Sites",
                      label = "Site:",
                      choices = dplyr::arrange(dplyr::filter(Site_Info, ARMs == T), Longitude)$SiteName)
        }
        else if (input$arm_site_radio == "All Sites") {
          tagList(
            sliderInput(inputId = "arm_year_slider", label = "Year:",
                        min = min(ARM_Year_Species()$SurveyYear), 
                        max = max(ARM_Year_Species()$SurveyYear), 
                        value = min(ARM_Year_Species()$SurveyYear),
                        sep = "", step = 1, animate = TRUE),
            h5("Animation Note: Animals with many measurements take a long time to plot. ",
               "Plots are cached within a session. ",
               "Run the animation once and allow all plots to complete (watch year in top left corner). ",
               "Re-run to show smooth animation from cached plots.") 
          )
          
        }
      })
      
      ARM_Year_Species <- reactive({ARM_Data() %>% dplyr::filter(CommonName == input$arm_species)})
      
      ARM_Site_Levels <- reactive({
        if (input$arm_year_slider < 2001) {Site_Info %>% dplyr::filter(SiteNumber < 17) %>% dplyr::arrange(Longitude)}
        else if (input$arm_year_slider > 2000 & input$arm_year_slider < 2005) {
          Site_Info %>% dplyr::filter(SiteNumber < 22) %>% dplyr::arrange(Longitude)}
        else if (input$arm_year_slider > 2004) {Site_Info %>% dplyr::arrange(Longitude)}
      })
      
      ARM_Size_Year_Data <- reactive({
        ARM_Year_Species() %>% 
          dplyr::filter(SurveyYear == input$arm_year_slider) %>% 
          dplyr::mutate(SiteCode = factor(SiteCode, levels = ARM_Site_Levels()$SiteCode)) 
      })
      
      ARM_Size_Site_Data <- reactive(ARM_Data() %>% dplyr::filter(SiteName == input$ARM_Sites))
      
      arm_species_choice <- reactive({
        if (input$arm_site_radio == "One Site") {levels(factor(ARM_Size_Site_Data()$CommonName))}
        else if (input$arm_site_radio == "All Sites") {levels(factor(ARM_Data()$CommonName))}
      })
      
      output$arm_species_UI <- renderUI({
        selectInput(inputId = "arm_species", label = "Species:", choices = arm_species_choice())
      })
      
      ARM_Size_Site_Data_Subset <- reactive({ARM_Size_Site_Data() %>% dplyr::filter(CommonName == input$arm_species)})
      
      output$arm_site_plot <- renderPlot({
        ggplot2::ggplot() +
          ggplot2::geom_boxplot(data = ARM_Size_Site_Data_Subset(), width = 150,
                                aes(x = Date, y = Size_mm, group = SurveyYear, color = CommonName)) +
          ggplot2::geom_point(data = ARM_Size_Site_Data_Subset(), size = 1, color = "black",
                              aes(x = Date, y = Mean_Size, group = SurveyYear)) +
          ggplot2::geom_label(data = ARM_Size_Site_Data_Subset(), size = 3, hjust = .5, vjust = 0,
                              aes(x = Date, y = -Inf, label = ARM_Size_Site_Data_Subset()$Total_Count)) +
          ggplot2::geom_hline(yintercept = 0) +
          ggplot2::scale_y_continuous(limits = c(0, NA), expand = expansion(mult = c(0.1, 0))) +
          ggplot2::scale_x_date(date_labels = "%Y", breaks = unique(ARM_Size_Site_Data_Subset()$Date), expand = expansion(mult = c(0.01, 0.01)),
                                limits = c(min(ARM_Size_Site_Data_Subset()$Date) - 150, max(ARM_Size_Site_Data_Subset()$Date) + 150)) +
          ggplot2::labs(title = ARM_Size_Site_Data_Subset()$ScientificName,
                        subtitle = glue("{ARM_Size_Site_Data_Subset()$IslandName} {ARM_Size_Site_Data_Subset()$SiteName}"), 
                        color = "Common Name", x = "Year", y = "Size Distribution") +
          ggplot2::scale_color_manual(values = SpeciesColor) +
          Boxplot_theme()
      }) %>% 
        shiny::bindCache(ARM_Size_Site_Data_Subset(), cache = cachem::cache_disk("./cache/sizes-cache"))
      
      output$arm_year_plot <- renderPlot({
        ggplot2::ggplot() +
          ggplot2::geom_boxplot(data = ARM_Size_Year_Data(), aes(x = SiteCode, y = Size_mm, group = SiteCode, color = CommonName)) +
          ggplot2::geom_point(data = ARM_Size_Year_Data(), size = 1, color = "black", aes(x = SiteCode, y = Mean_Size, group = SurveyYear)) +
          ggplot2::geom_label(data = ARM_Size_Year_Data(), size = 3, hjust = .5, vjust = 0, aes(x = SiteCode, y = -Inf, label = ARM_Size_Year_Data()$Total_Count)) +
          ggplot2::geom_hline(yintercept = 0) +
          ggplot2::scale_y_continuous(limits = c(0, NA), expand = expansion(mult = c(0.1, 0.01))) +
          ggplot2::scale_x_discrete(drop = FALSE) +
          ggplot2::labs(title = ARM_Size_Year_Data()$SurveyYear, color = "Common Name", x = NULL, y = "Size Distribution",
                        caption = "Sites arranged by longitude (west to east)") +
          ggplot2::scale_color_manual(values = SpeciesColor) +
          Boxplot_theme()
        
      }) %>% 
        shiny::bindCache(ARM_Size_Year_Data(), cache = cachem::cache_disk("./cache/sizes-cache"))
      
    }
    
  }
 
  { # Reports   -----
    output$Annual_Report <- renderUI({ 
      tags$iframe(style="height:750px; width:100%; scrolling=yes", src = glue("Annual_Reports/{input$Report}.pdf"))
    })
    
    Text_Data <- reactive(Text %>% dplyr::filter(Year == input$Cloud))
    
    
    output$cloud_plot <- renderPlot(bg = "black", {
      wordcloud::wordcloud(
        words = Text_Data()$word,
        freq = Text_Data()$n, min.freq = 1, scale = c(4, .75),
        max.words = input$cloud_n, random.order = FALSE, rot.per = 0.25,
        colors = brewer.pal(8, "Dark2"))
    }) %>% 
      shiny::bindCache(input$cloud_n, Text_Data(), cache = cachem::cache_disk("./cache/word-cache"))
    
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




# TODO add kelp and gorgonian species guide and protocol guide
# TODO add shell size frequency guides







