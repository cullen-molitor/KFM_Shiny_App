

{ # Protocol Tab Panel Module   ----
  protocol_UI <- function(id, label = "proto") {
    ns <- NS(id)
    tagList(
      tags$hr(),
      fluidRow(
        column(
          3, 
          selectInput(inputId = ns("protocol_selector"),
                      label = "Choose a Protocol:",
                      choices = Protocols)
        ),
        column(
          3,
          radioButtons(inputId = ns("Overview_Practical"),
                       label = "View:", inline = TRUE,
                       choices = c("Overview", "Pratical Guide", "Data Sheet"))
        )
      ),
      conditionalPanel(
        condition = "input.Overview_Practical == 'Overview'", ns = ns,
        fluidRow(
          column(
            6, uiOutput(outputId = ns("text"))
          ),
          column(
            6, 
            tags$hr(), imageOutput(outputId = ns("proto_pic1")),
            tags$br(), imageOutput(outputId = ns("proto_pic2")),
            tags$br(), imageOutput(outputId = ns("proto_pic3")),
            tags$br(), imageOutput(outputId = ns("proto_pic4")),
            tags$br(), imageOutput(outputId = ns("proto_pic5"))
          )
        )
      ),
      conditionalPanel(
        condition = "input.Overview_Practical == 'Pratical Guide'", ns = ns,
        uiOutput(outputId = ns("protocol_guide"))
      ),
      conditionalPanel(
        condition = "input.Overview_Practical == 'Data Sheet'", ns = ns,
        uiOutput(outputId = ns("data_sheet"))
      )
    )
  }
  
  protocol_Server <- function(id) {
    moduleServer(
      id,
      function(input, output, session) {
        
        filename_text <- reactive(glue("Text/Protocols/{input$protocol_selector}.md"))
        filename_pic_1 <- reactive({glue::glue("www/Photos/Protocols/{input$protocol_selector} (1).jpg")})
        filename_pic_2 <- reactive({glue::glue("www/Photos/Protocols/{input$protocol_selector} (2).jpg")})
        filename_pic_3 <- reactive({glue::glue("www/Photos/Protocols/{input$protocol_selector} (3).jpg")})
        filename_pic_4 <- reactive({glue::glue("www/Photos/Protocols/{input$protocol_selector} (4).jpg")})
        filename_pic_5 <- reactive({glue::glue("www/Photos/Protocols/{input$protocol_selector} (5).jpg")})
        filename_proto <- reactive({glue::glue("Handbook/Protocol_Guides/{input$protocol_selector}_protocol_guide.pdf")})
        filename_data <- reactive({glue::glue("Handbook/Datasheets/{input$protocol_selector}.pdf")})
        
        output$text <- renderUI(includeMarkdown(path = filename_text()))
        
        output$proto_pic1 <- renderImage({list(src = filename_pic_1(), width = 600, height = 400)}, deleteFile = FALSE)
        
        output$proto_pic2 <- renderImage({list(src = filename_pic_2(), width = 600, height = 400)}, deleteFile = FALSE)
        
        output$proto_pic3 <- renderImage({list(src = filename_pic_3(), width = 600, height = 400)}, deleteFile = FALSE)
        
        output$proto_pic4 <- renderImage({list(src = filename_pic_4(), width = 600, height = 400)}, deleteFile = FALSE)
        
        output$proto_pic5 <- renderImage({list(src = filename_pic_5(), width = 600, height = 400)}, deleteFile = FALSE)
        
        output$protocol_guide <- renderUI({tags$iframe(style = "height:600px; width:100%; scrolling=yes", src = glue::glue(filename_proto()))})
        
        output$data_sheet <- renderUI({tags$iframe(style = "height:600px; width:100%; scrolling=yes", src = glue::glue(filename_data()))
        })
      }
    )
  } 
}

{ # Site Selector Module   ----
  Site_Selector_UI <- function(id, label = "site") {
    ns <- NS(id)
    selectInput(inputId = ns("Site_Selector"),
                label = "Choose a Site:",
                choices = Site_Info$Isl_SiteName)
    # dplyr::arrange(Site_Info, Longitude)$Isl_SiteName   # arrange choices by longitude
  }
  
  Site_Selector_Server <- function(id) {
    moduleServer(
      id,
      function(input, output, session) {
        reactive(dplyr::filter(Site_Info, Isl_SiteName == input$Site_Selector))
      }
    )
  } 
}

{ # Species Selector Module   ----
  Species_Selector_UI <- function(id, label = "species") {
    ns <- NS(id)
    selectInput(inputId = ns("Species_Selector"),
                label = "Choose a Species:",
                choices = Site_Info$Isl_SiteName)
    # dplyr::arrange(Site_Info, Longitude)$Isl_SiteName   # arrange choices by longitude
  }
  
  Species_Selector_Server <- function(id) {
    moduleServer(
      id,
      function(input, output, session) {
        reactive(dplyr::filter(Species_Info, CommonName == input$Species_Selector))
      }
    )
  } 
}

{ # Biodiversity Tab Module  ----
  diversity_UI <- function(id, label = "bio") {
    ns <- NS(id)
    tagList(
      fluidRow(
        column(
          4, uiOutput(outputId = ns("Diversity_Text"))
        ),
        column(
          8,
          fluidRow(
            tags$hr(),
            column(
              3, radioButtons(inputId = ns("Diversity_Plot_Options"),
                              label = "Choose a Plot Type:",
                              choices = c("Smooth Line (LOESS)",
                                          "Map Bubbles"))
            ),
            column(
              3, conditionalPanel(condition = "input.Diversity_Plot_Options == 'Smooth Line (LOESS)'", ns = ns,
                                  radioButtons(inputId = ns("Data_Options"),
                                               label = "Choose a Data Summary:",
                                               choices = c("All Sites",
                                                           "Original 16 Sites",
                                                           "MPA Reference Sites",
                                                           "Individual Site"))),
              conditionalPanel(condition = "input.Diversity_Plot_Options == 'Map Bubbles'", ns = ns,
                               selectInput(inputId = ns("map_center"),
                                            label = "Center Map on:",
                                            choices = c(unique(Site_Info$IslandName))))
            ),
            conditionalPanel(condition = "input.Diversity_Plot_Options == 'Smooth Line (LOESS)'", ns = ns,
                             column(
                               2, radioButtons(inputId = ns("Sclaes_Selector"),
                                               label = "Scale Opitions:",
                                               choices = c("Free Y", "Fixed Y")))
            ),
            conditionalPanel(condition = "input.Diversity_Plot_Options == 'Map Bubbles'", ns = ns,
                             column(
                               6, sliderInput(inputId = ns("map_slider"),
                                              label = "Select a Year:",
                                              min = min(Diversity$SurveyYear),
                                              max = max(Diversity$SurveyYear),
                                              value = min(Diversity$SurveyYear),
                                              width = "100%",
                                              sep = "", step = 1, animate = TRUE))
            ),
            column(
              3, conditionalPanel(condition = "input.Data_Options == 'Individual Site'", ns = ns,
                                  selectInput(inputId = ns("Site_Selector"),
                                              label = "Choose a Site:",
                                              choices = Site_Info$SiteName))
            )
          ),
          fluidRow(
            conditionalPanel(condition = "input.Diversity_Plot_Options == 'Smooth Line (LOESS)'", ns = ns,
                             uiOutput(outputId = ns('plotUI'))),
            conditionalPanel(condition = "input.Diversity_Plot_Options == 'Map Bubbles'", ns = ns,
                             plotOutput(outputId = ns("map"))
            )
          )
        ) 
      )
    )
  }
  
  diversity_Server <- function(id) {
    moduleServer(
      id,
      function(input, output, session) {
       
        output$plotUI <- renderUI({
          plotOutput(outputId = session$ns("Diversity_Plot"),
                     height = if (input$Data_Options == "Individual Site"){350} else {750})
        })
        
        filename_text <- reactive(glue("Text/Biodiversity/{id}_index.md"))
        
        output$Diversity_Text <- renderUI(includeMarkdown(path = filename_text()))
        
        data <- reactive({
          if (id == "shannon" & (input$Data_Options == "All Sites" | 
                                 input$Data_Options == "Original 16 Sites" |
                                 input$Data_Options == "Individual Site")) {
            Diversity %>% 
              dplyr::rename(Index = shannon_all) 
          } 
          else if (id == "shannon" & input$Data_Options == "MPA Reference Sites") {
            Diversity %>% 
              dplyr::rename(Index = shannon_2005) 
          }
          else if (id == "simpson") {
            Diversity %>% 
              dplyr::rename(Index = simpson)
          }
          else if (id == "richness" & (input$Data_Options == "All Sites" | 
                                      input$Data_Options == "Original 16 Sites" |
                                      input$Data_Options == "Individual Site")) {
            Diversity %>% 
              dplyr::rename(Index = richness_all) %>% 
              dplyr::filter(SurveyYear > 1986, SiteCode != "MM" | SurveyYear > 2004) 
          } 
          else if (id == "richness" & input$Data_Options == "MPA Reference Sites") {
            Diversity %>% 
              dplyr::rename(Index = richness_2005) %>% 
              dplyr::filter(SurveyYear > 1986)  
          }
        })
        
        plot_title <- reactive({
          if (id == "shannon") {
            "Shannon-Wiener Diversity Index" 
          } 
          else if (id == "simpson") {
            "Gini-Simpson Diversity Index" 
          }
          else if (id == "richness") {
            "Species Richness" 
          }
        })
        
        data_subset <- reactive({
          if (input$Data_Options == "All Sites") {
            data()
          } else if (input$Data_Options == "Original 16 Sites") {
            data() %>% 
              dplyr::filter(SiteNumber < 17) 
          } else if (input$Data_Options == "MPA Reference Sites") {
            data() %>% 
              dplyr::filter(Reference == TRUE, SurveyYear > 2004)
          } else {
            data() %>% 
              dplyr::filter(SiteName == input$Site_Selector)
          }
        })
        
        map_data <- reactive({
          if (id == "shannon") {
            Diversity %>% 
              dplyr::rename(Index = shannon_all) %>% 
            dplyr::filter(SurveyYear == input$map_slider, IslandName == input$map_center) 
          } 
          else if (id == "simpson") {
            Diversity %>% 
              dplyr::rename(Index = simpson) %>% 
              dplyr::filter(SurveyYear == input$map_slider, IslandName == input$map_center)
          }
          else if (id == "richness") {
            Diversity %>% 
              dplyr::rename(Index = richness_all) %>% 
              dplyr::filter(SurveyYear == input$map_slider, IslandName == input$map_center)
          }
        })
        
        yscale <- reactive({
          if (input$Sclaes_Selector == "Fixed Y") {
            c(min(data_subset()$Index), max(data_subset()$Index))
          }
          else {
            c(min(data_subset()$Index), NA)
          }
        })
        
        output$Diversity_Plot <- renderPlot({ # Plot     ----
          if (input$Data_Options == "All Sites") {
            Split <- base::split(data_subset(), f = data_subset()$IslandName) 
            p1 <- ggplot(Split$`San Miguel Island`,
                         aes(x = Date, y = Index, color = SiteCode, linetype = SiteCode)) + 
              geom_smooth(size = .5, se = FALSE, method = 'loess', formula = 'y ~ x') +
              ggplot2::scale_x_date(date_labels = "%Y", date_breaks = "year", expand = c(0, 0),
                                    limits = c(lubridate::ymd(min(data_subset()$Date)),
                                               lubridate::ymd(max(data_subset()$Date)))) +
              scale_y_continuous(limits = yscale(), 
                                 expand = expansion(mult =0.1)) +
              labs(title = plot_title(),  
                   color = "Site Code", linetype = "Site Code",
                   x = NULL, y = NULL) +
              facet_grid(rows = vars(IslandName), scales = "fixed") +
              scale_color_manual(values = SiteColor, guide = guide_legend(ncol = 2)) +
              scale_linetype_manual(values = SiteLine, guide = guide_legend(ncol = 2)) +
              all_sites_theme()
            p2 <- p1 %+% Split$`Santa Rosa Island` +
              labs(title = NULL, subtitle = NULL)
            p3 <- p1 %+% Split$`Santa Cruz Island` +
              labs(title = NULL, subtitle = NULL)
            p4 <- p1 %+% Split$`Anacapa Island` +
              labs(title = NULL, subtitle = NULL)
            p5 <- p1 %+% Split$`Santa Barbara Island`  +
              labs(title = NULL, subtitle = NULL, x = "Year") +
              theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1, size = 10))
            arrange_plot <- ggpubr::ggarrange(
              p1, p2, p3, p4, p5, ncol = 1, heights = c(.9, .75, .75, .75, 1),
              align = "v", common.legend = FALSE)
            annotate_plot <- ggpubr::annotate_figure(
              arrange_plot, left = text_grob("Index Value", color = "black", rot = 90, size = 16)
            )
            print(annotate_plot)
          } 
          else if (input$Data_Options == "Original 16 Sites") {
            p1 <- ggplot2::ggplot(data_subset(), aes(x = Date, y = Index, linetype = ReserveYear)) +
              ggplot2::geom_smooth(size = 1, span = 0.75, method = 'loess', formula = 'y ~ x',
                                   aes(color = ReserveYear)) +
              ggplot2::scale_x_date(date_labels = "%Y", date_breaks = "year", expand = c(0, 0)) +
              ggplot2::scale_y_continuous(limits = yscale(), expand = expansion(mult =0.1), oob = squish) +
              ggplot2::scale_colour_manual(values = Island_Colors) +
              ggplot2::labs(title = plot_title(),
                            x = NULL, y = NULL,
                            linetype = "Reserve Status",
                            color = "Reserve Status") +
              timeseries_top_theme()
            
            p2 <- ggplot2::ggplot(data_subset(), aes(x = Date, y = Index, color = IslandName)) +
              ggplot2::geom_smooth(size = 1, span = .75, method = 'loess', formula = 'y ~ x') +
              ggplot2::scale_x_date(date_labels = "%Y", date_breaks = "year", expand = c(0, 0)) +
              ggplot2::scale_y_continuous(limits = yscale(), expand = expansion(mult =0.1), oob = squish) +
              ggplot2::scale_colour_manual(values = Island_Colors) +
              ggplot2::labs(x = NULL, y = NULL,
                            color = "Island") +
              timeseries_top_theme()
            
            p3 <- ggplot2::ggplot() +
              geom_rect(data = SST_Anomaly_Index,
                        aes(xmin = DateStart, xmax = DateEnd, ymin = -Inf, ymax = 0, fill = ONI_ANOM)) +
              scale_fill_gradient2(high = "darkred", mid = "white", low = "navyblue", midpoint = 0,
                                   guide = guide_colorbar(direction = "horizontal", title.position = "top",
                                                          order = 3, barheight = unit(.2, "cm"))) +
              ggplot2::geom_smooth(data = data_subset(), method = 'loess', formula = 'y~x', size = 1, se = F, span = .75,
                                   aes(x = Date, y = Index, color = IslandName, linetype = ReserveStatus)) +
              ggplot2::geom_hline(aes(yintercept = 0)) +
              ggplot2::scale_x_date(date_labels = "%Y", date_breaks = "year", expand = c(0, 0),
                                    limits = c(lubridate::ymd(min(data_subset()$Date)),
                                               lubridate::ymd(max(data_subset()$Date)))) +
              ggplot2::scale_y_continuous(expand = expansion(mult =0.1),
                                          limits = yscale(), oob = squish) +
              ggplot2::guides(color = guide_legend(order = 1),
                              linetype = guide_legend(order = 2, override.aes = list(col = 'black'))) +
              ggplot2::scale_colour_manual(values = Island_Colors) +
              ggplot2::labs(x = "Survey Year", y = NULL,
                            color = "Island",
                            fill = "Oceanic Niño Index",
                            linetype = "Reserve Status") +
              timeseries_bottom_theme()
            Diversity_Plot <-ggarrange(p1, p2, p3, ncol = 1, align = "v", heights = c(.8, .8, 1))
            Diversity_annotated <- ggpubr::annotate_figure(
              Diversity_Plot,
              left = text_grob("Index Value",
                               family ="Cambria", color = "black", rot = 90, size = 13))
            print(Diversity_annotated)
          } 
          else if (input$Data_Options == "MPA Reference Sites") {
            p1 <- ggplot2::ggplot(data_subset(), aes(x = Date, y = Index, linetype = ReserveStatus)) +
              ggplot2::geom_smooth(size = 1, span = 0.75, method = 'loess', formula = 'y ~ x',
                                   aes(color = ReserveStatus)) +
              ggplot2::scale_x_date(date_labels = "%Y", date_breaks = "year", expand = c(0, 0)) +
              ggplot2::scale_y_continuous(limits = yscale(), expand = expansion(mult =0.1), oob = squish) +
              ggplot2::scale_colour_manual(values = Island_Colors) +
              ggplot2::labs(title = plot_title(),
                            x = NULL, y = NULL,
                            linetype = "Reserve Status",
                            color = "Reserve Status") +
              timeseries_top_theme()
            
            p2 <- ggplot2::ggplot(data_subset(), aes(x = Date, y = Index, color = IslandName)) +
              ggplot2::geom_smooth(size = 1, span = .75, method = 'loess', formula = 'y ~ x') +
              ggplot2::scale_x_date(date_labels = "%Y", date_breaks = "year", expand = c(0, 0)) +
              ggplot2::scale_y_continuous(limits = yscale(), expand = expansion(mult =0.1), oob = squish) +
              ggplot2::scale_colour_manual(values = Island_Colors) +
              ggplot2::labs(x = NULL, y = NULL,
                            color = "Island") +
              timeseries_top_theme()
            
            p3 <- ggplot2::ggplot() +
              geom_rect(data = SST_Anomaly_Index,
                        aes(xmin = DateStart, xmax = DateEnd, ymin = -Inf, ymax = 0, fill = ONI_ANOM)) +
              scale_fill_gradient2(high = "darkred", mid = "white", low = "navyblue", midpoint = 0,
                                   guide = guide_colorbar(direction = "horizontal", title.position = "top",
                                                          order = 3, barheight = unit(.2, "cm"))) +
              ggplot2::geom_smooth(data = data_subset(), method = 'loess', formula = 'y~x', size = 1, se = F, span = .75,
                                   aes(x = Date, y = Index, color = IslandName, linetype = ReserveStatus)) +
              ggplot2::geom_hline(aes(yintercept = 0)) +
              ggplot2::scale_x_date(date_labels = "%Y", date_breaks = "year", expand = c(0, 0),
                                    limits = c(lubridate::ymd(min(data_subset()$Date)),
                                               lubridate::ymd(max(data_subset()$Date)))) +
              ggplot2::scale_y_continuous(expand = expansion(mult =0.1),
                                          limits = yscale(), oob = squish) +
              ggplot2::guides(color = guide_legend(order = 1),
                              linetype = guide_legend(order = 2, override.aes = list(col = 'black'))) +
              ggplot2::scale_colour_manual(values = Island_Colors) +
              ggplot2::labs(x = "Survey Year", y = NULL,
                            color = "Island",
                            fill = "Oceanic Niño Index",
                            linetype = "Reserve Status") +
              timeseries_bottom_theme()
            Diversity_Plot <-ggarrange(p1, p2, p3, ncol = 1, align = "v", heights = c(.8, .8, 1))
            Diversity_annotated <- ggpubr::annotate_figure(
              Diversity_Plot,
              left = text_grob("Index Value",
                               family ="Cambria", color = "black", rot = 90, size = 13))
            print(Diversity_annotated)
          } 
          else if (input$Data_Options == "Individual Site") {
            ggplot2::ggplot() +
              geom_rect(data = SST_Anomaly_Index,
                        aes(xmin = DateStart, xmax = DateEnd, ymin = -Inf, ymax = 0, fill = ONI_ANOM)) +
              scale_fill_gradient2(high = "darkred", mid = "white", low = "navyblue", midpoint = 0,
                                   guide = guide_colorbar(direction = "horizontal", title.position = "top",
                                                          order = 3, barheight = unit(.2, "cm"))) +
              ggplot2::geom_smooth(data = data_subset(), method = 'loess', formula = 'y~x', size = 1, se = F, span = .75,
                                   aes(x = Date, y = Index, color = SiteName, linetype = ReserveStatus)) +
              ggplot2::geom_hline(aes(yintercept = 0)) +
              ggplot2::scale_x_date(date_labels = "%Y", date_breaks = "year", expand = c(0, 0),
                                    limits = c(lubridate::ymd(min(data_subset()$Date)),
                                               lubridate::ymd(max(data_subset()$Date)))) +
              ggplot2::scale_y_continuous(expand = expansion(mult =0.1),
                                          limits = yscale(), oob = squish) +
              ggplot2::guides(color = guide_legend(order = 1),
                              linetype = guide_legend(order = 2, override.aes = list(col = 'black'))) +
              ggplot2::scale_colour_manual(values = SiteColor) +
              ggplot2::labs(x = "Survey Year", y = "Index Value",
                            title = plot_title(),
                            subtitle = glue("{data_subset()$SiteName} {data_subset()$IslandName}"),
                            color = "Site",
                            fill = "Oceanic Niño Index",
                            linetype = "Reserve Status") +
              timeseries_bottom_theme()
          }
        })
        
        output$map <-  renderPlot({ # Map     ----
          ggplot()+  
            geom_sf(data = dplyr::filter(CINP, IslandName == input$map_center)) +
            geom_text(data = dplyr::filter(Site_Info, IslandName == input$map_center), size = 5,
                      aes(x = Island_Longitude, y = Island_Latitude - .003, label = IslandName)) +
            geom_text(data = map_data(), size = 3,
                       aes(x = Longitude, y = Latitude, label = SiteCode)) +
            geom_point(data = map_data(),
              aes(x = Longitude, y = Latitude, size = Index , color = ReserveStatus),
              shape = 1, stroke = 1) +
            scale_color_manual(values = Island_Colors) +
            scale_y_continuous(expand = expansion(mult = 0.1)) +
            scale_x_continuous(expand = expansion(mult = 0.1)) +
            scale_size_continuous(range = c(10, 25), guide = guide_legend()) +
            theme_void() +
            theme(legend.position = "none")
        })
      }
    )
  } 
}

{ # Foundation Species Module  -----
  foundation_UI <- function(id, label = "foundation_species") {
    ns <- NS(id)
    tagList(
      fluidRow(
        column(4, imageOutput(outputId = ns("pic"))),
        column(4, uiOutput(outputId = ns("text"))),
        column(4, DTOutput(outputId = ns("table")))
      ), tags$hr()
    )
  }
  
  foundation_Server <- function(id) {
    moduleServer(
      id,
      function(input, output, session) {
        # otters
        Species_Code <- reactive({
          if (id == "kelp") {2002}
          else if (id == "p_urchin") {11006}
          else if (id == "r_urchin") {11005}
          else if (id == "r_abalone") {9002}
          else if (id == "lobsta") {8001}
          else if (id == "sheep") {14013}
          else if (id == "sunflower") {11003}
          else if (id == "giant") {11002}
          else if (id == "sargassum") {2017}
          else if (id == "undaria") {2009}
        })
        
        pic_filename <- reactive(glue::glue("www/Photos/Indicator_Species/{Species_Code()}.jpg"))
        text_filname <- reactive(glue::glue("Text/Species/{id}.md"))
        
        output$pic <- renderImage({list(src = pic_filename(), width = "100%", height = "100%")}, delete = FALSE)
        
        output$text <- renderUI(includeMarkdown(path = text_filname()))
        
        table_data <- reactive({
          Species_Info %>%
            dplyr::filter(Species == Species_Code()) %>%
            dplyr::select(ScientificName, Trophic_Broad, Habitat_Broad, 
                          Geographic_Range, Size_Range, ID_Short, Abundance,
                          Commercial_Fishery, Recreational_Fishery) %>%
            dplyr::rename(`Trophic Level` = Trophic_Broad, 
                          Habitat = Habitat_Broad,
                          Range = Geographic_Range, 
                          `Size Range` = Size_Range, 
                          Identification = ID_Short, 
                          `Commercial Fishery` = Commercial_Fishery,
                          `Recreational Fishery` = Recreational_Fishery) %>% 
            tidyr::pivot_longer(-ScientificName, names_to = "Category", values_to = "Information") %>%
            dplyr::select(Category, Information)
        }) 
        
        output$table <- renderDT({
          datatable(
            table_data(), rownames = FALSE,  
            options = list(
              searching = FALSE, lengthChange = FALSE, paging = FALSE,
              ordering = FALSE, info = FALSE, 
              initComplete = JS(
                "function(settings, json) {",
                "$(this.api().table().header()).css({'background-color': '#3c8dbc', 'color': '#fff'});",
                "}"))) %>%
            formatStyle(
              names(table_data()),
              color = "black",
              backgroundColor = 'white'
            )
        })
        
      }
    )
  }
}

{ # Species Guides Module   ----
  species_guide_UI <- function(id, label = "species_guide") {
    ns <- NS(id)
    tagList(
      selectInput(inputId = ns("protocol_selector"),
                  label = "Choose a Protocol:",
                  choices = Protocols),
      uiOutput(outputId = ns("guide"))
    )
  }
  
  species_guide_Server <- function(id) {
    moduleServer(
      id,
      function(input, output, session) {
        
        output$guide <- renderUI({tags$iframe(
          style = "height:650px; width:100%; scrolling=yes",
          src = glue::glue_col("Handbook/Species_Guides/{input$protocol_selector}_species_guide.pdf"))
        })
      }
    )
  }
  
}


{ # Taxa Module  -----
  Taxa_UI <- function(id, label = "taxa") {
    ns <- NS(id)
    tagList(
      fluidRow(
        column(
          3,
          radioButtons(inputId = ns("category"), label = "Choose a Category:", choices = c("Invertebrates", "Algae", "Fish"))
        ),
        column(
          3, 
          uiOutput(outputId = ns("species_ui"))
        )
      ),
      
      fluidRow(
        column(4, imageOutput(outputId = ns("pic")), h5("Not all taxa currently have photos yet.")),
        column(4, DTOutput(outputId = ns("taxonomy_table"))),
        column(4, DTOutput(outputId = ns("meta_table")))
      ), tags$hr()
    )
  }
  
  Taxa_Server <- function(id) {
    moduleServer(
      id,
      function(input, output, session) {
        
        species_subset <- reactive({
          Species_Info %>% 
            dplyr::filter(Classification == input$category) %>% 
            dplyr::arrange(CommonName)
        })
        
        output$species_ui <- renderUI({
          selectInput(inputId = session$ns("species"), label = "Select a Taxa:", choices = species_subset()$CommonName)
        })
        
        Species_Code <- reactive({
          Species_Info %>% 
            dplyr::filter(CommonName == input$species) %>% 
            dplyr::distinct(CommonName, .keep_all = TRUE)
        })
        
        pic_filename <- reactive(glue::glue("www/Photos/Indicator_Species/{Species_Code()$Species}.jpg"))
        
        output$pic <- renderImage({list(src = pic_filename(), width = "100%", height = "100%")}, delete = FALSE)
        
        taxonomy_table_data <- reactive({
          Species_Info %>%
            dplyr::filter(Species == Species_Code()$Species) %>%
            dplyr::select(Species, Kingdom, Phylum, Subphylum, Class, Order, Family, Genus, `Species (Used by KFM)`,
                          `Currently Accepted Name`, `Authority (Accepted)`) %>%
            tidyr::pivot_longer(-Species, names_to = "Category", values_to = "Information") %>%
            dplyr::select(Category, Information)
        }) 
        
        output$taxonomy_table <- renderDT({
          datatable(
            taxonomy_table_data(), rownames = FALSE,  
            options = list(
              searching = FALSE, lengthChange = FALSE, paging = FALSE,
              ordering = FALSE, info = FALSE, 
              initComplete = JS(
                "function(settings, json) {",
                "$(this.api().table().header()).css({'background-color': '#3c8dbc', 'color': '#fff'});",
                "}"))) %>%
            formatStyle(
              names(taxonomy_table_data()),
              color = "black",
              backgroundColor = 'white'
            )
        })
        
        meta_table_data <- reactive({
          Species_Info %>%
            dplyr::filter(Species == Species_Code()$Species) %>%
            dplyr::select(ScientificName, Trophic_Broad, Habitat_Broad, 
                          Geographic_Range, Size_Range, ID_Short, Abundance,
                          Commercial_Fishery, Recreational_Fishery) %>%
            dplyr::rename(`Trophic Level` = Trophic_Broad, 
                          Habitat = Habitat_Broad,
                          Range = Geographic_Range, 
                          `Size Range` = Size_Range, 
                          Identification = ID_Short, 
                          `Commercial Fishery` = Commercial_Fishery,
                          `Recreational Fishery` = Recreational_Fishery) %>% 
            tidyr::pivot_longer(-ScientificName, names_to = "Category", values_to = "Information") %>%
            dplyr::select(Category, Information)
        }) 
        
        output$meta_table <- renderDT({
          datatable(
            meta_table_data(), rownames = FALSE,  
            options = list(
              searching = FALSE, lengthChange = FALSE, paging = FALSE,
              ordering = FALSE, info = FALSE, 
              initComplete = JS(
                "function(settings, json) {",
                "$(this.api().table().header()).css({'background-color': '#3c8dbc', 'color': '#fff'});",
                "}"))) %>%
            formatStyle(
              names(meta_table_data()),
              color = "black",
              backgroundColor = 'white'
            )
        })
        
      }
    )
  }
}

{ # Variable Importance Module   -----
  
  VI_UI <- function(id){
    ns <- NS(id)
    tagList(
      fluidRow(
        column(
          4, uiOutput(outputId = ns("text")) 
        ),
        column(
          8,
          fluidRow(
            tags$hr(),
            column(
              3,
              radioButtons(inputId = ns("Data_VI"),
                           label = "Data Options:",
                           choices = c("All Years (Fewer Species)",
                                       "Years > 2004 (All Species)"))
            ),
            column(
              3, 
              radioButtons(inputId = ns("VI_Plot_Type"),
                           label = "Plot Options",
                           choices = c("Variable Importance", 
                                       "Partial Dependence"))
            ),
            column(
              3, uiOutput(outputId = ns("VI_Isl_Options"))
            )
          ),
          
            conditionalPanel(
              condition = "input.VI_Plot_Type == 'Variable Importance'", ns = ns,
              fluidRow(plotOutput(outputId = ns("VI_Plot"), height = 600))),
            conditionalPanel(
              condition = "input.VI_Plot_Type == 'Partial Dependence'", ns = ns,
              fluidRow(h5("Please be patient, these take a while to calculate and plot")),
              fluidRow(plotOutput(outputId = ns("PD_Plot"), height = 350)))
          
        )
      )
    )
  } 

  VI_Server <- function(id) {
    moduleServer(
      id,
      function(input, output, session) {
        
        filename_text <- reactive(glue::glue("Text/Variable_Importance/{id}_importance.md"))
        
        output$text <- renderUI({
          if (input$VI_Plot_Type == 'Variable Importance')
          includeMarkdown(path = filename_text())
          else {includeMarkdown(path = "Text/Variable_Importance/pd_plot.md")}
          })
        
        rf_species <- reactive({
          if (id == 'reserve' & input$Data_VI == 'All Years (Fewer Species)') {
            RF_Importance <- RF_Importance_All %>% 
              arrange(desc(MeanDecreaseAccuracy))
            a <- c(as.character(RF_Importance$Common_Name))
            names(a) <- c(RF_Importance$CommonName)
            return(a)
          } 
          else if (id == 'reserve' & input$Data_VI == 'Years > 2004 (All Species)') {
            RF_Importance <- RF_Importance_2005 %>% 
              arrange(desc(MeanDecreaseAccuracy))
            a <- c(as.character(RF_Importance$Common_Name))
            names(a) <- c(RF_Importance$CommonName)
            return(a)
          }
          else if (id == 'island' & input$Data_VI == 'All Years (Fewer Species)') {
            RF_Importance <- RF_Importance_All %>% 
              arrange(desc(MeanDecreaseAccuracy_Isl))
            a <- c(as.character(RF_Importance$Common_Name))
            names(a) <- c(RF_Importance$CommonName)
            return(a)
          } 
          else if (id == 'island' & input$Data_VI == 'Years > 2004 (All Species)') {
            RF_Importance <- RF_Importance_2005 %>% 
              arrange(desc(MeanDecreaseAccuracy_Isl))
            a <- c(as.character(RF_Importance$Common_Name))
            names(a) <- c(RF_Importance$CommonName)
            return(a)
          }
        })
        
        output$VI_Isl_Options <- renderUI({
          if (id == 'island' & input$VI_Plot_Type == 'Variable Importance') {
           selectInput(inputId = session$ns("VI_Isl"),
                      label = "Island Options:",
                      choices = c("All Islands", Site_Info$IslandName)) 
          } 
          else if (input$VI_Plot_Type == 'Partial Dependence') {
            selectInput(inputId = session$ns("VI_Species_Input"),
                        label = "Choose a species",
                        choices = rf_species()) 
            }
          
        })
        
        Variable_Accuracy <- reactive({
          if (id == 'reserve' & input$Data_VI == "All Years (Fewer Species)") {
            RF_Importance_All %>% 
              dplyr::arrange(desc(MeanDecreaseAccuracy)) %>%  
              dplyr::mutate(CommonName1 = paste(CommonName, row_number()),
                            CommonName1 = factor(CommonName1, levels = rev(CommonName1)),
                            xvalue = MeanDecreaseAccuracy) %>% 
              head(30) %>% 
              droplevels()
          } 
          else  if (id == 'reserve' & input$Data_VI == "Years > 2004 (All Species)") {
            RF_Importance_2005 %>% 
              dplyr::arrange(desc(MeanDecreaseAccuracy)) %>%  
              dplyr::mutate(CommonName1 = paste(CommonName, row_number()),
                            CommonName1 = factor(CommonName1, levels = rev(CommonName1)),
                            xvalue = MeanDecreaseAccuracy) %>% 
              head(30) %>% 
              droplevels()
          } 
          else  if (id == 'island' & input$Data_VI == "All Years (Fewer Species)" & input$VI_Isl == "All Islands") {
            RF_Importance_All %>% 
              dplyr::arrange(desc(MeanDecreaseAccuracy_Isl)) %>%  
              dplyr::mutate(CommonName1 = paste(CommonName, row_number()),
                            CommonName1 = factor(CommonName1, levels = rev(CommonName1)),
                            xvalue = MeanDecreaseAccuracy_Isl) %>% 
              head(30) %>% 
              droplevels()
          } 
          else if (id == 'island' & input$Data_VI == "All Years (Fewer Species)" & input$VI_Isl != "All Islands") {
            RF_Importance_All %>% 
              dplyr::rename(xvalue = input$VI_Isl) %>% 
              dplyr::arrange(desc(xvalue)) %>%  
              dplyr::mutate(CommonName1 = paste(CommonName, row_number()),
                            CommonName1 = factor(CommonName1, levels = rev(CommonName1))) %>% 
              head(30) %>% 
              droplevels()
          } 
          else if (id == 'island' & input$Data_VI == "Years > 2004 (All Species)" & input$VI_Isl == "All Islands") {
            RF_Importance_2005  %>% 
              dplyr::arrange(desc(MeanDecreaseAccuracy_Isl)) %>%  
              dplyr::mutate(CommonName1 = paste(CommonName, row_number()),
                            CommonName1 = factor(CommonName1, levels = rev(CommonName1)),
                            xvalue = MeanDecreaseAccuracy_Isl) %>% 
              head(30) %>% 
              droplevels()
          }
          else if (id == 'island' & input$Data_VI == "Years > 2004 (All Species)" & input$VI_Isl != "All Islands") {
            RF_Importance_All %>% 
              dplyr::rename(xvalue = input$VI_Isl) %>% 
              dplyr::arrange(desc(xvalue)) %>%  
              dplyr::mutate(CommonName1 = paste(CommonName, row_number()),
                            CommonName1 = factor(CommonName1, levels = rev(CommonName1))) %>% 
              head(30) %>% 
              droplevels()
          } 
          
        })
        
        Variable_Gini <- reactive({
          if (input$Data_VI == "All Years (Fewer Species)") {
            RF_Importance_All %>% 
              dplyr::arrange(desc(MeanDecreaseGini_Isl)) %>%  
              dplyr::mutate(CommonName1 = paste(CommonName, row_number()))  %>% 
              head(30) %>% 
              droplevels()
          } else {
            RF_Importance_2005 %>% 
              dplyr::arrange(desc(MeanDecreaseGini_Isl)) %>%  
              dplyr::mutate(CommonName1 = paste(CommonName, row_number()))  %>% 
              head(30) %>% 
              droplevels()
          }
        })
        
        output$VI_Plot <- renderPlot({
          
          Accuracy <- 
            ggplot(
              Variable_Accuracy(), aes(x = xvalue, y = CommonName1, color = Targeted)) +
            geom_point() +
            geom_segment(
              size = 1, 
              aes(x = min(xvalue) - .5, xend = xvalue, 
                  y = CommonName1, yend = CommonName1)) +
            labs(x = "Mean Decrease in % Accuracy", y = NULL, 
                 color = NULL, linetype = NULL) +
            scale_x_continuous(expand = expansion(mult = c(0,.1)), 
                               limits = c(min(Variable_Accuracy()$xvalue) - .5, NA)) +
            scale_color_manual(values = Target_Colors) +
            theme_classic() +
            theme(axis.text = element_text(size = 12),
                  axis.title = element_text(size = 12),
                  legend.text = element_text(size = 12))
          
          Gini <- ggplot(Variable_Gini(), aes(x = MeanDecreaseGini_Isl, color = Targeted, 
                                                  y = reorder(CommonName1, MeanDecreaseGini_Isl))) +
            geom_point() +
            geom_segment(size = 1,
                         aes(x = min(MeanDecreaseGini_Isl) - .5, xend = MeanDecreaseGini_Isl,
                             y = CommonName1, yend = CommonName1)) +
            labs(x = "Mean Decrease in Gini Index", y = NULL, 
                 color = NULL, linetype = NULL) +
            scale_x_continuous(expand = expansion(mult = c(0,.1)), 
                               limits = c(min(Variable_Gini()$MeanDecreaseGini_Isl) - .5, NA)) +
            scale_color_manual(values = Target_Colors) +
            theme_classic() +
            theme(axis.text = element_text(size = 12),
                  axis.title = element_text(size = 12),
                  legend.text = element_text(size = 12))
          ggarrange(Accuracy, Gini, ncol = 2, align = "h", common.legend = TRUE, legend = "bottom")
        })
        
        pdp_labels <- reactive({
          if (input$Data_VI == 'All Years (Fewer Species)') {
            RF_Importance_All %>% 
              dplyr::filter(Common_Name == input$VI_Species_Input)
          } 
          else if (input$Data_VI == 'Years > 2004 (All Species)') {
            RF_Importance_2005 %>% 
              dplyr::filter(Common_Name == input$VI_Species_Input)
          }
        })
        
        output$PD_Plot <- renderPlot({
          if (input$Data_VI == 'All Years (Fewer Species)') {
            do.call(
            "partialPlot", 
            list(x = RF_Island_Model_All, pred.data = as.data.frame(Mixed_All), 
                 x.var = pdp_labels()$Common_Name,
                 main = paste("Partial Dependence on", pdp_labels()$CommonName),
                 xlab = pdp_labels()$Data_Type))
          } 
          else if (input$Data_VI == 'Years > 2004 (All Species)') {
            do.call(
              "partialPlot", 
              list(x = RF_Island_Model_2005, pred.data = as.data.frame(Mixed_2005), 
                   x.var = pdp_labels()$Common_Name,
                   main = paste("Partial Dependence on", pdp_labels()$CommonName),
                   xlab = pdp_labels()$Data_Type))
          }
          
        })
        
      }
    )
  }
  
}

{ # Time Series Module   -----
  
  Time_UI <- function(id, label = "time") {
    ns <- NS(id)
    tagList(
      fluidRow(
        column(
          4,
          uiOutput(outputId = ns('text'))
        ),
        column(
          8,
          fluidRow(
            column(
              3,
              radioButtons(inputId = ns("Data_Options"),
                           label = "Choose a Data Summary:",
                           choices = c("All Sites", "Original 16 Sites", "MPA Reference Sites", "Individual Site"))
            ),
            column(
              2,
              radioButtons(inputId = ns("taxa"), label = "Choose a Category:", choices = c('Invertebrates', 'Algae', 'Fish')),
              conditionalPanel(
                condition = "input.taxa == 'Fish'", ns = ns,
                uiOutput(outputId = ns("Fishy"))
              )
            ),
            column(
              3,
              uiOutput(outputId = ns("speciesUI")),
              conditionalPanel(
                condition = "input.Data_Options == 'Individual Site'", ns = ns,
                uiOutput(outputId = ns("siteUI"))
              )
            ),
            column(
              2,
              radioButtons(inputId = ns("axis"), label = "Y Scale:", choices = c('Free', 'Fixed'))
            ),
            conditionalPanel(
              condition = "input.Data_Options == 'All Sites' | input.Data_Options == 'Individual Site'", ns = ns,
              column(
                2,
                radioButtons(inputId = ns("line"), label = "Line Type:", choices = c('Smooth', 'Sharp'))
              )
            )
          ),
          fluidRow(
            uiOutput(outputId = ns('plotUI'))
          )
        )
      ),
      tags$hr(),
      fluidRow(
        column(4, imageOutput(outputId = ns("pic")), h5("Not all taxa currently have photos yet.")),
        column(4, DTOutput(outputId = ns("taxonomy_table"))),
        column(4, DTOutput(outputId = ns("meta_table")))
      ), 
      tags$hr(),
      DTOutput(outputId = ns("data_table"), height = 575), 
      tags$hr()  
    )
  }
  
  Time_Server <- function(id) {
    moduleServer(
      id,
      function(input, output, session) {
        
        output$Fishy <- renderUI({
          if (id == "biomass") {
            NULL
          }
          else if (id == "density"){
            radioButtons(inputId = session$ns("Fish_Survey"), inline = T, label = "Survey Type:", choices = c("RDFC", "VFT"))
          }
        })
        
        species_subset <- reactive({
          if (id == "biomass") {
            Biomass %>% 
              dplyr::filter(Classification == input$taxa) %>%
              dplyr::arrange(CommonName) 
            }
          else if (id == "density" & input$taxa != "Fish") {
            Density %>% 
              dplyr::filter(Classification == input$taxa) %>%
              dplyr::arrange(CommonName) 
          }
          else if (id == "density" & input$taxa == "Fish") {
            Density %>% 
              dplyr::filter(Classification == input$taxa,
                            Survey_Type == input$Fish_Survey) %>%
              dplyr::arrange(CommonName) 
          }
        })
        
        output$speciesUI <- renderUI({
            selectInput(inputId = session$ns("species"), label = "Choose a Species:",
                        choices = unique(species_subset()$CommonName))
          
        })
        
        output$plotUI <- renderUI({
          plotOutput(outputId = session$ns("time_plot"),
                     height = if (input$Data_Options == "Individual Site"){350} else {750})
          })
        
        output$siteUI <- renderUI({Site_Selector_UI(session$ns("site_selector"))})
        
        site <- Site_Selector_Server("site_selector")
        
        filename_text <- reactive(glue::glue("Text/Biomass_Density/{id}.md"))
        
        output$text <- renderUI(includeMarkdown(path = filename_text()))
        
        data <- reactive({
          if (id == "biomass") {
            Biomass %>% 
              dplyr::filter(CommonName == input$species) %>% 
              dplyr::mutate(yvalue = Mean_Biomass)
          }
          else if (id == "density") {
            Density %>% 
              dplyr::filter(CommonName == input$species) %>% 
              dplyr::mutate(yvalue = Mean_Density)
          }
        })
        
        data_subset <- reactive({
          if (input$Data_Options == "All Sites") {data()} 
          else if (input$Data_Options == "Original 16 Sites") {data() %>% dplyr::filter(SiteNumber < 17)} 
          else if (input$Data_Options == "MPA Reference Sites") {data() %>% dplyr::filter(Reference == TRUE, SurveyYear > 2004)} 
          else {data() %>% dplyr::filter(SiteName == site()$SiteName)}
        })
        
        ylab <- reactive({if (id == "biomass") {"Mean Biomass (g/m²)"} else {"Mean Density (#/m²)"}})
        
        yscale <- reactive({if (input$axis == "Fixed") {c(0, max(data_subset()$yvalue))} else {c(0, NA)}})
        
        output$time_plot <- renderPlot({
          if (input$Data_Options == "All Sites" & input$line == "Smooth") {
            Split <- base::split(data_subset(), f = data_subset()$IslandName) 
            p1 <- ggplot2::ggplot(Split$`San Miguel Island`,
                                  aes(x = Date, y = yvalue, color = SiteCode, linetype = SiteCode)) + 
              ggplot2::geom_smooth(size = .5, se = FALSE, method = 'loess', formula = 'y ~ x') +
              ggplot2::scale_x_date(date_labels = "%Y", date_breaks = "year", expand = c(0, 0),
                                    limits = c(lubridate::ymd(min(data_subset()$Date)),
                                               lubridate::ymd(max(data_subset()$Date)))) +
              ggplot2::scale_y_continuous(limits = yscale(), expand = expansion(mult = c(0, .01))) +
              ggplot2::labs(title = Split$`San Miguel Island`$CommonName,  
                            color = "Site Code", linetype = "Site Code",
                            x = NULL, y = NULL) +
              ggplot2::facet_grid(rows = vars(IslandName), scales = "fixed") +
              ggplot2::scale_color_manual(values = SiteColor, guide = guide_legend(ncol = 2)) +
              ggplot2::scale_linetype_manual(values = SiteLine, guide = guide_legend(ncol = 2)) +
              all_sites_theme()
            p2 <- p1 %+% Split$`Santa Rosa Island` +
              ggplot2::labs(title = NULL, subtitle = NULL)
            p3 <- p1 %+% Split$`Santa Cruz Island` +
              ggplot2::labs(title = NULL, subtitle = NULL)
            p4 <- p1 %+% Split$`Anacapa Island` +
              ggplot2::labs(title = NULL, subtitle = NULL)
            p5 <- p1 %+% Split$`Santa Barbara Island`  +
              ggplot2::labs(title = NULL, subtitle = NULL, x = "Year") +
              ggplot2::theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1, size = 10))
            arrange_plot <- ggpubr::ggarrange(
              p1, p2, p3, p4, p5, ncol = 1, heights = c(.9, .75, .75, .75, 1),
              align = "v", common.legend = FALSE)
            annotate_plot <- ggpubr::annotate_figure(
              arrange_plot, left = text_grob(paste(ylab()), color = "black", rot = 90, size = 16)
            )
            print(annotate_plot)
          } 
          else if (input$Data_Options == "All Sites" & input$line == "Sharp") {
            Split <- base::split(data_subset(), f = data_subset()$IslandName) 
            p1 <- ggplot2::ggplot(Split$`San Miguel Island`,
                                  aes(x = Date, y = yvalue, color = SiteCode, linetype = SiteCode)) + 
              ggplot2::geom_line(size = .5) +
              ggplot2::scale_x_date(date_labels = "%Y", date_breaks = "year", expand = c(0, 0),
                                    limits = c(lubridate::ymd(min(data_subset()$Date)),
                                               lubridate::ymd(max(data_subset()$Date)))) +
              ggplot2::scale_y_continuous(limits = yscale(), expand = expansion(mult = c(0, .01))) +
              ggplot2::labs(title = Split$`San Miguel Island`$CommonName,  
                            color = "Site Code", linetype = "Site Code",
                            x = NULL, y = NULL) +
              ggplot2::facet_grid(rows = vars(IslandName), scales = "fixed") +
              ggplot2::scale_color_manual(values = SiteColor, guide = guide_legend(ncol = 2)) +
              ggplot2::scale_linetype_manual(values = SiteLine, guide = guide_legend(ncol = 2)) +
              all_sites_theme()
            p2 <- p1 %+% Split$`Santa Rosa Island` +
              labs(title = NULL, subtitle = NULL)
            p3 <- p1 %+% Split$`Santa Cruz Island` +
              labs(title = NULL, subtitle = NULL)
            p4 <- p1 %+% Split$`Anacapa Island` +
              labs(title = NULL, subtitle = NULL)
            p5 <- p1 %+% Split$`Santa Barbara Island`  +
              labs(title = NULL, subtitle = NULL, x = "Year") +
              theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1, size = 10))
            arrange_plot <- ggpubr::ggarrange(
              p1, p2, p3, p4, p5, ncol = 1, heights = c(.9, .75, .75, .75, 1),
              align = "v", common.legend = FALSE)
            annotate_plot <- ggpubr::annotate_figure(
              arrange_plot, left = text_grob(paste(ylab()), color = "black", rot = 90, size = 16)
            )
            print(annotate_plot)
          } 
          else if (input$Data_Options == "Original 16 Sites") {
            p1 <- ggplot2::ggplot(data_subset(), aes(x = Date, y = yvalue, linetype = ReserveYear)) +
              ggplot2::geom_smooth(size = 1, span = 0.75, method = 'loess', formula = 'y ~ x',
                                   aes(color = ReserveYear)) +
              ggplot2::scale_x_date(date_labels = "%Y", date_breaks = "year", expand = c(0, 0)) +
              ggplot2::scale_y_continuous(limits = yscale(), expand = expansion(mult = c(0, .01)), oob = squish) +
              ggplot2::scale_colour_manual(values = Island_Colors) +
              ggplot2::labs(title = data_subset()$CommonName,
                            x = NULL, y = NULL,
                            linetype = "Reserve Status",
                            color = "Reserve Status") +
              timeseries_top_theme()
            
            p2 <- ggplot2::ggplot(data_subset(), aes(x = Date, y = yvalue, color = IslandName)) +
              ggplot2::geom_smooth(size = 1, span = .75, method = 'loess', formula = 'y ~ x') +
              ggplot2::scale_x_date(date_labels = "%Y", date_breaks = "year", expand = c(0, 0)) +
              ggplot2::scale_y_continuous(limits = yscale(), expand = expansion(mult = c(0, .01)), oob = squish) +
              ggplot2::scale_colour_manual(values = Island_Colors) +
              ggplot2::labs(x = NULL, y = NULL,
                            color = "Island") +
              timeseries_top_theme()
            
            p3 <- ggplot2::ggplot() +
              ggplot2::geom_rect(data = SST_Anomaly_Index,
                                 aes(xmin = DateStart, xmax = DateEnd, ymin = -Inf, ymax = 0, fill = ONI_ANOM)) +
              ggplot2::scale_fill_gradient2(high = "darkred", mid = "white", low = "navyblue", midpoint = 0,
                                            guide = guide_colorbar(direction = "horizontal", title.position = "top",
                                                                   order = 3, barheight = unit(.2, "cm"))) +
              ggplot2::geom_smooth(data = data_subset(), method = 'loess', formula = 'y~x', size = 1, se = F, span = .75,
                                   aes(x = Date, y = yvalue, color = IslandName, linetype = ReserveStatus)) +
              ggplot2::geom_hline(aes(yintercept = 0)) +
              ggplot2::scale_x_date(date_labels = "%Y", date_breaks = "year", expand = c(0, 0),
                                    limits = c(lubridate::ymd(min(data_subset()$Date)),
                                               lubridate::ymd(max(data_subset()$Date)))) +
              ggplot2::scale_y_continuous(expand = expansion(mult = c(0.1, .01)),
                                          limits = yscale(), oob = squish) +
              ggplot2::guides(color = guide_legend(order = 1),
                              linetype = guide_legend(order = 2, override.aes = list(col = 'black'))) +
              ggplot2::scale_colour_manual(values = Island_Colors) +
              ggplot2::labs(x = "Survey Year", y = NULL,
                            color = "Island",
                            fill = "Oceanic Niño Index",
                            linetype = "Reserve Status") +
              timeseries_bottom_theme()
            Diversity_Plot <-ggarrange(p1, p2, p3, ncol = 1, align = "v", heights = c(.8, .8, 1))
            Diversity_annotated <- ggpubr::annotate_figure(
              Diversity_Plot,
              left = text_grob(ylab(), color = "black", rot = 90, size = 13))
            print(Diversity_annotated)
          } 
          else if (input$Data_Options == "MPA Reference Sites") {
            p1 <- ggplot2::ggplot(data_subset(), aes(x = Date, y = yvalue, linetype = ReserveStatus)) +
              ggplot2::geom_smooth(size = 1, span = 0.75, method = 'loess', formula = 'y ~ x',
                                   aes(color = ReserveStatus)) +
              ggplot2::scale_x_date(date_labels = "%Y", date_breaks = "year", expand = c(0, 0)) +
              ggplot2::scale_y_continuous(limits = yscale(), expand = expansion(mult = c(0, .01)), oob = squish) +
              ggplot2::scale_colour_manual(values = Island_Colors) +
              ggplot2::labs(title = data_subset()$CommonName,
                            x = NULL, y = NULL,
                            linetype = "Reserve Status",
                            color = "Reserve Status") +
              timeseries_top_theme()
            
            p2 <- ggplot2::ggplot(data_subset(), aes(x = Date, y = yvalue, color = IslandName)) +
              ggplot2::geom_smooth(size = 1, span = .75, method = 'loess', formula = 'y ~ x') +
              ggplot2::scale_x_date(date_labels = "%Y", date_breaks = "year", expand = c(0, 0)) +
              ggplot2::scale_y_continuous(limits = yscale(), expand = expansion(mult = c(0, .01)), oob = squish) +
              ggplot2::scale_colour_manual(values = Island_Colors) +
              ggplot2::labs(x = NULL, y = NULL,
                            color = "Island") +
              timeseries_top_theme()
            
            p3 <- ggplot2::ggplot() +
              ggplot2::geom_rect(data = SST_Anomaly_Index,
                                 aes(xmin = DateStart, xmax = DateEnd, ymin = -Inf, ymax = 0, fill = ONI_ANOM)) +
              ggplot2::scale_fill_gradient2(high = "darkred", mid = "white", low = "navyblue", midpoint = 0,
                                            guide = guide_colorbar(direction = "horizontal", title.position = "top",
                                                                   order = 3, barheight = unit(.2, "cm"))) +
              ggplot2::geom_smooth(data = data_subset(), method = 'loess', formula = 'y~x', size = 1, se = F, span = .75,
                                   aes(x = Date, y = yvalue, color = IslandName, linetype = ReserveStatus)) +
              ggplot2::geom_hline(aes(yintercept = 0)) +
              ggplot2::scale_x_date(date_labels = "%Y", date_breaks = "year", expand = c(0, 0),
                                    limits = c(lubridate::ymd(min(data_subset()$Date)),
                                               lubridate::ymd(max(data_subset()$Date)))) +
              ggplot2::scale_y_continuous(expand = expansion(mult = c(0.1, .01)), limits = yscale(), oob = squish) +
              ggplot2::guides(color = guide_legend(order = 1),
                              linetype = guide_legend(order = 2, override.aes = list(col = 'black'))) +
              ggplot2::scale_colour_manual(values = Island_Colors) +
              ggplot2::labs(x = "Survey Year", y = NULL,
                            color = "Island",
                            fill = "Oceanic Niño Index",
                            linetype = "Reserve Status") +
              timeseries_bottom_theme()
            Diversity_Plot <- ggarrange(p1, p2, p3, ncol = 1, align = "v", heights = c(.8, .8, 1))
            Diversity_annotated <- ggpubr::annotate_figure(
              Diversity_Plot,
              left = text_grob(ylab(), color = "black", rot = 90, size = 13))
            print(Diversity_annotated)
          } 
          else if (input$Data_Options == "Individual Site" & input$line == "Smooth") {
            ggplot2::ggplot() +
              geom_rect(data = SST_Anomaly_Index,
                        aes(xmin = DateStart, xmax = DateEnd, ymin = -Inf, ymax = 0, fill = ONI_ANOM)) +
              ggplot2::scale_fill_gradient2(high = "darkred", mid = "white", low = "navyblue", midpoint = 0,
                                            guide = guide_colorbar(direction = "horizontal", title.position = "top",
                                                                   order = 3, barheight = unit(.2, "cm"))) +
              ggplot2::geom_smooth(data = data_subset(), method = 'loess', formula = 'y~x', size = 1, se = F, span = .75,
                                   aes(x = Date, y = yvalue, color = SiteName, linetype = ReserveStatus)) +
              ggplot2::geom_hline(aes(yintercept = 0)) +
              ggplot2::scale_x_date(date_labels = "%Y", date_breaks = "year", expand = c(0, 0),
                                    limits = c(lubridate::ymd(min(data_subset()$Date)),
                                               lubridate::ymd(max(data_subset()$Date)))) +
              ggplot2::scale_y_continuous(expand = expansion(mult = c(0.1, .01)),
                                          limits = yscale(), oob = squish) +
              ggplot2::guides(color = guide_legend(order = 1),
                              linetype = guide_legend(order = 2, override.aes = list(col = 'black'))) +
              ggplot2::scale_colour_manual(values = SiteColor) +
              ggplot2::labs(x = "Survey Year", y = ylab(),
                            title = data_subset()$CommonName,
                            subtitle = glue("{data_subset()$SiteName} {data_subset()$IslandName}"),
                            color = "Site",
                            fill = "Oceanic Niño Index",
                            linetype = "Reserve Status") +
              timeseries_bottom_theme()
          }
          else if (input$Data_Options == "Individual Site" & input$line == "Sharp") {
            ggplot2::ggplot() +
              ggplot2::geom_rect(data = SST_Anomaly_Index,
                                 aes(xmin = DateStart, xmax = DateEnd, ymin = -Inf, ymax = 0, fill = ONI_ANOM)) +
              ggplot2::scale_fill_gradient2(high = "darkred", mid = "white", low = "navyblue", midpoint = 0,
                                            guide = guide_colorbar(direction = "horizontal", title.position = "top",
                                                                   order = 3, barheight = unit(.2, "cm"))) +
              ggplot2::geom_line(data = data_subset(), method = 'loess', formula = 'y~x', size = 1, se = F, span = .75,
                                 aes(x = Date, y = yvalue, color = SiteName, linetype = ReserveStatus)) +
              ggplot2::geom_hline(aes(yintercept = 0)) +
              ggplot2::scale_x_date(date_labels = "%Y", date_breaks = "year", expand = c(0, 0),
                                    limits = c(lubridate::ymd(min(data_subset()$Date)),
                                               lubridate::ymd(max(data_subset()$Date)))) +
              ggplot2::scale_y_continuous(expand = expansion(mult = c(0.1, .01)),
                                          limits = yscale(), oob = squish) +
              ggplot2::guides(color = guide_legend(order = 1),
                              linetype = guide_legend(order = 2, override.aes = list(col = 'black'))) +
              ggplot2::scale_colour_manual(values = SiteColor) +
              ggplot2::labs(x = "Survey Year", y = ylab(),
                            title = data_subset()$CommonName,
                            subtitle = glue("{data_subset()$SiteName} {data_subset()$IslandName}"),
                            color = "Site",
                            fill = "Oceanic Niño Index",
                            linetype = "Reserve Status") +
              timeseries_bottom_theme()
          }
        })
        
        Species_Code <- reactive({
            Species_Info %>% 
              dplyr::filter(CommonName == input$species) %>% 
              dplyr::distinct(CommonName, .keep_all = TRUE)
        })
        
        pic_filename <- reactive(glue::glue("www/Photos/Indicator_Species/{Species_Code()$Species}.jpg"))
        
        output$pic <- renderImage({list(src = pic_filename(), width = "100%", height = "100%")}, delete = FALSE)
        
        taxonomy_table_data <- reactive({
          Species_Info %>%
            dplyr::filter(Species == Species_Code()$Species) %>%
            dplyr::select(Species, Kingdom, Phylum, Subphylum, Class, Order, Family, Genus, `Species (Used by KFM)`,
                          `Currently Accepted Name`, `Authority (Accepted)`, CommonName) %>%
            dplyr::rename(`Common Name` = CommonName) %>% 
            tidyr::pivot_longer(-Species, names_to = "Category", values_to = "Information") %>%
            dplyr::select(Category, Information)
        }) 
        
        output$taxonomy_table <- renderDT({
          datatable(
            taxonomy_table_data(), rownames = FALSE,  
            options = list(
              searching = FALSE, lengthChange = FALSE, paging = FALSE,
              ordering = FALSE, info = FALSE, 
              initComplete = JS(
                "function(settings, json) {",
                "$(this.api().table().header()).css({'background-color': '#3c8dbc', 'color': '#fff'});",
                "}"))) %>%
            formatStyle(
              names(taxonomy_table_data()),
              color = "black",
              backgroundColor = 'white'
            )
        })
        
        meta_table_data <- reactive({
          Species_Info %>%
            dplyr::filter(Species == Species_Code()$Species) %>%
            dplyr::select(ScientificName, Trophic_Broad, Habitat_Broad, 
                          Geographic_Range, Size_Range, ID_Short, Abundance,
                          Commercial_Fishery, Recreational_Fishery) %>%
            dplyr::rename(`Trophic Level` = Trophic_Broad, 
                          Habitat = Habitat_Broad,
                          Range = Geographic_Range, 
                          `Size Range` = Size_Range, 
                          Identification = ID_Short, 
                          `Commercial Fishery` = Commercial_Fishery,
                          `Recreational Fishery` = Recreational_Fishery) %>% 
            tidyr::pivot_longer(-ScientificName, names_to = "Category", values_to = "Information") %>%
            dplyr::select(Category, Information)
        }) 
        
        output$meta_table <- renderDT({
          datatable(
            meta_table_data(), rownames = FALSE,  
            options = list(
              searching = FALSE, lengthChange = FALSE, paging = FALSE,
              ordering = FALSE, info = FALSE, 
              initComplete = JS(
                "function(settings, json) {",
                "$(this.api().table().header()).css({'background-color': '#3c8dbc', 'color': '#fff'});}"))) %>%
            formatStyle(names(meta_table_data()), color = "black", backgroundColor = 'white')
        })
        
        data_table_data <- reactive({
          if (id == "biomass"){
            data_subset() %>% 
              dplyr::select(IslandName, SiteName, Survey_Type, SurveyYear, CommonName, Mean_Biomass, ReserveStatus) %>% 
              dplyr::rename(`Island Name` = IslandName, 
                            `Site Name` = SiteName,
                            `Survey Type` = Survey_Type,
                            `Survey Year` = SurveyYear, 
                            `Common Name` = CommonName, 
                            `Mean Biomass (g/m²)` = Mean_Biomass, 
                            `Reserve Status` = ReserveStatus)
          }
          else if (id == "density") {
            data_subset() %>% 
              dplyr::select(IslandName, SiteName, Survey_Type, SurveyYear, CommonName, Mean_Density, ReserveStatus) %>% 
              dplyr::rename(`Island Name` = IslandName, 
                            `Site Name` = SiteName,
                            `Survey Type` = Survey_Type,
                            `Survey Year` = SurveyYear, 
                            `Common Name` = CommonName, 
                            `Mean Density (#/m²)` = Mean_Density,
                            `Reserve Status` = ReserveStatus)
          }
        })
        
        output$data_table <- renderDT({
          datatable(data_table_data(), rownames = FALSE, extensions = c('Buttons', 'ColReorder'),
                    options = list(
                      scrollY = "500px", scrollX = TRUE, paging = FALSE,
                      ordering = TRUE, info = FALSE, dom = 'Bfrtip', colReorder = TRUE,
                      buttons =  c('copy', 'csv', 'excel', 'pdf', 'print'),
                      initComplete = JS(
                        "function(settings, json) {",
                        "$(this.api().table().header()).css({'background-color': '#3c8dbc', 'color': '#fff'});}"),
                      columnDefs = list(list(className = 'dt-center', targets = 0:6)))) %>% 
            formatStyle(names(data_table_data()), color = "black", backgroundColor = 'white')
        }) 
        
      }
    )
  }
  
}






