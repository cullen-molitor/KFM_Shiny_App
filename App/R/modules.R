


{ # Protocol Tab Panel Module   ----
  protocol_UI <- function(id, label = "proto") {
    ns <- NS(id)
    tagList(
      tags$hr(),
      radioButtons(inputId = ns("Overview_Practical"),
                   label = "Choose:",
                   inline = TRUE,
                   choices = c("Overview", "Pratical Guide", "Species Guide")),
      conditionalPanel(
        condition = "input.Overview_Practical == 'Overview'", ns = ns,
        fluidRow(
          column(
            6, uiOutput(outputId = ns("text"))
          ),
          column(
            6, tags$hr(),
            imageOutput(outputId = ns("proto_pic1")),
            tags$br(),
            imageOutput(outputId = ns("proto_pic2")),
            tags$br(),
            imageOutput(outputId = ns("proto_pic3")),
            tags$br(),
            imageOutput(outputId = ns("proto_pic4")),
            tags$br(),
            imageOutput(outputId = ns("proto_pic5"))
          )
        )
      ),
      conditionalPanel(
        condition = "input.Overview_Practical == 'Pratical Guide'", ns = ns,
        uiOutput(outputId = ns("protocol_guide"))
      ),
      conditionalPanel(
        condition = "input.Overview_Practical == 'Species Guide'", ns = ns,
        uiOutput(outputId = ns("species_guide"))
      )
    )
  }
  
  protocol_Server <- function(id) {
    moduleServer(
      id,
      function(input, output, session) {
        
        filename_text <- reactive(glue("Text/Protocols/{id}.md"))
        filename_pic_1 <- reactive({glue::glue("www/Photos/Protocols/{id} (1).jpg")})
        filename_pic_2 <- reactive({glue::glue("www/Photos/Protocols/{id} (2).jpg")})
        filename_pic_3 <- reactive({glue::glue("www/Photos/Protocols/{id} (3).jpg")})
        filename_pic_4 <- reactive({glue::glue("www/Photos/Protocols/{id} (4).jpg")})
        filename_pic_5 <- reactive({glue::glue("www/Photos/Protocols/{id} (5).jpg")})
        
        output$text <- renderUI(
          includeMarkdown(path = filename_text()))
        
        output$proto_pic1 <- renderImage({
          list(src = filename_pic_1(), width = 600, height = 400)
        }, deleteFile = FALSE)
        
        output$proto_pic2 <- renderImage({
          list(src = filename_pic_2(), width = 600, height = 400)
        }, deleteFile = FALSE)
        
        output$proto_pic3 <- renderImage({
          list(src = filename_pic_3(), width = 600, height = 400)
        }, deleteFile = FALSE)
        
        output$proto_pic4 <- renderImage({
          list(src = filename_pic_4(), width = 600, height = 400)
        }, deleteFile = FALSE)
        
        output$proto_pic5 <- renderImage({
          list(src = filename_pic_5(), width = 600, height = 400)
        }, deleteFile = FALSE)
        
        output$protocol_guide <- renderUI({
          tags$iframe(
            style = "height:600px; width:100%; scrolling=yes",
            src = glue::glue(
              "Handbook/Protocol_Guides/{id}_protocol_guide.pdf"))
        })
        
        output$species_guide <- renderUI({
          tags$iframe(
            style = "height:600px; width:100%; scrolling=yes",
            src = glue::glue(
              "Handbook/Species_Guides/{id}_species_guide.pdf"))
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
                                            choices = c(#"North Islands",
                                                        unique(Site_Info$IslandName))))
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
                             # leafletOutput(outputId = ns("Diversity_leaf"),
                                           # height = 500)
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
                     height = if (input$Data_Options == "Individual Site"){
                       350} else {750})
        })
        
        filename_text <- reactive(glue("Text/{id}_index.md"))
        
        output$Diversity_Text <- renderUI(includeMarkdown(path = filename_text()))
        
        data <- reactive({
          if (id == "shannon") {
            Diversity %>% 
              dplyr::rename(Index = shannon) 
          } 
          else if (id == "simpson") {
            Diversity %>% 
              dplyr::rename(Index = simpson)
          }
        })
        
        plot_title <- reactive({
          if (id == "shannon") {
            "Shannon-Wiener Diversity Index" 
          } 
          else if (id == "simpson") {
            "Gini-Simpson Diversity Index" 
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
          data() %>% 
            dplyr::filter(SurveyYear == input$map_slider, IslandName == input$map_center)
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
                            title = glue("{data_subset()$SiteName} {data_subset()$IslandName}"),
                            subtitle = plot_title(),
                            color = "Island",
                            fill = "Oceanic Niño Index",
                            linetype = "Reserve Status") +
              timeseries_bottom_theme()
          }
        })
        
        output$map <-  renderPlot({ # Map     ----
          ggplot()+  
            geom_sf(data = dplyr::filter(CINP, IslandName == input$map_center)) +
            geom_text(data = dplyr::distinct(dplyr::filter(Site_Info, IslandName == input$map_center)),
                      aes(x = Island_Longitude, y = Island_Latitude - .003, label = IslandName), size = 5) +
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








