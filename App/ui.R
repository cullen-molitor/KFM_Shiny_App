
# Use ALT + O to see outline


# Define the User Interface for application 
# Dashboard Page  ----
ui <- dashboardPage(
  title = 'KFM App',  
  skin = 'blue',
  # .. Dashboard Header  ----
  dashboardHeader( 
    title =tags$strong(
      'CHIS-KFM', 
      tags$img(height = 50, width = 40, src = 'Graphics/Arrowhead.png')),
    titleWidth = 200
    ),
  # .. Dashboard Sidebar   ----
  dashboardSidebar(
    width = 200,
    sidebarMenu(
      id = 'sidebar',
      style = 'position: fixed; overflow: visible;',
      # ...... Sidebar - About  ----
      menuItem(text = 'About', icon = icon('globe'),
               badgeColor = 'green', tabName = 'about'),
      # ...... Sidebar - Species  ----
      menuItem(text = 'Species', icon = icon('fish'),
               badgeColor = 'green',tabName = 'species'),
      # ...... Sidebar - Protocols  ----
      menuItem(text = 'Protocols', icon = icon('grip-horizontal'),
               badgeColor = 'green', tabName = 'protocols'),
      # ...... Sidebar - Maps  ----
      menuItem(text = 'Sampling Locations', icon = icon('globe'),
               badgeColor = 'green', tabName = 'maps'),
      # ...... Sidebar - Biodiversity  ----
      menuItem(text = 'Biodiversity', icon = icon('rainbow'), 
               badgeColor = 'green', tabName = 'diversity'),
      # ...... Sidebar - Community Similarity  ----
      menuItem(text = 'Community Similarity', icon = icon('balance-scale'),
               badgeColor = 'green', tabName = 'com_sim'),
      # ...... Sidebar - Important Species  ----
      menuItem(text = 'Important Species', icon = icon('otter'),
               badgeColor = 'green', tabName = 'imp_spe'),
      # ...... Sidebar - Biomass  ----
      menuItem(text = 'Biomass', icon = icon('hippo'),
               badgeColor = 'green', tabName = 'biomass'),
      # ...... Sidebar - Density  ----
      menuItem(text = 'Density', icon = icon('gem'),
               badgeColor = 'green', tabName = 'density'),
      # ...... Sidebar - Size Frequencies  ----
      menuItem(text = 'Size Frequency', icon = icon('ruler'),
               badgeColor = 'green', tabName = 'sizes'),
      # ...... Sidebar - Reports  ----
      menuItem(text = 'Reports', icon = icon('newspaper'),
               badgeColor = 'green', tabName = 'reports'),
      # ...... Sidebar - Literature Cited  ----
      menuItem(text = 'Literature Cited', icon = icon('book-reader'),
               badgeColor = 'green', tabName = 'lit')
    )
  ),
  # .. Dashboard Body  ----
  dashboardBody(
    tabItems(
      # ...... Body - About    ---- 
      tabItem(
        tabName = 'about',
        h1("Channel Islands National Park's Kelp Forest Monitoring Program"),
        tabsetPanel(
          # ............ Tab - Disclaimer  ----
          tabPanel(
            title = "Disclaimer",
            fluidRow(
              column(
                6, includeMarkdown(path = "Text/About/disclaimer.md")
              ),
              column(
                6,
                tags$hr(),
                imageOutput(outputId = "disc_pic_1", height = 400) %>% 
                  shinycssloaders::withSpinner(),
                div(HTML("Diver in a kelp forest on Santa Barbara Island")),
                div(HTML("Photo: Laurie Montgomery"))
              )
            )
          ),
          # ............ Tab - App Basics  ----
          tabPanel(
            title = "App Basics",
            fluidRow(
              column(
                6, includeMarkdown(path = "Text/About/app_intro.md")
              ),
              column(
                6, 
                tags$hr(),
                imageOutput(outputId = "basics_pic_1", height = 400) %>% 
                  shinycssloaders::withSpinner(),
                div(HTML("California sea lion (<em>Zalophus californianus</em>) swimming through the kelp forest on Santa Barbara Island")),
                div(HTML("Photo: Brett Seymour")),
                tags$br(),
                imageOutput(outputId = "basics_pic_2", height = 400) %>% 
                  shinycssloaders::withSpinner(),
                div(HTML("Diver swimming through the kelp forest on Santa Barbara Island")),
                div(HTML("Photo: Kenan Chan")),
                tags$br(),
                imageOutput(outputId = "basics_pic_3", height = 400) %>% 
                  shinycssloaders::withSpinner(),
                div(HTML("Typical kelp forest scene on Anacapa Island")),
                div(HTML("Photo: Kenan Chan"))
              )
            )
          ),
          # ............ Tab - KFMP History  ----
          tabPanel(
            title = "KFMP History",
            fluidRow(
              column(
                6, includeMarkdown(path = "Text/About/history.md")
              ),
              column(
                6, 
                tags$hr(),
                imageOutput(outputId = "history_pic_1", height = 450) %>% 
                  shinycssloaders::withSpinner(),
                div(HTML("Map of Channel Islands National Park")),
                div(HTML("Map: Rockne Rudolph")),
                tags$br(),
                imageOutput(outputId = "history_pic_2", height = 400) %>% 
                  shinycssloaders::withSpinner(),
                div(HTML("Diver holding a California spiny lobster (<em>panulirus interruptus</em>), a highly targeted fishery species")),
                div(HTML("Photo: Brett Seymour")),
                tags$br(),
                imageOutput(outputId = "history_pic_3", height = 400) %>% 
                  shinycssloaders::withSpinner(),
                div(HTML("Red abalone (<em>Haliotis rufescens</em>) on San Miguel Island")),
                div(HTML("Photo: Kelly Moore"))
              )
            )
          ),
          # ............ Tab - Acknowledgments  ----
          tabPanel(
            title = "Acknowledgments",
            fluidRow(
              column(
                6, includeMarkdown(path = "Text/About/acknowledgments.md")
              ),
              column(
                6, 
                tags$hr(),
                imageOutput(outputId = "ack_pic_1", height = 400) %>% 
                  shinycssloaders::withSpinner(),
                div(HTML("California sea lion swimming through the kelp forest on Santa Barbara Island")),
                div(HTML("Photo: Laurie Montgomery")),
                tags$br(),
                imageOutput(outputId = "ack_pic_2", height = 400) %>% 
                  shinycssloaders::withSpinner(),
                div(HTML("Interpretative Ranger discussing giant kelp (<em>Macrocystis pyrifera</em>) during Channel Islands Live broadcast")),
                div(HTML("Photo: Brett Seymour")),
                tags$br(),
                imageOutput(outputId = "ack_pic_3", height = 400) %>% 
                  shinycssloaders::withSpinner(),
                div(HTML("NPS dive vessel Sea Ranger II approaching the peir at Scorpion Anchorage")),
                div(HTML("Photo: Kenan Chan"))
              )
            )
          ),
          # ............ Tab - Acronyms  ----
          tabPanel(
            title = "Acronyms",
            h2("Common Acronyms"),
            fluidRow(
              column(
                6, 
                DTOutput(outputId = "Acro_Table")
              ),
              column(
                6, 
                imageOutput(outputId = "acr_pic_1", height = 400) %>% 
                  shinycssloaders::withSpinner(),
                div(HTML("California sea lion (<em>Zalophus californianus</em>) playing with southern sea palm (<em>Eisenia arborea</em>) on Santa Barbara Island")),
                div(HTML("Photo: Brett Seymour")),
                tags$br(),
                imageOutput(outputId = "acr_pic_2", height = 400) %>% 
                  shinycssloaders::withSpinner(),
                div(HTML("KFM diver conducting Random Point Contacts (RPCs)")),
                div(HTML("Photo: Shaun Wolfe"))
                
              )
            )
          ),
          # ............ Tab - Blogs  ----
          tabPanel(
            title = "Blog Posts",
            fluidRow(
              column(
                6,
                h2("OWUSS KFM Blogs"),
                NPS_Blog_tagList
              ),
              column(
                6, 
                tags$hr(),
                imageOutput(outputId = "blog_pic_1", height = 400) %>% 
                  shinycssloaders::withSpinner(),
                div(HTML("Giant kelp (<em>Macrocystis pyrifera</em>)")),
                div(HTML("Photo: Brett Seymour"))
              )
            )
          ),
          # ............ Tab - FAQ  ----
          tabPanel(
            title = "FAQ",
            fluidRow(
              column(
                6, includeMarkdown(path = "Text/About/FAQ.md")
              ),
              column(
                6, 
                tags$hr(),
                imageOutput(outputId = "faq_pic_1", height = 400) %>% 
                  shinycssloaders::withSpinner(),
                div(HTML("Kelp forest canopy as seen from below")),
                div(HTML("Photo: Kenan Chan"))
              )
            )
          )
        )
      ),
      # ...... Body - Species    ---- 
      tabItem(
        tabName = 'species',
        h1("Kelp Forest Species"),
        tabsetPanel(
          # ............ Tab - Foundation Species  ----
          tabPanel(
            title = "Foundation Species",
            includeMarkdown(path = "Text/Species/foundation.md"),
            foundation_UI(id = "kelp"),
            foundation_UI(id = "p_urchin"),
            foundation_UI(id = "r_urchin"),
            foundation_UI(id = "r_abalone"),
            foundation_UI(id = "lobsta"),
            foundation_UI(id = "sheep"),
            foundation_UI(id = "sunflower"),
            foundation_UI(id = "giant-spined")
          ),
          # ............ Tab - Invasive Species  ----
          tabPanel(
            title = "Invasive Species",
            tags$hr(),
            includeMarkdown(path = "Text/Species/invasive.md"),
            tags$hr(),
            foundation_UI(id = "sargassum"),
            foundation_UI(id = "undaria")
          ),
          # ............ Tab - KFM Taxa   ----
          tabPanel(
            title = "KFM Taxa",
            tags$hr(),
            radioButtons(inputId = "Species_Options",
                         label = "Choose a Category",
                         inline = TRUE,
                         choices = c("Overview", "Protocol Taxa Guides", "Taxa")),
            conditionalPanel(
              condition = "input.Species_Options == 'Overview'",
              fluidRow(
                column(
                  6, 
                  includeMarkdown("Text/Species/species_selection.md") 
                ),
                column(
                  6, tags$hr(),
                  tags$img(height = 400, width = 600, src = 'Photos/Kelp_Forest_Scenes/Kenan_Chan/1 (2).jpg'),
                  tags$br(), tags$hr(),
                  tags$img(height = 400, width = 600, src = 'Photos/Kelp_Forest_Scenes/Kenan_Chan/1 (10).jpg')
                )
              ),
              tags$hr(),
              fluidRow(
                column(
                  6,
                  tags$img(height = 400, width = 600, src = 'Photos/Kelp_Forest_Scenes/Brett_Seymour/1 (7).jpg')
                ),
                column(
                  6, 
                  tags$img(height = 400, width = 600, src = 'Photos/Kelp_Forest_Scenes/Brett_Seymour/1 (8).jpg')
                )
              )
            ),
            conditionalPanel(
              condition = "input.Species_Options == 'Protocol Taxa Guides'",
              tags$hr(),
              species_guide_UI(id = "species")
            ),
            conditionalPanel(
              condition = "input.Species_Options == 'Taxa'",
              Taxa_UI(id = "species")
            )
          ),
          # ............ Tab - Common Diseases  ----
          tabPanel(
            title = "Diseases Guides",
            tags$hr(),
            fluidRow(
              column(
                2,
                radioButtons(inputId = "disease",
                             label = "Choose a Taxa:", 
                             choices = c("Sea Stars", "Sea Urchin", "Abalone"))
              ),
              column(
                10,
                conditionalPanel(
                  condition = "input.disease == 'Sea Stars'",
                  uiOutput(outputId = "SSWD")),
                conditionalPanel(
                  condition = "input.disease == 'Sea Urchin'",
                  uiOutput(outputId = "urchins")),
                conditionalPanel(
                  condition = "input.disease == 'Abalone'",
                  fluidRow(
                    column(
                      6,
                      includeMarkdown("Text/Species/abalone_disease.md")
                    ),
                    column(
                      6,
                      imageOutput(outputId = "abalone", height = 400) %>% 
                        shinycssloaders::withSpinner(),
                      h4("Farmed red abalone (Haliotis rufescens). The animal on the right shows classic signs of WS."),
                      h5("Photo Credit: CDFW")
                    )
                  )
                )
              )
            )
          ),
          # ............ Tab - External Resources  ----
          tabPanel(
            title = "External Resources",
            h2("Coming soon... "),
            h4("PISCO - Partnership for Interdisciplinary Studies of Coastal Oceans"),
            h4("SBC LTER - Santa Barbara Coastal Long Term Ecological Research"),
            h4("RCCA - Reef Check California"),
            h4("Vantuna"),
            h4("MARINE - Multi-Agency Rocky Intertidal Network"),
            h4("SIMoN - Sanctuary Integrated Monitoring Network"),
            h4("SEANET - Hopkins Marine Station's Guide to Nearshore Plants and Animals of the Monterey Bay"),
            h4("CDFW MSP - California Department of Fish and Wild Life Marine Species Portal"),
            h4("SCB MBON - Southern California Bight Marine Biodiversity Observation Network"),
            h4("Macro Algae Herbarium Portal"),
            h4("Spotting Giant Sea Bass")
          )
        )
      ),
      # ...... Body - Protocols    ---- 
      tabItem(
        tabName = 'protocols',
        h1("Kelp Forest Monitoring Protocols"),
        tabsetPanel(
          # ............ Tab - Protocol Selection  ----
          tabPanel(
            title = "KFM Protocol Selection",
            fluidRow(
              column(
                6, 
                includeMarkdown("Text/Protocols/protocol_selection.md") 
              ),
              column(
                6, 
                tags$hr(),
                tags$img(height = 400, width = 600, src = 'Photos/Protocols/1m/1m (2).jpg'),
                h5("1 m² Quadrats"),
                h5("Photo: Shaun Wolfe"),
                tags$img(height = 400, width = 600, src = 'Photos/Protocols/5m/5m (1).jpg'),
                h5("5 m² Quadrats"),
                h5("Photo: Shaun Wolfe")
              )
            ),
            tags$hr(),
            fluidRow(
              column(
                6,
                tags$img(height = 400, width = 600, src = 'Photos/Protocols/bands/bands (1).jpg'),
                h5("Band Transects"),
                h5("Photo: Shaun Wolfe") 
              ),
              column(
                6, 
                tags$img(height = 400, width = 600, src = 'Photos/Protocols/rpcs/rpcs (1).jpg'),
                h5("Random Point Contacts (RPCs)"),
                h5("Photo: Shaun Wolfe")
              )
            )
          ),
          # ............ Tab - KFM Protocols  ----
          tabPanel(
            title = "KFM Protocols",
            protocol_UI(id = "protocol")
          )
        )
      ),
      # ...... Body - Sampling Locations    ---- 
      tabItem(
        tabName = 'maps',
        h1("Kelp Forest Monitoring Sampling Locations"),
        tabsetPanel(
          # ............ Tab - Site Selection   ----
          tabPanel(
            title = "Site History",
            fluidRow(
              column(
                6,
                includeMarkdown(path = "Text/Sites/site_history.md")
              ),
              column(
                6,
                tags$hr(),
                imageOutput(outputId = "site_image1", height = 400) %>% 
                  shinycssloaders::withSpinner(),
                h5("Sea surface temperature around the Channel Islands on 31 August 2003"),
                h5("Photo: NOAA"),
                imageOutput(outputId = "site_image2", height = 400) %>% 
                  shinycssloaders::withSpinner(),
                h5("A diver laying lead line for a new monitoring site (2005)"),
                h5("Photo: KFMP"),
                imageOutput(outputId = "site_image3", height = 400) %>% 
                  shinycssloaders::withSpinner(),
                h5("NPS dive vessel, the Sea Ranger II at Pelican Bay, Santa Cruz Island"),
                h5("Photo: Kenan Chan"),
                imageOutput(outputId = "site_image4", height = 400) %>% 
                  shinycssloaders::withSpinner(),
                h5("The Sea Ranger II anchored at Scorpion Anchorage, Santa Cruz Island"),
                h5("Photo: Shaun Wolfe"),
                imageOutput(outputId = "site_image5", height = 400) %>% 
                  shinycssloaders::withSpinner(),
                h5("The Sea Ranger II en route to Prisoners Harbor, Santa Cruz Island"),
                h5("Photo: Kenan Chan")
              )
            ),
            fluidRow(
              column(
                6,
                imageOutput(outputId = "site_image6", height = 400) %>% 
                  shinycssloaders::withSpinner(),
                h5("A diver crashing through the surface to begin monitoring efforts"),
                h5("Photo: Brett Seymour")
              ),
              column(
                6,
                imageOutput(outputId = "site_image7", height = 400) %>% 
                  shinycssloaders::withSpinner(),
                h5("Sea Ranger II anchored in Cuyler Harbor, San Miguel Island"),
                h5("Photo: Kenan Chan")
              )
            )
          ),
          # ............ Tab - Leaflet  ----
          tabPanel(
            title = "Leaflet Maps",
            tags$hr(),
            fluidRow(
              column(
                6,
                leafletOutput(outputId = "Leaflet",
                              height = 500, width = '100%')
              ),
              column(
                6, 
                includeMarkdown(path = "Text/Sites/leaflet.md")
              )
            )
            
          ),
          # ............ Tab - Static Imagery  ----
          tabPanel(
            title = "Static Imagery",
            tags$hr(),
            fluidRow(
              column(
                3,
                radioButtons(inputId = "Sat_Isl_Site",
                             label = "Choose a Category:",
                             inline = TRUE,
                             choices = c("Park", "Island", "MPA", "Site"))
              ),
              column(
                3,
                conditionalPanel(
                  condition = "input.Sat_Isl_Site == 'Island'",
                  selectInput(inputId = "Sat_Isl",
                              label = "Choose a Location:",
                              choices = Island_Levels_Long)
                ),
                conditionalPanel(
                  condition = "input.Sat_Isl_Site == 'MPA'",
                  selectInput(inputId = "Sat_MPA",
                              label = "Choose a Location:",
                              choices = MPA_Levels_Long)
                ),
                conditionalPanel(
                  condition = "input.Sat_Isl_Site == 'Site'",
                  Site_Selector_UI(id = "Site_Sat")
                )
              )
            ),
            fluidRow(
              column(
                7,
                imageOutput(outputId = "satMap", height = 800) %>% 
                  shinycssloaders::withSpinner()
              ),
              conditionalPanel(
                condition = "input.Sat_Isl_Site != 'Park'",
                fluidRow(
                  column(
                    5,
                    uiOutput(outputId = "map_text")
                  )
                )
              )
            ),
            conditionalPanel(
              condition = "input.Sat_Isl_Site == 'Park'",
              fluidRow(
                DTOutput(outputId = "Park_Table")
              )
            ),
            conditionalPanel(
              condition = "input.Sat_Isl_Site != 'Park'",
              fluidRow(
                DTOutput(outputId = "Site_Table")
              )
            )
          ),
          # ............ Tab - Bathymetric Imagery  ----
          tabPanel(
            title = "Bathymetric Imagery",
            tags$hr(),
            selectInput(inputId = "Bath_Maps_Site",
                        label = "Choose a Site:",
                        choices = dplyr::filter(Site_Info, Bath == TRUE)$SiteName),
            imageOutput(outputId = "Bathymetry_Map", height = 800) %>% 
              shinycssloaders::withSpinner()
          ),
          # ............ Tab - ARM Locations  ----
          tabPanel(
            title = "ARM Location Graphics",
            tags$hr(),
            fluidRow(
              column(
                2,
                radioButtons(inputId = "Arm_Maps_Site",
                             label = "Choose a Site:",
                             choices = dplyr::filter(dplyr::arrange(Site_Info, Longitude), ARMs == TRUE)$Isl_SiteName)
              ),
              column(
                10,
                imageOutput(outputId = "ARM_Map", height = 750) %>% 
                  shinycssloaders::withSpinner()
              )
            )
          ),
          # ............ Tab - Site Descriptions  ----
          tabPanel(
            title = "Site Descriptions",
            tags$hr(),
            fluidRow(
              column(
                2,
                radioButtons(inputId = "Site_Description_Site", label = "Choose a Site:",
                             choices = dplyr::arrange(Site_Info, Longitude)$Isl_SiteName)
              ),
              column(
                10,
                imageOutput(outputId = "Site_Description", height = 1000) %>% 
                  shinycssloaders::withSpinner()
              )
            )
          ),
          # ............ Tab - Transportation  ----
          tabPanel(
            title = "Transportation",
            tags$hr(),
            h2("Coming Soon..."),
            h5("Information and pictures about the NPS dive boat Sea Ranger II.")
          )
        )
      ),
      # ...... Body - Biodiversity    ---- 
      tabItem(
        tabName = 'diversity',
        h1("Kelp Forest Community Biodiversity"),
        tabsetPanel(
          # ............ Tab - About   ----
          tabPanel(
            title = "About",
            fluidRow(
              column(
                6,
                includeMarkdown(path = "Text/Biodiversity/biodiversity.md")
              ),
              column(
                6,
                tags$hr(),
                imageOutput(outputId = "diversity_pic1", height = 400) %>% 
                  shinycssloaders::withSpinner(),
                h5("Text"),
                h5("Photo: ..."), 
                imageOutput(outputId = "diversity_pic2", height = 400) %>% 
                  shinycssloaders::withSpinner(),
                h5("Text"),
                h5("Photo: ..."),
                imageOutput(outputId = "diversity_pic3", height = 400) %>% 
                  shinycssloaders::withSpinner(),
                h5("Text"),
                h5("Photo: ...")
              )    
            ),
            fluidRow(
              column(
                6,
                imageOutput(outputId = "diversity_pic4", height = 400) %>% 
                  shinycssloaders::withSpinner(),
                h5("Text"),
                h5("Photo: ...")
              ),
              column(
                6,
                imageOutput(outputId = "diversity_pic5", height = 400) %>% 
                  shinycssloaders::withSpinner(),
                h5("Text"),
                h5("Photo: ...")
              )
            )
          ),
          # ............ Tab - Species Richness  ----
          tabPanel(
            title = "Species Richness",
            diversity_UI(id = "richness")
          ),
          # ............ Tab - Shannon's Index  ----
          tabPanel(
            title = "Shannon-Wiener Diversity Index",
            diversity_UI(id = "shannon")
          ),
          # ............ Tab - Simpson's Index  ----
          tabPanel(
            title = "Gini–Simpson Diversity Index",
            diversity_UI(id = "simpson")
          )
        ),
        tags$hr()
      ),
      # ...... Body - Community Similarity    ---- 
      tabItem(
        tabName = 'com_sim',
        h1("Kelp Forest Community Similarity"),
        tabsetPanel(
          # ............ Tab - About  ----
          tabPanel(
            title = "About",
            # tags$hr(),
            fluidRow(
              column(
                6, includeMarkdown("Text/nMDS/com_sim.md")
              ),
              column(
                6, 
                tags$hr(),
                imageOutput(outputId = "com_pic_1", height = 400) %>% 
                  shinycssloaders::withSpinner(), 
                h5("Healthy kelp forest with mature canopy forming plants."),
                h5("Photo: Kenan Chan"),
                imageOutput(outputId = "com_pic_2", height = 400) %>% 
                  shinycssloaders::withSpinner(), 
                h5("A once forested reef, now a barren area dominated by sea urchins."),
                h5("Photo: Kenan Chan")
              )
            )
          ),
          # ............ Tab - 2D  ----
          tabPanel(
            title = "2-Dimensional",
            fluidRow(
              column(
                4, includeMarkdown("Text/nMDS/2d.md")
                
              ),
              column(
                8,
                fluidRow(
                  column(
                    3, 
                    radioButtons(inputId = "radio_2D_years",
                                 label = "Data Options:",
                                 choices = c("All Years (Fewer Species)",
                                             "Years > 2004 (All Species)"))
                  ),
                  column(
                    3, 
                    radioButtons(inputId = "radio_2D_color",
                                 label = "Color sites by:",
                                 choices = c("Reserve Status",
                                             "Island Name"))
                  ),
                  column(
                    6,
                    conditionalPanel(
                      condition = "input.radio_2D_years == 'All Years (Fewer Species)'",
                      sliderInput(inputId = "slider2d_all", label = "Select a Year:",
                                  min = 1982, max = max(nMDS$SurveyYear), value = 1982,
                                  width = "100%", sep = "", step = 1, animate = TRUE)
                    ),
                    conditionalPanel(
                      condition = "input.radio_2D_years == 'Years > 2004 (All Species)'",
                      sliderInput(inputId = "slider2d_2005", label = "Select a Year:",
                                  min = 2005, max = max(nMDS$SurveyYear), value = 2005,
                                  width = "100%", sep = "", step = 1, animate = TRUE)
                    )
                  )
                ),
                fluidRow(
                  plotOutput(outputId = "Two_D", height = 500) 
                  # %>% 
                    # shinycssloaders::withSpinner()
                )
              )
            ),
            tags$hr()
          ),
          # ............ Tab - 3D  ----
          tabPanel(
            title = "3-Dimensional",
            fluidRow(
              column(
                4, 
                includeMarkdown("Text/nMDS/3d.md")),
              column(
                8,
                fluidRow(
                  column(
                    3, 
                    radioButtons(inputId = "radio_3D_years",
                                 label = "Data Options:",
                                 choices = c("All Years (Fewer Species)",
                                             "Years > 2004 (All Species)"))
                  ),
                  column(
                    3, 
                    radioButtons(inputId = "radio_3D_color",
                                 label = "Color sites by:",
                                 choices = c("Reserve Status",
                                             "Island Name"))
                  ),
                  column(
                    6,
                    conditionalPanel(
                      condition = "input.radio_3D_years == 'All Years (Fewer Species)'",
                      sliderInput(inputId = "slider3d_all", label = "Select a Year:",
                                  min = 1982, max = max(nMDS$SurveyYear), value = 1982,
                                  width = "100%", sep = "", step = 1, animate = TRUE)
                    ),
                    conditionalPanel(
                      condition = "input.radio_3D_years == 'Years > 2004 (All Species)'",
                      sliderInput(inputId = "slider3d_2005", label = "Select a Year:",
                                  min = 2005, max = max(nMDS$SurveyYear), value = 2005,
                                  width = "100%", sep = "", step = 1, animate = TRUE)
                    )
                  )
                ),
                fluidRow(
                  plotlyOutput(outputId = "Three_D",
                               height = 450, width = '100%') 
                  # %>% 
                    # shinycssloaders::withSpinner()
                ) 
              )
            ),
            tags$hr()
          )
        )
      ),
      # ...... Body - Important Species    ---- 
      tabItem(
        tabName = 'imp_spe',
        h1("Species with Strong Reserve or Island Effects"),
        tabsetPanel(
          # ............ Tab - About   ----
          tabPanel(
            title = "About",
            fluidRow(
              column(
                8,
                includeMarkdown(path = "Text/Variable_Importance/variable_importance.md")
              ),
              column(
                4,
                fluidRow(
                  imageOutput(outputId = "cucumba", height = 400) %>% 
                    shinycssloaders::withSpinner()
                ),
                h5("Warty sea cucumber, a highly targeted species"), 
                fluidRow(
                  imageOutput(outputId = "lobsta", height = 400) %>% 
                    shinycssloaders::withSpinner()
                ),
                h5("California spiny lobster, a highly targeted species"),
                fluidRow(
                  imageOutput(outputId = "rose", height = 400) %>% 
                    shinycssloaders::withSpinner()
                ),
                h5("White-spotted rose anemone, a cold water species typical of the Oregonian province"),
                fluidRow(
                  imageOutput(outputId = "kelkel", height = 400) %>% 
                    shinycssloaders::withSpinner()
                ),
                h5("Kellet's whelk, a fished species, though most catch is incidental to trap fisheries")
              )
            )
          ),
          # ............ Tab - Marine Reserve Model   ----
          tabPanel(
            title = "Marine Reserve Model",
            VI_UI(id = "reserve")
          ),
          # ............ Tab - Island Model   ----
          tabPanel(
            title = "Island Model",
            VI_UI(id = "island")
          ),
          # ............ Tab - Indicator Species Analysis   ----
          tabPanel(
            title = "Indicator Species Analysis",
            h2("Coming Soon..."),
            h5("Indicator species analysis (ISA) will identify species that are significantly associated with"),
            h5("specific island and marine reserve groupings. This is a more targeted approach to "),
            h5("identifying species than the random forest models. The output is more specific and will "),
            h5("tell a user which exactly which island a species is strongly associated with.")
          )
        )
      ),
      # ...... Body - Biomass    ---- 
      tabItem(
        tabName = 'biomass',
        h1("Kelp Forest Biomass Trends"),
        tabsetPanel(
          # ............ Tab - About   ----
          tabPanel(
            title = "About",
            fluidRow(
              column(
                6,
                includeMarkdown(path = "Text/Biomass_Density_Sizes/biomass.md")
              ),
              column(
                6,
                tags$hr(),
                imageOutput(outputId = "Biomass_pic_1", height = 400) %>% 
                  shinycssloaders::withSpinner(),
                h5("Photo: Kenan Chan"),
                imageOutput(outputId = "Biomass_pic_2", height = 400) %>% 
                  shinycssloaders::withSpinner(),
                h5("Photo: Kenan Chan"),
                imageOutput(outputId = "Biomass_pic_3", height = 400) %>% 
                  shinycssloaders::withSpinner(),
                h5("Photo: Shaun Wolfe")
              )
            ),
            fluidRow(
              column(
                6,
                imageOutput(outputId = "Biomass_pic_4", height = 400) %>% 
                  shinycssloaders::withSpinner(),
                h5("Photo: Shaun Wolfe")
              ),
              column(
                6,
                imageOutput(outputId = "Biomass_pic_5", height = 400) %>% 
                  shinycssloaders::withSpinner(),
                h5("Photo: Shaun Wolfe")
              )
            )
          ),
          # ............ Tab - Time Series   ----
          tabPanel(
            title = "Time Series",
            Time_UI(id = "biomass")
          ),
          # ............ Tab - Ratios   ----
          tabPanel(
            title = "Ratios",
            Ratio_UI(id = 'biomass_ratio')
          ),
          # ............ Tab - Stacked   ----
          # tabPanel(
          #   title = "Stacked"
          # ),
          # ............ Tab - Map Bubbles   ----
          tabPanel(
            title = "Map Bubbles",
            bubbles_UI(id = "biomass_bubbles")
          )
        )
      ),
      # ...... Body - Density    ---- 
      tabItem(
        tabName = 'density',
        h1("Kelp Forest Density Trends"),
        tabsetPanel(
          # ............ Tab - About   ----
          tabPanel(
            title = "About",
            fluidRow(
              column(
                6,
                includeMarkdown(path = "Text/Biomass_Density_Sizes/density.md")
              ),
              column(
                6,
                tags$hr(),
                imageOutput(outputId = "Density_pic_1", height = 400) %>% 
                  shinycssloaders::withSpinner(),
                h5("Photo: Brett Seymour"),
                imageOutput(outputId = "Density_pic_2", height = 400) %>% 
                  shinycssloaders::withSpinner(),
                h5("Photo: Brett Seymour")
              )
            ),
            fluidRow(
              column(
                6,
                imageOutput(outputId = "Density_pic_3", height = 400) %>% 
                  shinycssloaders::withSpinner(),
                h5("Photo: Brett Seymour")
              ),
              column(
                6,
                imageOutput(outputId = "Density_pic_4", height = 400) %>% 
                  shinycssloaders::withSpinner(),
                h5("Photo: Shaun Wolfe")
              )
            )
          ),
          # ............ Tab - Time Series   ----
          tabPanel(
            title = "Time Series",
            Time_UI(id = "density")
          ),
          # ............ Tab - Ratios   ----
          tabPanel(
            title = "Ratios",
            Ratio_UI(id = 'density_ratio')
          ),
          # ............ Tab - Map Bubbles   ----
          tabPanel(
            title = "Map Bubbles",
            bubbles_UI(id = "density_bubbles")
          )
        )
      ),
      # ...... Body - Size Frequencies    ---- 
      tabItem(
        tabName = 'sizes',
        h1("Kelp Forest Natural Habitat Size Frequency Distributions"),
        tabsetPanel(
          # ............ Tab - About  ----
          tabPanel(
            title = "About",
            fluidRow(
              column(
                6,
                includeMarkdown(path = "Text/Biomass_Density_Sizes/sizes.md")
              ),
              column(
                6,
                tags$hr(),
                imageOutput(outputId = "Size_pic_1", height = 400) %>% 
                  shinycssloaders::withSpinner(),
                h5("Text..."),
                h5("Photo: ..."),
                imageOutput(outputId = "Size_pic_2", height = 400) %>% 
                  shinycssloaders::withSpinner(),
                h5("Text..."),
                h5("Photo: ...")
              )
            )
          ),
          # ............ Tab - Box Plot  ----
          tabPanel(
            title = "Box Plot",
            tags$hr(),
            fluidRow(
              column(
                2,
                fluidRow(
                  column(
                    6,
                    radioButtons(inputId = "size_site_radio", label = "Choose:",
                                 choices = c("One Site", "All Sites"))
                  ),
                  column(
                    6,
                    radioButtons(inputId = "size_category", label = "Category:", 
                                 choices = c("Invertebrates", "Algae", "Fish"))
                  )
                ),
                fluidRow(
                  column(
                    12,
                    uiOutput(outputId = "size_species_UI")
                    # selectInput(inputId = "size_species",
                    #             label = "Species:", choices = c())
                  )
                ),
                fluidRow(
                  column(
                    12,
                    uiOutput(outputId = "size_site_year")  
                  )
                )
              ),
              column(
                10,
                conditionalPanel(
                  condition = "input.size_site_radio == 'One Site'",
                  plotOutput(outputId = "size_site_plot", height = 400) 
                  # %>% 
                    # shinycssloaders::withSpinner()
                ),
                conditionalPanel(
                  condition = "input.size_site_radio == 'All Sites'",
                  plotOutput(outputId = "size_year_plot", height = 400) 
                  # %>% 
                    # shinycssloaders::withSpinner()
                )
              )
            )
          ),
          # ............ Tab - Histogram  ----
          tabPanel(
            title = "Histogram",
            h3("Comming soon... animated histograms")
          ),
          # ............ Tab - ARMs  ----
          tabPanel(
            title = "ARMs",
            tags$hr(),
            fluidRow(
              column(
                2,
                fluidRow(
                  column(
                    6,
                    radioButtons(inputId = "arm_site_radio", label = "Choose:",
                                 choices = c("One Site", "All Sites"))
                  )
                ),
                fluidRow(
                  column(
                    12,
                    uiOutput(outputId = "arm_species_UI")
                  )
                ),
                fluidRow(
                  column(
                    12,
                    uiOutput(outputId = "arm_site_year")  
                  )
                )
              ),
              column(
                10,
                conditionalPanel(
                  condition = "input.arm_site_radio == 'One Site'",
                  plotOutput(outputId = "arm_site_plot", height = 400) 
                ),
                conditionalPanel(
                  condition = "input.arm_site_radio == 'All Sites'",
                  plotOutput(outputId = "arm_year_plot", height = 400) 
                )
              )
            )
          )
        )
      ),
      # ...... Body - Reports    ---- 
      tabItem(
        tabName = 'reports',
        h1("Published Reports and Documents"),
        tabsetPanel(
          # ............ Tab - Annual Reports  ----
          tabPanel(
            title = "Annual Reports",
            tags$hr(),
            fluidRow(
              column(
                2,
                radioButtons(inputId = "report_cloud", label = "View:", choices = c("Report", "Word Cloud")),
                conditionalPanel(
                  condition = "input.report_cloud == 'Report'",
                  radioButtons(inputId = "Report", label = "Year:", choices = c(2013:1990, "1982-1989"))
                ),
                conditionalPanel(
                  condition = "input.report_cloud == 'Word Cloud'",
                  radioButtons(inputId = "Cloud", label = "Year:", choices = c("All Years", 2013:1990, "1982-1989"))
                )
              ),
              column(
                10,
                conditionalPanel(
                  condition = "input.report_cloud == 'Word Cloud'",
                  fluidRow(
                    sliderInput(inputId = "cloud_n", label = "Number of Phrases:", 
                                min = 10, max = 125, value = 30, width = "50%", step = 1)
                  )
                ),
                fluidRow(
                  conditionalPanel(
                    condition = "input.report_cloud == 'Report'",
                    htmlOutput(outputId = "Annual_Report", height = 750)
                  ),
                  conditionalPanel(
                    condition = "input.report_cloud == 'Word Cloud'",
                    plotOutput(outputId = "cloud_plot", height = 750) %>% 
                      shinycssloaders::withSpinner()
                  )
                )
              )
            ) 
          ),
          # ............ Tab - Handbook   ----
          tabPanel(
            title = "Handbook",
            tags$hr(),
            fluidRow(
              column(
                2,
                radioButtons(inputId = "old_handy", label = "Select a Version:", 
                             choices = c("1997 - Revision 1 Vol. 1", "1997 - Revision 1 Vol. 2", "1988 - Original"))
              ),
              column(
                10,
                htmlOutput(outputId = "Handbook", height = 750)
              )  
            )
          ),
          # ............ Tab - Program Reviews   ----
          tabPanel(
            title = "Program Reviews",
            tags$hr(),
            fluidRow(
              column(
                2,
                radioButtons(inputId = "reviews", label = "Select a Version:", 
                             choices = c("1996 Design Review", "1994 Evaluation"))
              ),
              column(
                10,
                htmlOutput(outputId = "ReviewsOutput", height = 750)
              )  
            )
          ),
          # ............ Tab - Collaborative Reports   ----
          tabPanel(
            title = "Collaborative Reports",
            tags$hr(),
            fluidRow(
              column(
                2,
                radioButtons(inputId = "collab", label = "Select a Version:", 
                             choices = c("2018 - Wave Energy"))
              ),
              column(
                10,
                htmlOutput(outputId = "CollaborativeOutput", height = 750)
              )  
            )  
          )
        )
      ),
      # ...... Body - Literature Cited    ---- 
      tabItem(
        tabName = 'lit',
        includeMarkdown(path = "Text/About/lit.md"),
        tags$img(height = 533, width = 800, src = 'Photos/Kelp_Forest_Scenes/Kenan_Chan/1 (12).jpg')
      )
    ),
    tags$hr(),
    # .. Styles   ----
    tags$head(tags$style(HTML(
      '.skin-blue .main-sidebar {background-color: black;}',
      '.skin-blue .main-sidebar .sidebar .sidebar-menu .active a{background-color: #3c8dbc;}',
      '.tabbable>.nav>li>a{background-color: #3c8dbc;  color:white}',
      '.tabbable>.nav>li[class=active]>a{background-color: lightslategray; color:white}',
      ".main-header {position: fixed; width:100%;}",
      ".content {margin-top: 50px;}",
      ".shiny-output-error { visibility: hidden; }",
      ".shiny-output-error:before { visibility: hidden; }")))
  )
)  
# End UI   ----
  
  
  
  
  
  
  
  
  
  
  
  