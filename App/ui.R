
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
      menuItem(text = 'About', 
               icon = icon('globe'),
               badgeColor = 'green',
               tabName = 'about'),
      # ...... Sidebar - Species  ----
      menuItem(text = 'Species', 
               icon = icon('fish'),
               badgeColor = 'green',
               tabName = 'species'),
      # ...... Sidebar - Protocols  ----
      menuItem(text = 'Protocols', 
               icon = icon('grip-horizontal'),
               badgeColor = 'green',
               tabName = 'protocols'),
      # ...... Sidebar - Maps  ----
      menuItem(text = 'Sampling Locations', 
               icon = icon('globe'),
               badgeColor = 'green',
               tabName = 'maps'),
      # ...... Sidebar - Biodiversity  ----
      menuItem(text = 'Biodiversity',
               icon = icon('rainbow'),
               badgeColor = 'green',
               tabName = 'diversity'),
      # ...... Sidebar - Community Similarity  ----
      menuItem(text = 'Community Similarity', 
               icon = icon('balance-scale'),
               badgeColor = 'green',
               tabName = 'com_sim'),
      # ...... Sidebar - Important Species  ----
      menuItem(text = 'Important Species', 
               icon = icon('otter'),
               badgeColor = 'green',
               tabName = 'imp_spe'),
      # ...... Sidebar - Biomass  ----
      menuItem(text = 'Biomass', 
               icon = icon('hippo'),
               badgeColor = 'green',
               tabName = 'biomass'),
      # ...... Sidebar - Density  ----
      menuItem(text = 'Density', 
               icon = icon('gem'),
               badgeColor = 'green',
               tabName = 'density'),
      # ...... Sidebar - Size Frequencies  ----
      menuItem(text = 'Size Frequency', 
               icon = icon('ruler'),
               badgeColor = 'green',
               tabName = 'sizes'),
      # ...... Sidebar - Reports  ----
      menuItem(text = 'Reports', 
               icon = icon('newspaper'),
               badgeColor = 'green',
               tabName = 'reports'),
      # ...... Sidebar - Literature Cited  ----
      menuItem(text = 'Literature Cited', 
               icon = icon('book-reader'),
               badgeColor = 'green',
               tabName = 'lit')
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
          tabPanel("Disclaimer",
                   fluidRow(
                     column(
                       8, includeMarkdown(path = "Text/about.md")
                     ),
                     column(
                       4, tags$img(height = 332, width = 500, 
                                   src = 'Photos/Kelp_Forest_Scenes/Laurie_Montgomery/1 (2).jpg'),
                       h5("Diver in a kelp forest on Santa Barbara Island"),
                       h5("Photographed by Laurie Montgomery (NPS KFMP Diver)")
                     )
                   )
          ),
          # ............ Tab - KFMP History  ----
          tabPanel("KFMP History",
                   fluidRow(
                     column(
                       8, includeMarkdown(path = "Text/history.md")
                     ),
                     column(
                       4, tags$img(height = 332, width = 500, 
                                   src = 'Photos/Kelp_Forest_Scenes/Brett_Seymour/1 (3).jpg'),
                       h5("Antler kelp (Pelagophycus porra) on Santa Cruz Island"),
                       h5("Photographed by Brett Seymour (NPS Submerged Resources Center)")
                     )
                   )
          ),
          # ............ Tab - Acknowledgments  ----
          tabPanel("Acknowledgments",
                   fluidRow(
                     column(
                       8, includeMarkdown(path = "Text/acknowledgments.md")
                     ),
                     column(
                       4, tags$img(height = 332, width = 500, 
                                   src = 'Photos/Kelp_Forest_Scenes/Laurie_Montgomery/1 (3).jpg'),
                       h5("California sea lion swimming through the kelp forest on Santa Barbara Island"),
                       h5("Photographed by Laurie Montgomery (NPS KFMP Diver)")
                     )
                   )
          ),
          # ............ Tab - Introduction  ----
          tabPanel("Introduction",
                   fluidRow(
                     column(
                       8, includeMarkdown(path = "Text/introduction.md")
                     ),
                     column(
                       4, tags$img(height = 332, width = 500, 
                                   src = 'Photos/Kelp_Forest_Scenes/Brett_Seymour/1 (4).jpg'),
                       h5("California sea lion swimming through the kelp forest on Santa Barbara Island"),
                       h5("Photographed by Brett Seymour (NPS Submerged Resources Center)")
                     )
                   )
          ),
          # ............ Tab - Acronyms  ----
          tabPanel("Acronyms",
                   fluidRow(
                     column(
                       8, includeMarkdown(path = "Text/acronyms.md")
                     ),
                     column(
                       4, tags$img(height = 332, width = 500, 
                                   src = 'Photos/Kelp_Forest_Scenes/Brett_Seymour/1 (6).jpg'),
                       h5("California sea lion playing with southern sea palm (Eisenia arborea) on Santa Barbara Island"),
                       h5("Photographed by Brett Seymour (NPS Submerged Resources Center)")
                     )
                   )
          ),
          # ............ Tab - Blogs  ----
          tabPanel("Blog Posts",
                   tags$hr(), 
                   fluidRow(
                     column(
                       8, NPS_Blog_tagList
                     ),
                     column(
                       4, tags$img(height = 332, width = 500, 
                                   src = 'Photos/Kelp_Forest_Scenes/Brett_Seymour/1 (6).jpg'),
                       h5("California sea lion playing with southern sea palm (Eisenia arborea) on Santa Barbara Island"),
                       h5("Photographed by Brett Seymour (NPS Submerged Resources Center)")
                     )
                   )
          ),
          # ............ Tab - FAQ  ----
          tabPanel("FAQ",
                   includeMarkdown(path = "Text/FAQ.md")
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
            tags$hr(),
            includeMarkdown(path = "Text/Species/foundation.md"),
            tags$hr(),
            foundation_UI(id = "kelp"),
            foundation_UI(id = "p_urchin"),
            foundation_UI(id = "r_urchin"),
            foundation_UI(id = "r_abalone"),
            foundation_UI(id = "lobsta"),
            foundation_UI(id = "sheep"),
            foundation_UI(id = "sunflower"),
            foundation_UI(id = "giant")
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
          # ............ Tab - KFM Species Selection  ----
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
                  tags$img(height = 400, width = 600, src = 'Photos/Kelp_Forest_Scenes/Brett_Seymour/1 (7).jpg'), 
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
            radioButtons(inputId = "disease",
                         label = "Choose a Taxa:", 
                         inline = TRUE,
                         choices = c("Sea Stars",
                                     "Sea Urchin",
                                     "Abalone")),
            conditionalPanel(condition = "input.disease == 'Sea Stars'",
                             uiOutput(outputId = "SSWD")),
            conditionalPanel(condition = "input.disease == 'Sea Urchin'",
                             uiOutput(outputId = "urchins")),
            conditionalPanel(condition = "input.disease == 'Abalone'",
                             fluidRow(
                               column(
                                 8,
                                 includeMarkdown("Text/Species/abalone_disease.md")
                               ),
                               column(
                                 4,
                                 imageOutput(outputId = "abalone"),
                                 h4("Farmed red abalone (Haliotis rufescens). The animal on the right shows classic signs of WS."),
                                 h5("Photo Credit: CDFW")
                               )
                             )
            )
          ),
          # ............ Tab - External Resources  ----
          tabPanel(
            title = "External Resources",
            tags$hr(),
            h2("Coming soon... "),
            h4("PISCO - Partnership for Interdisciplinary Studies of Coastal Oceans"),
            h4("SBC LTER - Santa Barbara Coastal Term Ecological Research"),
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
                tags$img(height = 400, width = 600, src = 'Photos/Protocols/1m (2).jpg'),
                tags$br(), tags$hr(),
                tags$img(height = 400, width = 600, src = 'Photos/Protocols/5m (1).jpg')
              ),
            ),
            tags$hr(),
            fluidRow(
              column(
                6,
                tags$img(height = 400, width = 600, src = 'Photos/Protocols/bands (1).jpg'), 
              ),
              column(
                6, 
                tags$img(height = 400, width = 600, src = 'Photos/Protocols/rpcs (1).jpg')
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
      # ...... Body - Maps    ---- 
      tabItem(
        tabName = 'maps',
        h1("Kelp Forest Monitoring Sampling Locations"),
        tabsetPanel(
          # ............ Tab - Site Selection and History  ----
          tabPanel(
            title = "Site History",
            tags$hr(),
            includeMarkdown(path = "Text/site_history.md")
          ),
          # ............ Tab - Leaflet  ----
          tabPanel(
            title = "Leaflet Maps",
            tags$hr(),
            leafletOutput(outputId = "Leaflet",
                          height = 500, width = '50%')
          ),
          # ............ Tab - Satellite Imagery  ----
          tabPanel(
            title = "Satellite Imagery",
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
                  Site_Selector_UI(id = "Site_Sat"),
                )
              )
            ),
            imageOutput(outputId = "satMap",
                        height = 1000)
          ),
          # ............ Tab - Bathymetric Imagery  ----
          tabPanel(
            title = "Bathymetric Imagery",
            tags$hr(),
            selectInput(inputId = "Bath_Maps_Site",
                        label = "Choose a Site:",
                        choices = dplyr::filter(Site_Info, Bath == TRUE)$SiteName),
            imageOutput(outputId = "Bathymetry_Map",
                        height = 800)
          ),
          # ............ Tab - ARM Locations  ----
          tabPanel(
            title = "ARM Location Graphics",
            tags$hr(),
            selectInput(inputId = "Arm_Maps_Site",
                        label = "Choose a Site:",
                        choices = dplyr::filter(Site_Info, ARMs == TRUE)$SiteName),
            imageOutput(outputId = "ARM_Map",
                        height = 800)
          ),
          # ............ Tab - Site Descriptions  ----
          tabPanel(
            title = "Site Descriptions",
            tags$hr(),
            Site_Selector_UI(id = "Site_Descriptions"),
            htmlOutput(outputId = 'SitePDF')
          )
        )
      ),
      # ...... Body - Biodiversity    ---- 
      tabItem(
        tabName = 'diversity',
        h1("Kelp Forest Community Biodiversity"),
        tabsetPanel(
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
            tags$hr(),
            fluidRow(
              column(
                6, tags$img(height = 332, width = 500, src = 'Photos/Kelp_Forest_Scenes/Kenan_Chan/1 (3).jpg')
              ),
              column(
                6, tags$img(height = 332, width = 500, src = 'Photos/Kelp_Forest_Scenes/Kenan_Chan/1 (15).jpg')
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
                      sliderInput(inputId = "slider2d_all",
                                  label = "Select a Year:",
                                  min = min(nMDS_2D_All$SurveyYear),
                                  max = max(nMDS_2D_All$SurveyYear),
                                  value = min(nMDS_2D_All$SurveyYear),
                                  width = "100%",
                                  sep = "", step = 1, animate = TRUE)
                    ),
                    conditionalPanel(
                      condition = "input.radio_2D_years == 'Years > 2004 (All Species)'",
                      sliderInput(inputId = "slider2d_2005",
                                  label = "Select a Year:",
                                  min = min(nMDS_2D_2005$SurveyYear),
                                  max = max(nMDS_2D_2005$SurveyYear),
                                  value = min(nMDS_2D_2005$SurveyYear),
                                  width = "100%",
                                  sep = "", step = 1, animate = TRUE)
                    )
                  )
                ),
                fluidRow(
                  plotOutput(outputId = "Two_D", height = 500)
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
                      sliderInput(inputId = "slider3d_all",
                                  label = "Select a Year:",
                                  min = min(nMDS_3D_All$SurveyYear),
                                  max = max(nMDS_3D_All$SurveyYear),
                                  value = min(nMDS_3D_All$SurveyYear),
                                  width = "100%",
                                  sep = "", step = 1, animate = TRUE)
                    ),
                    conditionalPanel(
                      condition = "input.radio_3D_years == 'Years > 2004 (All Species)'",
                      sliderInput(inputId = "slider3d_2005",
                                  label = "Select a Year:",
                                  min = min(nMDS_3D_2005$SurveyYear),
                                  max = max(nMDS_3D_2005$SurveyYear),
                                  value = min(nMDS_3D_2005$SurveyYear),
                                  width = "100%",
                                  sep = "", step = 1, animate = TRUE)
                    )
                  )
                ),
                fluidRow(
                  plotlyOutput(outputId = "Three_D",
                               height = 450, width = '100%')
                ) 
              )
            ),
            tags$hr()
          )
        )
      ),
      # ...... Body - Indicator Species    ---- 
      tabItem(
        tabName = 'imp_spe',
        h1("Species with Strong Reserve or Island Effects"),
        tabsetPanel(
          # ............ Tab - About   ----
          tabPanel(
            title = "About",
            fluidRow(
              column(
                6,
                includeMarkdown(path = "Text/Variable_Importance/variable_importance.md")
              ),
              column(
                6
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
          # ............ Tab - Time Series   ----
          tabPanel(
            title = "Time Series",
            Time_UI(id = "biomass")
          ),
          # ............ Tab - Ratios   ----
          tabPanel(
            title = "Ratios"
          ),
          # ............ Tab - Map Bubbles   ----
          tabPanel(
            title = "Map Bubbles"
          )
        )
      ),
      # ...... Body - Density    ---- 
      tabItem(
        tabName = 'density',
        h1("Kelp Forest Density Trends"),
        tabsetPanel(
          # ............ Tab - Time Series   ----
          tabPanel(
            title = "Time Series",
            Time_UI(id = "density")
          ),
          # ............ Tab - Ratios   ----
          tabPanel(
            title = "Ratios"
          ),
          # ............ Tab - Map Bubbles   ----
          tabPanel(
            title = "Map Bubbles"
          )
        )
      ),
      # ...... Body - Size Frequencies    ---- 
      tabItem(
        tabName = 'sizes',
        h1("Kelp Forest Natural Habitat Size Frequency Distributions"),
        tags$img(height = 533, width = 800, src = 'Photos/Kelp_Forest_Scenes/Kenan_Chan/1 (13).jpg')
      ),
      # ...... Body - Reports    ---- 
      tabItem(
        tabName = 'reports',
        h1("Published Reports and Documents"),
        tabsetPanel(
          # ............ Tab - Annual Reports  ----
          tabPanel(
          title = "Annual Reports"  
          ),
          # ............ Tab - Handbook   ----
          tabPanel(
            title = "Handbook"  
          ),
          # ............ Tab - Program Reviews   ----
          tabPanel(
            title = "Program Reviews"  
          ),
          # ............ Tab - Collaborative Reports   ----
          tabPanel(
            title = "Collaborative Reports"  
          )
        ),
        tags$img(height = 533, width = 800, src = 'Photos/Kelp_Forest_Scenes/Kenan_Chan/1 (6).jpg')
      ),
      # ...... Body - Literature Cited    ---- 
      tabItem(
        tabName = 'lit',
        h1("Literature Cited"),
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
  
  
  
  
  
  
  
  
  
  
  
  