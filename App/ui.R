
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
      # ...... Sidebar - Indicator Species  ----
      menuItem(text = 'Indicator Species', 
               icon = icon('otter'),
               badgeColor = 'green',
               tabName = 'ind_spe'),
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
               tabName = 'sizes')
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
          # ............ Tab - KFM Species Selection  ----
          tabPanel(
            "KFM Species Selection",
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
            )
          ),
          # ............ Tab - Foundation Species  ----
          tabPanel(
            "Foundation Species",
            tags$hr(),
            includeMarkdown(path = "Text/Species/foundation.md"),
            tags$hr(),
            foundation_UI(id = "kelp"),
            tags$hr(),
            foundation_UI(id = "p_urchin")
          ),
          # ............ Tab - Species Guides  ----
          tabPanel(
            "KFM Indicator Species"
          ),
          # ............ Tab - Common Diseases  ----
          tabPanel(
            "Common Diseases"
          ),
          # ............ Tab - External Resources  ----
          tabPanel(
            "External Resources"
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
              )
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
          # ............ Tab - 1 m² Quadrats  ----
          tabPanel(
            title = "1 m² Quadrats",
            protocol_UI(id = "1m")
          ),
          # ............ Tab - 5 m² Quadrats  ----
          tabPanel(
            title = "5 m² Quadrats",
            protocol_UI(id = "5m")
          ),
          # ............ Tab - Band Transects  ----
          tabPanel(
            title = "Band Transects",
            protocol_UI(id = "bands")
          ),
          # ............ Tab - RPCs  ----
          tabPanel(
            title = "RPCs",
            protocol_UI(id = "rpcs")
          ),
          # ............ Tab - NHSF  ----
          tabPanel(
            title = "NHSF",
            protocol_UI(id = "nhsf")
          ),
          # ............ Tab - ARMs  ----
          tabPanel(
            title = "ARMs",
            protocol_UI(id = "arms")
          ),
          # ............ Tab - RDFC  ----
          tabPanel(
            title = "RDFC",
            protocol_UI(id = "rdfc")
          ),
          # ............ Tab - VFT  ----
          tabPanel(
            title = "VFT",
            protocol_UI(id = "vft")
          ),
          # ............ Tab - FSF  ----
          tabPanel(
            title = "FSF",
            protocol_UI(id = "fsf")
          ),
          # ............ Tab - Video Transects  ----
          tabPanel(
            title = "Video Transects",
            protocol_UI(id = "vtt")
          ),
          # ............ Tab - Temp Loggers  ----
          tabPanel(
            title = "Temp Loggers",
            protocol_UI(id = "temp")
          ),
          # ............ Tab - Species List  ----
          tabPanel(
            title = "Species List",
            protocol_UI(id = "species")
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
          tabPanel(
            title = "3-Dimensional",
            tags$hr(),
            fluidRow(
              column(6),
              column(
                3, radioButtons(inputId = "radio_3D_years",
                                label = "Data Options:",
                                choices = c("All Years (Fewer Species)",
                                            "Years > 2004 (All Species)"))
              ),
              column(
                3, radioButtons(inputId = "radio_3D_color",
                                label = "Color sites by:",
                                choiceNames = c("Reserve Status",
                                            "Island"),
                                choiceValues = c("ReserveStatus",
                                                 "IslandName"))
              )
            ),
            fluidRow(
              column(
                6,
              ),
              column(
                6, plotlyOutput(outputId = "Three_D",
                         height = 600, width = '100%')
              )
            ),
            
          ),
          tabPanel(
            title = "2-Dimensional",
            tags$hr()
          )
        )
      ),
      # ...... Body - Indicator Species    ---- 
      tabItem(
        tabName = 'ind_spe',
        h1("Kelp Forest Indicator Species"),
        tabsetPanel(
          tabPanel(
            title = "Marine Reserve Indicator Species",
            tags$hr(),
            fluidRow(
              column(
                4
              ),
              column(
                3,
                radioButtons(inputId = "radio_ISA_years",
                             label = "Data Options:",
                             choices = c("All Years (Fewer Species)",
                                         "Years > 2004 (All Species)"))
              ),
              column(
                3, 
                radioButtons(inputId = "radio_ISA_plot_type",
                             label = "Plot Options",
                             choices = c("Variable Importance", 
                                         "Partial Dependence"))
              ),
              column(
                3,
                conditionalPanel(condition = "input.radio_ISA_plot_type == 'Partial Dependence' 
                                 & input.radio_ISA_years == 'All Years (Fewer Species)'",
                                 selectInput(inputId = "select_ISA_species_all",
                                             label = "Choose a species",
                                             choices = RF_Importance_All_Years$CommonName)),
                conditionalPanel(condition = "input.radio_ISA_plot_type == 'Partial Dependence' 
                                 & input.radio_ISA_years == 'Years > 2004 (All Species)'",
                                 selectInput(inputId = "select_ISA_species_2005",
                                             label = "Choose a species",
                                             choices = RF_Importance_2005$CommonName))
              )
            ),
            fluidRow(
              column(
                4, includeMarkdown(path = "Text/variable_importance.md")
              ),
              column(
                8, 
                conditionalPanel(condition = "input.radio_ISA_plot_type == 'Variable Importance'",
                                 plotOutput(outputId = "ISA_plot", height = 600))
                
              )
            )
          ),
          tabPanel("Island Indicator Species")
          )
      ),
      # ...... Body - Biomass    ---- 
      tabItem(
        tabName = 'biomass',
        h1("Kelp Forest Biomass Trends"),
        tags$img(height = 533, width = 800, src = 'Photos/Kelp_Forest_Scenes/Kenan_Chan/1 (5).jpg')
      ),
      # ...... Body - Density    ---- 
      tabItem(
        tabName = 'density',
        h1("Kelp Forest Density Trends"),
        tags$img(height = 533, width = 800, src = 'Photos/Kelp_Forest_Scenes/Kenan_Chan/1 (6).jpg')
      ),
      # ...... Body - Size Frequencies    ---- 
      tabItem(
        tabName = 'sizes',
        h1("Kelp Forest Natural Habitat Size Frequency Distributions"),
        tags$img(height = 533, width = 800, src = 'Photos/Kelp_Forest_Scenes/Kenan_Chan/1 (13).jpg')
      ),
      # ...... Body - Literature Cited    ---- 
      tabItem(
        tabName = 'lit',
        h1("Literature Cited"),
        tags$img(height = 533, width = 800, src = 'Photos/Kelp_Forest_Scenes/Kenan_Chan/1 (12).jpg')
      )
    ),
    # .. Styles   ----
    tags$head(tags$style(HTML(
      '.skin-green .main-sidebar {background-color: black;}',
      '.skin-green .main-sidebar .sidebar .sidebar-menu .active a{background-color: lightslategray;}',
      '.tabbable>.nav>li>a{background-color: #3c8dbc;  color:white}',
      '.tabbable>.nav>li[class=active]>a{background-color: lightslategray; color:white}',
      ".main-header {position: fixed; width:100%;}",
      ".content {margin-top: 50px;}")))
  )
)  
# End UI  ----
  
  
  
  
  
  
  
  
  
  
  
  