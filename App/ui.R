



# Define the User Interface for application 

ui <- dashboardPage(
  title = 'KFM App',  
  skin = 'green',
  dashboardHeader( 
    title =tags$strong(
      'CHIS-KFM', 
      tags$img(height = 50, width = 40, src = 'Graphics/Arrowhead.png')),
    titleWidth = 200
    ),
  dashboardSidebar(
    width = 200,
    sidebarMenu(
      id = 'sidebar',
      style = 'position: fixed; overflow: visible;',
      menuItem(text = 'About', 
               icon = icon('globe'),
               badgeColor = 'green',
               tabName = 'about'),
      menuItem(text = 'Species', 
               icon = icon('fish'),
               badgeColor = 'green',
               tabName = 'species'),
      menuItem(text = 'Protocols', 
               icon = icon('grip-horizontal'),
               badgeColor = 'green',
               tabName = 'protocols'),
      menuItem(text = 'Maps', 
               icon = icon('globe'),
               badgeColor = 'green',
               tabName = 'maps'),
      menuItem(text = 'Biodiversity',
               icon = icon('rainbow'),
               badgeColor = 'green',
               tabName = 'diversity'),
      menuItem(text = 'Community Similarity', 
               icon = icon('city'),
               badgeColor = 'green',
               tabName = 'com_sim'),
      menuItem(text = 'Indicator Species', 
               icon = icon('otter'),
               badgeColor = 'green',
               tabName = 'ind_spe'),
      menuItem(text = 'Biomass', 
               icon = icon('hippo'),
               badgeColor = 'green',
               tabName = 'biomass'),
      menuItem(text = 'Density', 
               icon = icon('gem'),
               badgeColor = 'green',
               tabName = 'density'),
      menuItem(text = 'Size Frequency', 
               icon = icon('ruler'),
               badgeColor = 'green',
               tabName = 'sizes')
    ),
    tags$head(tags$style(HTML(
      '.skin-green .main-sidebar {background-color: black;} 
      .skin-green .main-sidebar .sidebar .sidebar-menu .active a{background-color: lightslategray;}
      .tabbable > .nav > li > a {background-color: black;  color:white}
      .tabbable > .nav > li[class=active]    > a {background-color: lightslategray; color:white}'
        ))),
  tags$head(tags$style(
    type = 'text/css', 
    ".main-header {position: fixed; width:100%;}",
    ".content {margin-top: 50px;}"))
  ),
  dashboardBody( 
    tabItems(
      tabItem(
        tabName = 'about',
        h1("Channel Islands National Park's Kelp Forest Monitoring Program"),
        tabsetPanel(
          tabPanel("Disclaimer",
                   includeMarkdown(path = "Text/about.md"),
                   tags$img(height = 772.72, width = 1000, src = 'Maps/Island/KFM_Sites.jpg')
          ),
          tabPanel("KFMP History",
                   includeMarkdown(path = "Text/history.md")
          ),
          tabPanel("FAQ",
                   includeMarkdown(path = "Text/FAQ.md")
                   )
        )
      ),
      tabItem(
        tabName = 'species',
        h1("Kelp Forest Species"),
        tabsetPanel(
          tabPanel("Foundation Species",
                   tags$hr(),
                   fluidRow(
                     column(
                       4, tags$img(
                         width = 400,
                         src = 'Photos/Indicator_Species/2002.jpg')
                     ),
                     column(
                       4, 
                       includeMarkdown(path = "Text/kelp.md")
                     ),
                     column(
                       4, 
                       includeMarkdown(path = "Text/kelp.md")
                     )
                   ),
                   tags$hr(),
                   fluidRow(
                     column(
                       4, tags$img(
                         width = 400,
                         src = 'Photos/Indicator_Species/11006.jpg')
                     ),
                     column(
                       4, 
                       includeMarkdown(path = "Text/urchin.md")
                     ),
                     column(
                       4, 
                       includeMarkdown(path = "Text/urchin.md")
                     )
                   )
          ),
          tabPanel("KFM Species List"
          ),
          tabPanel("Protocol Species Guides"
          ),
          tabPanel("Common Diseases"
          ),
          tabPanel("External Resources"
          )
        )
      ),
      tabItem(
        tabName = 'protocols',
        h1("Kelp Forest Monitoring Protocols"),
        tabsetPanel(
          tabPanel("1 m² Quadrats"
          ),
          tabPanel("5 m² Quadrats"
          ),
          tabPanel("Band Transects"
          ),
          tabPanel("RPCs"
          ),
          tabPanel("NHSF"
          ),
          tabPanel("ARMs"
          ),
          tabPanel("RDFC"
          ),
          tabPanel("VFT"
          ),
          tabPanel("FSF"
          ),
          tabPanel("Video Transects"
          ),
          tabPanel("Temp Loggers"
          ),
          tabPanel("Species List"
          )
        )
      ),
      tabItem(
        tabName = 'maps',
        h1("Kelp Forest Monitoring Sampling Locations"),
        tags$img(height = 533, width = 800, src = 'Photos/Kelp_Forest_Scenes/Laurie_Montgomery/1 (3).jpg')
      ),
      tabItem(
        tabName = 'diversity',
        h1("Kelp Forest Community Biodiversity"),
        tags$img(height = 533, width = 800, src = 'Photos/Kelp_Forest_Scenes/Kenan_Chan/1 (2).jpg')
      ),
      tabItem(
        tabName = 'com_sim',
        h1("Kelp Forest Community Similarity"),
        tags$img(height = 533, width = 800, src = 'Photos/Kelp_Forest_Scenes/Kenan_Chan/1 (3).jpg')
      ),
      tabItem(
        tabName = 'ind_spe',
        h1("Kelp Forest Indicator Species"),
        tabsetPanel(
          tabPanel("Marine Reserve Indicator Species"),
          tabPanel("Island Indicator Species")
          ),
        tags$img(height = 533, width = 800, src = 'Photos/Kelp_Forest_Scenes/Kenan_Chan/1 (4).jpg')
      ),
      tabItem(
        tabName = 'biomass',
        h1("Kelp Forest Biomass Trends"),
        tags$img(height = 533, width = 800, src = 'Photos/Kelp_Forest_Scenes/Kenan_Chan/1 (5).jpg')
      ),
      tabItem(
        tabName = 'density',
        h1("Kelp Forest Density Trends"),
        tags$img(height = 533, width = 800, src = 'Photos/Kelp_Forest_Scenes/Kenan_Chan/1 (6).jpg')
      ),
      tabItem(
        tabName = 'sizes',
        h1("Kelp Forest Natural Habitat Size Frequency Distributions"),
        tags$img(height = 533, width = 800, src = 'Photos/Kelp_Forest_Scenes/Kenan_Chan/1 (13).jpg')
      )
    )
  )
)  
  
  
  
  
  
  
  
  
  
  
  
  