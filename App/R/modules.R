


{ # Protocol Tab Panel Module   ----
  protocol_UI <- function(id, label = "proto") {
    ns <- NS(id)
    tagList(
      tags$hr(),
      fluidRow(
        column(
          6, uiOutput(outputId = ns("text"))
        ),
        column(
          6, imageOutput(outputId = ns("proto_pic"))
        )
      )
    )
  }
  
  protocol_Server <- function(id) {
    moduleServer(
      id,
      function(input, output, session) {
        filename1 <- reactive(glue("Text/Protocols/{id}.md"))
        output$text <- renderUI(
          includeMarkdown(path = filename1())
        )
        filename2 <- reactive({glue::glue("www/Photos/Protocols/{id}.jpg")})
        output$proto_pic <- renderImage({
          list(src = filename2(),
               width = 600,
               height = 400)
        }, deleteFile = FALSE)
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








