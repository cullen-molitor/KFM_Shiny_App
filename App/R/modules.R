


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








