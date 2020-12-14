
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
