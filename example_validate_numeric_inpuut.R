library(shiny)
ui <- fluidPage(
  numericInput("num", label = "text not allowed", value = 1),
  verbatimTextOutput("value")
)
server <- function(input, output) {
  
  numbers <- reactive({
    validate(
      need(is.numeric(input$num), "Please input a number")
    )
  })
  output$value <- renderPrint({ numbers() })      
}
shinyApp(ui = ui, server = server)