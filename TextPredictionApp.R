library(shiny)
source("StupidBackoff.R")

ui <- fluidPage(
    textInput(inputId = "input.text", label = "Enter some text!"),
    verbatimTextOutput("text.orig"),
    tableOutput("predictions")
)

server <- function(input, output) {
    output$text.orig <- renderText(input$input.text)
    predictions <- reactive({
        pred <- predictNextWord(input$input.text)
        pred$word
    })
    output$predictions <- renderTable({predictions()})
}

shinyApp(ui = ui, server = server)
