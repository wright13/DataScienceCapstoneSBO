library(shiny)
source("StupidBackoff.R")

ui <- fluidPage(
    textInput(inputId = "textIn", label = "Enter some text!"),
    verbatimTextOutput("textOrig"),
    uiOutput("button1")
)

server <- function(input, output) {
    output$textOrig <- renderText(input$textIn)
    pred1 <- reactive({
        pred <- predictNextWord(input$textIn)
        if (pred != "<UNK>") {
            return(pred$word[1])
        } else return(pred)
    })

    
    # output$button1 <- renderUI({
    #     if (!is.na(pred1())) actionButton("selectPred1", label = pred1())
    # })
    
    observeEvent(input$textIn, {
        if (str_sub(input$textIn, -1, -1) == " ") {
            removeUI("#pred1")
            if (pred1() != "<UNK>") {
                insertUI(
                    selector = "#textIn",
                    where = "afterEnd",
                    ui = actionButton("pred1",
                                   pred1())
                )
            }
        }
        if (trimws(input$textIn, "both") == "") {
            removeUI("#pred1")
        }
    })
}

shinyApp(ui = ui, server = server)
