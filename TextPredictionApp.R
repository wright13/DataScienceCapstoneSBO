library(shiny)
source("StupidBackoff.R")

ui <- fluidPage(
    textInput(inputId = "textIn", label = "Enter some text!"),
    verbatimTextOutput("textOrig"),
    uiOutput("button1")
)

server <- function(input, output) {
    output$textOrig <- renderText(input$textIn)
    pred <- reactive({
        pred <- predictNextWord(input$textIn)
        return(pred$word)
    })

    # output$button1 <- renderUI({
    #     if (!is.na(pred1())) actionButton("selectPred1", label = pred1())
    # })
    
    observeEvent(input$textIn, {
        if (trimws(input$textIn, "both") == "") {
            removeUI("#pred1")
            removeUI("#pred2")
            removeUI("#pred3")
        } else if (str_sub(input$textIn, -1, -1) == " ") {
            removeUI("#pred1")
            removeUI("#pred2")
            removeUI("#pred3")
            if (!is.na(pred()[3])) {
                insertUI(
                    selector = "#textIn",
                    where = "afterEnd",
                    ui = actionButton("pred1",
                                      pred()[3])
                )
            }
            if (!is.na(pred()[2])) {
                insertUI(
                    selector = "#textIn",
                    where = "afterEnd",
                    ui = actionButton("pred2",
                                      pred()[2])
                )
            }
            insertUI(
                selector = "#textIn",
                where = "afterEnd",
                ui = actionButton("pred3",
                                  pred()[1])
            )
        }
    })
}

shinyApp(ui = ui, server = server)
