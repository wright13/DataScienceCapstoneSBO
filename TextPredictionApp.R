library(shiny)
source("StupidBackoff.R")

ui <- fluidPage(
    tags$script('
      Shiny.addCustomMessageHandler("refocus",
            function(e_id) {
            document.getElementById(e_id).focus();
                                  });'),
    
    fluidRow(
        column(width = 12,
               h1("Word Predictor")
               )
    ),
    sidebarLayout(
        sidebarPanel(),
        mainPanel(
            fluidRow(
                column(width = 12, h4("Enter some text!"))
            ),
            fluidRow(
                column(width = 8, textInput(inputId = "textIn", label = NULL, width = '100%')),
                column(width = 4, uiOutput("predButton1", inline = TRUE), 
                       uiOutput("predButton2", inline = TRUE),
                       uiOutput("predButton3", inline = TRUE)
                )
            ),
            fluidRow(
                actionButton("test", label = "test")
            )
        )
    )
    
)

server <- function(input, output, session) {

    pred <- reactive({
        pred <- predictNextWord(input$textIn)
        return(pred$word)
    })
    
    observeEvent(input$pred1, {
        updateTextInput(session, inputId = "textIn", value = paste0(trimws(input$textIn, "right"), " ", pred()[1], " "))
        session$sendCustomMessage("refocus",list("textIn"))
    })
    
    observeEvent(input$pred2, {
        updateTextInput(session, inputId = "textIn", value = paste0(trimws(input$textIn, "right"), " ", pred()[2], " "))
        session$sendCustomMessage("refocus",list("textIn"))
    })
    
    observeEvent(input$pred3, {
        updateTextInput(session, inputId = "textIn", value = paste0(trimws(input$textIn, "right"), " ", pred()[3], " "))
        session$sendCustomMessage("refocus",list("textIn"))
    })

    output$predButton1 <- renderUI({
        if (trimws(input$textIn, "both") != "") {
            actionButton("pred1", pred()[1])
        }
    })
    
    output$predButton2 <- renderUI({
        if (trimws(input$textIn, "both") != "") {
            if (!is.na(pred()[2])) actionButton("pred2", pred()[2])
        }
    })
    
    output$predButton3 <- renderUI({
        if (trimws(input$textIn, "both") != "") {
            if (!is.na(pred()[3])) actionButton("pred3", pred()[3])
        }
    })

}

shinyApp(ui = ui, server = server)
