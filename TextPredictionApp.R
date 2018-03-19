library(shiny)
library(shinythemes)
source("StupidBackoff.R")

ui <- fluidPage(theme = shinytheme("darkly"),
    tags$head(
        tags$style(HTML("
            div.form-group.shiny-input-container{
                width: 100%;
                height: 100%;
            }
        "))
    ),
    tags$script('
      Shiny.addCustomMessageHandler("refocus",
            function(e_id) {
            document.getElementById(e_id).focus();
                                  });'),
    
    fluidRow(
        column(width = 12,
               h1("FinishMyThought")
        )
    ),
    sidebarLayout(
        sidebarPanel(
            h3("About"),
            p("If you've ever tried to write something, you know how hard it can be to put your thoughts into text. 
              Sometimes it seems impossible to figure out what your next word should be. That's where FinishMyThought comes in. 
              FinishMyThought analyzes millions of lines of text sourced from blogs, news, and Twitter to suggest your next word. With the eloquence of Twitter and the 
              blogosphere at your disposal, you'll never suffer from writer's block again."),
            h3("Instructions"),
            tags$ul(
                tags$li("Begin typing into the text box"),
                tags$li("When you pause for thought, up to three word suggestions will appear"),
                tags$li("Click on a suggestion to insert it as the next word"),
                tags$li("Need more space to type? Click and drag the bottom right corner of the text box to resize it.")
            )
        ),
        mainPanel(
            fluidRow(
                column(width = 12, h4("Enter some text!"))
            ),
            fluidRow(
                column(width = 8, textAreaInput(inputId = "textIn", label = NULL, width = '100%', resize = "vertical")),
                column(width = 4, uiOutput("predButton1", inline = TRUE), 
                       uiOutput("predButton2", inline = TRUE),
                       uiOutput("predButton3", inline = TRUE)
                )
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
        updateTextAreaInput(session, inputId = "textIn", value = paste0(trimws(input$textIn, "right"), " ", pred()[1], " "))
        session$sendCustomMessage("refocus",list("textIn"))
    })
    
    observeEvent(input$pred2, {
        updateTextAreaInput(session, inputId = "textIn", value = paste0(trimws(input$textIn, "right"), " ", pred()[2], " "))
        session$sendCustomMessage("refocus",list("textIn"))
    })
    
    observeEvent(input$pred3, {
        updateTextAreaInput(session, inputId = "textIn", value = paste0(trimws(input$textIn, "right"), " ", pred()[3], " "))
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
