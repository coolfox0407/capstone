
# Shiny UI logic

suppressWarnings(library(shiny))
suppressWarnings(library(markdown))

set.seed(12345)

shinyUI(navbarPage("Coursera's Data Science (Capstone) - NextGEN NLP word predictor",
                   tabPanel("Predict the Next Word",
                            HTML("<strong>Author: Hariharan D</strong>"),
                            br(),
                            HTML("<strong>Date: 02-Nov-2017</strong>"),
                            br(),
                            HTML('<center><img src = "header.png"></center>'),
                            # Sidebar
                            sidebarLayout(
                              sidebarPanel(
                                helpText("Enter the text to predict the next word!"),
                                textInput("inputString", "Enter the text here",value = ""),
                                br(),
                                br(),
                                br(),
                                br()
                              ),
                              mainPanel(
                                h2("Predicted Word"),
                                verbatimTextOutput("prediction"),
                                strong("Sentence Input:"),
                                tags$style(type='text/css', '#text1 {background-color: rgba(255,255,0,0.40); color: blue;}'), 
                                textOutput('text1'),
                                br(),
                                strong("Note:"),
                                tags$style(type='text/css', '#text2 {background-color: rgba(255,255,0,0.40); color: black;}'),
                                textOutput('text2')
                              )
                            )
                            
                   ),
                   tabPanel("About",
                            mainPanel(
                              HTML('<center><img src = "header.png"></center>'),
                              includeMarkdown("ReadMe.html")
                            )
                   )
              )
        )