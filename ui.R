#### Capstone ui.R
library(shinyIncubator)

shinyUI(fluidPage(theme = "bootstrap.css",
  #tags$style(type='text/css', "body {background-color: #dce0f4; 
             #font-family: ‘Palatino Linotype’, ‘Book Antiqua’, Palatino, serif}"),

  progressInit(),  
  tags$style(type="text/css",
             ".shiny-output-error { visibility: hidden; }",
             ".shiny-output-error:before { visibility: hidden; }"),
  
  headerPanel("SwiftKey-Coursera Capstone Project: Word Predictor"),
  
  mainPanel(
    p("Built for the Cousera Data Science Capstone Project in conjunction with SwiftKey. The goal of the application 
       is to predict the last word of a given phrase or sentence or as the actual Swiftkey functionality, next-word 
       prediction as the user types. This application provides comparison for two smoothing methods, Good-Turing and Kneser-Ney, 
       in conjunction with a Katz Back-off model." ),
  helpText(HTML("For the background information on this app,
       please visit <a href = \"http://rpubs.com/Snowcreeks/49178\">here.</a>")),   
  helpText(HTML("The source codes are available on <a href = \"https://github.com/PeggyFan?tab=repositories\">Github.</a>"))
  ),
  
  tabsetPanel(
    tabPanel('Text Input',
             fluidRow(column(9, 
                    textInput("text1", label = h3("Text input (Good-Turing)"), value = "Enter text..."),
                    actionButton("updateGT", "Enter"), 
                    tags$style(type='text/css', "#text1 { width: 800px; }")),    
             column(1, h5('Most likely'),
                    textOutput("text2"),
                    br()),
             column(1, h5('Maybe'),
                    textOutput("text3"),
                    br()), 
             column(1, h5('Last try'),
                    textOutput("text4"),
                    br())
             ),
             
    fluidRow(column(9, 
                    textInput("text5", label = h3("Text input (Kneser-Ney)"), value = "Enter text..."),
                    actionButton("updateKN", "Enter"), 
                    tags$style(type='text/css', "#text5 { width: 800px; }")),
             column(1, h5('Most likely'),
                    textOutput("text6"),
                    br()),
             column(1, h5('Maybe'),
                    textOutput("text7"),
                    br()), 
             column(1, h5('Last try'),
                    textOutput("text8"),
                    br())),
    style = "background-color: #d1d4dd;"
  ),
  
    tabPanel('Real-time Text Box',
             fluidRow(column(5, h4("Text area Good-Turing"),
              tags$textarea(id="textGT", rows="8", cols="60", "..."))),
                            fluidRow(column(1, uiOutput('b1')), 
                                 column(1, uiOutput('b2')),
                                 column(1, uiOutput('b3')),
                                 column(1, uiOutput('b4')),
                                 column(1, uiOutput('b5'))),
             
             fluidRow(column(5, h4("Text area Kneser-Ney"),
              tags$textarea(id="textKN", rows="8", cols="60", "..."))),
                            fluidRow(column(1, uiOutput('b6')), 
                                 column(1, uiOutput('b7')),
                                 column(1, uiOutput('b8')),
                                 column(1, uiOutput('b9')),
                                 column(1, uiOutput('b10'))),
             
             style = "background-color: #d1d4dd;"
             )
  
  
    ) 
          
))
