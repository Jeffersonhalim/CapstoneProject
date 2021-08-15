#
# ui.R
#
# Capstone App created using Shiny and R Studio 
#
# 
# Christian Halim
# 
# August 14, 2021
#

library(shiny)

shinyUI(fluidPage(
    
    titlePanel("Capstone App"),
    h5("by Christian Halim"),
    h5("Please wait a few seconds for the data to load"),
    
    mainPanel(
        textInput("textIn", 
                  label = h3("Text input: "), 
                  value = ""),
        h3("Text output: "),
        textOutput("textOut"),
    )
)
)
