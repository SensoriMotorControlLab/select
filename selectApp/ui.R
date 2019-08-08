## --------------------------------
##
## Script name: ui.R
##
## Purpose of script: user interface stuff for the Select app
##
## Author: Shanaa Modchalingam
##
## Date created: 2019-07-15
##
## Email: s.modcha@gmail.com
##
## --------------------------------

# packages used on the shinyApp
library(data.table)
library(tidyverse)
library(shinyFiles)

# hotkeys
jscode <- '$(document).keyup(function(e) {
    if (e.key == ".") {
      $("#nextButton").click();
    }
    else if (e.key == ",") {
      $("#prevButton").click();
    }
  });'

ui <- fluidPage(

  tags$head(tags$script(HTML(jscode))),
  
  # Application title
  titlePanel("Select!"),
  
  # Sidebar with a slider input for number of bins 
  # note: columns should add up to 12
  fluidRow(
    column(4,
           h3("Current File: "),
           htmlOutput("currentFileTxt", ),
           br(),
           h3("Current Trial: "),
           htmlOutput("currentTrialTxt", ),
           htmlOutput("keepStatusTxt",),
           br(),
           h3("Trials Selected: "),
           htmlOutput("trialsSelectedTxt", )
    ),
    
    column(4,
           plotOutput("reachPlot", )
    ),
    
    # Show a plot of the generated distribution
    column(4,
           plotOutput("distPlot", )
    )
  ),
  
  hr(),
  
  fluidRow(
    column(4,
           shinyFilesButton('files', 
                            'Choose File(s)', 
                            'Please select a file', multiple = TRUE, icon = icon("file")),
           actionButton("runSelectButton",
                        "Run Selection", icon = icon("play-circle")),
           br(),
           hr(),
           actionButton("prevButton", 
                        "Previous Trial", icon = icon("angle-left")),
           actionButton("nextButton", 
                        "Next Trial", icon = icon("angle-right")),
           br(),
           br(),
           actionButton("keepButton",
                         "Keep Trial", icon = icon("thumbs-up"),
                        style="color: #fff; background-color: #2c8236; border-color: #2e6da4"),
           actionButton("removeButton",
                        "Remove Trial", icon = icon("thumbs-down"),
                        style="color: #fff; background-color: #8c3331; border-color: #2e6da4"),
           br(),
           br(),
           actionButton("saveButton",
                        "Save Selected File", icon = icon("save"), 
                        style="color: #fff; background-color: #337ab7; border-color: #2e6da4"),
           br(),
           hr(),
           actionButton("prevFileButton", 
                        "Previous File", icon = icon("angle-double-left")),
           actionButton("nextFileButton", 
                        "Next File", icon = icon("angle-double-right")),
           br(),
           hr(),
           br(),
           actionButton("selectAllButton",
                        "Auto-select all files")
           ),
    
    # Show a plot of the generated distribution
    column(8,
           plotOutput("velPlot", click = clickOpts(id="velClick")),
           tableOutput("contents")
    )
  )  
)