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


### Add additional packages needed here
### Only works for CRAN packages (manually write library statement for GitHub packages)
packages <- c("tidyverse", "data.table", "shinyFiles")
# if a package is installed, it will be loaded
# if any are not, the missing package(s) will be installed and loaded
package.check <- lapply(packages, FUN = function(x) {
  if (!require(x, character.only = TRUE)) {
    install.packages(x, dependencies = TRUE, repos = "http://cran.us.r-project.org")
  }
  library(x, character.only = TRUE)
})

# hotkeys
jscode <- '$(document).keyup(function(e) {
    if (e.key == ".") {
      $("#nextTrialButton").click();
    }
    else if (e.key == ",") {
      $("#prevTrialButton").click();
    }
  });'

ui <- fluidPage(
  tags$head(
    tags$script(HTML(jscode)),
    tags$style(
      HTML(".shiny-notification {
             position:fixed;
             top: 3%;
             bottom: 88%;
             left: 40%;
             right: 40%;
             }
             ")
    )
  ),

  # Application title
  titlePanel("Select!"),

  # note: columns should add up to 12
  fluidRow(
    column(
      4,
      h3("Current File: "),
      htmlOutput("currentFileTxt", ),
      br(),
      htmlOutput("infoTxt", ),
      htmlOutput("keptStatusTxt", ),
      br(),
      h3("Trials Selected: "),
      htmlOutput("trialsSelectedTxt", ),
      br(),
      br(),
      shinyFilesButton("files",
        "Choose File(s)",
        "Please select a file",
        multiple = TRUE, icon = icon("file")
      ),
      actionButton("runSelectButton",
        "Start Selecting",
        icon = icon("play-circle", verify_fa = FALSE)
      )
    ),
    column(
      4,
      plotOutput("reachPlot", )
    ),

    # Show a plot of the generated distances
    column(
      4,
      div(shinyFilesButton("settingsButton",
        "Choose Settings",
        "Please select a file",
        multiple = FALSE,
        icon = icon("file")
      ), align = "right"),
      # div(checkboxInput("isCollapsedData", "TRIAL ROWS", FALSE), align = "right"),
      plotOutput("distPlot", )
    )
  ),
  hr(),
  fluidRow(
    column(
      4,
      hr(),
      actionButton("prevStepButton",
        "Previous Step",
        icon = icon("angle-left")
      ),
      actionButton("nextStepButton",
        "Next Step",
        icon = icon("angle-right")
      ),
      br(),
      actionButton("prevTrialButton",
        "Previous Trial",
        icon = icon("angle-left")
      ),
      actionButton("nextTrialButton",
        "Next Trial",
        icon = icon("angle-right")
      ),
      br(),
      br(),
      textInput("chooseTrialText", NULL,
        placeholder = "Trial/Step", width = "40%"
      ),
      actionButton("goToStepButton", "Go to Step"),
      actionButton("goToTrialButton", "Go to Trial"),
      br(),
      br(),
      br(),
      actionButton("keepButton",
        "Keep Trial",
        icon = icon("thumbs-up"),
        style = "color: #fff; background-color: #2c8236; border-color: #2e6da4"
      ),
      actionButton("flagButton",
        "Flag Trial",
        icon = icon("thumbs-down"),
        style = "color: #fff; background-color: #8c3331; border-color: #2e6da4"
      ),
      actionButton("setMaxVButton",
        "Override max velocity",
        icon = icon("exclamation-triangle", verify_fa = FALSE),
        style = "color: #fff; background-color: #d6a333; border-color: #d66c33"
      ),
      actionButton("setMoveStartButton",
        "Override move start",
        icon = icon("exclamation-triangle", verify_fa = FALSE),
        style = "color: #fff; background-color: #d6a333; border-color: #d66c33"
      ),
      actionButton("setMoveEndButton",
        "Override move end",
        icon = icon("exclamation-triangle", verify_fa = FALSE),
        style = "color: #fff; background-color: #d6a333; border-color: #d66c33"
      ),
      br(),
      br(),
      actionButton("saveButton",
        "Save Selected File",
        icon = icon("save", , verify_fa = FALSE),
        style = "color: #fff; background-color: #337ab7; border-color: #2e6da4"
      ),
      br(),
      hr(),
      actionButton("prevFileButton",
        "Previous File",
        icon = icon("angle-double-left", verify_fa = FALSE)
      ),
      actionButton("nextFileButton",
        "Next File",
        icon = icon("angle-double-right", verify_fa = FALSE)
      )
      # br(),
      # hr(),
      # br(),
      # actionButton(
      #   "selectAllButton",
      #   "Auto-select all files"
      # )
    ),

    # Show a plot of the generated velocity
    column(
      7,
      plotOutput("velPlot", click = clickOpts(id = "velClick")),
      tableOutput("contents")
    )
  )
)