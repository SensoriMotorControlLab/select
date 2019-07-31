library(shiny)
library(shinyFiles)
library(data.table)

ui <- shinyUI(pageWithSidebar(
  headerPanel(
    'Selections with shinyFiles',
    'shinyFiles example'
  ),
  sidebarPanel(
    shinyFilesButton('file', 'File select', 'Please select a file', TRUE)
  ),
  mainPanel(
    tags$h4('The output of a file selection'),
    tableOutput("contents")
  )
))

server <- shinyServer(function(input, output, session) {
  shinyFileChoose(input, 'file', roots=c(wd='.')) # can do filetypes = c('', '.csv') here
  output$contents <- renderTable({
    inFile <- parseFilePaths(roots=c(wd='.'), input$file)
    print(inFile)
    if(NROW(inFile)){
      df <- fread(as.character(inFile$datapath))
      head(df)
    }
  })
})

runApp(list(
  ui=ui,
  server=server
))