# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("Select!"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      fileInput("files", 
                "File(s) to be selected", 
                multiple = TRUE),
      actionButton("prevButton", 
                   "Previous Trial"),
      actionButton("nextButton", 
                   "Next Trial")
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("distPlot", ),
      dataTableOutput("data")
    )
  )
)