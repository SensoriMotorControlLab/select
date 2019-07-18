# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("Select!"),
  
  # Sidebar with a slider input for number of bins 
  # note: columns should add up to 12
  fluidRow(
    column(8,
           plotOutput("reachPlot", )
    ),
    
    # Show a plot of the generated distribution
    column(4,
           plotOutput("distPlot", )
    )
  ),
  
  fluidRow(
    column(5,
           fileInput("files", 
                     "File(s) to be selected", 
                     multiple = TRUE),
           actionButton("prevButton", 
                        "Previous Trial"),
           actionButton("nextButton", 
                        "Next Trial")
           
    ),
    
    # Show a plot of the generated distribution
    column(7,
           plotOutput("velPlot", )
    )
  )  
)