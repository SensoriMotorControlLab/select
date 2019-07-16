# TO DO:
# We should make a unique values list for trial num
# counter will determine which index on that list we access

# Define server logic required to draw a histogram

server <- function(input, output) {
  
  library(data.table)
  library(tidyverse)
  
  
  ## Reactive stuff: We will use these later, like functions
  currentTrial <- reactiveValues(countervalue = 1) 
  
  # read in df
  currentData <- reactive({
    df <- fread(input$files$datapath, stringsAsFactors = FALSE)
    
    df
    })
  
  # df containing only the current trial
  currentTrialDF <- reactive({
    df <- currentData() %>%
      filter(trial_num == currentTrial$countervalue)
    
    df
  })
  
  uniqueTrials <- reactive({
    uniqueTrials <- currentData()$trial_num %>%
      unique()
    
    print(uniqueTrials)
    
    uniqueTrials
  })
  
  # using currentTrialDF, make a plottable velocity df
  velocityDF <- reactive({
    df <- currentTrialDF()
    
    
    
    
    
    
    
    
    
    
    
    
    
    ## Fill this
    
    
    
    
    
  })
  
  
  ## Other backend stuff
  observeEvent(input$nextButton, {
    currentTrial$countervalue <- currentTrial$countervalue + 1
    
    print(currentTrial$countervalue)
  })
  
  observeEvent(input$prevButton, {
    currentTrial$countervalue <- currentTrial$countervalue - 1
    
    print(currentTrial$countervalue)
  })
  
  
  ## Things to display
  output$data <- renderDataTable(options = list(pageLength = 5), {
    if(!is.null(input$files)){
      currentTrialDF()
      }
    })
  
  output$distPlot <- renderPlot( {
    # generate bins based on input$bins from ui.R
    if(!is.null(input$files)){
      
      # read in df
      df <- currentTrialDF()
      
      p <- df %>%
        ggplot(aes(x = mousex_px, y = mousey_px)) +
        geom_point() +
        scale_y_continuous(limits = c(-50, 1000),
                           name = "x-position") +
        scale_x_continuous(limits = c(-800, 800),
                           name = "y-position") +
        theme_minimal()
        
      
      p
    }
  })
}

##---- 
## Testing
# currentTrialDF <- fread("sampleData/stepwiseExp/1/1_aligned_traning_1.txt", stringsAsFactors = FALSE) %>%
#   filter(trial_num == 1)
# df <- currentTrialDF
