# TO DO:

# Define server logic required to draw a histogram

server <- function(input, output) {
  
  library(data.table)
  library(tidyverse)
  
  
  ## Reactive stuff: We will use these later, like functions
  currentTrial <- reactiveValues(countervalue = 1) 
  
  # read in df
  currentData <- reactive({
    df <- fread(input$files$datapath, stringsAsFactors = FALSE)
    
    currentTrial$countervalue <- 1
    
    df
    })
  
  # df containing only the current trial
  currentTrialDF <- reactive({
    df <- currentData() %>%
      filter(trial_num == uniqueTrials()[currentTrial$countervalue])
    
    df
  })
  
  uniqueTrials <- reactive({
    uniqueTrials <- currentData()$trial_num %>%
      unique()
    
    uniqueTrials
  })
  
  # using currentTrialDF, make a plottable velocity df
  velocityDF <- reactive({
    df <- currentTrialDF()
    
    
    
    
    
    
    
    
    
    
    
    
    
    ## Fill this
    
    
    
    
    
  })
  
  
  ## Other backend stuff
  observeEvent(input$nextButton, {
    
    if(currentTrial$countervalue == length(uniqueTrials())){
      currentTrial$countervalue <- 1
    }
    
    else {
      currentTrial$countervalue <- currentTrial$countervalue + 1
    }
    
    print(currentTrial$countervalue)
  })
  
  observeEvent(input$prevButton, {
    
    if(currentTrial$countervalue == 1){
      currentTrial$countervalue <- length(uniqueTrials())
    }
    
    else {
      currentTrial$countervalue <- currentTrial$countervalue - 1
    }
    
    print(currentTrial$countervalue)
  })
  
  
  ## Things to display
  output$data <- renderDataTable(options = list(pageLength = 6), {
    if(!is.null(input$files)){
      currentTrialDF() %>%
        head()
      }
    })
  
  output$distPlot <- renderPlot( {
    # generate bins based on input$bins from ui.R
    if(!is.null(input$files)){
      
      # read in df
      df <- currentTrialDF()
      
      p <- df %>%
        ggplot(aes(x = mousex_px, y = mousey_px)) +
        geom_point(size = 4, colour = "#ffa46b", alpha = 0.5) +
        scale_y_continuous(limits = c(-100, 1500),
                           name = "x-position") +
        scale_x_continuous(limits = c(-800, 800),
                           name = "y-position") +
        coord_fixed() +
        annotate("text", x = -500, y = 900, size = 8,
                 label = paste("Trial: ", currentTrial$countervalue)) +
        theme_minimal() +
        theme(text = element_text(size=20))
      
      p
    }
  })
}

##---- 
## Testing
# currentTrialDF <- fread("sampleData/stepwiseExp/1/1_aligned_traning_1.txt", stringsAsFactors = FALSE) %>%
#   filter(trial_num == 1)
# df <- currentTrialDF
