# TO DO:

# Define server logic required to draw a histogram



server <- function(input, output) {
  
  library(data.table)
  library(tidyverse)
  source("analysisFunctions.R")
  
  
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
  
  fitDF <- reactive({
    
    df <- currentTrialDF()
    
    fitDF <- df %>%
      select(time_s, mousex_px, mousey_px)
    
    # add a distance row
    fitDF$distance <- df %>% 
      transmute(mousex_px = mousex_px - homex_px, mousey_px + homey_px) %>%
      apply(1, vector_norm)
    
    # fit a spline to the distance data
    fit_fun <- smooth.spline(x = fitDF$time_s, y = fitDF$distance, df = 6)
    
    # add a spline column
    fitDF$spline <- predict(fit_fun, fitDF$time_s)$y
    
    # add a speed column
    fitDF$speed <- predict(fit_fun, fitDF$time_s, deriv = 1)$y
    
    fitDF
    
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
  # output$data <- renderDataTable(options = list(pageLength = 6), {
  #   if(!is.null(input$files)){
  #     currentTrialDF() %>%
  #       head()
  #     }
  #   })
  
  output$reachPlot <- renderPlot( {
    # generate bins based on input$bins from ui.R
    if(!is.null(input$files)){
      
      # read in df
      df <- fitDF()
      
      p <- df %>%
        ggplot(aes(x = mousex_px, y = mousey_px)) +
        geom_point(size = 4, colour = "#ffa46b", alpha = 0.5) +
        geom_point(data = filter(df, speed == max(speed))[1, ], 
                   size = 6, colour = "red", shape = 10, 
                   stroke = 2, alpha = .5) +
        scale_y_continuous(limits = c(-100, 1000),
                           name = "y-position") +
        scale_x_continuous(limits = c(-800, 800),
                           name = "x-position") +
        coord_fixed() +
        annotate("text", x = -500, y = 900, size = 8,
                 label = paste("Trial: ", currentTrial$countervalue)) +
        theme_minimal() +
        theme(text = element_text(size=20))
      
      p
    }
  })
    
  output$distPlot <- renderPlot( {
    # generate bins based on input$bins from ui.R
    if(!is.null(input$files)){
      
      # read in df
      df <- fitDF()
      
      p <- df %>%
        ggplot(aes(x = time_s, y = distance)) +
        geom_point(size = 4, colour = "#ffa46b", alpha = 0.5) +
        geom_line(aes(y = spline), alpha = 0.5, size = 2) + 
        geom_point(data = filter(df, speed == max(speed))[1, ], 
                   size = 8, colour = "red", shape = 10, 
                   stroke = 2, alpha = .5) +
        scale_y_continuous(name = "distance from home") +
        scale_x_continuous(name = "time") +
        theme_minimal() +
        theme(text = element_text(size=20))
      
      p
    }
  })
  
  output$velPlot <- renderPlot( {
    # generate bins based on input$bins from ui.R
    if(!is.null(input$files)){
      
      # read in df
      df <- fitDF()
      
      p <- df %>%
        ggplot(aes(x = time_s, y = speed)) +
        geom_line(size = 3, alpha = .5) +
        geom_point(data = filter(df, speed == max(speed))[1, ], 
                   size = 8, colour = "red", shape = 10, 
                   stroke = 2, alpha = .5) +
        scale_y_continuous(name = "speed") +
        scale_x_continuous(name = "time") +
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
