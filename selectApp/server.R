## --------------------------------
##
## Script name: server.R
##
## Purpose of script: server stuff for the Select app
##
## Author: Shanaa Modchalingam
##
## Date created: 2019-07-15
##
## Email: s.modcha@gmail.com
##
## --------------------------------

server <- function(input, output) {
  

  source("analysisFunctions.R")
  
  ## Reactive stuff: We will use these later, like functions
  currentTrial <- reactiveValues(counterValue = 1, fitDF = NULL, chooseMaxV = FALSE) 
  currentFile <- reactiveValues(filePath = NULL,  fileNum = 1,
                                df = NULL, dataList = list(), 
                                min_x = NULL, max_x = NULL, 
                                min_y = NULL, max_y = NULL)
  allFiles <- reactiveValues(inFile = NULL)
  globalValues <- reactiveValues(lazyMode = FALSE)
  volumes <- c(Home = fs::path_home(), WD = '.', getVolumes()())
  
  clickOpts(id = "velClick", clip = TRUE)
  
  # read in df
  loadFilePaths <- reactive({
    allFiles$inFile <- parseFilePaths(roots=c(volumes), input$files)
  })
  
  storeCurrentData <- reactive({
    
    currentFile$filePath <- as.character(allFiles$inFile$datapath[currentFile$fileNum])
    
    df <- fread(currentFile$filePath, stringsAsFactors = FALSE)
    
    # get maximum x and y (for plotting)
    currentFile$min_x <- min(df$mousex_px)
    currentFile$max_x <- max(df$mousex_px)
    currentFile$min_y <- min(df$mousey_px)
    currentFile$max_y <- max(df$mousey_px)
    
    # print(currentFile$filePath)
    
    # reset the counterValue
    currentTrial$counterValue <- 1
    
    # reset the fitDF
    currentTrial$fitDF <- NULL
    
    # reset the maxV toggle
    currentTrial$chooseMaxV <- FALSE
    
    #reset dataList
    currentFile$dataList <- list()
    
    currentFile$df <- df
    
    })
  
  # df containing only the current trial
  currentTrialDF <- reactive({
    df <- currentFile$df %>%
      filter(trial_num == uniqueTrials()[currentTrial$counterValue])
    
    df
  })
  
  # returns a vector of the unique trial_num in current file
  uniqueTrials <- reactive({
    uniqueTrials <- currentFile$df$trial_num %>%
      unique()
    
    uniqueTrials
  })
  
  # a tibble with time, mousex, mousey, spline, speed, seleted, maxV column
  fitDF <- reactive({
    
    fitDF <- currentTrialDF() %>%
      select(time_s, mousex_px, mousey_px)
    
    # add a distance row
    fitDF$distance <- currentTrialDF() %>% 
      transmute(mousex_px = mousex_px - homex_px, mousey_px + homey_px) %>%
      apply(1, vector_norm)
    
    # fit a spline to the distance data
    fit_fun <- smooth.spline(x = fitDF$time_s, y = fitDF$distance, df = 7)
    
    # add a spline column
    fitDF$spline <- predict(fit_fun, fitDF$time_s)$y
    
    # add a speed column
    fitDF$speed <- predict(fit_fun, fitDF$time_s, deriv = 1)$y
    
    
    currentTrial$fitDF <- fitDF
  })
  
  addSelectedCols <- reactive({
    df <- currentTrial$fitDF
    
    worked <- FALSE
    
    
    # make selected and maxV columns if they don't already exist in currentFile$dataList
    # if the do exist, just pull them from there
    try({
    
      if (is.null(currentFile$dataList[[currentTrial$counterValue]])){
        df$selected <- 1
        df$maxV <- 0
        
        df$maxV[df$time_s == filter(df, speed == max(speed))[1, ]$time_s] <- 1
      }
      else {
        df$selected <- currentFile$dataList[[currentTrial$counterValue]]$selected
        df$maxV <- currentFile$dataList[[currentTrial$counterValue]]$maxV
      }
      
      worked <- TRUE
    }, silent = TRUE)
    
    # if that dataList[[counterValue]] doesn't exist at all, the above code will not work
    if(!worked){
      df$selected <- 1
      df$maxV <- 0
      
      df$maxV[df$time_s == filter(df, speed == max(speed))[1, ]$time_s] <- 1
    }
    
    currentTrial$fitDF <- df
    
  })
  
  # save button stuff 
  mergeAndSave <- reactive({
    pathToSave <- currentFile$filePath %>%
      str_sub(1, -5)
    
    pathToSave <- paste(pathToSave, "selected.csv", sep = "_")
    
    # concatenate the selected columns
    selected_df <- do.call(rbind, currentFile$dataList)
    
    # add the selected_df columns to df
    selected_df <- cbind2(currentFile$df, selected_df)
    
    # print(pathToSave)
    
    fwrite(selected_df, file = pathToSave)
  })
  
  
  ## ----
  ## Other backend stuff 
  
  # loading in a file
  shinyFileChoose(input, 'files', roots = volumes) # can do filetypes = c('', '.csv') here
  
  # the "Next" button
  # this will also add the current trial to the list
  observeEvent(input$nextButton, {
    
    validate(
      need(!is.null(currentFile$df), "Please load some data to select.")
    )
    
    ## add the df to list
    currentFile$dataList[[currentTrial$counterValue]] <- select(currentTrial$fitDF, selected, maxV)
    
    ## move to next trial
    
    if(currentTrial$counterValue == length(uniqueTrials())){
      currentTrial$counterValue <- 1
    }
    
    else {
      currentTrial$counterValue <- currentTrial$counterValue + 1
    }
    
    fitDF()
    addSelectedCols()
    
  })
  
  # The "Previous" button
  observeEvent(input$prevButton, {
    
    validate(
      need(!is.null(currentFile$df), "Please load some data to select.")
    )

    # print(currentFile$dataList)
    currentFile$dataList[[currentTrial$counterValue]] <- select(currentTrial$fitDF, selected, maxV)
    
    # go to the last trial if the current trisl is "1"
    if(currentTrial$counterValue == 1){
      currentTrial$counterValue <- length(uniqueTrials())
    }
    
    else {
      currentTrial$counterValue <- currentTrial$counterValue - 1
    }
    
    # print(currentTrial$counterValue)
    # start selecting the new data
    fitDF()
    addSelectedCols()
    
  })
  
  # The "Next File" button
  observeEvent(input$nextFileButton, {
    
    validate(
      need(!is.null(currentFile$df), "Please load some data to select.")
    )
    
    ## move to next file
    
    if(currentFile$fileNum == length(allFiles$inFile$datapath)){
      currentFile$fileNum <- 1
    }
    
    else {
      currentFile$fileNum <- currentFile$fileNum + 1
    }
    
    # print(currentFile$fileNum)
    
    # start selecting the new data
    storeCurrentData()
    fitDF()
    addSelectedCols()
    
  })
  
  # The "Previous File" button
  observeEvent(input$prevFileButton, {
    
    validate(
      need(!is.null(currentFile$df), "Please load some data to select.")
    )
    
    
    # go to the last trial if the current trisl is "1"
    if(currentFile$fileNum == 1){
      currentFile$fileNum <- length(allFiles$inFile$datapath)
    }
    
    else {
      currentFile$fileNum <- currentFile$fileNum - 1
    }
    
    # print(currentFile$fileNum)
    
    # start selecting the new data
    storeCurrentData()
    fitDF()
    addSelectedCols()
    
  })
  
  
  # After file is chosen, clicking this sets the currentFile$df to something
  observeEvent(input$runSelectButton, {
    validate(
      need(length(input$files) != 1,
           message = "Please load some data to select."))
    
    loadFilePaths()
    
    # start selecting the new data
    storeCurrentData()
    fitDF()
    addSelectedCols()
  })
  
  observeEvent(input$saveButton, {
    validate(
      need(!is.null(currentFile$df), 
           message = "Please load some data to select."),
      need(length(currentFile$dataList) == length(uniqueTrials()), 
           showNotification("Please finish selecting", type = "error"))
    )
    
    mergeAndSave()
    
    showNotification("Saved!", type = "message")
  })
  
  
  # Keep button
  observeEvent(input$keepButton, {
    validate(
      need(!is.null(currentFile$df), 
           message = "Please load some data to select.")
    )
    
    currentTrial$fitDF$selected <- 1
  })
  
  # Remove buttons
  observeEvent(input$removeButton, {
    validate(
      need(!is.null(currentFile$df), 
           message = "Please load some data to select.")
    )
    
    currentTrial$fitDF$selected <- 0
  })
  
  
  # The select all button
  observeEvent(input$selectAllButton, {
    validate(
      need(!is.null(currentFile$df), 
           message = "Please load some data to select.")
    )
    
    for (filePath in allFiles$inFile$datapath){
      # read the file
      fileDF <- fread(filePath, stringsAsFactors = FALSE)
      
      # make empty list
      trialList <- list()
      trialListCounter <- 1
      
      for (trialNum in unique(fileDF$trial_num)){
        trialDF <- fileDF %>% 
          filter(trial_num == trialNum)
        
        fitDF <- trialDF %>%
          select(time_s, mousex_px, mousey_px)
        
        # add a distance row
        fitDF$distance <- trialDF %>% 
          transmute(mousex_px = mousex_px - homex_px, mousey_px + homey_px) %>%
          apply(1, vector_norm)
        
        # fit a spline to the distance data
        fit_fun <- smooth.spline(x = fitDF$time_s, y = fitDF$distance, df = 7)
        
        # add a spline column
        fitDF$spline <- predict(fit_fun, fitDF$time_s)$y
        
        # add a speed column
        fitDF$speed <- predict(fit_fun, fitDF$time_s, deriv = 1)$y
        
        
        # do the selection
        fitDF$selected <- 1
        fitDF$maxV <- 0
        
        fitDF$maxV[fitDF$time_s == filter(fitDF, speed == max(speed))[1, ]$time_s] <- 1
        
        # add this to list
        trialList[[trialListCounter]] <- fitDF %>%
          select(selected, maxV)
        
        trialListCounter <- trialListCounter + 1
      }
      
      # merge and save
      pathToSave <- filePath %>%
        str_sub(1, -5)
      
      pathToSave <- paste(pathToSave, "selected.csv", sep = "_")
      
      # concatenate the selected columns
      selected_df <- do.call(rbind, trialList)
      
      # add the selected_df columns to df
      selected_df <- cbind2(fileDF, selected_df)
      
      # print(pathToSave)
      
      fwrite(selected_df, file = pathToSave)
    }
    
    showNotification("Done selecting!", type = "message")
    
  })
  
  # change maxV point
  observeEvent(input$velClick,{
    # print(paste("x = ", input$velClick$x))
    if (currentTrial$chooseMaxV == TRUE) {
      currentTrial$fitDF$maxV <- 0
      currentTrial$fitDF$maxV[which.min(abs(currentTrial$fitDF$time_s -input$velClick$x))] <- 1
      
      currentTrial$chooseMaxV <- FALSE
    }
  })
  
  # the set max velocity button
  observeEvent(input$setMaxVButton, {
    
    validate(
      need(!is.null(currentFile$df), "Please load some data to select.")
    )
    
    ## add the df to list
    ## currentFile$dataList[[currentTrial$counterValue]] <- select(currentTrial$fitDF, selected, maxV)
    
    currentTrial$chooseMaxV <- TRUE
    
  })
  
  # the go to trial button
  observeEvent(input$goToTrialButton, {
    validate(
      need(!is.null(currentFile$df), "Please load some data to select."),
      need(!is.na(as.integer(input$chooseTrialText)), "Please enter integer."),
      need(as.integer(input$chooseTrialText) <= length(uniqueTrials()) && as.integer(input$chooseTrialText) > 0, "Out of range.")
    )
    
    ## add the df to list
    currentFile$dataList[[currentTrial$counterValue]] <- select(currentTrial$fitDF, selected, maxV)
    
    ## move to trial
    currentTrial$counterValue <- as.integer(input$chooseTrialText)
    
    # do things to the trial
    fitDF()
    addSelectedCols()
    
  })
  
  # # change the trial via input
  # observeEvent(input$chooseTrialText, {
  #   validate(
  #     need(!is.null(currentFile$df), "Please load some data to select."),
  #     need(!is.na(as.integer(input$chooseTrialText)), "Please enter integer.")
  #   )
  #   
  #   print(as.integer(input$chooseTrialText))
  # })
  
  
  ## ----
  
  ## plots
  
  output$reachPlot <- renderPlot( {
    if(!is.null(currentFile$df)) {  
      # read in df
      df <- currentTrial$fitDF
      
      p <- df %>%
        ggplot(aes(x = mousex_px, y = mousey_px)) +
        geom_point(size = 4, colour = "#337ab7", alpha = 0.5) +
        geom_point(data = filter(df, maxV == 1), 
                   size = 6, colour = "#8c3331", shape = 10, 
                   stroke = 2, alpha = .8) +
        scale_y_continuous(limits = c(currentFile$min_y - (currentFile$max_y - currentFile$min_y) * .1, 
                                      currentFile$max_y + (currentFile$max_y - currentFile$min_y) * .1),
                           name = "y-position") +
        scale_x_continuous(limits = c(currentFile$min_x - (currentFile$max_x - currentFile$min_x) * .1, 
                                      currentFile$max_x + (currentFile$max_x - currentFile$min_x) * .1),
                           name = "x-position") +
        coord_fixed() +
        # annotate("text", x = -500, y = 900, size = 8,
        #          label = paste("Trial: ", currentTrial$counterValue)) +
        theme_minimal() +
        theme(text = element_text(size=20))
      
      # change background colour
      if(currentTrial$fitDF$selected[1] == 1) {
      # keep/remove colour
        p <- p + theme(panel.background = element_rect(fill = "#8fbfa0", colour = "#8fbfa0", 
                                                       size = 0.5, linetype = "solid"))
      }
      
      else{
        p <- p + theme(panel.background = element_rect(fill = "#bf918f", colour = "#bf918f", 
                                                       size = 0.5, linetype = "solid"))
      }
      
      p
    }
  })
    
  output$distPlot <- renderPlot( {
    if(!is.null(currentFile$df)) {  
      # read in df
      df <- currentTrial$fitDF
      
      p <- df %>%
        ggplot(aes(x = time_s, y = distance)) +
        geom_point(size = 4, colour = "#337ab7", alpha = 0.5) +
        geom_line(aes(y = spline), alpha = 0.5, size = 2) + 
        geom_point(data = filter(df, maxV == 1), 
                   size = 8, colour = "#8c3331", shape = 10, 
                   stroke = 2, alpha = .8) +
        scale_y_continuous(name = "distance from home") +
        scale_x_continuous(name = "time") +
        theme_minimal() +
        theme(text = element_text(size=20))
      
      p
    }
  })
  
  output$velPlot <- renderPlot( {
    if(!is.null(currentFile$df)) {  
      
      # read in df
      df <- currentTrial$fitDF
      
      
      p <- df %>%
        ggplot(aes(x = time_s, y = speed)) +
        geom_line(size = 3, alpha = .5) +
        geom_point(data = filter(df, maxV == 1), 
                   size = 8, colour = "#8c3331", shape = 10, 
                   stroke = 2, alpha = .8) +
        scale_y_continuous(name = "speed") +
        scale_x_continuous(name = "time") +
        theme_minimal() +
        theme(text = element_text(size=20))
      
      # if (!is.null(input$velClick$x)){
      #   p <- p + 
      #     geom_point(aes(x = input$velClick$x, 
      #                    y = filter(df, time_s == input$velClick$x)$speed), 
      #                size = 8, colour = "#8c3331", shape = 10, 
      #                stroke = 2, alpha = .8)
      # }
      
      
      p
    }
  })
  
  
  
  
  ##  Text
  
  output$currentFileTxt <- renderText( {
    if(!is.null(currentFile$df)) {  
      paste("<font size=4>", currentFile$filePath, "  ", "<b>", currentFile$fileNum, 
            "/", length(allFiles$inFile$datapath), "</font> </b>",
            sep = "")
    }
    else {
      paste("Please choose files to select, and run selection.")
    }
  })
  
  output$currentTrialTxt <- renderText( {
    if(!is.null(currentFile$df)) {  
      paste("<b> <font size=4>", currentTrial$counterValue, "/", 
            length(uniqueTrials()), "</font> </b>",
            sep = "")
    }
  })
  
  output$trialsSelectedTxt <- renderText( {
    if(!is.null(currentFile$df)) {  
      
      numSelected <- length(Filter(Negate(is.null), currentFile$dataList))
      
      if(numSelected == length(uniqueTrials())) {
        showNotification("All trials selected!", type = "message")
        
        paste("<b> <font color=\"#269148\" size=4>", numSelected, "/", 
              length(uniqueTrials()), "</font> </b>",
              sep = "")
      }
      else {
        paste("<b> <font size=4>", numSelected, "/", 
              length(uniqueTrials()), "</font> </b>",
              sep = "")
      }
    }
  })
  
  output$keepStatusTxt <- renderText( {
    if(!is.null(currentFile$df)) {  
      
      if(currentTrial$fitDF$selected[1] == 1) {
        paste("<b> <font color=\"#269148\" size=4> KEPT </font> </b>",
              sep = "")
      }
      else {
        paste("<b> <font color=\"#8c3331\" size=4> DELETED </font> </b>",
              sep = "")
      }
    }
  })
  
}


##---- 
## Testing
# currentTrialDF <- fread("sampleData/stepwiseExp/1/1_aligned_traning_1.txt", stringsAsFactors = FALSE) %>%
#   filter(trial_num == 1)
# df <- currentTrialDF
