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
  source("src/helper_funcs.R")
  ## STORED DATA

  # note: trial and step counters are NOT the values of the trial and step
  # see the "uniqueTrials" function for use
  currentTrial <- reactiveValues(
    trialCounter = 1, stepCounter = 1,
    fitDF = NULL,
    chooseMaxV = FALSE, chooseMoveStart = FALSE, chooseMoveEnd = FALSE, #toggles
    trialValuesDF = NULL
  )
  currentFile <- reactiveValues(
    filePath = NULL, fileNum = 1,
    df = NULL, dataList = list(),
    min_x = NULL, max_x = NULL,
    min_y = NULL, max_y = NULL,
    done_trial_list = array(),
    is_collapsed = FALSE
  )
  allFiles <- reactiveValues(inFile = NULL)
  globalValues <- reactiveValues(
    lazyMode = FALSE, settingsFilePath = NULL,
    settingsDF = NULL
  )
  volumes <- c(Home = fs::path_home(), WD = ".", getVolumes()())

  clickOpts(id = "velClick", clip = TRUE)

  ## ----
  ## FUNCTIONS

  # read in df
  loadFilePaths <- reactive({
    allFiles$inFile <- parseFilePaths(roots = c(volumes), input$files)
  })

  # read in settings (this runs on "Start Selecting")
  loadSettings <- reactive({

    # get the file path
    globalValues$settingsFilePath <- parseFilePaths(roots = c(volumes), input$settingsButton)

    # set the actual DF
    if (as.character(globalValues$settingsFilePath[4]) != "character(0)") {
      globalValues$settingsDF <- fread(as.character(globalValues$settingsFilePath[4]),
        stringsAsFactors = FALSE
      )
      # replace default_settings file
      fwrite(globalValues$settingsDF, file = "settings/default_settings.txt")
    } else {
      # use the default_settings file
      globalValues$settingsDF <- fread("settings/default_settings.txt",
        stringsAsFactors = FALSE
      )
    }
    # for testing
    # print(globalValues$settingsDF)
  })

  checkIfDataLoaded <- function() {
    if (is.null(currentFile$df)) {
      showNotification("Please choose some data to select.", type = "error")
    }

    validate(
      need(!is.null(currentFile$df),
        message = "Please choose some data to select."
      )
    )
  }


  loadNewFileAndSetDefaults <- reactive({
    currentFile$filePath <- as.character(allFiles$inFile$datapath[currentFile$fileNum])

    df <- fread(currentFile$filePath, stringsAsFactors = FALSE)

    # rename column headers based on settings
    if (!is.null(globalValues$settingsDF)) {
      df <- fix_headers(df, globalValues$settingsDF)
    } else {
      showNotification("Please choose a settings file.", type = "error")
    }

    # set the file df to this one
    currentFile$df <- df

    # reset is_collapsed
    currentFile$is_collapsed <- FALSE

    # filter out first tiral from df
    trial1 <- df %>%
      filter(trial_num == uniqueTrials()[1])

    if (nrow(trial1) == 1) {
      df <- build_df_from_rows(df)
      currentFile$is_collapsed <- TRUE
    }

    # get maximum x and y (for plotting)
    currentFile$min_x <- get_min_val(df)[1]
    currentFile$min_y <- get_min_val(df)[2]
    currentFile$max_x <- get_max_val(df)[1]
    currentFile$max_y <- get_max_val(df)[2]

    # print(currentFile$filePath)

    # reset the trialCounter
    currentTrial$trialCounter <- 1

    # reset the fitDF
    currentTrial$fitDF <- NULL

    # reset the toggles
    currentTrial$chooseMaxV <- FALSE
    currentTrial$chooseMoveStart <- FALSE
    currentTrial$chooseMoveEnd <- FALSE 

    # reset the trialValueDF
    currentTrial$trialValuesDF <- NULL

    # reset the done_trial_list
    currentFile$done_trial_list <- array()

    # reset dataList -- used to store rows that are bound later
    currentFile$dataList <- list()
    for (trial in range(length(uniqueTrials()))) {
      currentFile$dataList[[trial]] <- list()
    }
  })

  # df containing only the current trial
  currentTrialDF <- reactive({
    df <- currentFile$df %>%
      filter(trial_num == uniqueTrials()[currentTrial$trialCounter])

    if (nrow(df) == 1) {
      # build the df
      df <- build_df_from_rows(df)
    }

    df
  })

  # returns a vector of the unique trial_num in current file
  uniqueTrials <- reactive({
    uniqueTrials <- currentFile$df$trial_num %>%
      unique()

    # get rid of NAs
    uniqueTrials <- uniqueTrials[!sapply(uniqueTrials, is.na)]

    uniqueTrials
  })

  # returns a vector of the unique steps in current file
  uniqueSteps <- reactive({
    uniqueSteps <- currentTrialDF()$step %>%
      unique()

    # get rid of NAs
    uniqueSteps <- uniqueSteps[!sapply(uniqueSteps, is.na)]

    uniqueSteps
  })

  # make a tibble with time, mousex, mousey, spline, speed
  add_trial_fitDF <- reactive({
    # get the current trial and step
    fitDF <- currentTrialDF() %>%
      filter(step == uniqueSteps()[currentTrial$stepCounter]) %>%
      select(time, mouse_x, mouse_y, home_x, home_y)

    # fit distance and speed
    fitDF <- make_fitDF(step_df = fitDF)

    currentTrial$fitDF <- fitDF
  })

  # make a tibble with target location, etc
  make_trialValuesDF <- reactive({
    temp_trial_values_df <- currentTrialDF()
    # select only the relevant rows
    # if target_x exists, select those values
    if ("target_x" %in% colnames(temp_trial_values_df)) {
      temp_trial_values_df <- temp_trial_values_df %>%
        select(target_x, target_y) %>%
        head(1)
    } else {
      # if target_x doesn't exist, select the first row
      temp_trial_values_df <- data.frame(target_x = 0, target_y = 0)
    }

    currentTrial$trialValuesDF <- temp_trial_values_df
  })


  addSelectedCols_trial_fitDF <- reactive({
    df <- currentTrial$fitDF

    worked <- FALSE

    # make selected and max_v columns if they don't already exist in currentFile$dataList
    # if the do exist, just pull them from there
    try(
      {
        if (is.null(currentFile$dataList[[currentTrial$trialCounter]][[currentTrial$stepCounter]])) {
          df$selected <- 1
          df <- add_maxv_col(df)
          df <- set_movement_col(df)

        } else {
          df$selected <-
            currentFile$dataList[[currentTrial$trialCounter]][[currentTrial$stepCounter]]$selected
          df$max_v <-
            currentFile$dataList[[currentTrial$trialCounter]][[currentTrial$stepCounter]]$max_v
          df$movement <-
            currentFile$dataList[[currentTrial$trialCounter]][[currentTrial$stepCounter]]$movement
        }

        worked <- TRUE
      },
      silent = TRUE
    )

    # if that dataList[[trialCounter]] doesn't exist at all, the above code will not work
    if (!worked) {
      df$selected <- 1
      df <- add_maxv_col(df)
      df <- set_movement_col(df)
    }

    currentTrial$fitDF <- df
  })

  # save button stuff
  mergeAndSave <- reactive({
    pathToSave <- currentFile$filePath %>%
      str_sub(1, -5)

    pathToSave <- paste(pathToSave, "selected.csv", sep = "_")

    # concatenate the selected columns
    selected_df <- do.call(rbind, map(currentFile$dataList, bind_rows))

    # build expanded df if collapsed
    # otherwise, just use the currentFile$df
    if (currentFile$is_collapsed) {
      df <- build_df_from_rows_for_saving(currentFile$df)
    } else {
      df <- currentFile$df
    }

    # revert the headers back to the original ones
    df <- revert_headers(df, globalValues$settingsDF)

    # add the selected_df columns to df
    selected_df <- cbind2(df, selected_df)

    # print(pathToSave)

    fwrite(selected_df, file = pathToSave)
  })


  ## ----
  ## BUTTONS

  # loading in a file
  shinyFileChoose(input, "files",
    roots = volumes
  ) # can do filetypes = c('', '.csv') here

  # button to choose settings
  shinyFileChoose(input, "settingsButton",
    roots = volumes, filetypes = c("", "csv")
  )

  # the "Next Step" button
  observeEvent(input$nextStepButton, {
    checkIfDataLoaded()

    currentFile$dataList[[currentTrial$trialCounter]][[currentTrial$stepCounter]] <-
      currentTrial$fitDF %>%
      select(selected, max_v, movement)

    # append current trial to done_trial_list
    # if it's not already there and all steps are done
    if (length(currentFile$dataList[[currentTrial$trialCounter]]) == length(uniqueSteps())) {
      if (!(currentTrial$trialCounter %in% currentFile$done_trial_list)) {
        currentFile$done_trial_list <- c(
          currentFile$done_trial_list,
          currentTrial$trialCounter
        )
      }
    }

    ## move to next step
    if (currentTrial$stepCounter == length(uniqueSteps())) {
      currentTrial$stepCounter <- 1
    } else {
      currentTrial$stepCounter <- currentTrial$stepCounter + 1
    }

    add_trial_fitDF()
    make_trialValuesDF()
    addSelectedCols_trial_fitDF()
  })

  # The "Previous Step" button
  observeEvent(input$prevStepButton, {
    checkIfDataLoaded()

    # update the dataList at current step
    currentFile$dataList[[currentTrial$trialCounter]][[currentTrial$stepCounter]] <-
      currentTrial$fitDF %>%
      select(selected, max_v, movement)

    # append current trial to done_trial_list
    # if it's not already there and all steps are done
    if (length(currentFile$dataList[[currentTrial$trialCounter]]) == length(uniqueSteps())) {
      if (!(currentTrial$trialCounter %in% currentFile$done_trial_list)) {
        currentFile$done_trial_list <- c(
          currentFile$done_trial_list,
          currentTrial$trialCounter
        )
      }
    }

    # go to the last trial if the current trial is "1"
    if (currentTrial$stepCounter == 1) {
      currentTrial$stepCounter <- length(uniqueSteps())
    } else {
      currentTrial$stepCounter <- currentTrial$stepCounter - 1
    }

    # print(currentTrial$trialCounter)
    # start selecting the new data
    add_trial_fitDF()
    make_trialValuesDF()
    addSelectedCols_trial_fitDF()
  })

  # the "Next Trial" button
  # this will also add the current trial to the list
  observeEvent(input$nextTrialButton, {
    checkIfDataLoaded()

    ## add the dfs for all steps to dataList
    steps_in_trial <- seq(length(uniqueSteps()))
    # loop through steps in the trial
    for (step_num in steps_in_trial) {
      if (step_num == currentTrial$stepCounter) {
        # add the current step to the list
        currentFile$dataList[[currentTrial$trialCounter]][[step_num]] <-
          select(currentTrial$fitDF, selected, max_v, movement)
      } else {
        if (tryCatch(
          is.null(currentFile$dataList[[currentTrial$trialCounter]][[step_num]]), 
          error = function(e) {
          return(TRUE)
        })) {
          # above resolves TRUE if dataList is empty at that nested index

          # add the other steps to the list if they don't exist already
          # construct the df first
          temp_df <- currentTrialDF() %>%
            filter(step == uniqueSteps()[step_num]) %>%
            select(time, mouse_x, mouse_y, home_x, home_y)

          # assign to dataList
          temp_df <- make_fitDF(step_df = temp_df)
          temp_df$selected <- 1
          temp_df <- add_maxv_col(temp_df)
          temp_df <- set_movement_col(temp_df)

          currentFile$dataList[[currentTrial$trialCounter]][[step_num]] <-
            select(temp_df, selected, max_v, movement)
        }
      }
    }

    # append the current trial to done_trial_list if it's not already there
    if (!(currentTrial$trialCounter %in% currentFile$done_trial_list)) {
      currentFile$done_trial_list <- c(
        currentFile$done_trial_list,
        currentTrial$trialCounter
      )
    }

    ## move to next trial
    if (currentTrial$trialCounter == length(uniqueTrials())) {
      currentTrial$trialCounter <- 1
    } else {
      currentTrial$trialCounter <- currentTrial$trialCounter + 1
    }

    add_trial_fitDF()
    make_trialValuesDF()
    addSelectedCols_trial_fitDF()
  })

  # The "Previous Trial" button
  observeEvent(input$prevTrialButton, {
    checkIfDataLoaded()

    ## add the dfs for all steps to dataList
    steps_in_trial <- seq(length(uniqueSteps()))
    # loop through steps in the trial
    for (step_num in steps_in_trial) {
      if (step_num == currentTrial$stepCounter) {
        # add the current step to the list
        currentFile$dataList[[currentTrial$trialCounter]][[step_num]] <-
          select(currentTrial$fitDF, selected, max_v, movement)
      } else {
        if (tryCatch(
          is.null(currentFile$dataList[[currentTrial$trialCounter]][[step_num]]), 
          error = function(e) {
          return(TRUE)
        })) {
          # above resolves TRUE if dataList is empty at that nested index

          # add the other steps to the list if they don't exist already
          # construct the df first
          temp_df <- currentTrialDF() %>%
            filter(step == uniqueSteps()[step_num]) %>%
            select(time, mouse_x, mouse_y, home_x, home_y)

          # assign to dataList
          temp_df <- make_fitDF(step_df = temp_df)
          temp_df$selected <- 1
          temp_df <- add_maxv_col(temp_df)
          temp_df <- set_movement_col(temp_df)

          currentFile$dataList[[currentTrial$trialCounter]][[step_num]] <-
            select(temp_df, selected, max_v, movement)
        }
      }
    }

    # append the current trial to done_trial_list if it's not already there
    if (!(currentTrial$trialCounter %in% currentFile$done_trial_list)) {
      currentFile$done_trial_list <- c(
        currentFile$done_trial_list,
        currentTrial$trialCounter
      )
    }

    # go to the last trial if the current trial is "1"
    if (currentTrial$trialCounter == 1) {
      currentTrial$trialCounter <- length(uniqueTrials())
    } else {
      currentTrial$trialCounter <- currentTrial$trialCounter - 1
    }

    # print(currentTrial$trialCounter)
    # start selecting the new data
    add_trial_fitDF()
    make_trialValuesDF()
    addSelectedCols_trial_fitDF()
  })

  # The "Next File" button
  observeEvent(input$nextFileButton, {
    checkIfDataLoaded()

    ## move to next file

    if (currentFile$fileNum == length(allFiles$inFile$datapath)) {
      currentFile$fileNum <- 1
    } else {
      currentFile$fileNum <- currentFile$fileNum + 1
    }

    # start selecting the new data
    loadNewFileAndSetDefaults()
    add_trial_fitDF()
    make_trialValuesDF()
    addSelectedCols_trial_fitDF()
  })

  # The "Previous File" button
  observeEvent(input$prevFileButton, {
    checkIfDataLoaded()

    # go to the last trial if the current trisl is "1"
    if (currentFile$fileNum == 1) {
      currentFile$fileNum <- length(allFiles$inFile$datapath)
    } else {
      currentFile$fileNum <- currentFile$fileNum - 1
    }

    # start selecting the new data
    loadNewFileAndSetDefaults()
    add_trial_fitDF()
    make_trialValuesDF()
    addSelectedCols_trial_fitDF()
  })


  # After file is chosen, clicking this sets the currentFile$df to something
  observeEvent(input$runSelectButton, {

    # guard: do filepaths exist?
    if (length(input$files) == 1) {
      showNotification("Please choose some data to select.", type = "error")
    }

    validate(
      need(length(input$files) != 1,
        message = "Please choose some data to select."
      )
    )

    loadFilePaths()
    loadSettings()

    # get one file for checking colnames
    currentFile$filePath <-
      as.character(allFiles$inFile$datapath[currentFile$fileNum])
    df <- fread(currentFile$filePath, stringsAsFactors = FALSE)

    # guard: do headers match settings?
    if (length(setdiff(
      as.character(globalValues$settingsDF[1, ]),
      colnames(df)
    )) > 2) {
      showNotification("Column names don't match. Please check settings",
        type = "error"
      )
    }

    validate(
      need(length(setdiff(as.character(globalValues$settingsDF[1, ]), 
      colnames(df))) <= 2,
        message = "Column names don't match. Please check settings"
      )
    )

    # start selecting the new data
    loadNewFileAndSetDefaults()
    add_trial_fitDF()
    make_trialValuesDF()
    addSelectedCols_trial_fitDF()
  })

  observeEvent(input$saveButton, {
    checkIfDataLoaded()

    # guard: selecting complete?
    if (length(currentFile$done_trial_list) - 1 != length(uniqueTrials())) {
      showNotification("Please select all trials.", type = "error")
    }

    validate(
      need(
        length(currentFile$done_trial_list) - 1 == length(uniqueTrials()),
        message = "Please finish selecting."
      )
    )

    mergeAndSave()

    showNotification("Saved!", type = "message")
  })


  # Keep trial button
  observeEvent(input$keepButton, {
    checkIfDataLoaded()

    currentTrial$fitDF$selected <- 1
  })

  # Flag trial button
  observeEvent(input$flagButton, {
    checkIfDataLoaded()

    currentTrial$fitDF$selected <- 0
  })


  # The select all button
  observeEvent(input$selectAllButton, {
    checkIfDataLoaded()

    for (filePath in allFiles$inFile$datapath) {
      # read the file
      fileDF <- fread(filePath, stringsAsFactors = FALSE)

      # make empty list
      trialList <- list()
      trialListCounter <- 1

      for (trialNum in unique(fileDF$trial_num)) {
        trialDF <- fileDF %>%
          filter(trial_num == trialNum)

        fitDF <- trialDF %>%
          select(time, mouse_x, mouse_y)

        # add a distance row
        fitDF$distance <- trialDF %>%
          transmute(mouse_x = mouse_x - home_x, mouse_y + home_y) %>%
          apply(1, vector_norm)

        # fit a spline to the distance data
        fit_fun <- smooth.spline(x = fitDF$time, y = fitDF$distance, df = 7)

        # add a spline column
        fitDF$spline <- predict(fit_fun, fitDF$time)$y

        # add a speed column
        fitDF$speed <- predict(fit_fun, fitDF$time, deriv = 1)$y


        # do the selection
        fitDF$selected <- 1
        fitDF <- add_maxv_col(fitDF)
        fitDF <- set_movement_col(fitDF)

        # add this to list
        trialList[[trialListCounter]] <- fitDF %>%
          select(selected, max_v, movement)

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

  # change max_v point
  observeEvent(input$velClick, {
    print(paste("x = ", input$velClick$x))
    if (currentTrial$chooseMaxV) {
      currentTrial$fitDF$max_v <- 0
      currentTrial$fitDF$max_v[which.min(
        abs(
          currentTrial$fitDF$time - input$velClick$x))] <- 1

      currentTrial$chooseMaxV <- FALSE
    }
    else if (currentTrial$chooseMoveStart) {
       set_movement_col(currentTrial$fitDF, move_start = TRUE, x = input$velClick$x)
    }
    else if (currentTrial$chooseMoveEnd) {
       set_movement_col(currentTrial$fitDF, move_end = TRUE, x = input$velClick$x)
    }
  })

  # the set max velocity button
  observeEvent(input$setMaxVButton, {
    checkIfDataLoaded()

    currentTrial$chooseMaxV <- TRUE

    # set all other toggles to false
    currentTrial$chooseMoveStart <- FALSE
    currentTrial$chooseMoveEnd <- FALSE
  })

  # the set movement start button
  observeEvent(input$setMoveStartButton, {
    checkIfDataLoaded()

    currentTrial$chooseMoveStart <- TRUE

    # set all other toggles to false
    currentTrial$chooseMaxV <- FALSE
    currentTrial$chooseMoveEnd <- FALSE

  })

  # the set movement end button
  observeEvent(input$setMoveEndButton, {
    checkIfDataLoaded()

    currentTrial$chooseMoveEnd <- TRUE

    # set all other toggles to false
    currentTrial$chooseMaxV <- FALSE
    currentTrial$chooseMoveStart <- FALSE

  })

  # the go to trial button
  observeEvent(input$goToTrialButton, {
    checkIfDataLoaded()

    # guards: integer, trial out of range
    if (is.na(as.integer(input$chooseTrialText))) {
      showNotification("Please enter a number.", type = "error")
    }
    validate(
      need(
        !is.na(as.integer(input$chooseTrialText)),
        message = "Please enter an integer."
      )
    )

    if (as.integer(input$chooseTrialText) > length(uniqueTrials()) ||
      as.integer(input$chooseTrialText) < 1) {
      showNotification("Trial number out of range.", type = "error")
    }
    validate(
      need(
        as.integer(input$chooseTrialText) <= length(uniqueTrials()) &&
          as.integer(input$chooseTrialText) > 0,
        message = "Trial out of range."
      )
    )

    ## add the df to list
    currentFile$dataList[[currentTrial$trialCounter]] <- 
    select(currentTrial$fitDF, selected, max_v, movement)

    ## move to trial
    currentTrial$trialCounter <- as.integer(input$chooseTrialText)

    # do things to the trial
    add_trial_fitDF()
    make_trialValuesDF()
    addSelectedCols_trial_fitDF()
  })


  # # change the trial via input
  # observeEvent(input$chooseTrialText, {
  #   validate(
  #     need(!is.null(currentFile$df), "Please choose some data to select."),
  #     need(!is.na(as.integer(input$chooseTrialText)), "Please enter integer.")
  #   )
  #
  #   print(as.integer(input$chooseTrialText))
  # })


  ## ----

  ## PLOTS

  output$reachPlot <- renderPlot({
    if (!is.null(currentTrial$fitDF)) {
      # read in df
      df <- currentTrial$fitDF

      # plot the reach
      p <- df %>%
        ggplot(aes(x = mouse_x, y = mouse_y, colour = movement)) +
        geom_point(size = 4, alpha = 0.5) +
        geom_point(
          data = filter(df, max_v == 1),
          size = 6, colour = "#8c3331", shape = 10,
          stroke = 2, alpha = .8
        ) +
        scale_y_continuous(
          limits = c(
            currentFile$min_y - (currentFile$max_y - currentFile$min_y) * .1,
            currentFile$max_y + (currentFile$max_y - currentFile$min_y) * .1
          ),
          name = "y-position"
        ) +
        scale_x_continuous(
          limits = c(
            currentFile$min_x - (currentFile$max_x - currentFile$min_x) * .1,
            currentFile$max_x + (currentFile$max_x - currentFile$min_x) * .1
          ),
          name = "x-position"
        ) +
        coord_fixed() +
        # annotate("text", x = -500, y = 900, size = 8,
        #          label = paste("Trial: ", currentTrial$trialCounter)) +
        theme_minimal() +
        theme(text = element_text(size = 20)) +
        theme(legend.position = "none")

      # add target
      if (currentTrial$trialValuesDF$target_x != 0 || 
      currentTrial$trialValuesDF$target_y != 0) {
        p <- p + geom_point(
          data = currentTrial$trialValuesDF,
          aes(x = target_x, y = target_y),
          size = 3, colour = "#d6a333", shape = 19,
          stroke = 2
        )
      }

      # change background colour
      if (currentTrial$fitDF$selected[1] == 1) {
        # keep/flag colour
        p <- p + theme(panel.background = element_rect(
          fill = "#8fbfa0", colour = "#8fbfa0",
          size = 0.5, linetype = "solid"
        ))
      } else {
        p <- p + theme(panel.background = element_rect(
          fill = "#bf918f", colour = "#bf918f",
          size = 0.5, linetype = "solid"
        ))
      }

      p
    }
  })

  output$distPlot <- renderPlot({
    if (!is.null(currentTrial$fitDF)) {
      # read in df
      df <- currentTrial$fitDF

      p <- df %>%
        ggplot(aes(x = time, y = distance, colour = movement)) +
        geom_line(aes(y = spline), colour = "grey",
                      alpha = 0.7, size = 2) +
        geom_point(size = 4, alpha = 0.5) +
        geom_point(
          data = filter(df, max_v == 1),
          size = 8, colour = "#8c3331", shape = 10,
          stroke = 2, alpha = .8
        ) +
        scale_y_continuous(name = "distance from home") +
        scale_x_continuous(name = "time") +
        theme_minimal() +
        theme(text = element_text(size = 20)) +
        theme(legend.position = "none")

      p
    }
  })

  output$velPlot <- renderPlot({
    if (!is.null(currentTrial$fitDF)) {

      # read in df
      df <- currentTrial$fitDF


      p <- df %>%
        ggplot(aes(x = time, y = speed, colour = movement)) +
        geom_line(colour = "grey", alpha = 0.7, size = 3) +
        geom_point(size = 3, alpha = 0.5) +
        geom_point(
          data = filter(df, max_v == 1),
          size = 8, colour = "#8c3331", shape = 10,
          stroke = 2, alpha = .8
        ) +
        scale_y_continuous(name = "speed") +
        scale_x_continuous(name = "time") +
        theme_minimal() +
        theme(text = element_text(size = 20)) +
        theme(legend.position = "none")

      # if (!is.null(input$velClick$x)){
      #   p <- p +
      #     geom_point(aes(x = input$velClick$x,
      #                    y = filter(df, time == input$velClick$x)$speed),
      #                size = 8, colour = "#8c3331", shape = 10,
      #                stroke = 2, alpha = .8)
      # }

      p
    }
  })




  ##  Text

  output$currentFileTxt <- renderText({
    if (!is.null(currentTrial$fitDF)) {
      paste("<font size=4>", currentFile$filePath,
        "  ", "<b>", currentFile$fileNum,
        "/", length(allFiles$inFile$datapath), "</font> </b>",
        sep = ""
      )
    } else {
      paste("Please choose files to select, then click the start button.")
    }
  })

  output$infoTxt <- renderText({
    if (!is.null(currentTrial$fitDF)) {
      paste("<b> <font size=4> ",
        " Trial: ", currentTrial$trialCounter,
        "  Step: ", currentTrial$stepCounter, "/", length(uniqueSteps()),
        " </font> </b>",
        sep = ""
      )
    }
  })
  output$keptStatusTxt <- renderText({
    if (!is.null(currentTrial$fitDF)) {
      if (currentTrial$fitDF$selected[1] == 1) {
        paste("<b> <font color=\"#269148\" size=4> KEPT </font> </b>",
          sep = ""
        )
      } else {
        paste("<b> <font color=\"#8c3331\" size=4> DELETED </font> </b>",
          sep = ""
        )
      }
    }
  })

  output$trialsSelectedTxt <- renderText({
    if (!is.null(currentTrial$fitDF)) {
      numSelected <- length(currentFile$done_trial_list) - 1

      if (numSelected == length(uniqueTrials())) {
        showNotification("All trials selected!", type = "message")

        paste("<b> <font color=\"#269148\" size=4>", numSelected, "/",
          length(uniqueTrials()), "</font> </b>",
          sep = ""
        )
      } else {
        paste("<b> <font size=4>", numSelected, "/",
          length(uniqueTrials()), "</font> </b>",
          sep = ""
        )
      }
    }
  })

  # END OF SERVER
}


## ----
## Testing
# currentTrialDF <- fread("selectApp/sampleData/2/2_aligned_traning_1.txt", stringsAsFactors = FALSE) %>%
#   filter(trial_num == 1)
# df <- currentTrialDF