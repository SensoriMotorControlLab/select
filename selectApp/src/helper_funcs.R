## --------------------------------
##
## Script name: analysisFunctions.R
##
## Purpose of script: A few useful functions for data analysis --> should work well with tidyverse
##
## Author: Shanaa Modchalingam
##
## Date created: 2019-07-14
##
## Email: s.modcha@gmail.com
##
## --------------------------------
##
## Notes:
##
##
## --------------------------------


# input = a vector
# works well with group_by %>% summarise()
vector_confint <- function(vector, interval = 0.95) {
  # Standard deviation of sample
  vec_sd <- sd(vector, na.rm = TRUE)
  # Sample size
  n <- length(vector)
  # Error according to t distribution
  error <- qt((interval + 1) / 2, df = n - 1) * vec_sd / sqrt(n)
  # Confidence interval as a vector
  return(error)
}

# get the magnitude (euclidian normal) of a vector
# (this is faster than R's built in norm)
vector_norm <- function(vector) {
  sqrt(sum(vector^2))
}

# add maxV
add_maxv_col <- function(df) {

  df$max_v <- 0

  # df$max_v[df$time == filter(df, speed == max(speed))[1, ]$time] <- 1
  i <- 1
  fastest <- 0

  # loop through speed
  if (length(df$speed) < 3) {
        df$max_v <- 0
        df$max_v[1] <- 1
  }
  else {
    for (v in df$speed) {
      if (abs(v) > fastest) {
        fastest <- abs(v)
        df$max_v <- 0
        df$max_v[i] <- 1
      }
      i <- i + 1
    }
  }

  # fix maxV if the distance is too small
  if (filter(df, max_v == 1)$distance < max(df$distance) / 10) {
    # reset maxV
    df$max_v <- 0

    # set maxV to 30% of movement
    # NOTE: is 2 pointers useful here?
    # Maybe for reaches where the initiation time is very long..
    for (dist in df$distance) {
      if (dist > max(df$distance) * 3 / 10) {
        df$max_v[df$distance == dist] <- 1
        break
      }
    }
  }

  return(df)
}

# add moveStart and moveEnd
# this should be a column of 0s with 1s in the region of movement
set_movement_col <- function(df, move_start = 0, move_end = 0, debug = FALSE) {
  # if df doesn't have a max_v column, log an error
  if (!("max_v" %in% colnames(df))) {
    stop("df must have a max_v column")
  }

  if (move_start != 0 && move_end != 0) { # there is already a movement column, x is the TIME
    # everything including and after x and before the next 0 should be 1
    
    #debug
    if (debug) {
      print(paste("move_start:", move_start))
      print(paste("move_end:", move_end))
      print(df$movement)
    }

    # do
    df <- df %>%
          mutate(movement = ifelse(
            (df$time >= move_start) & (df$time <= move_end),
            1,
            0
          ))
    # make df$movement a factor
    df$movement <- factor(df$movement)

    return(df)
  }

  else {
    # add movement column
    df$movement <- 0

    # stop if there is no max_v
    if (!(1 %in% df$max_v)){
      return(df)
    }

    # max_v is the speed where max_v is 1
    max_v <- df$speed[df$max_v == 1][1]
    # threshold is max_v * 0.15
    vel_threshold <- abs(max_v) * 0.15

    # make an empty vector the same length as movement
    x <- factor(c(0, 1))
    move_vec <- rep(x, length.out = length(df$movement))
    i <- 1
    move_started <- FALSE
    move_ended <- FALSE

    # loop through speed
    for (speed in df$speed) {
      # if speed is above threshold
      if (move_started && move_ended) {
        # set movement to 0
        move_vec[i] <- 0
      } 
      else if (move_started) {
        if (abs(speed) > vel_threshold) {
          # set the vector to 1
          move_vec[i] <- 1
        } else {
          # if speed is below threshold
          # set the vector to 0         
          move_ended <- TRUE
          move_vec[i] <- 0
        }
      }
      else{
        if (abs(speed) > vel_threshold) {
          # set the vector to 1
          move_started <- TRUE
          move_vec[i] <- 1
        } else {
          # if speed is below threshold
          # set the vector to 0
          move_vec[i] <- 0
        }
      }
      i <- i + 1
    }

    # set movement to the vector
    df$movement <- move_vec
    
  }

  return(df)
}

fix_headers <- function(df, settings_df) {
  # required headers
  df <- df %>%
    rename(
      trial_num = settings_df$trial_num[1],
      mouse_x = settings_df$mouse_x[1],
      mouse_y = settings_df$mouse_y[1],
      time = settings_df$time[1]
    )
  # optional headers that default to 0
  home_headers <- c("home_x", "home_y", "step")
  # loop through optional headers
  for (i in home_headers) {
    temp_header <- settings_df %>%
      select(!!sym(i))
    temp_header <- as.character(temp_header[1, ])
    if (temp_header != "NA") {
      # this header exists
      # rename temp_header to i
      df <- df %>%
        rename(!!i := all_of(temp_header))
    } else {
      # this header doesn't exist
      # fill this thing with zeros
      df[[i]] <- 0
    }
  }
  temp_headers <- c("target_x", "target_y", "cursor_x", "cursor_y")
  # loop through optional headers
  for (i in temp_headers) {
    temp_header <- settings_df %>%
      select(!!sym(i))
    temp_header <- as.character(temp_header[1, ])

    if (temp_header != "NA") {
      # this header exists
      # rename temp_header to i
      df <- df %>%
        rename(!!i := all_of(temp_header))
    } else {
      # this header doesn't exist
      # fill this thing with "NA"s
      df[[i]] <- "NA"
    }
  }

  return(df)
}

# reverts to original headers
revert_headers <- function(df, settings_df) {
  # if header in settings_df is not NA
  # rename df header to settings_df[header]
  for (header in colnames(settings_df)) {
    if (!(header %in% c("value_type", "settings_name")) && !is.na(select(settings_df, !!header)[[1]])) {
      df <- df %>%
        rename(!!select(settings_df, !!header)[[1]] := header)
    }
  }

  return(df)
}

build_df_from_rows <- function(df) {
  # Should ONLY take required cols (to save space)
  # df should already have correct col names

  row_list <- list()
  i <- 1

  # populate rowlist
  for (trial_num_temp in df$trial_num) {
    if (is.na(trial_num_temp)) {
      next
    } else {
      trial_row <- df %>%
        filter(trial_num == trial_num_temp)

      trial_df <- data.frame(
        time = convert_cell_to_numvec(trial_row$time),
        mouse_x = convert_cell_to_numvec(trial_row$mouse_x),
        mouse_y = convert_cell_to_numvec(trial_row$mouse_y),
        step = convert_cell_to_numvec(trial_row$step)
      )

      trial_df$trial_num <- trial_num_temp

      # TO DO: populate optional headers
      # if home_x and home_y exist, populate them
      if ("home_x" %in% colnames(trial_df)) {
        trial_df$home_x <- convert_cell_to_numvec(trial_row$home_x)
        trial_df$home_y <- convert_cell_to_numvec(trial_row$home_y)
      } else {
        # if home_x and home_y don't exist, populate them with zeros
        trial_df$home_x <- 0
        trial_df$home_y <- 0
      }

      row_list[[i]] <- trial_df

      i <- i + 1
      # print(trial_num_temp)
    }
  }

  return(do.call(rbind, row_list))
}

build_df_from_rows_for_saving <- function(df) {
  # df should already have correct col names
  # length should match the selected_df length

  row_list <- list()
  i <- 1

  # populate rowlist
  for (trial_num_temp in df$trial_num) {
    if (is.na(trial_num_temp)) {
      next
    } else {
      trial_row <- df %>%
        filter(trial_num == trial_num_temp)

      # generate trial_df from the time column
      trial_df <- data.frame(
        time = convert_cell_to_numvec(trial_row$time)
      )

      # loop through colums in trial_row
      for (col in colnames(trial_row)) {
        # if col is not time
        if (col != "time") {
          # if trial_row[col] is a character string starting with "["
          if (typeof(trial_row[[col]]) == "character") {
            if (startsWith(trial_row[[col]], "[")) {
              # convert to numeric vector
              trial_df[col] <- convert_cell_to_numvec(trial_row[[col]])
            } else {
              # paste as is (will repeat)
              trial_df[col] <- trial_row[[col]]
            }
          } else {
            # paste as is (will repeat)
            trial_df[col] <- trial_row[[col]]
          }
        }
      }
      row_list[[i]] <- trial_df

      i <- i + 1
    }
  }

  return(do.call(rbind, row_list))
}

# get minimum values for plotting
get_min_val <- function(df) {
  # get min of mouse_x and target_x
  min_val_x <- min(df$mouse_x)
  # if target_x column exists
  if ("target_x" %in% colnames(df)) {
    # get min of target_x and min_val
    min_val_x <- min(df$target_x, min_val_x)
  }

  # repeat for mouse_y and target_y
  min_val_y <- min(df$mouse_y)
  if ("target_y" %in% colnames(df)) {
    min_val_y <- min(df$target_y, min_val_y)
  }

  return(c(min_val_x, min_val_y))
}

# get maximum values for plotting
get_max_val <- function(df) {
  # get max of mouse_x and target_x
  max_val_x <- max(df$mouse_x)
  # if target_x column exists
  if ("target_x" %in% colnames(df)) {
    # get max of target_x and max_val
    max_val_x <- max(df$target_x, max_val_x)
  }

  # repeat for mouse_y and target_y
  max_val_y <- max(df$mouse_y)
  if ("target_y" %in% colnames(df)) {
    max_val_y <- max(df$target_y, max_val_y)
  }

  return(c(max_val_x, max_val_y))
}

# make fit_df given time, mouse_x, mouse_y, home_x, home_y
make_fitDF <- function(step_df) {
  # add a distance row
  step_df$distance <- step_df %>%
    transmute(mouse_x = mouse_x - home_x, mouse_y - home_y) %>%
    apply(1, vector_norm)

  # fit a spline to the distance data
  if (length(unique(step_df$time)) >= 4) {
    fit_fun <- smooth.spline(x = step_df$time, y = step_df$distance, df = 10)

    # add a spline column
    step_df$spline <- predict(fit_fun, step_df$time)$y

    # add a speed column
    step_df$speed <- predict(fit_fun, step_df$time, deriv = 1)$y
  } else {
    step_df$spline <- 0
    step_df$speed <- 0
  }

  return(step_df)
}

# Functions from SMCL package
convert_cell_to_numvec <- function(v) {

  # remove opening square bracket:
  v <- gsub("\\[", replacement = "", x = v)
  # remove closing square bracket:
  v <- gsub("]", replacement = "", x = v)
  # split by commas:
  v <- strsplit(v, ",")
  # convert to numeric:
  v <- lapply(v, FUN = as.numeric)
  # make vector:
  v <- as.vector(unlist(v))

  return(v)
}