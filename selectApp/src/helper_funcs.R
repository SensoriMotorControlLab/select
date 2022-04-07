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

  df$max_v[df$time_s == filter(df, speed == max(speed))[1, ]$time_s] <- 1

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

fix_headers <- function(df, settings_df) {
  # required headers
  df <- df %>%
    rename(
      trial_num = settings_df$trial_num[1],
      mouse_x = settings_df$mouse_x[1],
      mouse_y = settings_df$mouse_y[1],
      time_s = settings_df$time[1]
    )
  # optional headers
  home_headers <- c("home_x", "home_y")
  # loop through optional headers
  for (i in home_headers) {
    # test
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
    # test
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

build_df_from_rows <- function(df) {
  # Should ONLY take required cols (to save space)
  # df should already have correct col names

  rowList <- list()
  i <- 1

  # populate rowlist
  for (trial_num_temp in df$trial_num) {
    if (is.na(trial_num_temp)) {
      next
    } else {
      trial_row <- df %>%
        filter(trial_num == trial_num_temp)

      trial_df <- data.frame(
        time_s = convert_cell_to_numvec(trial_row$time_s),
        mouse_x = convert_cell_to_numvec(trial_row$mouse_x),
        mouse_y = convert_cell_to_numvec(trial_row$mouse_y)
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

      rowList[[i]] <- trial_df

      i <- i + 1
      # print(trial_num_temp)
    }
  }

  return(do.call(rbind, rowList))
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