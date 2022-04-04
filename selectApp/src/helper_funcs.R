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
  # Mean of sample
  vec_mean <- mean(vector, na.rm = TRUE)
  # Error according to t distribution
  error <- qt((interval + 1)/2, df = n - 1) * vec_sd / sqrt(n)
  # Confidence interval as a vector
  # result <- c("lower" = vec_mean - error, "upper" = vec_mean + error)
  return(error)
}

# get the magnitude (euclidian normal) of a vector (this is faster than R's built in norm)
vector_norm <- function(vector){sqrt(sum(vector^2))}

# add maxV
add_maxV_col <- function(df){
  df$maxV <- 0
  
  df$maxV[df$time_s == filter(df, speed == max(speed))[1, ]$time_s] <- 1
  
  # fix maxV if the distance is too small
  if (filter(df, maxV == 1)$distance < max(df$distance)/10){
    #reset maxV
    df$maxV <- 0
    
    # set maxV to 30% of movement
    # note is 2 pointers useful here? Maybe for reaches where the initiation time is very long..
    for (dist in df$distance){
      if (dist > max(df$distance)*3/10){
        df$maxV[df$distance == dist] <- 1
        break
      }
    }
  }
  
  return(df)
}

fixHeaders <- function(df, settings_df){
  # required headers
  df <- df %>%
    rename(trial_num = settings_df$trial_num[1],
           mouse_x = settings_df$mouse_x[1],
           mouse_y = settings_df$mouse_y[1],
           time_s = settings_df$time[1])
  
  # optional headers
  home_headers <- c('home_x', 'home_y')
  
  # loop through optional headers
  for (i in home_headers){
    # test
    temp_header <- settings_df %>% select(!!sym(i))
    temp_header <- as.character(temp_header[1,])
    
    if (temp_header != "NA"){
      # this header exists
      # rename temp_header to i
      df <- df %>%
        rename(!!i := all_of(temp_header))
    }
    else {
      # this header doesn't exist
      # fill this thing with zeros
      df[[i]] <- 0
    }
  }
  
  temp_headers <- c('target_x', 'target_y', 'cursor_x', 'cursor_y')
  
  # loop through optional headers
  for (i in temp_headers){
    # test
    temp_header <- settings_df %>% select(!!sym(i))
    temp_header <- as.character(temp_header[1,])
    
    if (temp_header != "NA"){
      # this header exists
      # rename temp_header to i
      df <- df %>%
        rename(!!i := all_of(temp_header))
    }
    else {
      # this header doesn't exist
      # fill this thing with "NA"s
      df[[i]] <- "NA"
    }
  }
  
  return(df)
}

build_df_from_row <- function(df){
  # Should ONLY take required cols (to save space)
  
  
}
  
# Functions from SMCL package

convertCellToNumVector <- function(v) {
  
  # remove opening square bracket:
  v <- gsub('\\[', replacement='', x=v)
  # remove closing square bracket:
  v <- gsub(']', replacement='', x=v)
  # split by commas:
  v <- strsplit(v, ',')
  # convert to numeric:
  v <- lapply(v, FUN=as.numeric)
  # make vector:
  v <- as.vector(unlist(v))
  
  return(v)
  
}