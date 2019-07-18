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