#Functions for Arik NPB101D Science Communication Project


#confidence intervals
#functions from: https://community.rstudio.com/t/computing-confidence-intervals-with-dplyr/31868
lower_ci <- function(mean, se, n, conf_level = 0.95){
  lower_ci <- mean - qt(1 - ((1 - conf_level) / 2), n - 1) * se
}
upper_ci <- function(mean, se, n, conf_level = 0.95){
  upper_ci <- mean + qt(1 - ((1 - conf_level) / 2), n - 1) * se
}

#get rid of scientific notation for p-values
options(scipen=999, digits = 4)