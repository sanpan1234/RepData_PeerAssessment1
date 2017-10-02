#pacakges used
library(dplyr)
library(ggplot2)
#unzip data file
unzip("activity.zip")
#read in data (tbl_df is useful for dplyr for a better structure)
adata <- tbl_df(read.csv("activity.csv", stringsAsFactors = FALSE))
#convert dates
adata$date <- as.Date(adata$date)
str(adata)
# Required steps:
# Calculate the total number of steps taken per day
# Make a histogram of the total number of steps taken each day
# Calculate and report the mean and median of the total number of steps taken per day
# Group and Summarize using dplyr functions 
adata_grouped <- adata %>% filter(!is.na(steps)) %>%  group_by(date)
total_steps_per_day <- summarise(adata_grouped, total_steps = sum(steps))
hist(total_steps_per_day$total_steps, ylab = "Frequency", xlab = "Total Steps / Day", 
     main = "Histogram: Total Steps Taken Each Day", breaks = 10)
mean_total_steps_per_day <- mean(total_steps_per_day$total_steps)
mean_total_steps_per_day
median_total_steps_per_day <- median(total_steps_per_day$total_steps)
median_total_steps_per_day