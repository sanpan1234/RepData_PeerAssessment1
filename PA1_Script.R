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
# What is the average daily activity pattern?
# Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)
# Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?
adata_grouped <- adata %>% filter(!is.na(steps)) %>%  group_by(interval)
#mean across days 
average_across_days <- adata_grouped %>% summarise(average_steps_per_interval = mean(steps))
#time series
plot(average_across_days, type="l", col="darkgreen", ylab = "Average Steps / Interval",
     xlab = "Interval", main = "Average Daily Activity Pattern", xlim = c(0, 2400))
max_average_steps <- average_across_days[which.max(average_across_days$average_steps_per_interval),]
#Interval 835 appears to have the maximum number of steps, on average across all the days
max_average_steps

# Imputing missing values
# Note that there are a number of days/intervals where there are missing values (coded as NA). The presence of missing days may introduce bias into some calculations or summaries of the data.
# Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)
sum(is.na(adata$steps))
# Devise a strategy for filling in all of the missing values in the dataset. 
# The missing values would be imputed by re-using the dataset that we already created in the 
# previous process - using mean for the 5-minute interval (across days)
# Create a new dataset that is equal to the original dataset but with the missing data filled in.
adata_new <- adata
adata_new[is.na(adata$steps),1] <- average_across_days$average_steps_per_interval
# Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?
adata_new_grouped <- adata_new %>% group_by(date)
total_steps_per_day <- summarise(adata_new_grouped, total_steps = sum(steps))
hist(total_steps_per_day$total_steps, ylab = "Frequency", xlab = "Total Steps / Day", 
     main = "Histogram: Total Steps Taken Each Day", breaks = 10)
mean_total_steps_per_day <- mean(total_steps_per_day$total_steps)
mean_total_steps_per_day
median_total_steps_per_day <- median(total_steps_per_day$total_steps)
median_total_steps_per_day

# Create a new factor variable in the dataset with two levels – “weekday” and “weekend” 
# indicating whether a given date is a weekday or weekend day.
adata_new$weekday <- weekdays(adata_new$date)
adata_new$day_type <- as.factor(ifelse(adata_new$weekday == "Sunday" | adata_new$weekday == "Saturday", 
                             "weekend", "weekday"))
# Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) 
#and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). 
mean_steps_interval_daytype <- adata_new %>% group_by(day_type, interval) %>% 
  summarise(mean_steps = mean(steps))
#use lattice plotting system and create a panel plot
library(lattice)
xyplot(mean_steps~interval|day_type, mean_steps_interval_daytype, type="l",
       xlab = "Interval", ylab="No. of Steps", main="Avg. Daily Steps by Weekday Type",
       strip = TRUE, groups = day_type, col=c("darkgreen", "darkblue"),
       layout = c(1, 2))
