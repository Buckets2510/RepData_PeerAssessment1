---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---
## Assignment Introduction

The goal of the assignment is to create an R markdown document to answer the 
following questions about activity monitoring data:
1. Code for reading in the dataset and/or processing the data
2. Histogram of the total number of steps taken each day
3. Mean and median number of steps taken each day
4. Time series plot of the average number of steps taken
5. The 5-minute interval that, on average, contains the maximum number of steps
6. Code to describe and show a strategy for imputting missing data
7. Histogram of the total number of steps taken each day after missing values
are imputted
8. Panel plot comparing the average number of steps taken per 5-minute interval
across weekdays and weekends

## Questions 1: Loading and preprocessing the data
### Reading in the data set 
'''{r, echo = TRUE}
library(ggplot2)
library(lubridate)
library(dplyr)
path <- getwd()
unzip("activity.zip", exdir = path)
activityData <- read.csv("activity.csv")
'''

### Initial review of the data
'''{r, echo = TRUE}
head(activityData)
str(activityData)
# format the dates and format the data set
activityData$date <- as.POSIXct(activityData$date, format = "%Y/%m/%d")
days <- weekdays(activityData$date)
activityData <- cbind(activityData, days)
summary(activityData)
'''


## Question 2: What is mean total number of steps taken per day?
Missing values can be ignored in the data set. The summary call shows the
*steps* variable has NA values
'''{r, echo = TRUE}
totalSteps <- with(activityData, aggregate(steps, by = list(date), sum, na.rm = TRUE))
names(totalSteps) <- c("Date", "Steps")
# Change to data frame to use ggplot
totalStepsdf <- data.frame(totalSteps)
# Plot the histogram using ggplot
png("StepPlots.png")
ggplot(totalStepsdf, aes(x = Steps)) + 
geom_histogram(fill = "red", col = "black") + 
labs(title = "Total Steps Taken Per Day", x = "Steps", y = "Frequency")
dev.off()
'''

## Question 3: Report the mean and median of the steps taken per day
'''{r, echo = TRUE}
summarise(totalStepsdf, meanSteps = mean(totalStepsdf$Steps), 
          medianSteps = median(totalStepsdf$Steps))
'''


## Question 4: What is the average daily activity pattern?
'''{r, echo = TRUE}
#time series plot of the average number of steps taken
avgDailyActivity <- with(activityData, aggregate(steps, by = list(interval),
FUN = mean, na.rm = TRUE))
names(avgDailyActivity) <- c("Interval", "Steps")
# Change to data frame to use ggplot
avgDailydf <- data.frame(avgDailyActivity)
# Plot the line graph daily activity interval
png("DailyActivityPlot.png")
ggplot(avgDailydf, aes(x = Interval, y = Steps)) + 
geom_line(col = "blue") + 
labs(title = "Average Number of Steps Per Interval", x = "Interval", 
y = "Average Number of Steps")
dev.off()
'''

## Question 5: The 5-minute interval that, on average, contains the maximum number of steps
avgDailyActivity[which.max(avgDailyActivity$Steps),]$Interval


## Question 6: Code to describe and show a strategy for imputting missing data

'''{r, echo = TRUE}
# Calculate and report the total number of missing values (NAs). We know from the
# preprocessing step, the NAs are located in steps
sum(is.na(activityData$steps))
'''
A strategy is needed to impute the NAs with numeric measurements. The mean will
be used to fill the missing values. The 5 minute interval mean will be taken.

'''{r, echo = TRUE}
impute_steps <- avgDailyActivity$Steps[match(activityData$interval, 
avgDailyActivity$Interval)]
# Create a new data set with the imputed data
activityImputed <- transform(activityData, steps = ifelse(is.na(activityData$steps), 
yes = impute_steps, no = activityData$steps))
totalImputed <- aggregate(steps ~ date, activityImputed, sum)
names(totalImputed) <- c("Date","Steps")
'''

## Question 7: Histogram of the total number of steps taken each day after missing values are imputed
'''{r, echo = TRUE}
# Re-create the histogram from the 2nd question
png("ImputtedStepPlots.png")
ggplot(totalImputed, aes(x = Steps)) + 
geom_histogram(fill = "red", col = "black") + 
labs(title = "Total Steps Taken Per Day", x = "Steps", y = "Frequency")
dev.off()
totalSummary <- summarise(totalStepsdf, meanSteps = mean(totalStepsdf$Steps), 
          medianSteps = median(totalStepsdf$Steps))
imputedSummary <- summarise(totalImputed, meanImput = mean(totalImputed$Steps), 
          medianImput = median(totalImputed$Steps))
cbind(totalSummary, imputedSummary)
'''
The data calculated in both methods are close, but there is a slight variation in the imputed data

## Question 8: Are there differences in activity patterns between weekdays and weekends?
First step is to identify the weekdays or weekends of the imputed data
'''{r, echo = TRUE}
# Create a function to set the weekday or weekend ID
activityData$day <- sapply(activityData$date, function(x) {
if(weekdays(x) == "Saturday" | weekdays(x) == "Sunday")
{y <- "Weekend"}
else {y <- "Weekday"}
y
})
# Create the new day dataset and panel plot
activityDataDayType <- aggregate(steps ~ interval + day, activityData, mean, na.rm = TRUE)
png("PanelDayPlot.png")
ggplot(activityDataDayType, aes(x = interval, y = steps, color = day)) + 
geom_line() + 
labs(title = "Average Daily Steps by Day Type", x = "Interval", y = "Average Number of Steps") + 
facet_wrap(~day, ncol = 1, nrow = 2)
dev.off()
'''

