---
title: "PA1_template"
author: "Mark Odejobi"
date: "28 March 2017"
output: html_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
library(dplyr)
library(lattice)
```

---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---


## Loading and preprocessing the data
First, I will remove all variables from the environment
```{r}
activity <- read.csv("activity.csv")
activity$date <- as.Date(activity$date)
act_day <- group_by(activity,by=date)
sumStepsPerDay <- aggregate(steps ~ date, data=activity, 
                            FUN="sum", na.exclude=T)
meanStepsPerInterval <- aggregate(steps ~ interval, data=activity,
                                  FUN="mean", na.exclude=T)
```

##Make the histogram of the total number of steps per day
```{r, echo=FALSE}
hist(sumStepsPerDay$steps, breaks=10, main="Total number of steps per day",
     xlab="Steps per day")

```
## What is mean total number of steps taken per day?

The mean and median steps are calculated as follows:  
```{r}
mean(sumStepsPerDay$steps, na.rm=TRUE)
median(sumStepsPerDay$steps, na.rm=TRUE)

```


## What is the average daily activity pattern?

Next, I will try to visualize the daily activity pattern  

The below plot addresses the following items:

1.  Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)
2.  Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

```{r}
plot(steps ~ interval, data=meanStepsPerInterval, 
       type="l", grid=TRUE, ylab="Number of steps", 
       xlab="5-min. intervals from midnight", 
       main="Average number of steps by 5-minutes intervals")

 max_steps <- meanStepsPerInterval[order(-meanStepsPerInterval$steps),]
 max_steps[1,1]

```


## Imputing missing values

1. Calculate & Report The Number of Missing Values
```{r}
 sum(is.na(activity$steps))
 sum(is.na(activity$date))
 sum(is.na(activity$interval))
```


2. Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.

3. Create a new dataset that is equal to the original dataset but with the missing data filled in.

```{r}
activity_replace <- activity
for (i in 1:nrow(activity_replace)){
        if (is.na(activity_replace$steps[i])){
                sub <- meanStepsPerInterval$steps[meanStepsPerInterval$interval == activity_replace$interval[i]]
                activity_replace$steps[i] <- sub
        }
}
```

4. Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?

```{r}
sumStepsPerDayMissing <- aggregate(steps ~ date, 
                                   data=activity_replace, FUN="sum", na.exclude=T)
hist(sumStepsPerDayMissing$steps, breaks=10, main="Total number of steps per day, missing values edited",
     xlab="Steps per day")
```

The mean and median for the new dataset are computed below and do not differ very much from the mean and emdian in the original dataset

```{r}
mean(sumStepsPerDayMissing$steps, na.rm=TRUE)
median(sumStepsPerDayMissing$steps, na.rm=TRUE)
```

## Are there differences in activity patterns between weekdays and weekends?

1. Create a new factor variable in the dataset with two levels - "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.
2. Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis).
```{r}
activity_replace$day <- "weekday"
activity_replace$day[weekdays(as.Date(activity_replace$date), abb=T) %in% c("Sat","Sun")] <- "weekend"
meanStepsPerIntervalNoMissingDay <- aggregate(steps ~ interval + day, 
                                              data=activity_replace, FUN="mean")
```

```{r, include=FALSE}
   # add this chunk to end of mycode.rmd
   file.rename(from="PA1_template.md", 
               to="README.md")
```
