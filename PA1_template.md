---
title: "Reproducible Research: Peer Assessment 1"
author: "Udaya K Tejwani"
date: "March 9, 2021"
output:
  html_document:
    keep_md: yes
  word_document: default
---


## Loading and preprocessing the data
##### 1. Load the data (i.e. read.csv())


```r
setwd("C:\\Users\\Udaya\\Documents\\DataScience\\Course 5\\RepData_PeerAssessment1")

if(!file.exists('activity.csv')){

    unzip('activity.zip')

}

activityData <- read.csv('activity.csv')
```
##### 2. Process/transform the data (if necessary) into a format suitable for your analysis


```r
activityData$date <- as.Date(activityData$date)
```

## What is mean total number of steps taken per day?
##### 1. Calculate the total number of steps taken per day

```r
sum_steps <- tapply(activityData$steps, activityData$date, sum, na.rm=TRUE)
```
##### 2. Make a histogram of the total number of steps taken each day

```r
png("figures/fig1.png", width=648, height=432)
hist(sum_steps, 
      breaks=seq(from=0, to=25000, by=750),
      col="green", 
      xlab="Total number of steps", 
      ylim=c(0, 20), 
      main="Histogram of the total number of steps taken each \n day(NA removed)")
dev.off()
```

```
## png 
##   2
```

##### 3. Calculate and report the mean and median total number of steps taken per day


```r
stepsTakenPerDayMean <- mean(sum_steps)

stepsTakenPerDayMedian <- median(sum_steps)

print(paste("Mean: ", format(stepsTakenPerDayMean, nsmall = 2)))
```

```
## [1] "Mean:  9354.23"
```

```r
print(paste("Median:  ", format(stepsTakenPerDayMedian, nsmall = 2)))
```

```
## [1] "Median:   10395"
```
## What is the average daily activity pattern?

```r
averageSteps <- aggregate(x=list(meanSteps=activityData$steps), by=list(interval=activityData$interval), FUN=sum, na.rm=TRUE)
colnames(averageSteps)<-c("interval","steps")
```

##### 1. Make a time series plot


```r
png("figures/fig2.png", width=648, height=432)
ggplot(data=averageSteps, aes(x=interval, y=steps)) +

    geom_line() +

    xlab("5-minute interval") +

    ylab("average number of steps taken") 
dev.off()
```

```
## png 
##   2
```

##### 2.  Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?


```r
averageSteps[which(averageSteps$steps == max(averageSteps$steps)),]
```

```
##     interval steps
## 104      835 10927
```

## Imputing missing values
##### 1. Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with \color{red}{\verb|NA|} NAs)

```r
missingValues <- sum(is.na(activityData))
missingValues
```

```
## [1] 2304
```
##### 2 and 3. Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.  Create a new dataset that is equal to the original dataset but with the missing data filled in.

I will replace all the missing values in the dataset with the median for that 5-minute interval.


```r
replacewithmedian <- function(x) replace(x, is.na(x), median(x, na.rm = TRUE))
mediandata <- activityData%>% group_by(interval) %>% mutate(steps= replacewithmedian(steps))
head(mediandata)
```

```
## # A tibble: 6 x 3
## # Groups:   interval [6]
##   steps date       interval
##   <int> <date>        <int>
## 1     0 2012-10-01        0
## 2     0 2012-10-01        5
## 3     0 2012-10-01       10
## 4     0 2012-10-01       15
## 5     0 2012-10-01       20
## 6     0 2012-10-01       25
```

```r
sum(is.na(mediandata))
```

```
## [1] 0
```

##### 4. Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?

```r
sum_steps_updated <- tapply(mediandata$steps, mediandata$date, sum, na.rm=TRUE)
png("figures/fig3.png", width = 648, height=432)
hist(sum_steps_updated, 
      breaks=seq(from=0, to=25000, by=750),
      col="pink", 
      xlab="Total number of steps", 
      ylim=c(0, 20), 
      main="Histogram of the total number of steps taken each \n day(NA replaced with median)")
dev.off()
```

```
## png 
##   2
```




##### Calculate and report the mean and median total number of steps taken per day. 

```r
stepsTakenPerDayMeanUpdated <- mean(sum_steps_updated)

stepsTakenPerDayMedianUpdated <- median(sum_steps_updated)

print(paste("Mean: ", format(stepsTakenPerDayMeanUpdated, nsmall = 2)))
```

```
## [1] "Mean:  9503.869"
```

```r
print(paste("Median:  ", format(stepsTakenPerDayMedianUpdated, nsmall = 2)))
```

```
## [1] "Median:   10395"
```

```r
summary(sum_steps)
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##       0    6778   10395    9354   12811   21194
```

```r
summary(sum_steps_updated)
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##      41    6778   10395    9504   12811   21194
```
The impact of imputing the missing data is that the median remains the same but the mean changes.  This fact is also reflected in the histograms of the data before and after imputing.  In the left most portion of the histogram, the height of the leftmost bar is reduced and an additional bar appears.  

## Are there differences in activity patterns between weekdays and weekends?
##### 1. Create a new factor variable in the dataset with two levels - "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.

```r
##meandata$date <- as.Date(meandata$date)
mediandata$WeekendOrWeekday <- ifelse(weekdays(as.Date(mediandata$date)) %in% c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday"), "Weekday", "Weekend")
head(mediandata)
```

```
## # A tibble: 6 x 4
## # Groups:   interval [6]
##   steps date       interval WeekendOrWeekday
##   <int> <date>        <int> <chr>           
## 1     0 2012-10-01        0 Weekday         
## 2     0 2012-10-01        5 Weekday         
## 3     0 2012-10-01       10 Weekday         
## 4     0 2012-10-01       15 Weekday         
## 5     0 2012-10-01       20 Weekday         
## 6     0 2012-10-01       25 Weekday
```

##### 2. Make a panel plot containing a time series plot (i.e., type = "l"\color{red}{\verb|type = "l"|} type="l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis).


```r
library(grid)
library(gridExtra)
```

```
## 
## Attaching package: 'gridExtra'
```

```
## The following object is masked from 'package:dplyr':
## 
##     combine
```

```r
averageSteps_updated_wd <- aggregate(x=list(meanSteps=mediandata$steps), by=list(interval=mediandata$interval), FUN=sum, na.rm=TRUE, mediandata$WeekendOrWeekday=="Weekday")
colnames(averageSteps_updated_wd)<-c("interval","steps")
averageSteps_updated_wnd <- aggregate(x=list(meanSteps=mediandata$steps), by=list(interval=mediandata$interval), FUN=sum, na.rm=TRUE, mediandata$WeekendOrWeekday=="Weekend")
colnames(averageSteps_updated_wnd)<-c("interval","steps")
png("figures/fig4.png", width = 648, height = 432)
wdplot <- ggplot(data=averageSteps_updated_wd, aes(x=interval, y=steps)) +

    geom_line() + 
  
    xlab("5-minute interval") +
  
    ylab("average # of steps taken") +

    ggtitle("Comparison of Average Number of Steps in Each Interval\n on Weekdays")

wndplot <- ggplot(data=averageSteps_updated_wnd, aes(x=interval, y=steps)) +

    geom_line() + 
  
    xlab("5-minute interval") +

    ylab("average # of steps taken") +
  
    ggtitle("on Weekends")

grid.arrange(wdplot, wndplot, nrow = 2)
dev.off()
```

```
## png 
##   2
```
