---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---
Reproducible Research Week 2 Project
====================================

```r
library(ggplot2)

library(dplyr)
```

```
## 
## Attaching package: 'dplyr'
```

```
## The following objects are masked from 'package:stats':
## 
##     filter, lag
```

```
## The following objects are masked from 'package:base':
## 
##     intersect, setdiff, setequal, union
```

```r
if(!file.exists("./data"))
  {
    dir.create("./data")
  }
fileUrl <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"
download.file(fileUrl,destfile="./data/activity.zip")

unzip(zipfile="./data/activity.zip",exdir="./data")
activity <- read.csv("./data/activity.csv")
activity$date <- as.Date(activity$date)

head(activity)
```

```
##   steps       date interval
## 1    NA 2012-10-01        0
## 2    NA 2012-10-01        5
## 3    NA 2012-10-01       10
## 4    NA 2012-10-01       15
## 5    NA 2012-10-01       20
## 6    NA 2012-10-01       25
```


#total number of steps taken per day

```r
stepsPerDay <- activity %>%
  group_by(date) %>%
    summarize(sumsteps = sum(steps, na.rm = TRUE)) 
```

#Display of first 10 rows of data

```r
head(stepsPerDay,10)
```

```
## # A tibble: 10 x 2
##    date       sumsteps
##    <date>        <int>
##  1 2012-10-01        0
##  2 2012-10-02      126
##  3 2012-10-03    11352
##  4 2012-10-04    12116
##  5 2012-10-05    13294
##  6 2012-10-06    15420
##  7 2012-10-07    11015
##  8 2012-10-08        0
##  9 2012-10-09    12811
## 10 2012-10-10     9900
```


#histogram of the total number of steps taken each day

```r
hist(stepsPerDay$sumsteps, main="Histogram of Daily Steps",col="blue", xlab="Steps", ylim = c(0,30))
```

![](PA1_template_files/figure-html/unnamed-chunk-4-1.png)<!-- -->

#mean and median of the total number of steps taken per day

```r
meanPreNA <- round(mean(stepsPerDay$sumsteps),digits = 2)
medianPreNA <- round(median(stepsPerDay$sumsteps),digits = 2)
print(paste("The mean is: ", meanPreNA))
```

```
## [1] "The mean is:  9354.23"
```

```r
print(paste("The median is: ", medianPreNA))
```

```
## [1] "The median is:  10395"
```

#average daily activity pattern
#time series plot (i.e. type = “l”|) of the 5-minute interval (x-axis) and the average number of steps taken

```r
stepsPerInterval <- activity %>%
  group_by(interval) %>%
    summarize(meansteps = mean(steps, na.rm = TRUE)) 
```
    
#Display first 10 rows of data

```r
head(stepsPerInterval,10)
```

```
## # A tibble: 10 x 2
##    interval meansteps
##       <int>     <dbl>
##  1        0    1.72  
##  2        5    0.340 
##  3       10    0.132 
##  4       15    0.151 
##  5       20    0.0755
##  6       25    2.09  
##  7       30    0.528 
##  8       35    0.868 
##  9       40    0     
## 10       45    1.47
```

```r
plot(stepsPerInterval$meansteps ~ stepsPerInterval$interval,
     col="blue", type="l", xlab = "5 Minute Intervals", ylab = "Average Number of Steps",
     main = "Steps By Time Interval")
```

![](PA1_template_files/figure-html/unnamed-chunk-7-1.png)<!-- -->

# 5-minute interval, on average across all the days in the dataset, containing the maximum number of steps

```r
print(paste("Interval containing the most steps on average:  ",stepsPerInterval$interval[which.max(stepsPerInterval$meansteps)]))
```

```
## [1] "Interval containing the most steps on average:   835"
```

```r
print(paste("Average steps for that interval: ",round(max(stepsPerInterval$meansteps),digits=2)))
```

```
## [1] "Average steps for that interval:  206.17"
```

#Imputing missing values
#total number of missing values in the dataset 

```r
print(paste("The total number of rows with NA is: ",sum(is.na(activity$steps))))
```

```
## [1] "The total number of rows with NA is:  2304"
```

#strategy for filling in all of the missing values in the dataset

```r
head(activity,10)
```

```
##    steps       date interval
## 1     NA 2012-10-01        0
## 2     NA 2012-10-01        5
## 3     NA 2012-10-01       10
## 4     NA 2012-10-01       15
## 5     NA 2012-10-01       20
## 6     NA 2012-10-01       25
## 7     NA 2012-10-01       30
## 8     NA 2012-10-01       35
## 9     NA 2012-10-01       40
## 10    NA 2012-10-01       45
```

```r
activityNoNA <- activity  
  for (i in 1:nrow(activity))
    {
      if(is.na(activity$steps[i]))
        {
          activityNoNA$steps[i]<- stepsPerInterval$meansteps[activityNoNA$interval[i] ==  stepsPerInterval$interval]
        }
    }
head(activityNoNA,10)
```

```
##        steps       date interval
## 1  1.7169811 2012-10-01        0
## 2  0.3396226 2012-10-01        5
## 3  0.1320755 2012-10-01       10
## 4  0.1509434 2012-10-01       15
## 5  0.0754717 2012-10-01       20
## 6  2.0943396 2012-10-01       25
## 7  0.5283019 2012-10-01       30
## 8  0.8679245 2012-10-01       35
## 9  0.0000000 2012-10-01       40
## 10 1.4716981 2012-10-01       45
```

#steps taken each day

```r
stepsPerDay <- activityNoNA %>%
  group_by(date) %>%
    summarize(sumsteps = sum(steps, na.rm = TRUE)) 
    head(stepsPerDay,10)
```

```
## # A tibble: 10 x 2
##    date       sumsteps
##    <date>        <dbl>
##  1 2012-10-01   10766.
##  2 2012-10-02     126 
##  3 2012-10-03   11352 
##  4 2012-10-04   12116 
##  5 2012-10-05   13294 
##  6 2012-10-06   15420 
##  7 2012-10-07   11015 
##  8 2012-10-08   10766.
##  9 2012-10-09   12811 
## 10 2012-10-10    9900
```

```r
hist(stepsPerDay$sumsteps, main = "Histogram of Daily Steps", col="blue", xlab="Steps")
```

![](PA1_template_files/figure-html/unnamed-chunk-11-1.png)<!-- -->

```r
meanPostNA <- round(mean(stepsPerDay$sumsteps), digits = 2)
medianPostNA <- round(median(stepsPerDay$sumsteps), digits = 2)

print(paste("The mean is: ", mean(meanPostNA)))
```

```
## [1] "The mean is:  10766.19"
```

```r
print(paste("The median is: ", median(medianPostNA)))
```

```
## [1] "The median is:  10766.19"
```

```r
NACompare <- data.frame(mean = c(meanPreNA,meanPostNA),median = c(medianPreNA,medianPostNA))
rownames(NACompare) <- c("Pre NA Transformation", "Post NA Transformation")
print(NACompare)
```

```
##                            mean   median
## Pre NA Transformation   9354.23 10395.00
## Post NA Transformation 10766.19 10766.19
```


#differences in activity patterns between weekdays and weekends
#Create a new factor variable in the dataset

```r
activityDoW <- activityNoNA
activityDoW$date <- as.Date(activityDoW$date)
activityDoW$day <- ifelse(weekdays(activityDoW$date) %in% c("Saturday", "Sunday"), "weekend", "weekday")
activityDoW$day <- as.factor(activityDoW$day)
```

#panel plot containing a time series plot 

```r
activityWeekday <- filter(activityDoW, activityDoW$day == "weekday")
activityWeekend <- filter(activityDoW, activityDoW$day == "weekend")

activityWeekday <- activityWeekday %>%
  group_by(interval) %>%
    summarize(steps = mean(steps)) 
    activityWeekday$day <- "weekday"

activityWeekend <- activityWeekend %>%
  group_by(interval) %>%
    summarize(steps = mean(steps)) 
    activityWeekend$day <- "weekend"

wkdayWkend <- rbind(activityWeekday, activityWeekend)
wkdayWkend$day <- as.factor(wkdayWkend$day)


g <- ggplot (wkdayWkend, aes (interval, steps))
  g + geom_line() + facet_grid (day~.) + 
  theme(axis.text = element_text(size = 12),axis.title = element_text(size = 14)) + 
  labs(y = "Number of Steps") + labs(x = "Interval") + 
  ggtitle("Average Number of Steps - Weekday vs. Weekend") + 
  theme(plot.title = element_text(hjust = 0.5))
```

![](PA1_template_files/figure-html/unnamed-chunk-13-1.png)<!-- -->
