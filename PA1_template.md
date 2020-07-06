---
title: "Reproducible Research Assignment 1"
author: "Sunita Patil"
date: "7/4/2020"
output: 
  html_document: 
    keep_md: yes
---



## Activity Data
Personal acitivty data was collected at 5 minute intervals for months of October & November 2012 using activity monitoring devices. 
The data was downloaded from Coursera's Reproducible Research course and analyzed.


```r
url<-"https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"
download.file(url, destfile = "activity", method = "curl")
unzip("activity")
activity<-read.csv("activity.csv")
```

##Plot of total number of steps taken each day

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
TotalDailySteps<-activity %>% group_by(date) %>% summarise(TotalDailySteps=sum(steps))
hist(TotalDailySteps$TotalDailySteps, col = "blue", xlab="Total daily steps", ylab="frequency", main="Total daily steps")
```

![](PA1_template_files/figure-html/plot1-1.png)<!-- -->

## Mean & median steps taken each day

```r
MeanSteps<-activity %>% group_by(date) %>% summarise(MeanDailySteps=mean(steps, na.rm=TRUE))
print(MeanSteps)
```

```
## # A tibble: 61 x 2
##    date       MeanDailySteps
##    <fct>               <dbl>
##  1 2012-10-01        NaN    
##  2 2012-10-02          0.438
##  3 2012-10-03         39.4  
##  4 2012-10-04         42.1  
##  5 2012-10-05         46.2  
##  6 2012-10-06         53.5  
##  7 2012-10-07         38.2  
##  8 2012-10-08        NaN    
##  9 2012-10-09         44.5  
## 10 2012-10-10         34.4  
## # … with 51 more rows
```

```r
MedianSteps<-activity %>% group_by(date) %>% summarise(MedianDailySteps=median(steps))
print(MedianSteps)
```

```
## # A tibble: 61 x 2
##    date       MedianDailySteps
##    <fct>                 <dbl>
##  1 2012-10-01               NA
##  2 2012-10-02                0
##  3 2012-10-03                0
##  4 2012-10-04                0
##  5 2012-10-05                0
##  6 2012-10-06                0
##  7 2012-10-07                0
##  8 2012-10-08               NA
##  9 2012-10-09                0
## 10 2012-10-10                0
## # … with 51 more rows
```
## Time series plot of average number of steps taken

```r
AverageStepsPerInterval<-activity %>% group_by(interval) %>% summarise(AverageSteps=mean(steps, na.rm=TRUE))
plot(AverageStepsPerInterval$interval, AverageStepsPerInterval$AverageSteps, xlab="5 min interval", ylab="Average Steps per Interval", type="l", col="blue")
```

![](PA1_template_files/figure-html/plot2-1.png)<!-- -->

## Time interval with maximum number of steps

```r
print(AverageStepsPerInterval[AverageStepsPerInterval$AverageSteps==max(AverageStepsPerInterval$AverageSteps),]$interval)
```

```
## [1] 835
```

## Imputing missing data
Missing values indicated by NAs were replaced by average steps taken for the corresponding 5 min interval

```r
for (i in 1:nrow(activity)){
   if(is.na(activity$steps[i])){
     x<-activity$interval[i]
    activity$steps[i]<-AverageStepsPerInterval[AverageStepsPerInterval$interval==x,]$AverageSteps
    }
}
sum(is.na(activity$steps))
```

```
## [1] 0
```

## Histogram showing total number of steps taken each day after imputing missing values.

```r
TotalDailySteps<-activity %>% group_by(date) %>% summarise(TotalDailySteps=sum(steps))
hist(TotalDailySteps$TotalDailySteps, col = "blue", xlab="Total daily steps", ylab="frequency", main="Total daily steps")
```

![](PA1_template_files/figure-html/plot3-1.png)<!-- -->

## Panel plot comparing weekend activity to weekdays activity

```r
activity$date<-as.Date(activity$date)
activity$weekdays<-weekdays(activity$date, abbreviate = FALSE)
WeekdayData<-activity[activity$weekdays=="Monday" | activity$weekdays=="Tuesday" | activity$weekdays=="Wednesday" |activity$weekdays=="Thursday" | activity$weekdays=="Friday", ]
WeekendData<-activity[activity$weekdays=="Sunday" | activity$weekdays=="Saturday", ]
AvgWeekdayStepsPerInterval<-WeekdayData %>% group_by(interval) %>% summarise(AvgSteps=mean(steps))
AvgWeekendStepsPerInterval<-WeekendData %>% group_by(interval) %>% summarise(AvgSteps=mean(steps))

par(mfrow=c(2,1))
par(mar=c(2,2,2,2))
plot(AvgWeekdayStepsPerInterval$interval, AvgWeekdayStepsPerInterval$AvgSteps, type ="l", xlab="Interval", ylab="Number of steps", main="weekday", col="blue", ylim=c(0,250))
plot(AvgWeekendStepsPerInterval$interval, AvgWeekendStepsPerInterval$AvgSteps, type ="l", xlab="Interval", ylab="Number of steps", main="weekend", col="red", ylim=c(0,250))
```

![](PA1_template_files/figure-html/plot4-1.png)<!-- -->







