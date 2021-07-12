---
title: "Project Assignment 2"
author: "Ángel González"
date: "7/9/2021"
output:
  html_document: 
    keep_md: yes
  pdf_document: default
---




First, lets load the data

```r
#Data
library(ggplot2)
library(readr)
activity <-read_csv("data/activity.csv")
```

```
## 
## ── Column specification ────────────────────────────────────────────────────────
## cols(
##   steps = col_double(),
##   date = col_date(format = ""),
##   interval = col_double()
## )
```

## What is the mean of the total number of steps taken per day?
1. Calculate the total number of steps taken per day

```r
steps_day= tapply(activity$steps, activity$date,sum, na.rm=TRUE)

df= as.data.frame(steps_day)

steps=setNames(cbind(rownames(df),df,row.names= NULL),c("Date","Steps") )

print(head(steps,10))
```

```
##          Date Steps
## 1  2012-10-01     0
## 2  2012-10-02   126
## 3  2012-10-03 11352
## 4  2012-10-04 12116
## 5  2012-10-05 13294
## 6  2012-10-06 15420
## 7  2012-10-07 11015
## 8  2012-10-08     0
## 9  2012-10-09 12811
## 10 2012-10-10  9900
```

2. Make a histogram of the total number of steps taken each day

```r
ggplot(steps, aes(Steps))+
  geom_histogram(fill="#FF3563",col="black", binwidth = 2500)+
  labs(x="Steps", y="Frequency", title="Steps per day")+
    theme(plot.title = element_text(hjust = 0.5))
```

![](ReproducibleResearch_files/figure-html/unnamed-chunk-3-1.png)<!-- -->

3. Calculate and report the mean and median of the total number of steps taken per day

```r
mean= round(mean(steps$Steps),2)
median=round(median(steps$Steps),2)
```
The mean is 9354.23 and the median is 10395

## What is the average daily activity pattern?

1. Make a time series plot (i.e. 
type = "l"
type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)

```r
avgsteps= aggregate(steps~interval , data=activity, FUN= "mean")

ggplot(avgsteps, aes(interval,steps))+
  geom_line(color="#0099A9")+
  labs(x="Interval", y= "Number of Steps", title="Average of steps per 5 minutes interval")+
  theme(plot.title = element_text(hjust = 0.5))
```

![](ReproducibleResearch_files/figure-html/unnamed-chunk-5-1.png)<!-- -->



2. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?


```r
max_row= subset(avgsteps, steps==max(avgsteps$steps))
max= max_row[[1]]
```

The 5-minute interval that contains the maximum number of steps is 835. 


## Imputing missing values

1. Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with 
NAs)

```r
num_na= as.numeric(table(complete.cases(activity)))[1]
```
There are 2304 missing values in the data set


2. Devise a strategy for filling in all of the missing values in the dataset. 
I'm going to replace all the NAs with the average of the steps in a month


```r
activity[is.na(activity)]=mean(activity$steps, na.rm=TRUE)
```

3. Create a new dataset that is equal to the original dataset but with the missing data filled in.

```r
#Activity set has already the missing values filled in
head(activity)
```

```
## # A tibble: 6 x 3
##   steps date       interval
##   <dbl> <date>        <dbl>
## 1  37.4 2012-10-01        0
## 2  37.4 2012-10-01        5
## 3  37.4 2012-10-01       10
## 4  37.4 2012-10-01       15
## 5  37.4 2012-10-01       20
## 6  37.4 2012-10-01       25
```

4. Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day.


```r
new_steps= tapply(activity$steps, activity$date,sum, na.rm=TRUE)
df= as.data.frame(steps_day)

new_steps=setNames(cbind(rownames(df),df,row.names= NULL),c("Date","Steps") )

ggplot(new_steps, aes(Steps))+
  geom_histogram(fill="#8FC27F",col="black", binwidth = 2500)+
  labs(x="Steps", y="Frequency", title="Steps per day")+
    theme(plot.title = element_text(hjust = 0.5))
```

![](ReproducibleResearch_files/figure-html/unnamed-chunk-10-1.png)<!-- -->
#### New mean and median

```r
new_mean= round(mean(new_steps$Steps),2)
new_median=round(median(new_steps$Steps),2)
```

The mean is 9354.23 and the median is 10395
There are no significant changes. Thus, there is no impact on the estimates if we change the NAs values.


## Are there differences in activity patterns between weekdays and weekends?

1. Create a new factor variable in the dataset with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day.


```r
##add day type variable
# date with the appropriate format
activity$date= as.Date(activity$date, format= "%Y-%m-%d")
#weekdays
activity$weekday= weekdays(activity$date)
# Day type variabke
activity$day_type= ifelse(activity$weekday=='Saturday' | activity$weekday=='Sunday', 'weekend','weekday')
head(activity,10)
```

```
## # A tibble: 10 x 5
##    steps date       interval weekday day_type
##    <dbl> <date>        <dbl> <chr>   <chr>   
##  1  37.4 2012-10-01        0 Monday  weekday 
##  2  37.4 2012-10-01        5 Monday  weekday 
##  3  37.4 2012-10-01       10 Monday  weekday 
##  4  37.4 2012-10-01       15 Monday  weekday 
##  5  37.4 2012-10-01       20 Monday  weekday 
##  6  37.4 2012-10-01       25 Monday  weekday 
##  7  37.4 2012-10-01       30 Monday  weekday 
##  8  37.4 2012-10-01       35 Monday  weekday 
##  9  37.4 2012-10-01       40 Monday  weekday 
## 10  37.4 2012-10-01       45 Monday  weekday
```

2. Make a panel plot containing a time series plot (i.e. 
type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis)


```r
timeser= aggregate(steps~interval+day_type, data= activity, FUN="mean")

ggplot(timeser, aes(interval,steps))+
  geom_line(col="#9A7BB0")+
  facet_wrap(day_type~.)+
  labs(x="Interval of time", y="Number of steps", title= "Average steps per time interval by type of day")+
  theme(plot.title = element_text(hjust = 0.5, size=11))
```

![](ReproducibleResearch_files/figure-html/unnamed-chunk-13-1.png)<!-- -->

