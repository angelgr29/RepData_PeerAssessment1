---
title: "Project Assignment 2"
author: "Ángel González"
date: "7/9/2021"
output: html_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```


First, lets load the data
```{r}
#Data
library(ggplot2)
library(readr)
activity <-read_csv("data/activity.csv")
```

## What is the mean of the total number of steps taken per day?
1. Calculate the total number of steps taken per day
```{r}
steps_day= tapply(activity$steps, activity$date,sum, na.rm=TRUE)

df= as.data.frame(steps_day)

steps=setNames(cbind(rownames(df),df,row.names= NULL),c("Date","Steps") )

print(steps)
```

2. Make a histogram of the total number of steps taken each day
```{r}
ggplot(steps, aes(Steps))+
  geom_histogram(fill="#FF3563",col="black", binwidth = 2500)+
  labs(x="Steps", y="Frequency", title="Steps per day")+
    theme(plot.title = element_text(hjust = 0.5))
```

3. Calculate and report the mean and median of the total number of steps taken per day
```{r}
mean= round(mean(steps$Steps),2)
median=round(median(steps$Steps),2)
```
The mean is `r mean` and the median is `r format(median, scientific= FALSE)`

## What is the average daily activity pattern?

1. Make a time series plot (i.e. 
type = "l"
type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)
```{r}
avgsteps= aggregate(steps~interval , data=activity, FUN= "mean")

ggplot(avgsteps, aes(interval,steps))+
  geom_line(color="#0099A9")+
  labs(x="Interval", y= "Number of Steps", title="Average of steps per 5 minutes interval")+
  theme(plot.title = element_text(hjust = 0.5))
  
```



2. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

```{r}
max_row= subset(avgsteps, steps==max(avgsteps$steps))
max= max_row[[1]]
```

The 5-minute interval that contains the maximum number of steps is `r max`. 


## Imputing missing values

1. Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with 
NAs)
```{r}
num_na= as.numeric(table(complete.cases(activity)))[1]

```
There are `r num_na` missing values in the data set


2. Devise a strategy for filling in all of the missing values in the dataset. 
I'm going to replace all the NAs with the average of the steps in a month

```{r}
activity[is.na(activity)]=mean(activity$steps, na.rm=TRUE)
```

3. Create a new dataset that is equal to the original dataset but with the missing data filled in.
```{r}
#Activity set has already the missing values filled in
head(activity)
```

4. Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day.

```{r}
new_steps= tapply(activity$steps, activity$date,sum, na.rm=TRUE)
df= as.data.frame(steps_day)

new_steps=setNames(cbind(rownames(df),df,row.names= NULL),c("Date","Steps") )

ggplot(new_steps, aes(Steps))+
  geom_histogram(fill="#8FC27F",col="black", binwidth = 2500)+
  labs(x="Steps", y="Frequency", title="Steps per day")+
    theme(plot.title = element_text(hjust = 0.5))
```
#### New mean and median
```{r}
new_mean= round(mean(new_steps$Steps),2)
new_median=round(median(new_steps$Steps),2)
```

The mean is `r new_mean` and the median is `r format(new_median, scientific= FALSE)`
There are no significant changes. Thus, there is no impact on the estimates if we change the NAs values.


## Are there differences in activity patterns between weekdays and weekends?

1. Create a new factor variable in the dataset with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day.

```{r}
##add day type variable
# date with the appropriate format
activity$date= as.Date(activity$date, format= "%Y-%m-%d")
#weekdays
activity$weekday= weekdays(activity$date)
# Day type variabke
activity$day_type= ifelse(activity$weekday=='Saturday' | activity$weekday=='Sunday', 'weekend','weekday')
head(activity,10)
```

2. Make a panel plot containing a time series plot (i.e. 
type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis)

```{r}
timeser= aggregate(steps~interval+day_type, data= activity, FUN="mean")

ggplot(timeser, aes(interval,steps))+
  geom_line(col="#9A7BB0")+
  facet_wrap(day_type~.)+
  labs(x="Interval of time", y="Number of steps", title= "Average steps per time interval by type of day")+
  theme(plot.title = element_text(hjust = 0.5, size=11))


```

