---
title: "Reproducible Research - Week 2 Assignment"
author: "Avia Peron"
date: "29/08/2017"
output: html_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

First thing to do is to set my working directory to the appropriate folder and load the relevant packages.

```{r, results="hide"}
setwd("C:\\Users\\אביה\\Documents\\R\\Coursera\\Reproducible Research\\Week 2\\repdata_2Fdata%2Factivity")
library(dplyr)
library(ggplot2)
library(lubridate)
```

### Loading and preprocessing the data

1. Loading the data

```{r, results="hide"}
ActData <- read.csv("activity.csv")
```
2. Processing the data for more suitable format 

```{r, results="hide"}
ActData$date <- as.Date(ActData$date)
```

### What is mean total number of steps taken per day?

We use the "dplyr" pacage to sum the total number of steps taken per day. To make the results easy to understand, we will make a histogram.

```{r}
ADSumPerDay <- ActData %>% group_by(date) %>% summarise(SumStepDay = sum(steps, na.rm = TRUE))
qplot(ADSumPerDay$SumStepDay, xlab = "Total step per day",ylab = "Frequency" , binwidth = 1000)
```

Now we will calculate and report the mean and median total number of steps taken per day
```{r}
mean(ADSumPerDay$SumStepDay)
```
```{r}
median(ADSumPerDay$SumStepDay)
```

### What is the average daily activity pattern?

We need to make a time series plot of interval and the average number of steps taken. 

```{r}
ADAvgPerInt <- ActData %>% group_by(interval) %>% summarise(AvgInt = mean(steps, na.rm = TRUE))
AvgPlot <- ggplot(ADAvgPerInt, aes(interval, AvgInt)) + geom_line() + labs(x = "Interval", y = "Avg Steps")
print(AvgPlot)
```

Now we'll found which 5-minute interval contains the maximum number of steps.
```{r}
 head(ADAvgPerInt %>% arrange(-AvgInt), 1)
```

### Imputing missing values

First of all, we'll calculate the total number of missing values in the dataset.

```{r}
sum(is.na(ActData$steps))
```

Now let's duplicate and swich every NA value by the steps means that we found earlier.

```{r}
ActData2 <- ActData
ActData2$steps <- ifelse(is.na(ActData2$steps)== TRUE, ADAvgPerInt$AvgInt[ADAvgPerInt$interval %in% ActData2$interval], ActData2$steps)
```

Now we'll make a histogram like we did before, and then calculate the mean and median to chack if something change.

```{r}
ADSumPerDay2 <- ActData2 %>% group_by(date) %>% summarise(SumStepDay = sum(steps))
qplot(ADSumPerDay2$SumStepDay, xlab = "Total step per day",ylab = "Frequency" , binwidth = 1000)
```
```{r}
mean(ADSumPerDay2$SumStepDay)
```
```{r}
median(ADSumPerDay2$SumStepDay)
```

We can see that our method made the mean and the median equal. 

###Are there differences in activity patterns between weekdays and weekends?

Create a new factor variable to indicating whether a given date is a weekday or weekend day. We will do that with the 'lubridate' package and function 'wday'. 

```{r}
ActData2$weekday <- wday(ActData2$date, label=TRUE)
ActData2$WeekendOrNot <- as.factor(ifelse(ActData2$weekday == "Sat" | ActData2$weekday == "Sun", "Weekend", "Weekday"))
```

 Now we will make a panel plot of interval and the average number of steps taken across all weekday days or weekend days.
 
```{r}
ADAvgPerInt2 <- ActData2 %>% group_by(interval, WeekendOrNot) %>% summarise(AvgInt = mean(steps, na.rm = TRUE))
Weekplot <- ggplot(ADAvgPerInt2, aes(interval, AvgInt)) + geom_line() + facet_grid(WeekendOrNot~.) + labs(x = "Interval", y = "Avg Steps")
print(Weekplot)
```

We can see some difference between weekdays and weekends.