---
title: "Course Project 1"
author: "Manuel Chacón"
date: "28 de mayo de 2019"
output:
 html_document:
  keep_md: true
---

```r
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
library(ggplot2)
```




## Loading and preprocessing the data

```r
data <- read.csv('activity.csv')
```



## What is mean total number of steps taken per day?

we create  data_step, It's the number steps per day.

```r
data_step<-data %>% 
          group_by(date) %>% 
          summarize(steps_day=sum(steps), steps_mean = mean(steps), steps_median = median(steps,na.rm = TRUE))
```


```r
ggplot(data_step, aes(steps_day))+
  geom_histogram(color = 'red')+ ggtitle('Steps per days')+ylab('Frecuency')+xlab('Number of steps')
```

```
## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.
```

```
## Warning: Removed 8 rows containing non-finite values (stat_bin).
```

![](PA1_template_files/figure-html/unnamed-chunk-4-1.png)<!-- -->


```r
data_mean <- data_step %>% 
  select(date, steps_mean, steps_median)
```

## What is the average daily activity pattern?

```r
data_interval<-data %>% 
  group_by(interval) %>% 
  summarize(interval_mean = mean(steps, na.rm = TRUE))
```



```r
ggplot(data_interval, aes(interval, interval_mean))+geom_line(color='blue')+
  ggtitle('Average per interval')+xlab('Interval')+ylab('Average per interval')
```

![](PA1_template_files/figure-html/unnamed-chunk-7-1.png)<!-- -->

```r
data_interval %>% 
  select(interval, interval_mean) %>% 
  filter(interval_mean == max(interval_mean))
```

```
## # A tibble: 1 x 2
##   interval interval_mean
##      <int>         <dbl>
## 1      835          206.
```

## Imputing missing values
1- We calculate total number of missing value

```r
data %>%
  summarize(steps_na=sum(is.na(steps)),
            date_na=sum(is.na(date)), 
            interval_na=sum(is.na(interval)))
```

```
##   steps_na date_na interval_na
## 1     2304       0           0
```

2.-We create a new data that is equal to the original but with the missing data filled in.

```r
data_copy<-data

data_copy$steps[is.na(data_copy$steps)] <- mean(na.omit(data_copy$steps))
```
3.-Make a histogrom of the total number of steps taken each day and calculate and report the mean and median total number of steps taken per day.

```r
par(mfrow=c(1,2))
data %>%
  group_by(date) %>% 
  summarize(total_steps = sum(steps)) %>% 
  ggplot(aes(total_steps))+
  geom_histogram(color = 'red') +
  ggtitle('With NA')+
  xlab('Number of steps')+
  ylab('Frecuency')
```

```
## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.
```

```
## Warning: Removed 8 rows containing non-finite values (stat_bin).
```

![](PA1_template_files/figure-html/unnamed-chunk-11-1.png)<!-- -->

```r
data_copy %>%
  group_by(date) %>% 
  summarize(total_steps = sum(steps)) %>% 
  ggplot(aes(total_steps))+
  geom_histogram(color = 'blue')+
  ggtitle('Without NA')+
  xlab('Number of steps')+
  ylab('Frecuency')
```

```
## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.
```

![](PA1_template_files/figure-html/unnamed-chunk-11-2.png)<!-- -->

```r
data_copy %>% 
  group_by(date) %>% 
  summarize(steps_day=sum(steps), steps_mean = mean(steps), steps_median = median(steps))
```

```
## # A tibble: 61 x 4
##    date       steps_day steps_mean steps_median
##    <fct>          <dbl>      <dbl>        <dbl>
##  1 2012-10-01    10766.     37.4           37.4
##  2 2012-10-02      126       0.438          0  
##  3 2012-10-03    11352      39.4            0  
##  4 2012-10-04    12116      42.1            0  
##  5 2012-10-05    13294      46.2            0  
##  6 2012-10-06    15420      53.5            0  
##  7 2012-10-07    11015      38.2            0  
##  8 2012-10-08    10766.     37.4           37.4
##  9 2012-10-09    12811      44.5            0  
## 10 2012-10-10     9900      34.4            0  
## # ... with 51 more rows
```



## Are there differences in activity patterns between weekdays and weekends?

1.-Create a new factor variable in the dataset with two levels - "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.


```r
data_week<-data_copy %>% 
  mutate(Weekday = ifelse(weekdays(as.Date(date)) %in% c("lunes","martes",
                                                         'miercoles','jueves','viernes')
                          ,"weekday",'weekend' ) )
```

2.-Make a panel plot containing a time series plot of the 5-minute interval(x-axis) and the average of steps taken, averaged across all weekday days or weekend days(y-axis).


```r
data_weekday <-data_week %>% 
  filter(Weekday == 'weekday') %>% 
  group_by(interval) %>% 
  summarize(interval_step = mean(steps))

data_weekend <-data_week %>% 
  filter(Weekday == 'weekend') %>% 
  group_by(interval) %>% 
  summarize(interval_step = mean(steps))


par(mfrow=c(1,2))
ggplot(data_weekday, aes(interval, interval_step))+geom_line()+ggtitle('Average per interval on weekday')+xlab('Interval')+ylab('Average per interval')
```

![](PA1_template_files/figure-html/unnamed-chunk-14-1.png)<!-- -->

```r
ggplot(data_weekend, aes(interval, interval_step))+geom_line()+ggtitle('Average per interval on weekend')+xlab('Interval')+ylab('Average per interval')
```

![](PA1_template_files/figure-html/unnamed-chunk-14-2.png)<!-- -->

