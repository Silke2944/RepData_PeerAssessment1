---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---


## Loading and preprocessing the data

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
activity <- read.csv("activity.csv")
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

```r
tail(activity)
```

```
##       steps       date interval
## 17563    NA 2012-11-30     2330
## 17564    NA 2012-11-30     2335
## 17565    NA 2012-11-30     2340
## 17566    NA 2012-11-30     2345
## 17567    NA 2012-11-30     2350
## 17568    NA 2012-11-30     2355
```
Change the date variable: 

```r
activity$date <- as.Date(activity$date)
str(activity$date)
```

```
##  Date[1:17568], format: "2012-10-01" "2012-10-01" "2012-10-01" "2012-10-01" "2012-10-01" ...
```


## What is mean total number of steps taken per day?

Group steps by day: 

```r
steps_day <- activity %>% 
        group_by(date) %>%
        summarise(sum_steps = sum(steps, na.rm = TRUE))
```
Total number of steps taken per day is given in the following histogram. 

```r
hist(steps_day$sum_steps, main = "Total number of steps per day", col = "pink", xlab = "Steps", ylim=c(0,30))
```

![](PA1_template_files/figure-html/unnamed-chunk-4-1.png)<!-- -->


Calculation of the mean: 

```r
mean_total <- round(mean(steps_day$sum_steps, na.rm = TRUE))
mean_total
```

```
## [1] 9354
```
The mean is 9354 steps per day. 

Calculation of the median: 

```r
median_total <- round(median(steps_day$sum_steps, na.rm = TRUE))
median_total
```

```
## [1] 10395
```
The median is 10395 steps per day. 


## What is the average daily activity pattern?
Make a time series plot (i.e. type = "l" type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)

```r
steps_interval <- activity %>%
        group_by(interval) %>%
        summarise(steps_mean = mean(steps, na.rm = TRUE))

plot(steps_interval$steps_mean ~ steps_interval$interval, col = "pink", type = "l", main = "Average number of steps by interval", xlab = "5-minute interval", ylab = "Average number of steps")
```

![](PA1_template_files/figure-html/unnamed-chunk-7-1.png)<!-- -->


Find the interval that contains the maximum number of steps: 

```r
steps_interval[which.max(steps_interval$steps_mean), ]
```

```
## # A tibble: 1 × 2
##   interval steps_mean
##      <int>      <dbl>
## 1      835       206.
```
The 835 5-minute interval contains the maximum of steps i.e., 206 steps. 



## Imputing missing values
Total number of missing values: 

```r
sum(is.na(activity$steps))
```

```
## [1] 2304
```
The total number of missing values in 2304. 

Find a strategy to fill in these missing values: use the mean value of the 5-minute interval of the missing value. 
Creation of a new dataset:

```r
activity_new <- activity
for (i in 1:nrow(activity)){
        if (is.na(activity$steps[i])) {
                activity_new$steps[i] <- steps_interval$steps_mean[activity_new$interval[i] == steps_interval$interval]
        }
}
```


Histogram of the new dataset and calculation of the mean and median:

```r
steps_day_new <- activity_new %>% 
        group_by(date) %>%
        summarise(sum_steps_new = sum(steps, na.rm = TRUE))

hist(steps_day_new$sum_steps_new, main = "Total number of steps per day", col = "pink", xlab = "steps", ylim = c(0,40))
```

![](PA1_template_files/figure-html/unnamed-chunk-11-1.png)<!-- -->

Calculate the mean and median of the new dataset with the imputed values. 
Mean: 

```r
mean_total_new <- round(mean(steps_day_new$sum_steps_new, na.rm = TRUE))
mean_total_new
```

```
## [1] 10766
```
The mean is 10766 steps per day. 

Median: 

```r
median_total_new <- round(median(steps_day_new$sum_steps_new, na.rm = TRUE))
median_total_new
```

```
## [1] 10766
```
The median is 10766 steps per day. 

The impact of imputing missing data on the estimates of the estimates of the total daily number of steps is that both the mean and median increase. Because in the original dataset the missing values were ignored by using "na.rm = TRUE". 



## Are there differences in activity patterns between weekdays and weekends?

Create a new factor variable in the dataset with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day. (using the weekdays() function)

```r
activity_dayend <- activity_new
activity_dayend$date <- as.Date(activity_dayend$date)
activity_dayend$day <- ifelse(weekdays(activity_dayend$date) %in% c("Saterday","Sunday"), "weekend","weekday")
activity_dayend$day <- as.factor(activity_dayend$day)
```

Creation of a panel plot displaying the average number of steps taken, averaged across all weekday days or weekend days.

```r
activity_day <- filter(activity_dayend, activity_dayend$day == "weekday")
activity_end <- filter(activity_dayend, activity_dayend$day == "weekend") 

activity_day <- activity_day %>% 
        group_by(interval) %>% 
        summarise(steps = mean(steps))
activity_day$day <- "weekday"

activity_end <- activity_end %>% 
        group_by(interval) %>% 
        summarise(steps = mean(steps))
activity_end$day <- "weekend"

dayend <- rbind(activity_day,activity_end)
dayend$day <- as.factor(dayend$day)

library(ggplot2)
g <- ggplot(dayend, aes(interval, steps))
g + geom_line() + facet_grid(day~.)+
        theme(axis.text = element_text(size = 12), axis.title = element_text(size = 14)) +
        labs(y = "Number of steps") + labs(x="Interval") + 
        ggtitle("Average number of steps: weekday vs weekend") + 
        theme(plot.title = element_text(hjust=0.5))
```

![](PA1_template_files/figure-html/unnamed-chunk-15-1.png)<!-- -->
Based on the plot you see a difference in the average number of steps. On a weekday there is spike observed around interval 800 which corresponds to the morning. This decreases throughout the day while in the weekend there is a more consistent pattern observed. 
