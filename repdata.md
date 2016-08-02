---
title: "reproducible data research"
author: "Seke Pascal"
date: "July 23, 2016"
output: html_document
---

## Getting Started

This assignment makes use of data from a personal activity monitoring device. This device collects data at 5 minute intervals through out the day. The data consists of two months of data from an anonymous individual collected during the months of October and November, 2012 and include the number of steps taken in 5 minute intervals each day.

### Load packages

In this assignment we will explore the data using the `dplyr` package and visualize it using the `ggplot2` package for data visualization.

let's load the different package: 


```r
library(dplyr)
library(ggplot2)
```

### The data



```r
data <- read.csv("activity.csv") 
```


let's create the data will will work with by correcting the date format and interval format.


```r
act$date <- as.Date(act$date) 
```


```r
act$interval <- 60*floor((act$interval+1)/100) + (act$interval %% 100)
```

let's see the head of our new data.




### Histogram of the total number of steps taken each day

* Calculate the total number of steps taken per day


```r
T_step_day <- tapply(act$steps, act$date, sum, na.rm = TRUE)
```

* mean and median total number of steps per day.


```r
mean(T_step_day)
```

```
## [1] 9354.23
```


```r
median(T_step_day)
```

```
## [1] 10395
```

let's plot the histogram of total number of steps taken each day.


```r
hist(T_step_day, breaks=10, xlab="number of steps per day", main="Histogram of total steps per day")
```

![plot of chunk unnamed-chunk-7](figure/unnamed-chunk-7-1.png)


```r
abline(v=mean(T_step_day), col="red", lwd=2)
```

```
## Error in int_abline(a = a, b = b, h = h, v = v, untf = untf, ...): plot.new has not been called yet
```


```r
abline(v=median(T_step_day), col="green", lwd=2)
```

```
## Error in int_abline(a = a, b = b, h = h, v = v, untf = untf, ...): plot.new has not been called yet
```


```r
legend(x="topright", legend=c("mean","median"), col=c("red","green"), bty="n", lwd=3)
```

```
## Error in strwidth(legend, units = "user", cex = cex, font = text.font): plot.new has not been called yet
```


we notice that the histogram is left skew cause median is superior to the mean

### Time series plot of the average number of steps taken.

* the average step by interval and average per hour.


```r
avg_step_int <- tapply(act$steps, act$interval, mean, na.rm = TRUE)
```


```r
hour_int <- as.numeric(names(avg_step_int))/60
```

let us plot the time serie between the average step  and the average step per hour interval.


```r
plot(hour_int, avg_step_int, type="l", axes=F,
     xlab="daytime (h)", ylab="average number of steps in 5-min interval",
     main="Daily activity pattern")
axis(2)
axis(1, at=0:6*4, labels=paste(0:6*4,":00", sep=""))
```

![plot of chunk unnamed-chunk-13](figure/unnamed-chunk-13-1.png)

we notice that the peak is 8:30 to 10 

### Code to describe and show a strategy for imputing missing data

let's count all the missing value from the whole data


```r
sum(is.na(act))
```

```
## [1] 2304
```


* `head` with the NAs


```r
head(act)
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

* remove all NA's and check the head.


```r
act_noNA <- filter(act, !is.na(steps), !is.na(date), !is.na(interval))
```



```r
head(act_noNA)
```

```
##       steps       date interval
## 1 1.7169811 2012-10-01        0
## 2 0.3396226 2012-10-01        5
## 3 0.1320755 2012-10-01       10
## 4 0.1509434 2012-10-01       15
## 5 0.0754717 2012-10-01       20
## 6 2.0943396 2012-10-01       25
```

* Calculate the total number of steps taken per day with act_noNA.


```r
T_step_day2 <- tapply(act_noNA$steps, act_noNA$date, sum)
```

* mean and median total number of steps per day.


```r
mean(T_step_day2)
```

```
## [1] 10766.19
```


```r
median(T_step_day2)
```

```
## [1] 10766.19
```

we notice that the NA has disapear.

### Histogram of the total number of steps taken each day after missing values are imputed.


```r
hist(T_step_day2, breaks=10, xlab="number of steps per day", main="Histogram of total steps per day")
```

![plot of chunk unnamed-chunk-21](figure/unnamed-chunk-21-1.png)



```r
abline(v=mean(T_step_day2), col="red", lwd=3)
```

```
## Error in int_abline(a = a, b = b, h = h, v = v, untf = untf, ...): plot.new has not been called yet
```


```r
abline(v=median(T_step_day2), col="green", lwd=3, lty=2)
```

```
## Error in int_abline(a = a, b = b, h = h, v = v, untf = untf, ...): plot.new has not been called yet
```


```r
legend(x="topright", legend=c("mean","median"), col=c("red","green"), bty="n", lwd=3)
```

```
## Error in strwidth(legend, units = "user", cex = cex, font = text.font): plot.new has not been called yet
```

we notice that this plot is symetric because the mean is equal to the median.

* perfect equality  between the two sum


```r
sum(act$steps, na.rm = TRUE)
```

```
## [1] 570608
```



```r
sum(act_noNA$steps)
```

```
## [1] 656737.5
```


### Panel plot comparing the average number of steps taken per 5-minute interval across weekdays and weekends.







