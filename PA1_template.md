# Reproducible Research: Peer Assessment 1

This is the work of La Monte H.P. Yarroll <piggy+coursera@google.com>.


```r
figure.dir <- "figures"
raw.data.archive <- "activity.zip"
raw.data.csv <- "activity.csv"
as.time <- function (d, t) {
    strptime(as.character(d), "%Y-%m-%d") + (t %/% 100) * 3600 + (t %% 100) * 60
}
```

## Loading and preprocessing the data

```r
if (!file.exists(raw.data.csv)) {
   unzip(raw.data.archive)
}
steps.data <- read.csv(raw.data.csv)
steps.data$when <- as.time(steps.data$date, steps.data$interval)
steps.data$date <- as.Date(as.character(steps.data$date))
str(steps.data)
```

```
## 'data.frame':	17568 obs. of  4 variables:
##  $ steps   : int  NA NA NA NA NA NA NA NA NA NA ...
##  $ date    : Date, format: "2012-10-01" "2012-10-01" ...
##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...
##  $ when    : POSIXct, format: "2012-10-01 00:00:00" "2012-10-01 00:05:00" ...
```

## What is mean total number of steps taken per day?

1. Make a histogram of the total number of steps taken each day


```r
total.steps.by.day <- tapply(steps.data$steps, steps.data$date, sum, na.rm=TRUE, simplify=TRUE)
hist(total.steps.by.day,20)
```

![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-3.png) 

2. Calculate and report the **mean** and **median** total number of steps taken per day


```r
mean(total.steps.by.day)
```

```
## [1] 9354
```

```r
median(total.steps.by.day)
```

```
## [1] 10395
```
## What is the average daily activity pattern?

1. Make a time series plot (i.e. `type = "l"`) of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)


```r
mean.steps.by.interval <- tapply(steps.data$steps, steps.data$interval, mean, na.rm=TRUE, simplify=TRUE)
intervals <- as.time(rep("2012-10-01", length(mean.steps.by.interval)), as.numeric(names(mean.steps.by.interval)))
plot(intervals, mean.steps.by.interval, type="l")
```

![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5.png) 

2. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

```r
max.interval <- intervals[mean.steps.by.interval == max(mean.steps.by.interval)]
max.interval <- as.POSIXlt(max.interval)
sprintf("%02d:%02d", max.interval$hour, max.interval$min)
```

```
## [1] "08:35"
```
## Imputing missing values

1. Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with `NA`s)


```r
sum(!complete.cases(steps.data))
```

```
## [1] 2304
```

2. Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.

There are 8 whole days with every entry having the value NA:

```r
mean.steps.by.day <- tapply(steps.data$steps, steps.data$date, mean, na.rm=TRUE, simplify=TRUE)
sum(is.nan(mean.steps.by.day))
```

```
## [1] 8
```
We need a different method to impute missing data.

We set steps to the mean for the respective interval for every NA in steps.

```r
tmp <- data.frame(interval=as.integer(names(mean.steps.by.interval)), mean.steps=mean.steps.by.interval)
imputed.steps.data <- merge(steps.data, tmp, by="interval")
```

3. Create a new dataset that is equal to the original dataset but with the missing data filled in.


```r
missing <- is.na(imputed.steps.data$steps)
imputed.steps.data$steps[missing] <- imputed.steps.data$mean.steps[missing]
```

4. Make a histogram of the total number of steps taken each day and Calculate and report the **mean** and **median** total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?


```r
imputed.total.steps.by.day <- tapply(imputed.steps.data$steps, imputed.steps.data$date, sum, na.rm=TRUE, simplify=TRUE)
hist(imputed.total.steps.by.day,20)
```

![plot of chunk unnamed-chunk-11](figure/unnamed-chunk-11.png) 

```r
mean(imputed.total.steps.by.day)
```

```
## [1] 10766
```

```r
median(imputed.total.steps.by.day)
```

```
## [1] 10766
```

```r
mean(total.steps.by.day)
```

```
## [1] 9354
```

```r
median(total.steps.by.day)
```

```
## [1] 10395
```

Imputing missing values by this method amplified a single value to the
point that it became the mean and the median. It almost completely eliminated
the days with 0 steps.

## Are there differences in activity patterns between weekdays and weekends?

1. Create a new factor variable in the dataset with two levels -- "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.


```r
weekends <- weekdays(steps.data$date) %in% c("Saturday", "Sunday")
steps.data$weekpart <- rep("", length(steps.data$steps))
steps.data$weekpart[weekends] <- "weekend"
steps.data$weekpart[!weekends] <- "weekday"
steps.data$weekpart <- as.factor(steps.data$weekpart)
```
1. Make a panel plot containing a time series plot (i.e. `type = "l"`) of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis).


```r
tmp <- tapply(steps.data$steps, list(steps.data$interval, steps.data$weekpart), mean, na.rm=TRUE, simplify=TRUE)
mean.steps.by.interval.weekpart <- as.data.frame(tmp)
interval.numbers <- as.numeric(attr(tmp, "dimnames")[[1]])
interval <- as.time(rep("2012-10-01", length(interval.numbers)), interval.numbers)
mean.steps.by.interval.weekpart$interval <- interval
par(mfrow = c(2,1))
plot(mean.steps.by.interval.weekpart$interval, mean.steps.by.interval.weekpart$weekend, type="l", main="Weekend steps by time of day", xlab="time", ylab="mean steps")
plot(mean.steps.by.interval.weekpart$interval, mean.steps.by.interval.weekpart$weekday, type="l", main="Weekday steps by time of day", xlab="time", ylab="mean steps")
```

![plot of chunk unnamed-chunk-13](figure/unnamed-chunk-13.png) 

On the weekend, activity starts later but is much higher in the
afternoons and c ontinues later into the evening.
