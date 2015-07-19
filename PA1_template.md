# Reproducible Research: Peer Assessment 1


## Loading and preprocessing the data

```r
unzip("activity.zip")
activity <- read.csv("activity.csv")
```

## What is mean total number of steps taken per day?
- Calculating the total number of steps taken per day

```r
steps.date <- aggregate(steps ~ date, data=activity, FUN=sum)
```


- Making a histogram of the total number of steps taken each day

```r
barplot(steps.date$steps, names.arg=steps.date$date, xlab="date", ylab="steps")
```

![](PA1_template_files/figure-html/unnamed-chunk-3-1.png) 


- Calculate and report the mean and median of the total number of steps taken per day

```r
mean(steps.date$steps)
```

```
## [1] 10766.19
```

```r
median(steps.date$steps)
```

```
## [1] 10765
```


## What is the average daily activity pattern?
- Making a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)

```r
steps.interval <- aggregate(steps ~ interval, data=activity, FUN=mean)
plot(steps.interval, type="l")
```

![](PA1_template_files/figure-html/unnamed-chunk-5-1.png) 


- The 5-minute interval, on average across all the days in the dataset, which contains the maximum number of steps

```r
steps.interval$interval[which.max(steps.interval$steps)]
```

```
## [1] 835
```

## Imputing missing values
- Total number of missing values in the dataset

```r
sum(is.na(activity))
```

```
## [1] 2304
```


- A strategy for filling in all of the missing values in the dataset.
The means for 5 minutes intervals can be used as filters for missing values.


- Creating a new dataset that is equal to the original dataset but with the missing data filled in

```r
activity <- merge(activity, steps.interval, by="interval", suffixes=c("",".y"))
nas <- is.na(activity$steps)
activity$steps[nas] <- activity$steps.y[nas]
activity <- activity[,c(1:3)]
```


- Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?

```r
steps.date <- aggregate(steps ~ date, data=activity, FUN=sum)
barplot(steps.date$steps, names.arg=steps.date$date, xlab="date", ylab="steps")
```

![](PA1_template_files/figure-html/unnamed-chunk-9-1.png) 

```r
mean(steps.date$steps)
```

```
## [1] 10766.19
```

```r
median(steps.date$steps)
```

```
## [1] 10766.19
```

For the total number of steps per day the imact of missing data is almost negligible.

## Are there differences in activity patterns between weekdays and weekends?
- Creating a new factor variable in the dataset with two levels - "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.

```r
daytype <- function(date) {
    if (weekdays(as.Date(date)) %in% c("Saturday", "Sunday")) {
        "weekend"
    } else {
        "weekday"
    }
}
activity$daytype <- as.factor(sapply(activity$date, daytype))
```


- Making a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis).

```r
par(mfrow=c(2,1))
for (type in c("weekend", "weekday")) {
    steps.type <- aggregate(steps ~ interval,
                            data=activity,
                            subset=activity$daytype==type,
                            FUN=mean)
    plot(steps.type, type="l", main=type)
}
```

![](PA1_template_files/figure-html/unnamed-chunk-11-1.png) 

