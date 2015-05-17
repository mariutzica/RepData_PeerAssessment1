# Reproducible Research: Peer Assessment 1


## Loading and preprocessing the data

1. **Load the data.** The data is in a zipped format, so we unzip first, then read the comma separated information into the variable 'data'.


```r
data<-read.csv(unz("activity.zip","activity.csv"))
```

2. **Preprocess the data.** Grab the complete cases in data (diregard NAs). Convert the date to a factor so that we can split up information by day.


```r
data.complete<-data[complete.cases(data),]
data.complete$date<-as.factor(data.complete$date)
data$interval<-as.factor(data$interval)
data$date<-as.factor(data$date)
data.complete$interval<-as.factor(data.complete$interval)
```

## What is mean total number of steps taken per day?

1. **Total number of steps per day.** Calculate by day (level), dropping any days for which there is no data.


```r
stepsperday<-tapply(data.complete$steps,droplevels(data.complete$date),sum)
```

2. **Histogram of number of steps taken each day.**


```r
hist(stepsperday)
```

![](PA1_template_files/figure-html/unnamed-chunk-4-1.png) 

3. **Mean and median of the total number of steps taken per day.**


```r
meansteps<-mean(stepsperday)
mediansteps<-median(stepsperday)
```

The mean steps per day is 1.0766189\times 10^{4} and the median steps per day is 10765.


## What is the average daily activity pattern?

1. **Time series plot.** Plot of the average number of steps for each time interval (y-axis), taken over all days, v. the time interval, in increments of 5 (x-axis). 


```r
avintervalsteps<-tapply(data.complete$steps,droplevels(data.complete$interval),mean)
plot(levels(droplevels(data.complete$interval)),avintervalsteps,type="l")
```

![](PA1_template_files/figure-html/unnamed-chunk-5-1.png) 

2. **Interval with maximum steps.**


```r
maxsteps<-max(avintervalsteps)
indexmaxsteps<-levels(data.complete$interval)[avintervalsteps==maxsteps]
```

On average, the interval with the maximum number of steps is 835.


## Imputing missing values

1. **Missing values in the data set.**


```r
missing<-nrow(data)-nrow(data.complete)
```

The total number of incomplete data sets is 2304.

2. **Devise a way of filling in the missing values.** Here, we choose to fill in the missing values with the average value for that time interval over all days.


```r
index<-is.na(data$steps)
for (i in seq(along.with=index)) {
    if (index[i]) data$steps[i]<-avintervalsteps[data$interval[i]==levels(droplevels(data$interval))]
}
```

3. **Histogram of the total number of steps taken each day.**


```r
stepsperday.modify<-tapply(data$steps,droplevels(data$date),sum)
hist(stepsperday.modify)
```

![](PA1_template_files/figure-html/unnamed-chunk-7-1.png) 

4. **Calculate the new mean and median.**


```r
meansteps.modify<-mean(stepsperday.modify)
mediansteps.modify<-median(stepsperday.modify)
```

The mean steps per day is 1.0766189\times 10^{4} and the median steps per day is 1.0766189\times 10^{4}.
## Are there differences in activity patterns between weekdays and weekends?
