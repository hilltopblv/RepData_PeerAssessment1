# Reproducible Research: Peer Assessment 1

##1. Loading and preprocessing the data


```r
setwd("~/Documents/mygitrepos/RepData_PeerAssessment1")
activity <- read.csv(unzip(zipfile= "activity.zip", file="activity.csv"))
str(activity)
```

```
## 'data.frame':	17568 obs. of  3 variables:
##  $ steps   : int  NA NA NA NA NA NA NA NA NA NA ...
##  $ date    : Factor w/ 61 levels "2012-10-01","2012-10-02",..: 1 1 1 1 1 1 1 1 1 1 ...
##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...
```

```r
activity$date <- as.Date(as.character(activity$date , format = "%Y%m%d"))
```

##2.  What is mean total number of steps taken per day?
For this part of the assignment, you can ignore the missing values in the dataset.
Make a histogram of the total number of steps taken each day.
Calculate and report the mean and median total number of steps taken per day.


```r
library(dplyr)
```

```
## 
## Attaching package: 'dplyr'
## 
## The following object is masked from 'package:stats':
## 
##     filter
## 
## The following objects are masked from 'package:base':
## 
##     intersect, setdiff, setequal, union
```

```r
library(lubridate)

activity_by_date <- group_by(activity,date)
steps_by_date <- summarize(activity_by_date,daily_sum = sum(steps,na.rm = TRUE))

hist(steps_by_date$daily_sum, breaks=10, xlab="Total steps per day", ylim = c(0,20), main ="Figure 1: Histogram for daily total steps")
```

![](./PA1_template_files/figure-html/unnamed-chunk-2-1.png) 

```r
dev.copy(png, 'figure/figure1_dailyTotal.png')
```

```
## quartz_off_screen 
##                 3
```

```r
dev.off()
```

```
## pdf 
##   2
```

```r
# mean and median total number of steps taken per day
activity_mean <- mean(steps_by_date$daily_sum)
activity_mean
```

```
## [1] 9354.23
```

```r
activity_median <-median(steps_by_date$daily_sum)
activity_median 
```

```
## [1] 10395
```
Answer: The mean and median total number of steps taken per day are 9354, 10395 respectively. 


##3.  What is the average daily activity pattern?

Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)


```r
activity_by_interval <- group_by(activity,interval)
steps_by_interval <- summarize(activity_by_interval,five_minute_mean= mean(steps,na.rm = TRUE))
plot(steps_by_interval$interval, steps_by_interval$five_minute_mean, xlim =c (0,2355), type="l", xlab="Time (00:00 to 24:00)", ylab = "Steps per 5-minute", main="Figure 2: Average step per 5-minute")
```

![](./PA1_template_files/figure-html/unnamed-chunk-3-1.png) 

```r
dev.copy(png, 'figure/figure2_meanSteps.png')
```

```
## quartz_off_screen 
##                 3
```

```r
dev.off()
```

```
## pdf 
##   2
```

```r
#Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?
steps_by_interval[steps_by_interval$five_minute_mean ==(max(steps_by_interval$five_minute_mean)),]
```

```
## Source: local data frame [1 x 2]
## 
##   interval five_minute_mean
## 1      835         206.1698
```
Answer: Most of the activities happen between 6:00am to 10:00pm with peak at 8:35am.

##4. Imputing missing values

Note that there are a number of days/intervals where there are missing values (coded as NA). The presence of missing days may introduce bias into some calculations or summaries of the data.
Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)

```r
summary(activity)
```

```
##      steps             date               interval     
##  Min.   :  0.00   Min.   :2012-10-01   Min.   :   0.0  
##  1st Qu.:  0.00   1st Qu.:2012-10-16   1st Qu.: 588.8  
##  Median :  0.00   Median :2012-10-31   Median :1177.5  
##  Mean   : 37.38   Mean   :2012-10-31   Mean   :1177.5  
##  3rd Qu.: 12.00   3rd Qu.:2012-11-15   3rd Qu.:1766.2  
##  Max.   :806.00   Max.   :2012-11-30   Max.   :2355.0  
##  NA's   :2304
```

```r
sum(is.na (activity$steps))
```

```
## [1] 2304
```

Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.

Strategy: randomly select a value from the available values to fill out those missing values.
The R script random.imp is from http://www.stat.columbia.edu/~gelman/arm/missing.pdf


```r
random.imp <- function (a){
  missing <- is.na(a)
  n.missing <- sum(missing)
  a.obs <- a[!missing]
  imputed <- a
  imputed[missing] <- sample (a.obs, n.missing, replace=TRUE)
  return (imputed)
  }
```

Create a new dataset that is equal to the original dataset but with the missing data filled in.


```r
activity_filled <- activity
activity_filled$steps_imp <- random.imp (activity$step)
activity_filled_by_date <- group_by(activity_filled,date)
steps_filled_by_date <- summarize(activity_filled_by_date,  filled_daily_sum= sum(steps_imp))
```
Make a histogram of the total number of steps taken each day and 

Calculate and report the mean and median total number of steps taken per day. 


```r
hist(steps_filled_by_date$filled_daily_sum, breaks=10, ylim = c(0,20),xlab="Total steps per day", main="Figure 3: Histogram of total steps per day after imputing")
```

![](./PA1_template_files/figure-html/unnamed-chunk-7-1.png) 

```r
dev.copy(png, 'figure/figure3_dailyTotalImp.png')
```

```
## quartz_off_screen 
##                 3
```

```r
dev.off()
```

```
## pdf 
##   2
```

```r
filled_mean <- mean(steps_filled_by_date$filled_daily_sum)
filled_mean
```

```
## [1] 10564.72
```

```r
filled_median <- median(steps_filled_by_date$filled_daily_sum)
filled_median
```

```
## [1] 10571
```

```r
mean_diff  <- filled_mean - activity_mean 
median_diff  <- filled_median -activity_median 
```
Answer: The mean and median total number of steps taken per day are 10564, 10571 respectively. 

Question: Do these values differ from the estimates from the first part of the assignment? 

Answer: Yes. The average steps from the imputed dataset is  1210 steps higher than from non-imputed dataset. The median steps from the imputed dataset is  176 higher than from non-imputed dataset.


```r
steps_by_date[steps_by_date$daily_sum==0,]
```

```
## Source: local data frame [8 x 2]
## 
##         date daily_sum
## 1 2012-10-01         0
## 2 2012-10-08         0
## 3 2012-11-01         0
## 4 2012-11-04         0
## 5 2012-11-09         0
## 6 2012-11-10         0
## 7 2012-11-14         0
## 8 2012-11-30         0
```

```r
steps_filled_by_date[steps_by_date$daily_sum==0,]
```

```
## Source: local data frame [8 x 2]
## 
##         date filled_daily_sum
## 1 2012-10-01            11052
## 2 2012-10-08             9745
## 3 2012-11-01             6753
## 4 2012-11-04             8753
## 5 2012-11-09             9829
## 6 2012-11-10             9010
## 7 2012-11-14            10608
## 8 2012-11-30             8090
```
Question: What is the impact of imputing missing data on the estimates of the total daily number of steps?

Answer: The imputing missing data replaced the total daily number of 0s with the total of random numbers assigned to each 5-minutes interval for those days without available datea. So the days with missing values have similar values with those with values after imputing. From the histograms, we can see the frequency around the mean increased. 


##5.  Are there differences in activity patterns between weekdays and weekends?
Are there differences in activity patterns between weekdays and weekends?

For this part the weekdays() function may be of some help here. Use the dataset with the filled-in missing values for this part.

Create a new factor variable in the dataset with two levels -- "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.

Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). The plot should look something like the following, which was created using simulated data:


```r
activity_filled$weekind <- weekdays(activity_filled$date) 
activity_filled$weekind <- ifelse (weekdays(activity_filled$date) %in% c("Saturday","Sunday"),"Weekend", "Weekday")
activity_filled$weekind <-as.factor(activity_filled$weekind)

weekday_steps <- filter (activity_filled, weekind=="Weekday")
weekend_steps <- filter (activity_filled, weekind=="Weekend")

weekday_steps_by_interval <- group_by(weekday_steps,interval)
weekend_steps_by_interval <- group_by(weekend_steps,interval)

weekday_avg_steps <- summarize(weekday_steps_by_interval,five_minute_mean=mean(steps_imp))
weekend_avg_steps <- summarize(weekend_steps_by_interval,five_minute_mean=mean(steps_imp))

par(mfrow=c(2,1), mar=c(3,2,1,1))
plot(weekday_avg_steps$interval, weekday_avg_steps$five_minute_mean, type="l", xlim =c(0,2400), ylim = c(0, 200), xlab="Time", ylab = "Steps", main="Figure 4: Average steps per 5 minutes on weekday")
grid()
dev.copy(png, 'figure/figure4_weekdayMeanStepsImp.png')
```

```
## quartz_off_screen 
##                 3
```

```r
dev.off()
```

```
## pdf 
##   2
```

```r
plot(weekday_avg_steps$interval, weekday_avg_steps$five_minute_mean, type="l",  xlim =c(0,2400), ylim = c(0, 200), xlab="Time", ylab = "Steps", main="Figure 5: Average steps per 5 minutes (blue = weekend)")
lines(weekday_avg_steps$interval, weekend_avg_steps$five_minute_mean,col='blue')
grid()
```

![](./PA1_template_files/figure-html/unnamed-chunk-9-1.png) 

```r
dev.copy(png, 'figure/figure5_MeanStepsImp.png')
```

```
## quartz_off_screen 
##                 3
```

```r
dev.off()
```

```
## pdf 
##   2
```

```r
graphics.off()
```
Answer:  From these plots, it seems that the averages steps per 5-minute interval are lower in the mornings but higher in the afternoons on weekends than on weekdays.
