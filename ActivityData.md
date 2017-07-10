# RR Project 1

## Activity Data
This project uses a data set describing the number of steps taken during each five
minute interval during over a number of days. For example intervals start at midnight,
0005, 0010, 0015...2345, 2350, 2355.
Three variables are given, steps, interval, and date.

First, read in the code, look at structure:

```r
#Read in data saved in working directory\data folder
activity <- read.csv(".\\data\\activity.csv")
str(activity)
```

```
## 'data.frame':	17568 obs. of  3 variables:
##  $ steps   : int  NA NA NA NA NA NA NA NA NA NA ...
##  $ date    : Factor w/ 61 levels "2012-10-01","2012-10-02",..: 1 1 1 1 1 1 1 1 1 1 ...
##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...
```
### Steps / Day
The first analysis we will perform is looking at total steps per day by frequency
in a histogram. Then pull out mean and median.

```r
#Aggregate sum of steps by date
dateStep<-aggregate(activity$steps ~ activity$date, FUN=sum)

#Plot histogram of total steps/day
hist(dateStep[[2]],
     xlab="Steps per Day",
     main="Histogram of Total Steps per Day")
```

![](ActivityData_files/figure-html/stepsbydate-1.png)<!-- -->

```r
dev.copy(png, file="hist1.png", width=480, height=480)
```

```
## png 
##   3
```

```r
dev.off()
```

```
## png 
##   2
```

```r
#Summary of steps/day with mean and median pulled out
summary(dateStep[[2]])[3:4]
```

```
##   Median     Mean 
## 10765.00 10766.19
```

### Mean Steps / Interval
Next we wil investigate the average numer of steps taken per 5' interval.
Plotting as a line plot and finding the highest average

```r
#Aggregate mean of steps per interval time, rename resulting object
intStep <- aggregate(activity$steps ~ activity$interval, FUN=mean)
names(intStep) <- c("interval", "steps")

#plot mean steps/interval
plot(intStep, typ="l",
     xlab= "5 minute interval",
     ylab= "Mean Steps",
     main= "Mean Steps per 5 min. interval")
```

![](ActivityData_files/figure-html/steps/interval-1.png)<!-- -->

```r
dev.copy(png, file="plot1.png", width=480, height=480)
```

```
## png 
##   3
```

```r
dev.off()
```

```
## png 
##   2
```

Highest average steps/interval

```r
#Find interval with highest mean steps
intStep[intStep[,2] == max(intStep[,2]),]
```

```
##     interval    steps
## 104      835 206.1698
```

### Imputing data
A number of data are missing. We will impute those missing values by passing them
the mean number of steps taken for that interval. We will round the mean before imputing as it's difficult to take 1.678354 steps


```r
#Start to create data with missing values imputed
#Strategy: replace missing value with rounded mean of steps during that interval

#round mean steps/interval 
intStep$steps <- round(intStep$steps)

#subset activity dataset with missing values
imputeActivity <- activity[is.na(activity$steps), ]

#find number of missing values
length(imputeActivity$interval)
```

```
## [1] 2304
```

```r
#replace missing values with values from rounded mean data.frame
imputeActivity <- merge(imputeActivity, intStep, by="interval")
#reorder/rename resulting matrix to same as original matrix
imputeActivity <- imputeActivity[ , c(4,3,1)]
names(imputeActivity) <- c("steps", "date", "interval")

#create imputed object by binding non-na values with imputed values
imputeActivity <- rbind(imputeActivity, activity[!is.na(activity$steps), ])
```

With our new object that has no missing values, we will once again look at steps per day

```r
#aggregate new sum of steps/day, replot Histogram w/ new totals
imputeStepDay <- aggregate(imputeActivity$steps ~ imputeActivity$date, FUN=sum)
hist(imputeStepDay[[2]],
     xlab="Steps per Day",
     main="Histogram of Total Steps per Day (is.na values imputed)")
```

![](ActivityData_files/figure-html/imputed steps per day-1.png)<!-- -->

```r
dev.copy(png, file="hist2.png", width=480, height=480)
```

```
## png 
##   3
```

```r
dev.off()
```

```
## png 
##   2
```

We will take a look at the old and new mean and median.
Old:

```r
summary(dateStep[[2]])[3:4]
```

```
##   Median     Mean 
## 10765.00 10766.19
```
New:

```r
#view old and new mean/median
summary(imputeStepDay[[2]])[3:4]
```

```
##   Median     Mean 
## 10762.00 10765.64
```

Unsurprisingly similar as the data we imputed functionally were all the mean value.

### Weekend/Weekday steps/interval
Finally we will look at steps taken during weekdays vs. weekend
First we need to determine which dates correspond to weekdays/end

```r
#convert date strings to "r"-readable dates
imputeActivity$date <- strptime(imputeActivity$date, format="%Y-%m-%d")

#create new variable, first populate with day of week name,
#then rename to "Weekend" and "Weekday"
imputeActivity$dayofweek <- weekdays(imputeActivity$date)
imputeActivity$dayofweek <- ifelse(imputeActivity$dayofweek %in% 
                                       c("Saturday","Sunday"), "Weekend", "Weekday")

#subset weekend and weekday data
wkday <- subset(imputeActivity, imputeActivity$dayofweek == "Weekday")
wkend <- subset(imputeActivity, imputeActivity$dayofweek == "Weekend")
```

Next take mean of steps per interval in weekend and weekday data. Finally plot to compare.

```r
#take mean of step/interval for weekend/weekday
wkdayinterval <- aggregate(wkday$steps ~ wkday$interval, FUN=mean)
wkendinterval <- aggregate(wkend$steps ~ wkend$interval, FUN=mean)

#Create panel plot weekday/weekend step/interval
par(mfrow = c(2,1))
plot(wkdayinterval, typ = "l",
     xlab = "Weekday, 5' Interval",
     ylab = "Mean Steps",
     main = "Weekday/end meansteps per 5' Interval")
plot(wkendinterval, typ = "l",
     xlab = "Weekend, 5' Interval",
     ylab = "Mean Steps")
```

![](ActivityData_files/figure-html/wkend/day calc and plot-1.png)<!-- -->

```r
dev.copy(png, file="plot2.png", width=480, height=480)
```

```
## png 
##   3
```

```r
dev.off()
```

```
## png 
##   2
```
