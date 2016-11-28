This is an R Markdown document created to take you through analyzing activity data. Original raw data and projct requirments can be found here: <https://github.com/rdpeng/RepData_PeerAssessment1>.

**Let's load our libraries**

``` r
library(rmarkdown)
library(knitr)
```

*We can now begin using R Markdown to process our code*

**Lets set our working directory, so we can load the data we downloaded from Coursera**

``` r
setwd("C:/Users/anya/Documents/SPR")
```

**Loading and preprocessing the data** | *We have two variables for troubleshooting and viewing original data*

``` r
rawdata1 <- read.csv(file = "C:/Users/anya/Documents/SPR/activity.csv", header = TRUE, dec = ".", na.strings="NA")
z.rawdata_backup <- read.csv(file = "C:/Users/anya/Documents/SPR/activity.csv", header = TRUE, dec = ".", na.strings="NA")
```

**What is mean total number of steps taken per day?** | *For this part of the assignment, you can ignore the missing values in the dataset.*

``` r
#Remove NAs
rawdata <-na.omit(rawdata1)

#calculate the Mean and Interval per date
step_mean <- aggregate(x = rawdata1$steps, by= list(date=rawdata1$date),FUN = mean, na.rm = TRUE)

#Rename column back to 'steps' for clarity
colnames(step_mean)[2] <- "Steps"
```

    ##         date    Steps
    ## 1 2012-10-01      NaN
    ## 2 2012-10-02  0.43750
    ## 3 2012-10-03 39.41667
    ## 4 2012-10-04 42.06944
    ## 5 2012-10-05 46.15972
    ## 6 2012-10-06 53.54167

**Calculate the total number of steps taken per day**

``` r
#calculate the Sum Steps taken
step_sum <- aggregate(x = rawdata1$steps, by= list(date=rawdata1$date),FUN = sum, na.rm = TRUE)

#Rename column back to 'steps' for clarity
colnames(step_sum)[2] <- "Steps"
```

    ##         date Steps
    ## 1 2012-10-01     0
    ## 2 2012-10-02   126
    ## 3 2012-10-03 11352
    ## 4 2012-10-04 12116
    ## 5 2012-10-05 13294
    ## 6 2012-10-06 15420

**Make a histogram of the total number of steps taken each day** ![](ReproducibleResearch_AnalyzingActivityData_files/figure-markdown_github/unnamed-chunk-8-1.png)

**Calculate and report the mean and median of the total number of steps taken per day** | *What is the average daily activity pattern?*

``` r
mean(step_sum$Steps)
```

    ## [1] 9354.23

``` r
median(step_sum$Steps)
```

    ## [1] 10395

**Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis) Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?** | *Imputing missing values* |*Note that there are a number of days/intervals where there are missing values (coded as NA). The presence of missing days may introduce bias into some calculations or summaries of the data.*

``` r
StepsInterval <- aggregate(x = rawdata1$steps, by= list(interval=rawdata1$interval),FUN = mean, na.rm = TRUE)
colnames(StepsInterval)[2] <- "Steps"



plot(StepsInterval$interval, StepsInterval$Steps, type = "l", xlab = "Interval", ylab = "Average Number of Steps", main = "Average Number of Steps by Interval", ylim = c(0, max(StepsInterval$Steps) * 1.1))

MaxStep <- StepsInterval[StepsInterval$Steps == max( StepsInterval$Steps), ]

points(MaxStep$interval, 
       MaxStep$Steps, 
       pch = 16,
       col = "red")
```

![](ReproducibleResearch_AnalyzingActivityData_files/figure-markdown_github/unnamed-chunk-11-1.png)

    ## [1] "The maximum number of steps is 206.169811320755"

    ## [1] "and the corresponding interval is 835"

**Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)**

``` r
NaVal <- sum(is.na(rawdata1$steps))
NaVal
```

    ## [1] 2304

**Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.** **Create a new dataset that is equal to the original dataset but with the missing data filled in.** *We will replace the missing data with a mean*

``` r
rawdata_avgstep <-z.rawdata_backup
avg_step <-mean(StepsInterval$Steps) 
rawdata_avgstep[is.na(rawdata_avgstep)] <- avg_step
```

**Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day.**

    ##         date    Steps
    ## 1 2012-10-01 10766.19
    ## 2 2012-10-02   126.00
    ## 3 2012-10-03 11352.00
    ## 4 2012-10-04 12116.00
    ## 5 2012-10-05 13294.00
    ## 6 2012-10-06 15420.00

**Make a histogram of the total number of steps taken each day** ![](ReproducibleResearch_AnalyzingActivityData_files/figure-markdown_github/unnamed-chunk-16-1.png)

    ## [1] 10766.19

    ## [1] 10766.19

**Do these values differ from the estimates from the first part of the assignment?**

    ## [1] "Yes these values differ. Before we had a skew, and now we see a more normal distribution. Mean and Median number has increased, which is expected as we replaced zeros with an average."

"

**Are there differences in activity patterns between weekdays and weekends?** | *For this part the weekdays() function may be of some help here. Use the dataset with the filled-in missing values for this part.*

*Create a new factor variable in the dataset with two levels - "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.*

``` r
library (chron)
rawdata_avgstep$weekend <- chron::is.weekend(rawdata_avgstep$date)

rawdata_avgstep$DayType <- NA

rawdata_avgstep$DayType[rawdata_avgstep$weekend == 'TRUE'] <- 'weekend'
rawdata_avgstep$DayType[rawdata_avgstep$weekend == 'FALSE'] <- 'weekday'

rawdata_avgstep$weekend <- NULL

head(rawdata_avgstep)
```

    ##     steps       date interval DayType
    ## 1 37.3826 2012-10-01        0 weekday
    ## 2 37.3826 2012-10-01        5 weekday
    ## 3 37.3826 2012-10-01       10 weekday
    ## 4 37.3826 2012-10-01       15 weekday
    ## 5 37.3826 2012-10-01       20 weekday
    ## 6 37.3826 2012-10-01       25 weekday

*Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis).*

``` r
StepsByIntervalDayType <- aggregate(rawdata_avgstep$steps, 
    by = list(Interval = rawdata_avgstep$interval, DayType = rawdata_avgstep$DayType), 
    FUN = mean, 
    na.rm = TRUE)
colnames(StepsByIntervalDayType)[1:3] <- c("interval", "DayType", "Steps")


library(ggplot2)
ggplot(StepsByIntervalDayType, 
       aes(x = interval, 
           y = Steps)) +
    ylim(c(0, max(StepsByIntervalDayType$Steps) * 1.1)) +
    geom_line() +
    facet_wrap(~DayType, ncol = 1) +
    xlab("Interval") +
    ylab("Average Number of Steps") +
    ggtitle("Average Number of Steps by Day Type") 
```

![](ReproducibleResearch_AnalyzingActivityData_files/figure-markdown_github/unnamed-chunk-19-1.png)
