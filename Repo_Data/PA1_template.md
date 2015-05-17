Peer Assessment 1 - Reproducible Research

# Basic settings
```{r}
echo = TRUE  # Always make code visible
options(scipen = 1)  # Turn off scientific notations for numbers
```

# Loading + Transform/process data
```{r}
data <- read.csv("C:/Users/Stickman/R working directory/activity.csv", colClasses = c("integer", "Date", "factor"))
data$month <- as.numeric(format(data$date, "%m"))
NAless <- na.omit(data)
rownames(NAless) <- 1:nrow(NAless)
head(NAless)
dim(NAless)

library(ggplot2)

```

# What is the, mean total number of steps taken per day?
Note: Missing values (NA) in dataset are removed

##Qu: Plot a histogram of the total number of steps taken each day
```{r}

ggplot(NAless, aes(date, steps)) + geom_bar(stat = "identity", colour = "red", fill = "red", width = 0.7) + facet_grid(. ~ month, scales = "free") + labs(title = "Histogram of Total Number of Steps Taken Each Day", x = "Date", y = "Total number of steps")

```

##Qu: Calculate and report the mean and median total number of steps taken per day

Mean total number of steps taken per day:
```{r}

totalSteps <- aggregate(NAless$steps, list(Date = NAless$date), FUN = "sum")$x

mean(totalSteps)

```

Median total number of steps taken per day:
```{r}

median(totalSteps)

```

# What is the average daily activity pattern?

##Qu: Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)

```{r}

avgSteps <- aggregate(NAless$steps, list(interval = as.numeric(as.character(NAless$interval))), FUN = "mean")

names(avgSteps)[2] <- "meanOfSteps"

ggplot(avgSteps, aes(interval, meanOfSteps)) + geom_line(color = "red", size = 0.8) + labs(title = "Time Series Plot of the 5-minute Interval", x = "5-minute intervals", y = "Average Number of Steps Taken")

```

##Qu: Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?
```{r}

avgSteps[avgSteps$meanOfSteps == max(avgSteps$meanOfSteps), ]

```

# Imputing missing values

##Qu: The total number of rows with NAs:

```{r}

sum(is.na(data))

```

##Qu: Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.

(Note: The strategy used was to calculate the mean for that 5-minute interval, and replace the NA with that.)

##Qu: Create a new dataset that is equal to the original dataset but with the missing data filled in.

```{r}

extradata <- data 
for (i in 1:nrow(extradata)) {
    if (is.na(extradata$steps[i])) {
        extradata$steps[i] <- avgSteps[which(extradata$interval[i] == avgSteps$interval), ]$meanOfSteps
    }
}

head(extradata)
sum(is.na(extradata))

```

##Qu: Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. 

```{r}

ggplot(extradata, aes(date, steps)) + geom_bar(stat = "identity",
                                             colour = "red",
                                             fill = "red",
                                             width = 0.7) + facet_grid(. ~ month, scales = "free") + labs(title = "Histogram of Total Number of Steps Taken Each Day (NA replaced with extrapolated data)", x = "Date", y = "Total number of steps")

```

##Qu: Do these values differ from the estimates from the first part of the assignment? 
  - What is the impact of imputing missing data on the estimates of the total daily number of steps?

Mean total number of steps taken per day:
```{r}

newTotalSteps <- aggregate(extradata$steps, 
                           list(Date = extradata$date), 
                           FUN = "sum")$x
nMean <- mean(newTotalSteps)
nMean

```

Median total number of steps taken per day:
```{r}

nMedian <- median(newTotalSteps)
nMedian

```

Compare them with the two before imputing missing data:
```{r}

oMean <- mean(totalSteps)
nMean - oMean

oMedian <- median(totalSteps)
nMedian - oMedian

```

Findings (with the extrapolated data included): Mean remains the same, Median increases

# Are there differences in activity patterns between weekdays and weekends?

##Qu: Create a new factor variable in the dataset with two levels -- "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.

```{r}

head(extradata)
extradata$weekdays <- factor(format(extradata$date, "%A"))
levels(extradata$weekdays)
levels(extradata$weekdays) <- list(weekday = c("Monday", "Tuesday",
                                             "Wednesday", 
                                             "Thursday", "Friday"),
                                 weekend = c("Saturday", "Sunday"))
levels(extradata$weekdays)
table(extradata$weekdays)

```

##Qu: Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis).

```{r}

avgSteps <- aggregate(extradata$steps, 
                      list(interval = as.numeric(as.character(extradata$interval)), 
                           weekdays = extradata$weekdays),
                      FUN = "mean")
names(avgSteps)[3] <- "meanOfSteps"

library(lattice)
xyplot(avgSteps$meanOfSteps ~ avgSteps$interval | avgSteps$weekdays, 
       layout = c(1, 2), type = "l", 
       xlab = "Interval", ylab = "Number of steps")

```
