#Loading and preprocessing the data

#1. Load the data

#opening and viewing dataset
stepsdata <- read.csv("activity.csv")
head(stepsdata)

#2. Process/transform the data (if necessary) into a format suitable for your analysis  

#checking data type in columns
class(stepsdata[,1])
class(stepsdata[,2])
class(stepsdata[,3])

#converting date column to date data type
stepsdata[,2] <- as.Date(stepsdata[,2])

# What is mean total number of steps taken per day?

#1. Calculate the total number of steps taken per day
dayandsteps <- data.frame(day = stepsdata$date, steps = stepsdata$steps)
stepsperday <- aggregate(. ~ day, data=dayandsteps, FUN=sum)


#2. Make a histogram of the total number of steps taken each day
hist(stepsperday[,2])

#3. Calculate and report the mean and median of the total number of steps taken per day
meansteps <- mean(stepsperday[,2])
mediansteps <- median(stepsperday[,2])
print(paste("Mean steps per day: ",meansteps))
print(paste("Median steps per day: ",mediansteps))

#What is the average daily activity pattern?
  
#1. Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps
#taken, averaged across all days (y-axis)

intervalsteps <- data.frame(interval = stepsdata$interval, steps =stepsdata$steps)
averagestepsperinterval <- aggregate(. ~ interval, data=intervalsteps, FUN=mean)

plot(averagestepsperinterval[,1], averagestepsperinterval[,2], type = "l", main = "Average Steps Per Interval", xlab = "Interval", ylab = "Average Number of Steps")

#2. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?
maxperinterval <- max(averagestepsperinterval[,2])
position_max <- which(averagestepsperinterval[,2] == maxperinterval)
interval_with_max <- averagestepsperinterval[position_max,1]
print(paste("Interval with maximum number of steps: ",interval_with_max))

#Imputing missing values

#1. Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)
missingpositions <- which(is.na(stepsdata[,1]))
numberofmissing <- length(missingpositions)
print(paste("Number of mission values: ",numberofmissing))


#2. Devise a strategy for filling in all of the missing values in the dataset

#Calculate mean steps per interval
meanstepsperday <- aggregate(. ~ day, data=dayandsteps, FUN=mean)

meanstepsperday$day <- as.character(meanstepsperday$day)

averageint <- mean(averagestepsperinterval[,2])
averageint

#Since steps data is missing for the whole day of 
newrow1 <- c("2012-10-01", 37.3826)
newrow2 <- c("2012-10-08", 37.3826)
newrow3 <- c("2012-11-01", 37.3826)
newrow4 <- c("2012-11-04", 37.3826)
newrow5 <- c("2012-11-09", 37.3826)
newrow6 <- c("2012-11-10", 37.3826)
newrow7 <- c("2012-11-14", 37.3826)
newrow8 <- c("2012-11-30", 37.3826)
completemeanstepsperday <- rbind(newrow1,newrow2,newrow3,newrow4,newrow5,newrow6,newrow7,newrow8,meanstepsperday)
#Convert NAs into mean values per that day of 2012-10-01, we replace it with overall mean per interval:
completemeanstepsperday$day <- as.Date(completemeanstepsperday$day)
completemeanstepsperday$steps <- as.numeric(completemeanstepsperday$steps)



for (i in 1:17568) {
  
  
  if(is.na(stepsdata[i,1])) {
    #missday <- stepsdata[i,2]
    #dayposition <- which(completemeanstepsperday$day == missday)
    stepsdata[i,1] <- averageint
      }
  
}

#3. Create a new dataset that is equal to the original dataset but with the missing data filled in.
write.csv(stepsdata, file = "output2.csv")

#4.Make a histogram of the total number of steps taken each day and Calculate and report the mean 
#and median to1tal number of steps taken per day. Do these values differ from the estimates from the first
#part of the assignment? What is the impact of imputing missing data on the estimates of the total daily 
#number of steps?

dayandsteps2 <- data.frame(day = stepsdata$date, steps = stepsdata$steps)
stepsperday2 <- aggregate(. ~ day, data=dayandsteps2, FUN=sum)

hist(stepsperday2[,2])
meansteps2 <- mean(stepsperday2[,2])
mediansteps2 <- median(stepsperday2[,2])
print(paste("Mean steps per day: ",meansteps2))
print(paste("Median steps per day: ",mediansteps2))

#Are there differences in activity patterns between weekdays and weekends?

#1. Create a new factor variable in the dataset with two levels – “weekday” and “weekend” indicating whether 
#a given date is a weekday or weekend day.

weekendposition <- which((weekdays(stepsdata$date) == "Saturday")|(weekdays(stepsdata$date) == "Sunday"))
weekdayposition <- which(!(weekdays(stepsdata$date) == "Saturday")&!(weekdays(stepsdata$date) == "Sunday"))
addcolumn <- c(1:17568)
addcolumn[weekendposition] <- "Weekend"
addcolumn[weekdayposition] <- "Weekday"

newstepsdata <- cbind(stepsdata,daytype= addcolumn)

#2. Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and 
#the average number of steps taken, averaged across all weekday days or weekend days (y-axis). 
#See the README file in the GitHub repository to see an example of what this plot should look 
#like using simulated data.

weekdaysdata <- data.frame(steps = newstepsdata$steps[weekdayposition],date = newstepsdata$date[weekdayposition], interval = newstepsdata$interval[weekdayposition])
weekendsdata <- data.frame(steps = newstepsdata$steps[weekendposition],date = newstepsdata$date[weekendposition], interval = newstepsdata$interval[weekendposition])

intervalsteps1 <- data.frame(interval = weekdaysdata$interval, steps =weekdaysdata$steps)
averagestepsperinterval1 <- aggregate(. ~ interval, data=intervalsteps1, FUN=mean)

intervalsteps2 <- data.frame(interval = weekendsdata$interval, steps =weekendsdata$steps)
averagestepsperinterval2 <- aggregate(. ~ interval, data=intervalsteps2, FUN=mean)


par(mfcol=c(2,1))

plot(averagestepsperinterval2[,1], averagestepsperinterval2[,2], type = "l", main = "weekends", xlab = "Interval", ylab = "Average Number of Steps")
plot(averagestepsperinterval1[,1], averagestepsperinterval1[,2], type = "l", main = "weekdays", xlab = "Interval", ylab = "Average Number of Steps")