# Unzip and store data, clean up the raw file
activity <- unzip("activity.zip")
rawdata <- read.csv(activity)
rm(activity)

# Convert date variable from factor to date, and remove NAs
data <- na.omit(rawdata)
data$date <- as.Date(data$date)


stepsPerDay = aggregate(data[, 1], list(data$date), sum)
names(stepsPerDay) <- c("date", "totalSteps")

hist(stepsPerDay$totalSteps, 
        xlab="Total number of steps taken each day", 
         ylab="Frequency", 
         main="Histogram of total number of steps taken each day", col="red")


fiveMinIntervals = aggregate(data.table[, 1], list(data.table$interval), mean)
names(fiveMinIntervals) <- c("interval", "averageSteps")


plot(fiveMinIntervals$interval, fiveMinIntervals$averageSteps, 
     type="l",
     xlab="Interval",
     ylab="Average steps taken",
     main="Average number of steps taken")

maxStepsInterval = fiveMinIntervals[fiveMinIntervals$averageSteps == max(fiveMinIntervals$averageSteps),1]


totalNAs <- sum(is.na(data.table.raw))


for(i in 1:nrow(data.table.raw))
{
  if(is.na(data.table.raw[i,1]))
  {
    data.table.raw[i,1]<- mean(data.table.raw[,1], na.rm = TRUE)
  }
  if(is.na(data.table.raw[i,2]))
  {
    data.table.raw[i,2]<- mean(data.table.raw[,2], na.rm = TRUE)
  }
}



data.table.filled["dayType"] <- as.factor(c("weekday","weekend"))
data.table.filled$dayType <- "weekday"
data.table.filled$dayType[weekdays(data.table.filled$date) %in% c("Saturday", "Sunday")] <- "weekend"


stepsPerDayType <- aggregate(data.table.filled$steps, list(data.table.filled$interval, data.table.filled$dayType), mean)names(stepsPerDayType) <- c("interval", "totalSteps")
names(stepsPerDayType) <- c("interval", "dayType", "averageSteps")

qplot(interval, averageSteps, data=stepsPerDayType,
      +       geom="line",
      +       xlab="Interval",
      +       ylab="Number of Steps (Average)",
      +       main="Average steps taken Weekends vs. Weekdays",
      +       facets =dayType ~ .)

aggregate(data.table[, 1], list(data.table$date), mean)