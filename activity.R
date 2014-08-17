#----------------------------------------------------------------------------------------
# Miscellaneous Setup
#----------------------------------------------------------------------------------------

library(ggplot2)
library(knitr)
library(markdown)

# Go to my Home Directory, then grab data out of folder and read it in
setwd("C:/Users/Alan/Documents/Data Science Course (Coursera)/Reproducible Data Homework I")
aData <- read.csv("activity.csv", colClasses=c("integer", "Date", "integer"))
# aData <- na.omit(aData)
knit("PA1_template.Rmd")

#----------------------------------------------------------------------------------------
# Plot 1a and 1b (Mean and medium # of steps taken each day)
#----------------------------------------------------------------------------------------

# Subset the data
# why does this REMOVE the NA observations from the dataset, while the tapply puts in ZEROs?
#plot1data <- aggregate(steps~date, aData, sum, na.rm=T)
#meanSteps <- mean(plot1data$steps)
#medianSteps <- median(plot1data$steps)
#meanSteps
#medianSteps
#hist(plot1data$steps, breaks=8)

totalSteps <- tapply(aData$steps, aData$date, sum, na.rm=T)
meanSteps <- mean(totalSteps)
medianSteps <- median(totalSteps)

meanSteps
medianSteps

averageSteps <- tapply(aData$steps, aData$date, mean, na.rm=T)

png(file="part2plot.png")
hist(totalSteps, breaks=20, col="grey", xlab="Total Steps per Day", main="TOTAL Steps - Distribution")
hist(averageSteps, breaks=20, col="red", xlab="Average Steps per Day", main="AVERAGE Steps - Distribution")
dev.off()

#----------------------------------------------------------------------------------------
# Plot 2 (Average # of steps at each 5 minute time interval throughout the day)
#----------------------------------------------------------------------------------------
# Get the average # of steps per 5-minute Interval during a typical day
intervalSteps <- tapply(aData$steps, aData$interval, mean, na.rm=T)

# Now get the labels for the 5-minute intervals
intervalTimes <- unique(aData$interval)

# Now find which interval the MAXIMUM value resides in
maxInterval <- intervalTimes[which(intervalSteps==max(intervalSteps))]
maxInterval

png(file="part3plot.png")
par(xaxs="r",yaxs="r")
plot(intervalTimes, intervalSteps, xlab="5-Minute Time Interval of Day", ylab="Average # of Steps in Time Interval", main="Average # of Steps Throughout Day", type="l")
dev.off()

#----------------------------------------------------------------------------------------
# Part 4 (Imputing Missing Values)
#----------------------------------------------------------------------------------------
# Calculate the number of observations in the dataset that have NA's
numComplete <- nrow(aData) - sum(complete.cases(aData))
numComplete

# Populate NA's w/ the mean value for that given time interval across the dataset
imputeData <- transform(aData, steps=ifelse(is.na(steps), intervalSteps, steps))
summary(imputeData)
totalSteps <- tapply(imputeData$steps, imputeData$date, sum, na.rm=T)
imputeStepMean <- mean(totalSteps)
imputeStepMean

imputeSteps <- tapply(imputeData$steps, imputeData$date, mean, na.rm=T)

png(file="part4aplot.png")
hist(imputeSteps, breaks=20, col="orange", xlab="Average Steps per Day", main="AVERAGE Steps - Distribution")
dev.off()

imputeiSteps <- tapply(imputeData$steps, imputeData$date, sum, na.rm=T)
meaniSteps <- mean(imputeiSteps)
medianiSteps <- median(imputeiSteps)

meaniSteps
medianiSteps

# Compare to the non-imputed dataset
png(file="part4bplot.png")
averageSteps <- tapply(aData$steps, aData$date, mean, na.rm=T)
hist(averageSteps, breaks=20, col="red", xlab="Average Steps per Day", main="AVERAGE Steps - Distribution")
dev.off()

meanSteps
medianSteps

#----------------------------------------------------------------------------------------
# Part 5 (Is there a difference between activity on Weekdays and Weekends?) - obviously yes, but let's look anyway.
#----------------------------------------------------------------------------------------
# Create 2 separate subsets of data
dataWE <- subset(imputeData, weekdays(date) %in% c("Saturday", "Sunday"))
dataWD <- subset(imputeData, !weekdays(date) %in% c("Saturday", "Sunday"))

# Now find/aggregate by averaging the number of steps per interval for each data subset
dataWE <- aggregate(steps ~ interval, dataWE, mean)
dataWD <- aggregate(steps ~ interval, dataWD, mean)

# Add column with label for the 2 subsets
dataWE <- cbind(dataWE, day = rep("Weekend"))
dataWD <- cbind(dataWD, day = rep("Weekday"))

# Combine the subsets and a specify the levels of the "factor"
dataAll <- rbind(dataWE, dataWD)
levels(dataAll$day) <- c("Weekend", "Weekday")

png(file="part5plot.png")
ggplot(dataAll, aes(x = interval, y = steps)) + geom_line() + facet_grid(day ~ .) + labs(x = "5-Minute Interval Throughout Day", y = "Average Number of Steps (w/ Imputed Data)")
dev.off()








# Optional way of doing the same thing above...
#
#weekInfo <- factor(weekdays(imputeData$date) %in% c("Saturday","Sunday"), labels=c("weekday","weekend"), ordered=FALSE)
#
#iSteps <- aggregate(imputeData$steps, by=list(interval=imputeData$interval, weekday=weekInfo), mean)
#
#library(ggplot2)
#g <- ggplot(iSteps, aes(interval/60, x))
#g + geom_line() + facet_grid(weekday ~ .) +
#     scale_x_continuous(breaks=0:6*4, labels=paste(0:6*4,":00", sep="")) +
#     theme_bw() +
#     labs(y="average number of steps in 5-min interval") +
#     labs(x="time of day (h)") +
#     labs(title="Daily activity pattern")





