# load required packages
require(ggplot2)
require(lattice)

# Load the file reading in the 
setAs("character","myDate", function(from) as.Date(from, format="%Y-%m-%d") )
rdata = read.csv("activity.csv", colClasses=c("numeric", "myDate", "numeric"))

# Make a histogram of total number of steps taken a day
raggr_by_date = aggregate(steps ~ date, rdata, sum)
qp1 = qplot(date, steps, data=raggr_by_date, geom="histogram", stat="identity")
qp1 = qp1 + theme(axis.text.x=element_text(angle=90))
qp1

# calculate the mean and median of the steps taken per day,  is this enough??
mean(raggr_by_date$steps)
median(raggr_by_date$steps)

# time series plot of average steps taken in a given 5 minute interval averaged over
# the days
raggr_by_interval = aggregate(steps ~ interval, data=rdata, FUN=mean)
ggplot(raggr_by_interval, aes(interval, steps)) + geom_line() + theme(axis.text.x=element_text(angle=90))

# find the period that has the most average steps
raggr_by_interval$interval[raggr_by_interval[[2]] == max(raggr_by_interval$steps)]

# calculate the total number of periods in which the number of steps is missing
sum(is.na(rdata$steps))

# create a new data frame with missing values substituted by the average number
# of steps taken in that 5 minute period across all days
cdata = rdata
caggr_by_interval = aggregate(steps ~ interval, cdata, mean)

fn = function(x) {
  if (is.na(x[[1]])) {
    newval = caggr_by_interval$steps[caggr_by_interval$interval == as.numeric(x[[3]])]
  } else {
    newval = x[[1]]
  }
  newval
}
csteps = as.numeric(apply(rdata, 1, fn))
cdata$steps = csteps

# Make a histogram of total number of steps taken a day
caggr_by_date = aggregate(steps ~ date, cdata, sum)

# show only limited number of ticks and orient the tick labels vertically
qp2 = qplot(date, steps, data=caggr_by_date, geom="histogram", stat="identity")
qp2 = qp2 + theme(axis.text.x=element_text(angle=90))

# for comparison draw a line plot that shows both plots
# knit a new data frame from both aggregates
taggr = caggr_by_date
names(taggr) = c("date", "csteps")
xaggr = merge(taggr, raggr_by_date, by = 'date', all.x=TRUE)

ggplot(xaggr, aes(date)) + 
  geom_line(aes(y = csteps, colour = "processed steps")) + 
  geom_line(aes(y = steps, colour = "raw steps"))

# calculate the mean and median of the steps taken per day,  is this enough??
mean(caggr_by_date$steps)
median(caggr_by_date$steps)

# assign two factors weekend and weekday depending on the day of the activity
wdata = cdata
fn = function(x) {
  wdays = weekdays(as.Date(x[[2]]))
  if (wdays == "Saturday" | wdays == "Sunday") {
    "weekend"
  } else {
    "weekday"
  }
}
wdays = apply(wdata, 1, fn)
wdata$wdays = as.factor(wdays)

require(lattice)
waggr = aggregate(steps ~ interval+wdays, wdata, mean)
xyplot(steps~interval|wdays, data=waggr, type="l", xlab="Interval", ylab="Number of steps taken", layout=c(1,2))


