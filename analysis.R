# THis script was written for the "Reproducible Research" course on Coursera
# Programming Assignement 1

# Setup ####

library(data.table)
library(lubridate)

# Load the data ####

data          <- fread("activity.csv")
data$date     <- ymd(data$date)
data$interval <- formatC(data$interval, width = 4, format = "d", flag = "0")

# Histogram of number of step per day ####

step.per.day        <- tapply(data$steps, data$date, mean, na.rm = TRUE)
mean.step.per.day   <- mean(step.per.day, na.rm = TRUE)
median.step.per.day <- median(step.per.day, na.rm = TRUE)

hist(step.per.day, xlab = "Total number of steps taken each day", col = "red", breaks = 15)

# Average dayly activity pattern ####

agg.data <- data[, list(steps = mean(steps, na.rm = TRUE)), by = interval]

agg.data$interval <- as.POSIXct(agg.data$interval, format = "%H%M") # uses today's date by default

with(agg.data, plot(interval, steps, type = "l", 
                    main = "average daily activity pattern",
                    xlab = "time", ylab = "Average number of steps"))

rush.time.end   <- agg.data[which.max(agg.data$steps), interval]
rush.time.start <- rush.time.end - minutes(5)

# Imputing missing values ####

# number of incomplete cases
total.na <- sum(!complete.cases(data))

# create a copy of the dataset
new.data <- data

# replace NA with the mean for the corresponding 5-minutes interval
new.data$steps <- ifelse(is.na(new.data$steps), 
                         agg.data$steps[match(new.data$interval, agg.data$interval)],
                         new.data$steps)

# Number of step per day with imputed missing data
step.per.day2        <- tapply(new.data$steps, new.data$date, mean, na.rm = TRUE)
mean.step.per.day2   <- mean(step.per.day2, na.rm = TRUE)
median.step.per.day2 <- median(step.per.day2, na.rm = TRUE)

hist(step.per.day2, xlab = "Total number of steps taken each day", col = "red", breaks = 15)

# differences in activity patterns between weekdays and weekends ####

# define a new factor variable daytType = "weekday" or "weekend"
new.data <- new.data[, dayType := factor(ifelse(wday(date) %in% c(1,7), "weekend", "weekday"))]

# aggregate data by interval and dayType
agg.data.per.daytype <- new.data[, list(steps = mean(steps, na.rm = TRUE)), by = list(interval,dayType)]
agg.data.per.daytype$interval <- as.POSIXct(agg.data.per.daytype$interval, format = "%H%M")

# plot activity pattern for weekday and weekend
library(lattice)

# set x-axis format
with(agg.data.per.daytype, sek <<- seq( interval[1], interval[length(interval)], by="1 hour"))
x.position <- as.numeric(sek)
x.label <- format(sek, "%H", drop0trailing = TRUE)

# plot
xyplot(steps ~ interval | dayType, data = agg.data.per.daytype, type = "l", layout = c(1,2),
       main = "Difference in activity pattern : weekday vs weekend", xlab = "Time [hour]",
       ylab = "Average number of steps", scales=list(x=list(at= x.position,labels=x.label)))
                                                     