library(data.table)
library(lubridate)

# Load the data
data          <- fread("activity.csv")
data$date     <- ymd(data$date)
data$interval <- formatC(data$interval, width = 4, format = "d", flag = "0")

# Number of step per day
step.per.day        <- tapply(data$steps, data$date, mean, na.rm = TRUE)
mean.step.per.day   <- mean(step.per.day, na.rm = TRUE)
median.step.per.day <- median(step.per.day, na.rm = TRUE)

hist(step.per.day, xlab = "Total number of steps taken each day", col = "red", breaks = 15)

# Number of step vs time (for all days)
agg.data <- data[, list(steps = mean(steps, na.rm = TRUE)), by = interval]


times <- as.POSIXct(agg.data$interval, format = "%H%M") # uses today's date by default
plot(times, agg.data$steps, type = "l", 
     main = "average daily activity pattern",
     xlab = "time", ylab = "Average number of steps")

# 5-minute interval containing the maximum average number of steps
rush.time.end   <- times[which.max(agg.data$steps)]
rush.time.start <- rush.time.end - minutes(5)

# Number of incomplete cases :
total.na <- sum(!complete.cases(data)) # number of incomplete observations

# create a copy of the dataset and replace missing values.
# We replace with the mean for the corresponding 5-minutes interval

new.data <- data
new.data$steps <- ifelse(is.na(new.data$steps), 
                         agg.data$steps[match(new.data$interval, agg.data$interval)],
                         new.data$steps)

# Number of step per day with imputed missing data
step.per.day2        <- tapply(new.data$steps, new.data$date, mean, na.rm = TRUE)
mean.step.per.day2   <- mean(step.per.day2, na.rm = TRUE)
median.step.per.day2 <- median(step.per.day2, na.rm = TRUE)

hist(step.per.day2, xlab = "Total number of steps taken each day", col = "red", breaks = 15)

# differences in activity patterns between weekdays and weekends
new.data <- new.data[, dayType := factor(ifelse(wday(date) %in% c(1,7), "weekend", "weekday"))]





