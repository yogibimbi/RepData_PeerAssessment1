library(dplyr)

unzip("activity.zip")
data = read.csv("activity.csv")

# calculate steps taken per day
# group data by date, calculate daily total of steps
bydate = group_by(data, date)
steps = summarize(bydate, total = sum(steps))
hist(steps$total)

# data must be cleaned of NAs, otherwise mean and median will throw errors
cleaned = steps$total[!is.na(steps$total)]
print(paste("mean steps per day:", mean(cleaned)))
print(paste("median steps per day:", median(cleaned)))

# group steps by interval
cleaned_data = filter(data, !is.na(steps))
byinterval = group_by(cleaned_data, interval)
intsteps = summarize(byinterval, mean = mean(steps))
plot(intsteps, type="l", ylab = "steps / interval", xlab = "5-minute interval", main="average activity pattern")
print(paste("maximum number of steps per 5-minute interval:", max(intsteps$mean)))

# compute NAs
print("isNA")
print(table(is.na(data$steps)))
# if steps is NA, set steps to the mean for this interval, works on 2012-10-01, but somehow not on 2012-10-08, so mean and median are all screwed up as well (see further below)
no_na_data = mutate(data, steps = ifelse (is.na(steps), intsteps$mean[intsteps$interval == interval], steps))
# prepare data analogous to the first time around
no_na_bydate = group_by(no_na_data, date)
no_na_steps = summarize(no_na_bydate, total = sum(steps))
hist(no_na_steps$total)
# same for mean and median
print(paste("mean steps per day:", mean(no_na_steps$total)))
print(paste("median steps per day:", median(no_na_steps$total)))
