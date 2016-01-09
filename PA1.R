# oK, a few introductory remarks:
# The documentation for the assignment asks for a .rmd file to be created, from which
# knitr is supposed to spin an .html report. First of all, I must have watched the course 
# videos for week 1 three times over by now and still cannot figure out how, nor why
# to create a .rmd file
# There is a menu option in the file menu of my version of R-Studio (0.99.485 on MacOS)
# which says "Knit" and it generates an HTML file with inline graphics without having
# to jump through any hoops. This is what I used. There is another option directly
# underneath that which says "Compile Notebook..." but it does exactly the same.
# Instead of wasting more time investigating information that is not there I put the
# report and the source code out there because as far as I can say, in the spirit of
# Reproducible Research that is exactly what is required, albeit way less complicated
# than in the "proper way" that I can't figure out anyway...


# Loading and preprocessing the data

### Download and read in the data
if (!file.exists("activity.csv")) {
  download.file(
    "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip",
    "activity.zip"
  )
  unzip("activity.zip")
}
df <- read.csv("activity.csv", stringsAsFactors = FALSE)

### Create new variables
df$date <- as.Date(df$date)
df$day <- weekdays(df[,"date"], abbreviate = TRUE)
head(df)

### Compute average steps by date (i.e. for each day)
sum.by.date <- tapply(df$steps, df$date, sum, na.rm = TRUE)
mean.sum.by.date <- round(mean(sum.by.date))
med.sum.by.date <- round(median(sum.by.date))
head(sum.by.date)

### Compute average steps by 5-minute interval and by days of week and across all days
mean.by.intl <- tapply(df$steps, list(df$interval, df$day), mean, na.rm = TRUE)
mean.by.intl <- data.frame(interval = as.numeric(rownames(mean.by.intl)), mean.by.intl)
mean.by.intl$All <- tapply(df$steps, df$interval, mean, na.rm = TRUE)
max.index <- which(mean.by.intl$All == max(mean.by.intl$All))
max.intl <- mean.by.intl[max.index, "interval"]
head(mean.by.intl)

## Mean total number of steps taken per day?

hist(sum.by.date, xlab = "total steps taken per day", ylab = "frequency", main = NULL)
abline(v = mean.sum.by.date, col = "red", lwd = 2)
abline(v = med.sum.by.date, col = "blue", lwd = 2)
text(mean.sum.by.date, 2, paste("mean = ", mean.sum.by.date), col = "red", pos = 2)
text(med.sum.by.date, 2, paste("median = ", med.sum.by.date), col = "blue", pos = 4)

print(paste("The average number of steps taken per day is ", mean.sum.by.date, ", the median is ", med.sum.by.date))

barplot(sum.by.date, ylab = "total steps taken per day")

# Generating an average daily activity pattern

plot(
  mean.by.intl$interval,
  mean.by.intl$All,
  type = "l",
  xlab = "5-minute interval",
  ylab = "average steps taken through all days"
)
abline(v = max.intl, col = "red", lwd = 2)
text(max.intl, 5, paste("5-Minute Interval = ", max.intl), col = "red", pos = 4)

print(paste("5-minute interval containing the maximum amount of steps through all days is ", max.intl, "."))


# Imputing missing values

NA.rec <- which(is.na(df$steps))
NA.count <- length(NA.rec)
print(paste("There are ", NA.count, " missing values in the original data set."))

### Create a new dataset from the original with the missing substituted by the
### the mean for that interval for that day of the week
new.df <- df
for (i in NA.rec){
  new.df$steps[i] = mean.by.intl[mean.by.intl$interval == new.df$interval[i], new.df$day[i]]
}
new.df$type <- ifelse(new.df$day %in% c("Sat", "Sun"), "weekend", "weekday")
head(new.df)

### Compute average steps by date
new.sum.by.date <- tapply(new.df$steps, new.df$date, sum, na.rm = TRUE)
new.mean.sum.by.date <- round(mean(new.sum.by.date))
new.med.sum.by.date <- round(median(new.sum.by.date))
head(new.sum.by.date)

### Caclulate average steps by 5-minute interval and by weekend/weekday
library(reshape2)
new.mean.by.intl <- tapply(new.df$steps, list(new.df$interval, new.df$type), mean)
new.mean.by.intl <- data.frame(
  interval = as.numeric(rownames(new.mean.by.intl)),
  new.mean.by.intl
)
new.mean.by.intl <- melt(
  new.mean.by.intl,
  id = "interval",
  measure.vars = c("weekend", "weekday")
)
names(new.mean.by.intl) = c("interval", "type", "steps")
new.mean.by.intl$type <- as.factor(new.mean.by.intl$type)
head(new.mean.by.intl)

hist(
  new.sum.by.date,
  xlab = "total steps taken per day (new)",
  ylab = "frequency",
  main = NULL
)
abline(v = new.mean.sum.by.date, col = "red", lwd = 2)
abline(v = new.med.sum.by.date, col = "blue", lwd = 2)
text(new.mean.sum.by.date, 2, paste("mean = ", new.mean.sum.by.date), col = "red", pos = 2)
text(new.med.sum.by.date, 2, paste("median = ", new.med.sum.by.date), col = "blue", pos = 4)

print(paste("After imputing missing values, the new mean and median of steps taken per day are ", new.mean.sum.by.date, " and ", new.med.sum.by.date, " respectively, compared to ", mean.sum.by.date, " and ", med.sum.by.date, " before imputing."))

# differences in activity patterns between weekdays and weekends?

library(lattice)
xyplot(
  steps ~ interval | type,
  data = new.mean.by.intl,
  layout = c(1,2),
  type = "l",
  xlab = "5-minute interval",
  ylab = "mean steps taken"
)
