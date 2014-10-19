if (!file.exists("./data")){dir.create("./data")}
zippedURL <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"
download.file(zippedURL, destfile="./data/activity.zip",method="curl")
csvFile <- unzip("./data/activity.zip", exdir = "./data")
activityData <- read.csv(csvFile)

activityData$date <- as.Date(activityData$date, format="%Y-%m-%d")
summaryData <- summarise(group_by(activityData, date = factor(date)), mean = mean(steps, na.rm=TRUE), median = median(steps, na.rm=TRUE))
intervalSummary <- aggregate(data = activityData, steps ~ interval, FUN = mean)

intervalmean <- function(x) {
  intervalSummary$steps[x == intervalSummary$interval]
}

DF <- mutate(activityData, newinterval = intervalmean(interval))

newData <- activityData
for (i in 1:nrow(newData)) {
  if (is.na(newData$steps[i])) {
    newData$steps[i] <- intervalSummary[which(newData$interval[i] == intervalSummary$interval), ]$steps
  }
}

newData$weekdays <- weekdays(newData$date)