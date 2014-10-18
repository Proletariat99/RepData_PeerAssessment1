options(scipen=4)
activity_filename <- "activity.csv"
act <- read.csv(activity_filename, stringsAsFactors=FALSE)
complete_act <- act[complete.cases(act),]
bydate_act <- split(complete_act, complete_act$date, drop=TRUE)
coltest <- bydate_act[[1]][1:3,1:3]
print(coltest)
c <- complete_act #lazeh
a <- bydate_act  #lazy
daily_steps <- vector()
for(date in bydate_act){
  daily_steps <- append(daily_steps, as.numeric(sum(date$steps)))
  #  mean(bydate_act[["2012-11-29"]]$"steps")      # just for future use, if necessary 
}
myhist <- hist(daily_steps)
daily_mean <- mean(daily_steps)
daily_median <- median(daily_steps)
by_interval_act <- split(complete_act, complete_act$interval, drop=TRUE)
interval_steps <- vector()
for(interval in by_interval_act){
  interval_steps <- append(interval_steps, sum(interval$steps))
}

plot(interval_steps, type="l", ylab = "Steps Per Interval", xlab = "Interval Identifier")
max_interval <- which.max(interval_steps)
total_count <- length(act$date)
temp <- act[complete.cases(act),]
complete_count <- length(temp$date)
no_of_na <- total_count - complete_count



na_act <- act
interval_avg <- aggregate(act$steps ~ act$interval, FUN=mean)
names(interval_avg) <- c("interval", "avsteps")
ia <- interval_avg
for(i in 1:nrow(na_act)){
  if(is.na(na_act[i,1]) == TRUE){
    na_act[i,1] <- ia[ia$interval == na_act[i,3],]$avsteps
  } 
}

