doc$weekday <- sapply(weekdays(as.Date(doc$date)), function(x) if(x=="Sunday"|x=="Saturday") return("weekend") else return("weekday"))

state <- data.frame(state, region = state.region)
xyplot(interval ~ steps | weekday, data = doc, layout = c(1, 2))

weekdayAvgSteps <- sapply(split(doc$steps,doc$interval), function(x) mean(x,na.rm=TRUE))
plot(as.numeric(weekdayAvgSteps), as.numeric(names(weekdayAvgSteps)))