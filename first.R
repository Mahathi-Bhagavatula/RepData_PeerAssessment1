numOfSteps <- sapply(split(doc$steps,doc$date), function(x) sum(x,na.rm=TRUE))
repValues <- sapply(doc$date, fucntion(x) rep(x, numOfSteps[[x]]))

doc$dates <- as.numeric(doc$date)
repetitions <- sapply(split(doc$steps, doc$dates), function(x) sum(x,na.rm=TRUE))
hist(rep(as.numeric(names(repetitions)), repetitions), xlab="Days", ylab="steps in each day", main="Histogram of sum of steps in each day")
median(repetitions)
mean(repetitions)