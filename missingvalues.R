nrow(doc[is.na(doc$steps),])
doc1 <- doc[!is.na(doc$steps),]
doc$Mean <- sapply(split(doc$steps,doc$interval), function(x) mean(x,na.rm=TRUE))
index <- which(is.na(doc$steps), arr.ind = TRUE)
doc$steps[index] <- doc$Mean[index]