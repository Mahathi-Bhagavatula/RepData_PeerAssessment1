setwd("E:\\CourseEra\\Reproducable Research\\RepData_PeerAssessment1")
doc <- read.csv("activity.cdv")
avgSteps <- sapply(split(doc$steps,doc$interval), function(x) mean(x,na.rm=TRUE))
plot(as.numeric(avgSteps), as.numeric(names(avgSteps)))
avgSteps[max(avgSteps)]
     

