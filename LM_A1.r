library(ggplot2)

mydata <- read.table("C://Users//Utkarsh//Documents//homework//Learning and Memory//data.ssv",header=FALSE)


#Question 2 Part B

match_trials <- subset(mydata, mydata$V5 == 1)
false_alarm_trials <- subset(mydata, mydata$V7 == 1)

new_df <- data.frame(
  Category = c(sprintf("Total Match Trials\nMean = %f ms\nStandard Deviation = %f ms",mean(match_trials$V8),sd(match_trials$V8)),
               sprintf("False Alarm Trials\nMean = %f ms\nStandard Deviation = %f ms",mean(false_alarm_trials$V8),sd(false_alarm_trials$V8))),
  Means = c(mean(match_trials$V8),mean(false_alarm_trials$V8)),
  SDs = c(sd(match_trials$V8),sd(false_alarm_trials$V8))
)

ggplot(new_df,
        aes(x = Category, y = Means)) +
  geom_bar(position = position_dodge(), stat = "identity", colour = "black", width = 0.2) +
  geom_errorbar(aes(ymin = Means - SDs, ymax = Means + SDs), width = .1) +
  labs(title = "Average Reaction Time of Match Trials and False Alarms") +
  ylab("Reaction Time (in miliseconds)")


#Question 2 Part C

#Calculating the mean reaction time of the entire dataset
M <- mean(mydata$V8)

#Subsetting the entire dataset into two parts, one where the reaction time is
#lesser than the mean, and the other where the reaction time is greater
lesser <- subset(mydata, mydata$V8<M)
greater <- subset(mydata, mydata$V8>M)

#Calculating the error rate of both the subset datasets
errorness_lesser <- (1-(sum(lesser$V4)/nrow(lesser))) * 100
errorness_greater <- (1-(sum(greater$V4)/nrow(greater))) * 100


#Subsetting the dataset into a dataset which only has reaction times not 3000ms
hits <- subset(mydata, mydata$V8 < 3000)

#Calculating the mean of this new dataset
M2 <- mean(hits$V8)

#Subsetting this new dataset into two datasets based on the mean
lesser_hits <- subset(hits, hits$V8<M2)
greater_hits <- subset(hits, hits$V8>M2)

#Calculating the error rate of these two datasets
errorness_lesser_hits <- (1-(sum(lesser_hits$V4)/nrow(lesser_hits))) * 100
errorness_greater_hits <- (1-(sum(greater_hits$V4)/nrow(greater_hits))) * 100

#Running linear regression
summary(lm(hits$V4~hits$V8))
