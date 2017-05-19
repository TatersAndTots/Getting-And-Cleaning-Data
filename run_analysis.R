setwd("~/R_Coding/Mod3//Final Project")
rm(list=ls())
library(dplyr)

#Steps: Read the data in, strip the undeeded columns in the xtest and xtrain sets,
#cbind() all the pieces of the test set and training set together, add a column that tells
#which set each row came from (a factor column of Test or Train), and then rbind() the two sets into one table

#bring in all the pieces of our dataset
subtest <- read.table("~/R_Coding/Mod3/Final Project/UCI HAR Dataset/test/subject_test.txt", header = FALSE)
xtest <- read.table("~/R_Coding/Mod3/Final Project/UCI HAR Dataset/test/X_test.txt", header = FALSE)
ytest <- read.table("~/R_Coding/Mod3/Final Project/UCI HAR Dataset/test/y_test.txt", header = FALSE)
subtrain <- read.table("~/R_Coding/Mod3/Final Project/UCI HAR Dataset/train/subject_train.txt", header = FALSE)
xtrain <- read.table("~/R_Coding/Mod3/Final Project/UCI HAR Dataset/train/X_train.txt", header = FALSE)
ytrain <- read.table("~/R_Coding/Mod3/Final Project/UCI HAR Dataset/train/y_train.txt", header = FALSE)
feat <- read.table("~/R_Coding/Mod3/Final Project/UCI HAR Dataset/features.txt", header = FALSE)

#determine what columns are worth keeping. We need means and std's. Lets subset by anything with "mean" or "std" in it
featmstd <- subset(feat, grepl("mean", feat$V2) | grepl("std", feat$V2))
names <-as.character(featmstd[,2])
names <- gsub("\\(\\)", "", names)
names <- gsub("mean", "Mean", names)
names <- gsub("std", "Std", names)
drop <- featmstd$V1

#And now to actually subset by those columns
xtest2 <- xtest[drop]
xtrain2 <- xtrain[drop]

#Combining our data with a new factor variable to tell which set each row came from
test <- cbind(rep("Test", length(ytest$V1)),subtest, ytest, xtest2)
train <- cbind(rep("Train", length(ytrain$V1)), subtrain, ytrain, xtrain2)
names(test) <- c("Set", "Subject", "Activity", names)
names(train) <- c("Set", "Subject", "Activity", names)

#We combine the two sets into one data frame and clean up the Activity column
data <- rbind(test, train)
data[,3] <- as.factor(data[,3])

act <- function(x) {
  if(x == 1) {x <- "Walking"}
  else if(x == 2) {x <- "Walking Upstairs"}
  else if(x == 3) {x <- "Walking Downstairs"}
  else if(x == 4) {x <- "Sitting"}
  else if(x == 5) {x <- "Standing"}
  else if(x == 6) {x <- "Laying"}
}

data[,3] <- sapply(data[,3], act)

#Finally, we build a tidy dataset and write it to a file
data_tidy <- ddply(data, c("Subject","Activity"), numcolwise(mean))
write.table(data_tidy, "tidy dataset.txt")
