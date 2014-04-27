## 
##
## run_analysis.R : Creates a tidy data set from UCI HAR Dataset
## Set working directory to the root of the data set before running
##
run_analysis <- function()
{
  Xtrain <- read.table("train/X_train.txt")
  Ytrain <- read.table("train/Y_train.txt")
  Xtest <- read.table("test/X_test.txt")
  Ytest <- read.table("test/Y_test.txt")
  Subtrain <- read.table("train/subject_train.txt")
  Subtest <- read.table("test/subject_test.txt")
  features <- read.table("features.txt")
  activityLabels <- read.table("activity_labels.txt")
  
  ## 1: Merge the training and the test sets to create one data set.
  Y <- rbind(Ytrain, Ytest)
  X <- rbind(Xtrain, Xtest)
  Sub <- rbind(Subtrain, Subtest)
  
  ## replace non-descriptive column names with those from features.txt
  colnames(X) <- features[,2]
  
  ## 2. Extract only the measurements on the mean and standard deviation 
  ##   for each measurement. 
  X <- X[, sort(c(grep("std()", colnames(X), fixed=TRUE), grep("mean()", colnames(X), fixed=TRUE)))]
  
  ## 3. Use descriptive activity names to name the activities in the data set
  YActivities <- sapply(Y, function(t) activityLabels[t,2])
  colnames(YActivities) <- "Activity"
  
  ## 4. Appropriately label the data set with descriptive activity names. 
  X <- cbind(YActivities, X)
  colnames(Sub) <- "Subject"
  X <- cbind(Sub, X)
  
  ## 5. Create a second, independent tidy data set with the average of each variable for each activity and each subject. 
  aveByAct <- sapply(split(X, X$Activity), function(t) colMeans(t[,-(1:2)]))
  aveBySubj <- sapply(split(X, X$Subject), function(t) colMeans(t[,-(1:2)]))
  
  X2 <- cbind(aveByAct, aveBySubj)
  write.table(X2, "output.txt")
  
}
