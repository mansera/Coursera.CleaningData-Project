## First we set the working directory to the directory where the
## downloaded data is stored

      setwd("~/R/Johns Hopkins - Getting and Cleaning Data/UCI HAR Dataset")

## Second we have to bind together the training and the test sets
## into one data set

      tmp1 <- read.table("train/X_train.txt")
      tmp2 <- read.table("test/X_test.txt")
      X <- rbind(tmp1, tmp2)
      tmp1 <- read.table("train/subject_train.txt")
      tmp2 <- read.table("test/subject_test.txt")
      S <- rbind(tmp1, tmp2)
      tmp1 <- read.table("train/y_train.txt")
      tmp2 <- read.table("test/y_test.txt")
      Y <- rbind(tmp1, tmp2)

## Now we select the mean and standard deviation for each measurement

      features <- read.table("features.txt")
      indices_of_good_features <- grep("-mean\\(\\)|-std\\(\\)", features[, 2])
      X <- X[, indices_of_good_features]
      names(X) <- features[indices_of_good_features, 2]
      names(X) <- gsub("\\(|\\)", "", names(X))
      names(X) <- tolower(names(X))

## Now we use suitable names to describe the activities

      activities <- read.table("activity_labels.txt")
      activities[, 2] = gsub("_", "", tolower(as.character(activities[, 2])))
      Y[,1] = activities[Y[,1], 2]
      names(Y) <- "activity"

## We use the name as labels in the data set
      
      names(S) <- "subject"
      cleaned <- cbind(S, Y, X)
      write.table(cleaned, "merged_clean_data.txt")

## Now we create a tidy different data set that only
## have the average of each variable for each activity and each subject.
      
      uniqueSubjects = unique(S)[,1]
      numSubjects = length(unique(S)[,1])
      numActivities = length(activities[,1])
      numCols = dim(cleaned)[2]
      result = cleaned[1:(numSubjects*numActivities), ]
      row = 1
      
      for (s in 1:numSubjects) {
            
            for (a in 1:numActivities) {
                  
                  result[row, 1] = uniqueSubjects[s]
                  result[row, 2] = activities[a, 2]
                  tmp <- cleaned[cleaned$subject==s & cleaned$activity==activities[a, 2], ]
                  result[row, 3:numCols] <- colMeans(tmp[, 3:numCols])
                  row = row+1
            }
      }
      
## Now write the file that will be used as output with the tidy data set
      
      write.table(result, file="Tidy HA Smartphone Rec.txt", row.name=FALSE)
      
