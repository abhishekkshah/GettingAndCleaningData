#1. Merges the training and the test sets to create one data set.

X_train <- read.table("train/X_train.txt")
subject_train <- read.table("train/subject_train.txt")
y_train <- read.table("train/y_train.txt")

X_test <- read.table("test/X_test.txt")
subject_test <- read.table("test/subject_test.txt")
y_test <- read.table("test/y_test.txt")

X <- rbind(X_train, X_test)
subject <- rbind(subject_train, subject_test)
y <- rbind(y_train, y_test)

#2. Extracts only the measurements on the mean and standard deviation for each measurement. 
features <- read.table("features.txt")
indices <- grep("-mean\\(\\)|-std\\(\\)", features[, 2])
X <- X[, indices_of_good_features]
names(X) <- features[indices, 2]
names(X) <- gsub("\\(|\\)", "", names(X))

#3. Uses descriptive activity names to name the activities in the data set.

act_lab <- read.table("activity_labels.txt")
y[,1] = act_lab[y[,1], 2]
names(y) <- "activity"

#4. Appropriately labels the data set with descriptive activity names.

names(subject) <- "subject"
cleaned <- cbind(subject, y, X)
write.table(cleaned, "merged_clean_data.txt")

#5. Creates a 2nd, independent tidy data set with the average of each variable for each activity and each subject.

uniqueSubjects = unique(subject)[,1]
numSubjects = length(unique(subject)[,1])
numActivities = length(act_lab[,1])
numCols = dim(cleaned)[2]
result = cleaned[1:(numSubjects*numActivities), ]

row = 1
for (s in 1:numSubjects) {
  for (a in 1:numActivities) {
    result[row, 1] = uniqueSubjects[s]
    result[row, 2] = act_lab[a, 2]
    tmp <- cleaned[cleaned$subject==s & cleaned$activity==act_lab[a, 2], ]
    result[row, 3:numCols] <- colMeans(tmp[, 3:numCols])
    row = row+1
  }
}
write.table(result, "tidy_dataset.txt",row.name=FALSE)
