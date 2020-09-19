## Source of data for this project: https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip

## 1. Merge     Merges the training and the test sets to create one data set.
getwd()
x_train1 <- read.table("./UCI HAR Dataset/train/X_train.txt")
x_test1 <- read.table("./UCI HAR Dataset/test/X_test.txt")
x_data <- rbind(x_train1, x_test1)
rm()

y_train1 <- read.table("./UCI HAR Dataset/train/y_train.txt")
y_test1 <- read.table("./UCI HAR Dataset/test/y_test.txt")
y_data <- rbind(y_train, y_test)

rm()

subject_train1 <- read.table("./UCI HAR Dataset/train/subject_train.txt")
subject_test1 <- read.table("./UCI HAR Dataset/test/subject_test.txt")
subject_data <- rbind(subject_train1, subject_test1)

rm()

## 2. Extracts only the measurements on the mean and standard deviation for each
## measurement.

features <- read.table("./UCI HAR Dataset/features.txt") 
indices_of_good_features <- grep("-mean\\(\\)|-std\\(\\)", features[, 2])

x_data <- x_data[, indices_of_good_features]
names(x_data) <- features[indices_of_good_features, 2]
names(x_data) <- gsub("\\(|\\)", "", names(x_data))
names(x_data) <- tolower(names(x_data))

## 3. Uses descriptive activity names to name the activities in the data set

activities <- read.table("./UCI HAR Dataset/activity_labels.txt")
activities[, 2] = gsub("_", "", tolower(as.character(activities[, 2])))
y_data[,1] = activities[y_data[,1], 2]
names(y_data) <- "activity"

## 4. Appropriately labels the data set with descriptive variable names.

names(subject_data) <- "subject"
cleaned_data <- cbind(subject_data, y_data, x_data)
write.table(cleaned_data, "merged_clean_data.txt")


## 5. From the data set in step 4, creates a second, independent tidy data set 
## with the average of each variable for each activity and each subject.

unique_subjects = unique(subject_data)[,1]
num_subjects = length(unique_subjects)
num_activities = length(activities[,1])
num_cols = dim(cleaned_data)[2]
result = cleaned_data[1:(num_subjects*num_activities), ]

row = 1
for (s in 1:num_subjects){
    for (a in 1:num_activities){
        result[row, 1] = unique_subjects[s]
        result[row, 2] = activities[a, 2]
        tmp <- cleaned_data[cleaned_data$subject==s & cleaned_data$activity==activities[a, 2], ]
        result[row, 3:num_cols] <- colMeans(tmp[, 3:num_cols])
        row = row+1
    }
}
write.table(result, "data_set_with_the_averages.txt")

