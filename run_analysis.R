#  1. Merges the training and the test sets to create one data set.
#
#  2. Extracts only the measurements on the mean and standard deviation 
#     for each measurement. 
#
#  3. Uses descriptive activity names to name the activities in the data set
#
#  4. Appropriately labels the data set with descriptive variable names. 
#
#  5. From the data set in step 4, creates a second, independent tidy data 
#     set with the average of each variable for each activity and each 
#     subject.

url <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"

if(!file.exists("getdata_projectfiles_UCI HAR Dataset.zip")){
        download.file(url, destfile="getdata_projectfiles_UCI HAR Dataset.zip")
}

unzip("getdata_projectfiles_UCI HAR Dataset.zip")

activity_labels       <- read.table("./UCI HAR Dataset/activity_labels.txt")
features              <- read.table("./UCI HAR Dataset/features.txt")

test_subject       <- read.table("./UCI HAR Dataset/test/subject_test.txt")
test_X             <- read.table("./UCI HAR Dataset/test/X_test.txt")
test_y             <- read.table("./UCI HAR Dataset/test/y_test.txt")

train_subject       <- read.table("./UCI HAR Dataset/train/subject_train.txt")
train_X             <- read.table("./UCI HAR Dataset/train/X_train.txt")
train_y             <- read.table("./UCI HAR Dataset/train/y_train.txt")

#  Q1. Merges the training and the test sets to create one data set.

##      ------------------------------------------------------
##      1.2. First, merging all relevant test data together
##      ------------------------------------------------------

#       Q3. Uses descriptive activity names to name the activities in 
#       the data set
#       Q4. Appropriately labels the data set with descriptive variable names.

#       Labelling data set with descriptive variable (features) names
names(test_X) <- c(make.names(features[,2], unique = TRUE))

#       Merging activities ID's with corresponding names and changing column 
#       names for better usability
test_y <- merge(test_y, activity_labels, by.x = "V1", by.y = "V1")
names(test_y) <- c("activity_number","activity_label")

#       Changing the name of the column for subjects
names(test_subject) <- c("subject")

#       Merging together test data - subjects, activity names and measurements
test_X <- cbind(test_subject, test_y, test_X)
##      Q3, Q4 - done

##      ------------------------------------------------------
##      1.2. Second, merging all relevant train data together
##      ------------------------------------------------------

#       Q3. Uses descriptive activity names to name the activities in 
#       the data set
#       Q4. Appropriately labels the data set with descriptive variable names.

#       Labelling data set with descriptive variable (features) names
names(train_X) <- c(make.names(features[,2], unique = TRUE))

#       Merging activities ID's with corresponding names and changing column 
#       names for better usability
train_y <- merge(train_y, activity_labels, by.x = "V1", by.y = "V1")
names(train_y) <- c("activity_number","activity_label")

#       Changing the name of the column for subjects
names(train_subject) <- c("subject")

#       Merging together train data - subjects, activity names and measurements
train_X <- cbind(train_subject, train_y, train_X)
##      Q3, Q4 - done

#       Merging together test and train data -> Q1.
merged <- rbind(test_X, train_X)

## ------------------------------------------------------------------------
## -----_______________  Q1, Q3, Q4 - DONE --------------------------------
## ------------------------------------------------------------------------

#  ------------------------------------------------------------------------
#  Q2. Extracts only the measurements on the mean and standard deviation 
#       for each measurement.
# -------------------------------------------------------------------------
# Getting only the features list for the measurements on the mean and 
# standard deviation for each measurement. To do so, we apply grep() to find
# features names, which have "mean", "Mean" or "std" substrings.

# find the features wiht required substrings. Result - boolean vector
rows_to_select <- c(grep("mean", features$V2), grep("Mean", features$V2),grep("std", features$V2),grep("Std", features$V2))

# getting only features names for means and std
col_mean_std <- features[rows_to_select,"V2"]

# transform into names format, as it's done for the test_X above
col_mean_std <- make.names(col_mean_std)

#  COMMENT: We get only the names of features, related to mean and std 
#  measurements, but in the order, which is not the same, as they are 
#  listed in the features list or names(test_X), which might be ok for 
#  the analyst, who is looking and analyzing by variables' names

# getting a data set with only required measurements on mean and
# standard deviation for each measurement
merged <- merged[,c("subject","activity_number","activity_label", col_mean_std)]

## ------------------------------------------------------------------------
## ---------------------------  Q2  - DONE --------------------------------
## ------------------------------------------------------------------------


#  -------------------------------------------------------------------------
#  Q5. From the data set in step 4, creates a second, independent tidy data
#  set with the average of each variable for each activity and each subject.
# --------------------------------------------------------------------------


# we initialize tiny_data set with the calculated mean of a first variable for each 
# pair {subject, activity}.
tiny_data <- aggregate(merged[,4], by = list(subject = merged$subject, activity_number = merged$activity_number, activity_label = merged$activity_label), FUN = mean, na.action = na.omit)
# Because aggregate returns x as variable name, we change it back to the 
# original, from the merged data set
names(tiny_data)[4] <- names(merged[4])

# In the loop we calculate the mean of each remaining variable for each subject and 
# activity, and add them to the tiny_data set, using keypair - {subject, activity}.

# Setting the number of remaining required loop iterations
N <- length(names(merged))-4

for(i in 1:N){
        # calculating mean of a variable for each pair {subject, activity}
        agg <- aggregate(merged[,i+4], by = list(subject = merged$subject, activity_number = merged$activity_number, activity_label = merged$activity_label), FUN = mean, na.action = na.omit)
        
        # adding the calculated data to the tiny_data data set, using key pair - 
        # {subject, activity}
        tiny_data <- merge(tiny_data, agg, by = intersect(names(tiny_data),names(agg)))
        
        # Because aggregate returns x as variable name, we change it back to the 
        # original, from the merged data set, we used in this particular loop step
        names(tiny_data)[i+4] <- names(merged[i+4])
}

# Writing the final tiny data set into file
write.table(tiny_data, "tiny_data.txt", row.names = FALSE)
