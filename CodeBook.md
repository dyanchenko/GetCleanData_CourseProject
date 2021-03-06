# Getting and Cleaning Data: Course Project. Code Book

This code book is related to the Getting and Cleaning Data course and the Course Project.

# The task of the course project was formulated as following:

The purpose of this project is to demonstrate your ability to collect, work with, and clean a data set. The goal is to prepare tidy data that can be used for later analysis. You will be graded by your peers on a series of yes/no questions related to the project. You will be required to submit: 1) a tidy data set as described below, 2) a link to a Github repository with your script for performing the analysis, and 3) a code book that describes the variables, the data, and any transformations or work that you performed to clean up the data called CodeBook.md. You should also include a README.md in the repo with your scripts. This repo explains how all of the scripts work and how they are connected.  

You should create one R script called run_analysis.R that does the following: 

1. Merges the training and the test sets to create one data set.
2. Extracts only the measurements on the mean and standard deviation for each measurement. 
3. Uses descriptive activity names to name the activities in the data set
4. Appropriately labels the data set with descriptive variable names. 

5. From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.

# Solution

run_analysis.R script was developed to solve this task.

## Data, used in the script

The following variables were created to upload necessary data

activity_labels - data from the file "./UCI HAR Dataset/activity_labels.txt"
\
Contains the dictionary of activities in the format {activity nuber, activity label}
\
\
features        - data from the file "./UCI HAR Dataset/features.txt"
\
Contains the list of all the measurements, collected and computed, which also correspond to the columns of data in files test_X.txt and train_X.txt
\
\
test_X          - data from the file "./UCI HAR Dataset/test/X_test.txt"
\
Contains the measurement data. Each 561 column is a separate variable/measurement, with the name from the features.txt file in the same order
\
\
test_y          - data from the file "./UCI HAR Dataset/test/y_test.txt"
\
Each raw has defines the activity number (and the activity), to which the measurements data of the same raw of test_X file are related.
\
\
test_subject    - data from the file "./UCI HAR Dataset/test/subject_test.txt"
\
Each raw has defines the subject number (person, took part in the research), to which the measurements data of the same raw of test_X file are related.
\
\
"Test" files represent the data from the test phase of the research project. "Train" files represent the active phase of the research project and have the same format and meaning:
\
\
train_X         - data from the file "./UCI HAR Dataset/train/X_train.txt"
\
Contains the measurement data. Each 561 column is a separate variable/measurement, with the name from the features.txt file in the same order
\
\
train_y         - data from the file "./UCI HAR Dataset/train/y_train.txt"
\
Each raw has defines the activity number (and the activity), to which the measurements data of the same raw of train_X file are related.
\
\
train_subject   - data from the file "./UCI HAR Dataset/train/subject_train.txt"
\
Each raw has defines the subject number (person, took part in the research), to which the measurements data of the same raw of train_X file are related.
\
\
tiny_data       - the final tiny dataset, which is also recorded to the file "tiny_data.txt" as the result of the script execution.

## Variables used in the script
url             - URL path to the archive in the web with all the data required
\
merged          - the data set, which was produced by merging test and train data.
\
rows_to_select  - logical vector, indicating if the feature (variable) is required by the question 2 of the course project task.
\
col_mean_std    - character vector with the names of the features (variables), corresponding to rows_to_select logical vector
\
N               - temporary variable, used in the loop to get the final tiny data set for Q5. Represent the total number of steps of the loop, required.
\
agg             - temporary variable, used to get the aggregated data of mean for a particular variable, when building a tiny data set for Q5.
\
tiny_data       - final tiny data set for Q5.

## Transformations with the data

### 1. Donwloading and extracting archive into the working directory

The first step to solve the task is to download the archive required and extract data, which was done using the following code


```r
url <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"

if(!file.exists("getdata_projectfiles_UCI HAR Dataset.zip")){
        download.file(url, destfile="getdata_projectfiles_UCI HAR Dataset.zip")
}

unzip("getdata_projectfiles_UCI HAR Dataset.zip")
```

### 2. Downloading data from files into script variables
The second step - we read the data into the corresponding script variables, using read.table() function:

```r
activity_labels    <- read.table("./UCI HAR Dataset/activity_labels.txt")
features           <- read.table("./UCI HAR Dataset/features.txt")

test_subject       <- read.table("./UCI HAR Dataset/test/subject_test.txt")
test_X             <- read.table("./UCI HAR Dataset/test/X_test.txt")
test_y             <- read.table("./UCI HAR Dataset/test/y_test.txt")

train_subject      <- read.table("./UCI HAR Dataset/train/subject_train.txt")
train_X            <- read.table("./UCI HAR Dataset/train/X_train.txt")
train_y            <- read.table("./UCI HAR Dataset/train/y_train.txt")
```
\
Now we can do the transformations. But in order to solve the requirements of Q1-Q4, we will do them not in that straight forward way, but in a slightly different, which seemed to be more easy to control and manupulate.

### 3. Solving questions Q3 and Q4

First, we extract data from test and train data sets, merge them with the corresponding subject and activity sets, assigning descriptive activities names and veriables names of measurement in the raw data.

First, we do these transformations for test data:

```r
#       Labelling data set columns with descriptive variable (features) names
names(test_X) <- c(make.names(features[,2], unique = TRUE))

#       Merging activities ID's with corresponding names and changing column 
#       names for better usability
test_y <- merge(test_y, activity_labels, by.x = "V1", by.y = "V1")
names(test_y) <- c("activity_number","activity_label")

#       Changing the name of the single column for subject set
names(test_subject) <- c("subject")

#       Merging together test data - subjects, activity names and measurements
test_X <- cbind(test_subject, test_y, test_X)
```
and then we do the same for train data:

```r
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
```
Now in test_X and train_X we have tables in the format 
\
{subject, activity_number, activity_label, 561 measurement variables with descriptive names}

### 4. Solving question Q1
Thus, we solved questions 3 and 4. After this, we merge these two data sets and get the merged one:

```r
merged <- rbind(test_X, train_X)
```

### 5. Solving question Q2
To solve the question 2 we need to identify all variables, that are either mean or standard deviations. We can do this, if we can find "mean", "Mean" or "Std" string in the names of variables. These search and transformation are done with the following code:

```r
# finding the features, which have the required substrings in their names. Result - boolean vector
rows_to_select <- c(grep("mean", features$V2), grep("Mean", features$V2),grep("std", features$V2),grep("Std", features$V2))

# getting only features names for means and std
col_mean_std <- features[rows_to_select,"V2"]

# transform into names format, as it's done for the test_X above
col_mean_std <- make.names(col_mean_std)

# getting a data set with only required measurements on mean and
# standard deviation for each measurement
merged <- merged[,c("subject","activity_number","activity_label", col_mean_std)]
```
So, now merged data set has only the data and is in the format, that meets the requirements of questions 1 - 4.

### 6. Solving question Q5
To solve the question 5 we need to aggregate our merged data by subject and activity, and for each variable, remained after the question 2, we need to calculate the average (mean) value of it for each pair (subject, activity). We need to put the result into a new data set - tiny_data and record it into the file "tiny_data.txt"
\
This transformation is finally done, using the following code:

```r
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
```
