The run_analysis.R script containse the code that follows the 5 step described in the course project’s definition.

Download the dataset
Dataset  was downloaded and extracted under the folder called UCI HAR Dataset

Assign each data to variables
features <- features.txt 
The features selected for this database come from the accelerometer and gyroscope 3-axial raw signals tAcc-XYZ and tGyro-XYZ.

activities <- activity_labels.txt :  
List of activities and its corresponding codes are taken. 

subject_test <- test/subject_test.txt : 
contains test data of test subjects being observed

x_test <- test/X_test.txt : 
imported recorded features test data

y_test <- test/y_test.txt : 
imported test data of activities’code labels

subject_train <- test/subject_train.txt :
contains train data of 21-30 volunteer subjects being observed

x_train <- test/X_train.txt : 
contains recorded features of train data

y_train <- test/y_train.txt : 
contains train data of activities’code labels

Merges the training and the test sets to create one data set
xInfo  is created with x_train and x_test merging using rbind() function
yInfo  is created with y_train and y_test merging using rbind() function
Subject is created by merging subject_train and subject_test using rbind() function
Merged_Data is created by merging Subject, yInfo and xInfo using cbind() function

Extracts only the measurements on the mean and standard deviation for each measurement
CleanData  is generated by subsetting Merged_Data, selecting only columns: subject, code and the measurements on the mean and standard deviation (std) for each measurement

Uses descriptive activity names to name the activities in the data set
Entire numbers in code column of the CleanData replaced with corresponding activity taken from second column of the  activities variable

Appropriately labels the data set with descriptive variable names
code column in CleanData renamed into activities
All Acc in column’s name replaced by Accelerometer
All Gyro in column’s name replaced by Gyroscope
All BodyBody in column’s name replaced by Body
All Mag in column’s name replaced by Magnitude
All start with character f in column’s name replaced by Frequency
All start with character t in column’s name replaced by Time

From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject
FinalData is created by sumarizing CleanData taking the means of each variable for each activity and each subject, after groupped by subject and activity.
Export FinalData into FinalData.txt file.
