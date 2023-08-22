#1. Merges the training and the test sets to create one data set.
#2. Extracts only the measurements on the mean and standard deviation for each measurement. 
#3. Uses descriptive activity names to name the activities in the data set
#4. Appropriately labels the data set with descriptive variable names. 
#5. From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.

#Working directory is set to the folder the raw files live in.

#load relevant libraries
library(dplyr)

#Meta Data Loads
#Load Activity Labels
labels_activity<-read.table("activity_labels.txt", col.names = c("Code","Activity")) 

#Load Column Names
#this is the second column of data in the file. 
features<-read.table("features.txt", col.names = c("Number", "Features"))[,2]

#Test Data Loads
test_subject<-read.table("./test/subject_test.txt", col.names="Subject")
test_x<-read.table("./test/x_test.txt", col.names=features) 
test_y<-read.table("./test/y_test.txt", col.names="Activity")

#Update to label that it's test data
test_x<-cbind(test_x, Data_Source="Test")
test_y<-cbind(test_y, Data_Source="Test")

#Training Data Loads
training_subject<-read.table("./train/subject_train.txt", col.names="Subject")
training_x<-read.table("./train/x_train.txt", col.names=features)
training_y<-read.table("./train/y_train.txt", col.names="Activity")

#Update to label that it's training data
training_x<-cbind(training_x, Data_Source="Training")
training_y<-cbind(training_y, Data_Source="Training")

#Step 1: Merge Training and Test Datasets
#ensure order of test and training are the same for each step to ensure proper data alignment.
x_all <- rbind(test_x, training_x)
y_all <- rbind(test_y, training_y)
subject_all <- rbind(test_subject, training_subject)
merged_sets <- cbind(subject_all, y_all, x_all)

#Step 2: Limit Measurements to Mean and SD
measures <- select(merged_sets, Subject, Activity, matches("mean|std"))

#Step 3: Use descriptive activity names
measures$Activity <- factor(measures$Activity, levels=labels_activity[,1], labels=labels_activity[,2])

#Step 4: label the data set with descriptive variable names. 
#identify current column labels
names(measures)

#relabel for clear column meanings
#if the label starts with t or f.
names(measures)<-gsub("^t","Time_",names(measures))
names(measures)<-gsub("^f","Frequency_",names(measures))
names(measures)<-gsub("Acc|Accelleration","Acceleration_",names(measures))
names(measures)<-gsub("Gyro","Gyroscope_", names(measures))
names(measures)<-gsub("Mag","Magnitude_",names(measures))
names(measures)<-gsub(".X","_X",names(measures))
names(measures)<-gsub(".Y","_Y",names(measures))
names(measures)<-gsub(".Z","_Z",names(measures))
names(measures)<-gsub("tBody","TimeBody",names(measures))
names(measures)<-gsub("BodyBody|Body","Body_",names(measures))
names(measures)<-gsub("\\.meanfreq","_Mean_Frequency",ignore.case = TRUE)
names(measures)<-gsub("\\.mean","Mean",names(measures),ignore.case = TRUE)
names(measures)<-gsub("\\.std","STD",names(measures),ignore.case = TRUE)
names(measures)<-gsub("Jerk","Jerk_",names(measures))
names(measures)<-gsub("^angle|ang_","Angle",names(measures),ignore.case = TRUE)
names(measures)<-gsub("gravit|gravity","Gravity",names(measures),ignore.case = TRUE)
names(measures)<-sub("\\.","(",names(measures))
#if the period is at the end of the string or followed by xy or z then replace with )
names(measures)<-gsub("\\.$|\\._[XxYyZz]",")",names(measures))
#if the period is in the middle, replace with a comma
names(measures)<-gsub("\\.\\.|\\.", ",", names(measures))
names(measures)

#Step 5 From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.
tidydata<- aggregate(.~Subject + Activity, measures, mean)
#create the table
write.table(tidydata, file = "Tidy.txt", row.names = FALSE)