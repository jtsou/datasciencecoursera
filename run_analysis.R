#import training files

X_train = read.table("UCI HAR Dataset/train/X_train.txt")
Y_train = read.table("UCI HAR Dataset/train/Y_train.txt")
subject_train = read.table("UCI HAR Dataset/train/subject_train.txt")
#merge training files together
merged_train= cbind(Y_train,subject_train, X_train)

#import test files
X_test = read.table("UCI HAR Dataset/test/X_test.txt")
Y_test = read.table("UCI HAR Dataset/test/Y_test.txt")
subject_test = read.table("UCI HAR Dataset/test/subject_test.txt")
#merge test file together
merged_test= cbind(Y_test,subject_test, X_test)

#combine training and testing data together
combined = rbind(merged_train, merged_test)

#import feature
feature = read.table("UCI HAR Dataset/features.txt")
#get only second column
feature = as.character(feature$V2)

#import activity label
library(dplyr)
act = read.table("UCI HAR Dataset/activity_labels.txt")
act = rename(act, 'ActivityCode'=V1, 'Activity'= V2)

#Uses descriptive activity names to name the activities in the data set
#Appropriately labels the data set with descriptive variable names. 
colnames(combined)[1:2] = c("Activity","Subject")
colnames(combined)[3:563] = feature
#Extracts only the measurements on the mean and standard deviation 
#for each measurement
ms = grep('mean()|std()',names(combined))
ms = combined[,c(1,2,ms)]



#replace mean_std's V1 and V2 with real activity name
ms = merge(act, ms, by.x = "ActivityCode",by.y = 'Activity')
ms = select(ms, -ActivityCode)
View(ms)
#to see the list of names
names(ms)
#to make edit according to the feature_info.txt()
names(ms) =sub("^t", "TimeSignal", names(ms))
names(ms) =sub("^f", "FrequencySignal", names(ms))
names(ms) =sub("mean", "Mean", names(ms))
names(ms) =sub("std", "StandardDeviation", names(ms))
names(ms) =sub("X", "Xaxis", names(ms))
names(ms) =sub("Y", "Yaxis", names(ms))
names(ms) =sub("Z", "Zaxis", names(ms))
names(ms) =sub("\\()", "", names(ms))
names(ms) =gsub("-", "", names(ms))
names(ms) =sub("Acc", "Acceleration", names(ms))
names(ms) =sub("Gyro", "AngularVelocity", names(ms))
names(ms) =sub("BodyBody", "Body", names(ms))
names(ms)
View(ms)
#From the data set in step 4, creates a second, independent tidy data set 
#with the average of each variable for each activity and 
#each subject.

#melt the ms df by activity and subject to group the data
library(reshape2)
SubjectMelt = melt(ms, id=c('Activity','Subject'))
#View(SubjectMelt)

#get the mean
mean = dcast(SubjectMelt, Activity + Subject ~ variable, mean)
View(mean)

#write the table
write.table(mean, file = 'TinyDataProject.txt', row.names = FALSE)
