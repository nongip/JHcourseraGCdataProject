#run_analysis.R
##NOTE:start having unzipped data dir called "UCI HAR Dataset"  within dir "coursera3_project".  

##1.create R objects from the files spread around in the folder
setwd("./Roberto ASUS/JH-Coursera") #my default working directory, root directory
if(!file.exists("coursera3_project")) { #if dir "coursera3_project" doesn't exist create one
  dir.create("coursera3_project")
  }
setwd("./coursera3_project") #and set it as wd
getwd() #check
list.files("./") #lists files within 
setwd("./UCI HAR Dataset") #move inside unzipped folder UCI HAR Dataset
getwd() #check
list.files("./") #list files within

activity_labels<-readLines("activity_labels.txt") #read those descriptives from R
activity_labels #find 6 activities
features<-read.table("features.txt") #read features file: 2vars 561 obs and make R object   
features_info<-read.table("features_info.txt") #read technical metadata 
README<-readLines("README.txt") #read the general guide 
###TEST SUB-SAMPLE
setwd("./test") #go inside folder test
getwd() #check
list.files("./") #lists files within
subject_test<-read.table("subject_test.txt") #create R objects from them 
X_test<-read.table("X_test.txt")
y_test<-read.table("y_test.txt")
setwd("./Inertial Signals") #go within subdir Inertial Signals
getwd() #check
TestInertialSignalsFILENames<-list.files("./") #get list of files content for later automation of objects creation
TestInertialSignalsFILENames #9 files
body_acc_x_test<-read.table(TestInertialSignalsFILENames[1]) #each of these 9 a 128-variable-dataset with 2947 obs
body_acc_y_test<-read.table(TestInertialSignalsFILENames[2])
body_acc_z_test<-read.table(TestInertialSignalsFILENames[3])
body_gyro_x_test<-read.table(TestInertialSignalsFILENames[4])
body_gyro_y_test<-read.table(TestInertialSignalsFILENames[5])
body_gyro_z_test<-read.table(TestInertialSignalsFILENames[6])
total_acc_x_test<-read.table(TestInertialSignalsFILENames[7])
total_acc_y_test<-read.table(TestInertialSignalsFILENames[8])
total_acc_z_test<-read.table(TestInertialSignalsFILENames[9])
###TRAIN SUB-SAMPLE
setwd("../") #go back 2 levels with directory to "top of the tree"
setwd("../") 
getwd() #check
list.files("./") #see files within
setwd("./train") #now dive into folder train 
getwd() 
list.files("./")
subject_train<-read.table("subject_train.txt") #create R objects from inner files
X_train<-read.table("X_train.txt")
y_train<-read.table("y_train.txt")
setwd("./Inertial Signals") #dive into subdir Inertial Signals
getwd()
TrainInertialSignalsFILENames<-list.files("./") #file names stored for later automation 
TrainInertialSignalsFILENames #9 files
body_acc_x_train<-read.table(TrainInertialSignalsFILENames[1])
body_acc_y_train<-read.table(TrainInertialSignalsFILENames[2])
body_acc_z_train<-read.table(TrainInertialSignalsFILENames[3])
body_gyro_x_train<-read.table(TrainInertialSignalsFILENames[4])
body_gyro_y_train<-read.table(TrainInertialSignalsFILENames[5])
body_gyro_z_train<-read.table(TrainInertialSignalsFILENames[6])
total_acc_x_train<-read.table(TrainInertialSignalsFILENames[7])
total_acc_y_train<-read.table(TrainInertialSignalsFILENames[8])
total_acc_z_train<-read.table(TrainInertialSignalsFILENames[9])

setwd("../") #go back by 2 levels to "top of the tree"
setwd("../") 
getwd()

##2.have an understanding of the data structure: 
dim(features) #561 obs 2 vars
dim(X_test) #561-variable-dataset of 2947 obs
dim(y_test) #1-variable-dataset of 2947 obs
dim(subject_test) #1-variable-dataset of 2947 obs

dim(features) #561 obs 2 vars
dim(X_train) #561-variable-dataset of 7352 obs
dim(y_train) #1-variable-dataset of 7352 obs
dim(subject_train) #1-variable-dataset of 7352 obs

##3.merge subject, y and X files for train and test separately into syX_train and syX_test files
library(dplyr) #handy for further steps
###y_
y_trainN<-rename(y_train,activity=V1) #rename V1 as activity in both test and train 
y_testN<-rename(y_test,activity=V1)
###subject_
subject_trainN<-rename(subject_train,id=V1) #rename V1 as id in subject files of test and train
subject_testN<-rename(subject_test,id=V1)
###X_ 
featureslist<-features$V2 #take list of features and assign it as column names of X_train and X_test
colnames(X_train)<-featureslist
colnames(X_test)<-featureslist
##4 name the parameters of the files from "inertials" folders. The strategy to assign the n:th parameter of each file with file name followed by number "n"
train_SyX<-cbind(subject_trainN,y_trainN,X_train) #create merged file with subjects, activity and features for train
test_SyX<-cbind(subject_testN,y_testN,X_test)  #create merged file with subjects, activity and features for test
##5 merge the 9 "inertials" datasets to the syX_test into test_syXinertials and syX_train datasets into train_syXinertials
firstElement <- function(x){x[1]}					#create function that collects first element of lists
TrainInertialSignalsNames<-strsplit(TrainInertialSignalsFILENames, "\\.");TrainInertialSignalsNamesFE<-sapply(TrainInertialSignalsNames,firstElement) #expression to separate the name of single txt files from "txt", then use the above function to catch only the first element and get rid of txt
TestInertialSignalsNames<-strsplit(TestInertialSignalsFILENames, "\\.");TestInertialSignalsNamesFE<-sapply(TestInertialSignalsNames,firstElement)

InertialSignalsNamesFE<-gsub("train", "",TrainInertialSignalsNamesFE) # create list of those name without train (same could have been done starting from "test") to be later applied as variable names

library(stringr) #handy for further steps
names(body_acc_x_train)<-paste(InertialSignalsNamesFE[1],1:128) #apply those names to the 128 variables of the 9 files in order to obtain names of variables such as nameoffile_number of var. This way variables have names that clearly indicate origin of file but that can be distinguished
names(body_acc_y_train)<-paste(InertialSignalsNamesFE[2],1:128) 
names(body_acc_z_train)<-paste(InertialSignalsNamesFE[3],1:128) 
names(body_gyro_x_train)<-paste(InertialSignalsNamesFE[4],1:128) 
names(body_gyro_y_train)<-paste(InertialSignalsNamesFE[5],1:128) 
names(body_gyro_z_train)<-paste(InertialSignalsNamesFE[6],1:128) 
names(total_acc_x_train)<-paste(InertialSignalsNamesFE[7],1:128) 
names(total_acc_y_train)<-paste(InertialSignalsNamesFE[8],1:128) 
names(total_acc_z_train)<-paste(InertialSignalsNamesFE[9],1:128) 

names(body_acc_x_test)<-paste(InertialSignalsNamesFE[1],1:128) 
names(body_acc_y_test)<-paste(InertialSignalsNamesFE[2],1:128) 
names(body_acc_z_test)<-paste(InertialSignalsNamesFE[3],1:128) 
names(body_gyro_x_test)<-paste(InertialSignalsNamesFE[4],1:128) 
names(body_gyro_y_test)<-paste(InertialSignalsNamesFE[5],1:128) 
names(body_gyro_z_test)<-paste(InertialSignalsNamesFE[6],1:128) 
names(total_acc_x_test)<-paste(InertialSignalsNamesFE[7],1:128) 
names(total_acc_y_test)<-paste(InertialSignalsNamesFE[8],1:128) 
names(total_acc_z_test)<-paste(InertialSignalsNamesFE[9],1:128) 

train_SyXinertials<-train_SyX #create a merge of variables for the train group. In the beginning it consists only of activity, id and the features matrix X
train_SyXinertials<-cbind(train_SyXinertials,body_acc_x_train) #then the first dataset from Inertials is added
train_SyXinertials<-cbind(train_SyXinertials,body_acc_y_train) #then the second and so on
train_SyXinertials<-cbind(train_SyXinertials,body_acc_z_train)
train_SyXinertials<-cbind(train_SyXinertials,body_gyro_x_train)
train_SyXinertials<-cbind(train_SyXinertials,body_gyro_y_train)
train_SyXinertials<-cbind(train_SyXinertials,body_gyro_z_train)
train_SyXinertials<-cbind(train_SyXinertials,total_acc_x_train)
train_SyXinertials<-cbind(train_SyXinertials,total_acc_y_train)
train_SyXinertials<-cbind(train_SyXinertials,total_acc_z_train)
head(train_SyXinertials) #here final train merged dataset can be observed

test_SyXinertials<-test_SyX #the above operation is now repeated for the test individuals too 
test_SyXinertials<-cbind(test_SyXinertials,body_acc_x_test)
test_SyXinertials<-cbind(test_SyXinertials,body_acc_y_test)
test_SyXinertials<-cbind(test_SyXinertials,body_acc_z_test)
test_SyXinertials<-cbind(test_SyXinertials,body_gyro_x_test)
test_SyXinertials<-cbind(test_SyXinertials,body_gyro_y_test)
test_SyXinertials<-cbind(test_SyXinertials,body_gyro_z_test)
test_SyXinertials<-cbind(test_SyXinertials,total_acc_x_test)
test_SyXinertials<-cbind(test_SyXinertials,total_acc_y_test)
test_SyXinertials<-cbind(test_SyXinertials,total_acc_z_test)
head(test_SyXinertials)
##6.append train_syXinertials and test_syXinertials into DS
DS<-rbind(train_SyXinertials,test_SyXinertials) #append test merged file to train merged file
dim(DS) #the new dataset DS has the same columns of train_SyXinertials and test_SyXinertials, but rows= their respective rows 
##7.select only the variables that talk about mean and std. deviation
features 
DSms<-DS[,(grepl("(mean|std)[^meanFreq]",names(DS))|(names(DS)=="id"|names(DS)=="activity"))] #only those variables containing mean ord std, but NOT meanFreq are selected
head(DSms)
names(DSms) #
##8.give descriptive labels to activities
activity_labels #see the labels
activity_labelsW<-substring(activity_labels,3) #separate words from numbers
onesix<-as.numeric(1:6) #replicate the number vector
onesixdf<-as.data.frame(onesix) #create dataset column with the six numbers 
activity_labelsWdf<-as.data.frame(activity_labelsW) #create dataset column with the six activities
activityLabelsDF<-cbind(onesixdf,activity_labelsWdf) #create dataset with two columns: numbers 1:6 and the six activities
activityLabelsDFrn<-rename(activityLabelsDF,activity=onesix) #change the name of the numbers column to "activity" in order to have a key to match to the database
DSmsl<-merge(activityLabelsDFrn,DSms) #exploiting the common key "activity" the label is merged to the dataset
DSmsl$activity<-NULL #the variable activity is now useless therefore removed: this way the name "activity" can be applied to the labels' column
names(DSmsl)
DSmslr<-rename(DSmsl,activity=activity_labelsW)
names(DSmslr)
table(DSmslr$activity)
##9.Appropriately labels the data set with descriptive variable names.
features_info 
namesLab<-names(DSmslr) #see the names, looking not as descriptive as they could be
namesLab<-sub("^t","time",namesLab) #everything beginning with t is now time
namesLab<-sub("^f","frequency",namesLab) #everything beginning with f is now frequency
namesLab<-sub("Acc","_acceleration",namesLab) #everything reading Acc is now _acceleration
namesLab<-sub("std","standard_dev",namesLab) #everything reading std is now standard_dev
namesLab<-sub("\\(","",namesLab) #gets rid of parenthesis. Needs \\ symbol as an escape
namesLab<-sub("\\)","",namesLab) 
namesLab<-sub("\\-","_",namesLab) #gets ris of -, changes to _
namesLab<-sub("\\-","_",namesLab) 
names(DSmslr)<-namesLab #apply names to dataset
namesLabS<-namesLab[3:68] #create list of variables other than id and activity

##10.From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.
#groupedDSms<-group_by(DSms,id,activity) (not used)
aggregateDSmslr<-aggregate(DSmslr[,3:length(namesLabS)], list(DSmslr$id,DSmslr$activity), mean) #compactize to mean conditional on id and activity
aggregateDSmslrR<-rename(aggregateDSmslr,id=Group.1,activity=Group.2)
##11.write to txt
setwd("../")
getwd()
write.table(aggregateDSmslrR,file="JHD3pr_3.txt", row.names=FALSE) #write to txt for submission
