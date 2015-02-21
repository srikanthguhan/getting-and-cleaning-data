install.packages("dplyr","plyr","data.table")
library(dplyr)
library(plyr)
library(data.table)
path="D:/3. Getting and Cleaning data/UCI HAR Dataset/" 
##Change to your path if you want to run the codes on your local

##Reading Column names from features table
features<-read.table(file = paste0(path,"features.txt"),sep = " ",stringsAsFactors = FALSE)
features<-as.data.frame(select(features,V2))
col_types=rep("numeric",561)

##Reading Test data
X_test<-read.table(paste0(path,"test/X_test.txt"),colClasses = col_types)
subject_test<-read.table(file=paste0(path,"test/subject_test.txt"),col.names = "person")
y_test<-read.table(file=paste0(path,"test/y_test.txt"), col.names="activity")
for(i in 1:561){
names(X_test)[i]<-features[i,1]
}

##Adding the activity, set and respondent columns to the Test data
X_test<-cbind(set="Test", subject_test, y_test, X_test)
View(X_test)
rm(subject_test,y_test)

##Reading training data
X_train<-read.table(paste0(path,"train/X_train.txt"),colClasses = col_types)
subject_train<-read.table(file=paste0(path,"train/subject_train.txt"),col.names = "person")
y_train<-read.table(file=paste0(path,"train/y_train.txt"), col.names="activity")
for(i in 1:561){
        names(X_train)[i]<-features[i,1]
}

##Adding the activity, set and respondent columns to the Training data
X_train<-cbind(set="Training", subject_train, y_train, X_train)
rm(subject_train,y_train,features,col_types,i)
View(X_train)

##Combining Test and Training data
XTestTrain<-rbind(X_test,X_train)
View(XTestTrain)
tidyData<-XTestTrain[,1:3]
View(tidyData)

##Filtering Merged data for columns containing Means and Standard Deviations only
tidyData<-cbind(tidyData,
                XTestTrain[,grep("[Mm]ean\\(\\)|std\\(\\)",colnames(XTestTrain))])
View(tidyData)
rm(X_test,X_train,XTestTrain)

##Describing activities with the Activity Labels
##WALKING, WALKING UPSTAIRS and WALKING DOWNSTAIRS have been considered different
for(i in 1:10299){
if(tidyData[i,3]==1){
        tidyData[i,3]="WALKING"
} else if(tidyData[i,3]==2) {
        tidyData[i,3]="WALKING_UPSTAIRS"
} else if(tidyData[i,3]==3){
        tidyData[i,3]="WALKING_DOWNSTAIRS"
} else if(tidyData[i,3]==4){
        tidyData[i,3]="SITTING"
} else if(tidyData[i,3]==5){
        tidyData[i,3]="STANDING"
} else if(tidyData[i,3]==6){
        tidyData[i,3]="LAYING"}
}
View(tidyData)
unique(tidyData[,3])
rm(i)

##Averages of each variable for each activity and each subject
Averages<-aggregate(tidyData,by=list(tidyData[,1],tidyData[,2],tidyData[,3])
                                     ,FUN = mean)
Averages<-Averages[-c(4:6)]
setnames(Averages, old = c('Group.1','Group.2','Group.3'), 
         new = c('set','person','activity'))
## View(Averages)

##Writing to a text file
write.table(Averages,file = paste0(path,"AveragedTidyData.txt"),row.name=FALSE)
rm(path)

