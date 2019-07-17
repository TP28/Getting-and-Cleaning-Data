



#### Final Programming Assignment - Getting and cleaning Data ######


packages<-c("DBI", "ROracle", "ggplot2","stringr",
            "graphics","sqldf","reshape2","rpart","readxl","data.table","rmarkdown", "gridExtra",
            "RColorBrewer", "grid","compoisson","gtable","smbinning","plyr","dplyr",
            "ascii","knitr","kableExtra","formattable","ROCR","pROC","glmnet","scales","DT","pscl","R.utils","woeBinning",
            "xtable","xlsx","stargazer", "sas7bdat", "ggthemes","knitr")


lapply(packages, require, character.only = TRUE)



link1<- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"

download.file(url = link1, destfile = "C:/Users/pitzto/Documents/Fitness.zip")
unzip(zipfile = "C:/Users/pitzto/Documents/Fitness.zip",exdir = "C:/Users/pitzto/Documents/FitnessFiles" )
path1 <- "C:/Users/pitzto/Documents/FitnessFiles/UCI HAR Dataset/test"
path2<- "C:/Users/pitzto/Documents/FitnessFiles/UCI HAR Dataset/train"

testfilelist<-list.files(path1, include.dirs = TRUE, recursive = TRUE, full.names = TRUE)
trainfilelist<- list.files(path2, include.dirs = TRUE, recursive = TRUE, full.names = TRUE)


# consists of activity ids and the corresponding names
ActivityNames<- read.table(file = "C:/Users/pitzto/Documents/FitnessFiles/UCI HAR Dataset/activity_labels.txt")
names(ActivityNames)<- c("label_id","label")


VarDescription <-  read.table(file = "C:/Users/pitzto/Documents/FitnessFiles/UCI HAR Dataset/features.txt", head=FALSE)
names(VarDescription) <- c("column","variable_name")



# consists of the activity ids (labels) for each of the recorded activities
TrainingLabels <-  read.table(file = "C:/Users/pitzto/Documents/FitnessFiles/UCI HAR Dataset/train/Y_train.txt")
names(TrainingLabels)<- c("label_id")

TrainingSet <-  read.table(file = "C:/Users/pitzto/Documents/FitnessFiles/UCI HAR Dataset/train/X_train.txt")
names(TrainingSet)<-VarDescription$variable_name

SubjectTrain<- read.table(file = "C:/Users/pitzto/Documents/FitnessFiles/UCI HAR Dataset/train/subject_train.txt")
names(SubjectTrain)<-c("subject")

Group<-rep('train',7352)
names(Group)<-c("group")

Train_data<-cbind(SubjectTrain,TrainingLabels, TrainingSet,Group)


Train_data2<-merge(x = Train_data, y = ActivityNames, by.x="label_id", by.y = "label_id")






#### Part 1 - Merge Datasets


TestLabels <-  read.table(file = "C:/Users/pitzto/Documents/FitnessFiles/UCI HAR Dataset/test/Y_test.txt")
names(TestLabels)<- c("label_id")

TestSet <-  read.table(file = "C:/Users/pitzto/Documents/FitnessFiles/UCI HAR Dataset/test/X_test.txt")
names(TestSet)<- VarDescription$variable_name
names(TestSet)

SubjectTest<- read.table(file = "C:/Users/pitzto/Documents/FitnessFiles/UCI HAR Dataset/test/subject_test.txt")
names(SubjectTest)<- c("subject")

Group<-rep('test',2947)
names(Group)<-c("group")

Test_data<-cbind(SubjectTest,TestLabels,TestSet,Group)

Test_data2<-merge(x = Test_data,y = ActivityNames, by.x = "label_id",by.y = "label_id")


#### Set Training and Test together
Gesamtdatensatz<- rbind(Train_data2,Test_data2)
names(Gesamtdatensatz)<-tolower(names(Gesamtdatensatz))

#### Part 2 - filtere mean und std Daten heraus
# Tabelle features.txt enth?lt die Variablenbeschriftungen f?r die v1-v561 Variablen - ben?tigt werden nur mean und std



grep(pattern="(mean|std)",VarDescription$variable_name, value = TRUE)

Newcolumns<-tolower(VarDescription$variable_name[grep(pattern="(mean()|std())",VarDescription$variable_name)])

ColumnSelection<-c(as.character(Newcolumns),"label","group","subject","label_id")
intersect(names(Gesamtdatensatz),ColumnSelection) # check if variables format fits together



SelectedData<-subset(x = Gesamtdatensatz,select = ColumnSelection)


# Part 3 - use descriptive acitivty names to name the activities in the dataset
# Variable names are not really meaningful so far - give them more useful names
# features_info gives hints:
# Acc is Accelerator
# Gyro is Gyroscope
# Mag is Magnitude
# Bodybody can be abbreviated to body
# prefix t is time
# f is frequency

names(SelectedData)<- gsub("acc","accelerator",names(SelectedData))
names(SelectedData)<- gsub("gyro","gyroscope",names(SelectedData))
names(SelectedData)<- gsub("mag","magnitude",names(SelectedData))
names(SelectedData)<- gsub("bodybody","body",names(SelectedData))
names(SelectedData)<- gsub("^t","time",names(SelectedData))
names(SelectedData)<- gsub("^f","frequency",names(SelectedData))

# Teil 4 - create independent tidy dataset with averages of each variable for each activty and subject

TidyaggDataset<-aggregate(. ~ subject+label,data=SelectedData,FUN = mean)

## Produce Codebook

getwd()
setwd("O:/R/Coursera/Getting and Cleaning Data/Getting and Cleaning Data")
render("DataCleaning.Rmd")



