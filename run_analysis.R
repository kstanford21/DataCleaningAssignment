#packages necessary to run script
require(stringr)
require(dplyr)

#download source file 
url <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
download.file(url, destfile = "fitnesstracker.zip")

#unzip using microsoft windows

#Import training data(X_train) and apply appropriate names (y_train and subject_train)

    #get workingdirectory to navigate to subfolders
    WorkingDirectory <- getwd()

    #import dataset
    file <- paste(WorkingDirectory, "/fitnesstracker/train/X_train.txt", sep = "")
    train_data <- read.table(paste(file), fill = TRUE)
    
    #get row names
    file <- paste(WorkingDirectory, "/fitnesstracker/train/y_train.txt", sep = "")
    activity <- as.list(read.table(paste(file), fill = TRUE))
    file <- paste(WorkingDirectory, "/fitnesstracker/train/subject_train.txt", sep = "")
    subjects <- read.table(paste(file), fill = TRUE)
    #bind row names with test set
    train_data <- cbind(subjects, activity, train_data)

#Import test data(X_test) and apply appropriate names (y_test and subject_test)
    file <- paste(WorkingDirectory, "/fitnesstracker/test/X_test.txt", sep = "")
    test_data <- read.table(paste(file), fill = TRUE)
      
    #get row names
    file <- paste(WorkingDirectory, "/fitnesstracker/test/y_test.txt", sep = "")
    activity <- as.list(read.table(paste(file), fill = TRUE))
    file <- paste(WorkingDirectory, "/fitnesstracker/test/subject_test.txt", sep = "")
    subjects <- read.table(paste(file), fill = TRUE)
    
    #bind row names with test set
    test_data <- cbind(subjects, activity, test_data)

#merge training and test sets, apply column names from features file            
    #merge data
    all_data <- rbind(train_data, test_data) 

    #Get and set column names
    file <- paste(WorkingDirectory, "/fitnesstracker/features.txt", sep = "")
    features <- read.table(paste(file), fill = TRUE, stringsAsFactors = FALSE)  
    names<- c("subjects", "activity", features$V2)

    #set column names
    names <- gsub("\\()", "", names)
    names <- gsub("-", "", names)
    colnames(all_data) <- c(names)

#extract measurements on mean and standard deviation
    mean_sd <- append(grep("mean", colnames(all_data)), grep("std", colnames(all_data)))
    Mean_SD_data <- all_data[,c(1, 2, mean_sd)]
    Mean_SD_data$activity <- as.factor(Mean_SD_data$activity)
    
    #assign subjects meaningful name
    Mean_SD_data$subjects <- sapply(Mean_SD_data$subjects, function(x) paste("Subject", as.character(x), sep = ""))
    
    #assign activity label meaningful name
      #get descriptive activity labels
          file <- paste(WorkingDirectory, "/fitnesstracker/activity_labels.txt", sep = "")
          activity_labels <- read.table(paste(file), fill = TRUE)
          activity_labels <- as.character(activity_labels$V2)
      #assign labels
        Mean_SD_data$activity <- sapply(Mean_SD_data$activity, function(x) x = activity_labels[[as.numeric(x)]])

#create independent tidy dataset with the average for each variable for each activity for each subject
  SummaryMeans <- Mean_SD_data %>%  
    group_by(subjects, activity) %>%
    summarize_all(funs(mean))
 
#write summary table to file
  write.table(SummaryMeans, file= "SummaryMeans.txt", row.name = FALSE)
