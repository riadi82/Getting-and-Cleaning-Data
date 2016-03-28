
run_analysis <- function() {

	##set the file name and load the library
	library(reshape2)
	Zip_name <- "getdata_dataset.zip"

	## Download and unzip the dataset
  	fileURL <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip "
  	download.file(fileURL, Zip_name, mode='wb') 
	unzip(Zip_name) 
	
	## Get The Labels for the Columns 
	Name_activity <- read.table("UCI HAR Dataset/activity_labels.txt")
	Name_activity[,2] <- as.character(Name_activity[,2])
	features <- read.table("UCI HAR Dataset/features.txt")
	features[,2] <- as.character(features[,2])

	## Prepare The Labels and clean them  
      Selectfeatures <- grep(".*mean.*|.*std.*", features[,2])
	Selectfeatures.value <- features[Selectfeatures,2]
	Selectfeatures.value = gsub('-mean', 'Mean', Selectfeatures.value)
	Selectfeatures.value = gsub('-std', 'Std', Selectfeatures.value)
	Selectfeatures.value <- gsub('[()]', '', Selectfeatures.value)
      Selectfeatures.value <- gsub("-", "_",Selectfeatures.value )
	Selectfeatures.value <- gsub('BodyBody', 'Body', Selectfeatures.value)


	## Set Train Dataset  Only Get the mean and std data 
	x_train <- read.table("UCI HAR Dataset/train/X_train.txt")[Selectfeatures]
	y_train <- read.table("UCI HAR Dataset/train/Y_train.txt")
	subject_train <- read.table("UCI HAR Dataset/train/subject_train.txt")
	x_train <- cbind(subject_train, y_train, x_train)

      ## Set Test Dataset  Only Get the mean and std data 
	x_test <- read.table("UCI HAR Dataset/test/X_test.txt")[Selectfeatures]
	y_test <- read.table("UCI HAR Dataset/test/Y_test.txt")
	subject_test <- read.table("UCI HAR Dataset/test/subject_test.txt")
	x_test <- cbind(subject_test, y_test, x_test)

	# combine datasets and add labels to columns
	combine <- rbind(x_train, x_test)
      colnames(combine) <- c("subject", "activity", Selectfeatures.value)

      ##clean the data using the melt and dcast function from the reshape2 Library
	combine$activity <- factor(combine$activity, levels = Name_activity[,1], labels = Name_activity[,2])
	combine$subject <- as.factor(combine$subject)
	combine.melted <- melt(combine, id = c("subject", "activity"))
	combine.mean <- dcast(combine.melted, subject + activity ~ variable, mean) 
      ##also calculate the mean for every measurement column

write.table(combine.mean, "tidy_data.txt", row.names = FALSE, quote = FALSE)
}
