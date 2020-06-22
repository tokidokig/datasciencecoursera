# PRELIMINARIES: 
#library(tidyverse) 
# Working directory should be the same as this script 

# CODE FOR INSTRUCTIONS STEP 1: MERGE DATA
# Import necessary files from the UCI HAR Dataset folder:  
features <- read_delim("UCI HAR Dataset/features.txt", delim = " ",
            col_names = c("id", "feature"))
activityLabels <- read_delim("UCI HAR Dataset/activity_labels.txt",
            delim = " ", col_names = c("label_id", "activity")) 
# Create a vector from the feature column:
featureVect <- as.vector(features$feature)
# Import necessary files in UCI HAR Dataset/test folder:  
subjectTest <- read_table2("UCI HAR Dataset/test/subject_test.txt",
                col_names = "subject")
xTest <- read_table2("UCI HAR Dataset/test/X_test.txt", col_names = featureVect)
yTest <- read_table2("UCI HAR Dataset/test/y_test.txt", col_names = "label_id") 
# Import necessary files in UCI HAR Dataset/train folder: 
subjectTrain <- read_table2("UCI HAR Dataset/train/subject_train.txt",
                col_names = "subject")
xTrain <- read_table2("UCI HAR Dataset/train/X_train.txt", col_names = featureVect)
yTrain <- read_table2("UCI HAR Dataset/train/y_train.txt", col_names = "label_id")
# Preparation for merge
subjectCombo <- bind_rows(subjectTest, subjectTrain)
yCombo <- bind_rows(yTest, yTrain) %>% full_join(activityLabels, by = "label_id")
xCombo <- bind_rows(xTest, xTrain)
# Merge data in order:
BigMerge <- bind_cols(subjectCombo, yCombo, xCombo)


# CODE FOR INSTRUCTIONS STEPS 2-4 
# Create a subset of the featureVect vector that includes the 33 mean() measurements
#   and the 33 std() measurements in it.
meanVect <- grep("mean\\(\\)", featureVect, value = TRUE) 
stdVect <- grep("std\\(\\)", featureVect, value = TRUE) 
# Assemble a vector with additional variables
subVect <- c('subject', 'label_id', 'activity')
# Combine them: 
extractVect <- c(subVect, meanVect, stdVect)
# Subset BigMerge based upon the columns defined extractVect
smallBigMerge <- BigMerge %>% select(one_of(extractVect))


# CODE FOR INSTRUCTIONS STEP 5:
# Add new factor columns 
indepTidy <- smallBigMerge %>% mutate(activity2 = as_factor(smallBigMerge$activity),
            subject2 = as_factor(smallBigMerge$subject))
# Reorder the select only the necessary columns 
indepTidy <- indepTidy %>% select(c(activity2, subject2,
            "tBodyAcc-mean()-X":"fBodyBodyGyroJerkMag-std()")) 
# Rename the new factor columns to the simpler names 
indepTidy <- indepTidy %>% rename(activity = activity2, subject = subject2) 
# Take the mean of each combination of the activity and subject factors 
indepTidyAvg <- indepTidy %>% 
        group_by(activity, subject) %>% 
        summarise_at(vars("tBodyAcc-mean()-X":"fBodyBodyGyroJerkMag-std()"), mean) 
# Write the data to a txt file
write.table(indepTidyAvg, "independent_tidy_average.txt", row.names = FALSE)

