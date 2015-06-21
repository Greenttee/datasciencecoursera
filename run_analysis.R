##############################################################################
#    Human Activity Research Project
#
##############################################################################

# Basic housekeeping....
# Remember local path for data files downloaded
# load needed libraries: dplyr, reshape2, etc.
#
#

filePath<-"C:/Users/Tony Truong/Desktop/Coursera R/GetData/UCI HAR Dataset" # this is mostly for me...

library(dplyr)
library(reshape2)

##############################################################################
#
#       Part 1:  Merges the training and the test sets to create one data set.
#
##############################################################################

# Train and Test datasets each has 4 components in 4 different files
#     x data:   contains the calculated/filtered "features" of the
#               experimental data (contained in the "inertial" subfolders.
#               each row of this file contains features of one activity performed by one subject
#     y data:   activity performed.   indexed to match row-to-row with x data
#     subject:  the individual subject performing the activity .  indexed to match row-to-row with x data.
#     activity:  a "lookup" table to "decode" .  This will be used in Part 3 of the project
#
#
# Read in x Test and Train data files.

xTestData<-read.table("./test/X_test.txt")
xTrainData<-read.table("./train/X_train.txt")

# Combine the 2 datasets by row.

xCombinedData<-rbind(xTestData,xTrainData)  

# The next 3 commands read in the "descriptive" variable names for the combined data frame.
# the variable names from features.txt are descriptive and will be used as column headings
# as required in part 3 of the assignment.  I will go ahead and do it now...

Features <-as.list(read.table("./features.txt"))

FeatureColNames<-as.list(as.character(Features$V2)) 

# There should be 561 column names in FeatureColNames
# These are used as "descriptive" variable names in the data frame below.

colnames(xCombinedData)<-FeatureColNames

yTestData<-read.table("./test/y_test.txt")
yTrainData<-read.table("./train/y_train.txt")

yCombinedData<-rbind(yTestData,yTrainData)

#yCombinedData has 10299 observations and 1 variable (activity)

# For consistency, column name in y and Subject are made "descriptive" and human understandable to be tidy.
colnames(yCombinedData)<-"Activity"

subjectTestData<-read.table("./test/subject_test.txt")
subjectTrainData<-read.table("./train/subject_train.txt")

subjectCombinedData<-rbind(subjectTestData, subjectTrainData)


colnames(subjectCombinedData)<-"Subject"

LookupActivity<-read.table("./activity_labels.txt")

colnames(LookupActivity)<-c("Activity","ActivityDescription")

# Combine the two "identity" files:  subject and ydata (or activity description)

SubjectActivity<-cbind(subjectCombinedData, yCombinedData)

# The next 2 commands extract columns with headings containing "mean" or "sdt" 
# 


xMean<-xCombinedData[,grep("mean",colnames(xCombinedData))]
xStd<-xCombinedData[,grep("std",colnames(xCombinedData))]

# HARTidy data frame below is the combined Test & Train data set
# subsetted with mean and std variables only.
#
# Inertial data files (Test, Train) were not processed since no processing requirement were
# called for in the course project assignment.  
# I did take a quick look at the files and surmised that they contain the raw data reading 
# from the phones (observations were recorded over time, and some noise filtering was done to generate
# the datasets that we are working with)
# 
HARTidy<-cbind(SubjectActivity, xMean, xStd)

#HARTidy has 10299 observations and 81 variables. This step completes requirements 1,2 and 3 of the project

TestMerge<-merge(HARTidy,LookupActivity, by="Activity")

# Now we clean up TestMerge to
# replace Activity factors (1,2,3,4,5,6) with descriptive name of the corresponding activity (WALKING, LAYING, etc.)
# 
# First get rid of "Activity" column (factor 1,2,3,4,5,6)
TestMerge<-select(TestMerge, -Activity)

# Now move "ActivityDescription" from last column to first column

TestMerge<-TestMerge[,c(ncol(TestMerge),1:(ncol(TestMerge)-1))]

# TestMerge at this point satisfies requirement number 4 of the project

# Create a vector of column names

ColHeadings<-colnames(TestMerge)

# Any number of ways could be used to create essentially a "pivot" table with Activity and Subject groupings.
# The key here is to identify ActivityDescription and Subject as id's and the rest of
# the columns are measurements
# The data fram was molten as such below

meltTM<-melt(TestMerge, id=c("ActivityDescription","Subject"),measure.vars=ColHeadings[3:81])

# Finally recast the data with mean as the summarizing function.
Part5<-dcast(meltTM, ActivityDescription + Subject ~ variable, mean)

# Write out txt file for submission.

write.table(Part5, file="ActivityMeanbySubject.txt",row.names=FALSE)
