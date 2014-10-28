library(data.table)
library(reshape2)

# function to normalize the variable name 
#  Apply the translation table
#  Remove forbidden Characters
#  transform to lower case

features_normalization <- function(feat,translation_table,forbidden_chars) {
  
  
  apply(translation_table,1,function(r) {
    feat <<- gsub(r[1],r[2],feat)
  })
  
  
  tolower(gsub(forbidden_chars,'',feat))
  
}

# function to get and reshape a data set 
#    Read the Activity File
#    Read the Subject File
#    Read The Data File
#    Remove unnecessasy features
#    Append activity and subject columns

get_and_reshape_data_set <- function(data_set) {
  
  activity_file <- sprintf("./UCI HAR Dataset/%s/y_%s.txt",data_set,data_set)
  activities <- read.table(activity_file,header= FALSE,strip.white = TRUE, col.names = c('Id') )
  activities <- cbind(activities, id... = c(1:dim(activities)[1]))
  activities <- merge(activities,activity_labels,sort = FALSE)
  activities <- activities[order(activities$id...),]
  
  subject_file <- sprintf("./UCI HAR Dataset/%s/subject_%s.txt",data_set,data_set)
  subjects <- read.table(subject_file,header= FALSE,strip.white = TRUE, col.names = c('Subject') )
  
  file_name <- sprintf("./UCI HAR Dataset/%s/X_%s.txt",data_set,data_set)
  data_table <- data.table(read.table(file_name,header= FALSE,strip.white = TRUE, col.names = features ))
  data_table <- data_table[,feature_indexes, with = FALSE]
  data_table[, activity := activities[,3]]
  data_table[, subject := subjects]
  
}

# Read features

feature_file <- './UCI HAR Dataset/features.txt'
features <- read.table(feature_file,header= FALSE,strip.white = TRUE, stringsAsFactors = FALSE )
feature_indexes <- grep('mean(?!Freq)|std(?!Freq)',features[,2], perl = TRUE )

translation_table <- data.frame(orignal=c('std','Acc','Gyro','Mag','tBody','fBody'), translation=c('standarddeviation','accelerometer','gyroscope','magnitude','timebody','frequencybody'))
features <- features_normalization(features[,2],translation_table,'[()-]')

# Read activities

activity_label_file <- './UCI HAR Dataset/activity_labels.txt'
activity_labels <- read.table(activity_label_file,header= FALSE,strip.white = TRUE, col.names = c('Id','Name') )

# Read train data set 

train_table <- get_and_reshape_data_set('train')

# test data set

test_table <- get_and_reshape_data_set('test')

# combine data sets

all_table <- rbind(train_table,test_table)

# create a tidy data set applying the mean function 

m <-melt(all_table,id.vars = c('activity','subject'))
tidy_data <- dcast.data.table(m,activity + subject ~ variable,fun= mean)

# add mean as variable prefix

setnames(tidy_data,names(tidy_data)[-1:-2],sprintf('mean%s',names(tidy_data)[-1:-2]))

# write tidy data set 

output_file <- './tidydata.txt'

write.table(tidy_data,output_file,row.name=FALSE) 
