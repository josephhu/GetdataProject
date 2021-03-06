# Determine if this system is capable of using the reshape2 or the plyr libraries
#
useReshape2 <- TRUE
usePlyr <- TRUE

if (!require(reshape2)) {
    install.packages("reshape2")
    if (!require(reshape2)) {
        message("Library reshape2 could not be loaded.")
        useReshape2 <- FALSE
    }    
}

if (!require(plyr)) {
    install.packages("plyr")
    if (!require(plyr)) {
        message("Library plyr could not be loaded.")
        usePlyr <- FALSE
    }    
}

# run() is the main routine
# It illustrates how the functions can be called in order to generate the tidy data set
# 
run <- function() {
    
    localFile <- "UCI HAR Dataset.zip"
    if (!file.exists(localFile)) {
        download.data()
    }
    
    localDir <- unzip.data(localFile)
    
    merged <- merge.data(localDir)
    extracted <- extract.mean.std(merged)
    tidy <- gen.tidy.data(extracted)
    tidy <- rename.columns(tidy)
    
    write.table(tidy, "UCI_HAR_TidyData.txt", row.names=FALSE)
    
    #l_ply(colnames(tidy)[-(1:2)],gen.code.book)
    #l_ply(colnames(tidy)[-(1:2)],gen.feature)
    
}

# download.data() downloads the original zip file to the working directory
# if this file has not been dowloaded already
# 
download.data <- function(
    fileUrl="https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip",
    localFile="UCI HAR Dataset.zip") {

    download.file(fileUrl, destfile=localFile)
    
    if (!file.exists(localFile)) {
        stop("Could not download the data files")
    }
    
    localFile
}

# unzip.data() unzips the content of the zip file to a "data" subdirectory
# The "data" subdirectory is created as necessary
# 
unzip.data <- function(
    localFile="UCI HAR Dataset.zip") {

    if (!file.exists("./data")) {
        dir.create("./data")
    }
    
    unzip(localFile, exdir="./data")
    
    if (!file.exists("./data/UCI HAR Dataset")) {
        stop("Could not unzip the data files")
    }
    
    # After unzip, we will process the following files:
    #
    # ./data/UCI HAR Dataset/activity_labels.txt
    # ./data/UCI HAR Dataset/features.txt
    #
    # ./data/UCI HAR Dataset/train/subject_train.txt
    # ./data/UCI HAR Dataset/train/y_train.txt
    # ./data/UCI HAR Dataset/train/X_train.txt
    #
    # ./data/UCI HAR Dataset/test/subject_test.txt
    # ./data/UCI HAR Dataset/test/y_test.txt
    # ./data/UCI HAR Dataset/test/X_test.txt
    
    return( "./data/UCI HAR Dataset")
}

# merge.data() creates a big data frame by merging (using cbind() and rbind()) 
# the files that contain
# 1. the subject id's (subject_train/test.txt)
# 2. the activity (y_train/test.txt)
# 3. the measurements (X_train/test.txt)
# 
# The activity column's numeric values 1, 2, etc.. will be replaced with 
# the activity labels WALKING, WALKING_UPSTAIRS, etc. (from the activity_labels.txt file)
# 
merge.data <- function(
    localDir="./data/UCI HAR Dataset") {
    
    features <- read.table(paste0(localDir,"/features.txt"), 
                        col.names=c("row.number","feature"), stringsAsFactors=FALSE)
    
    train.sub  <- read.table(paste0(localDir,"/train/subject_train.txt"), col.names="subject")
    train.act  <- read.table(paste0(localDir,"/train/y_train.txt"),       col.names="activity")    
    train.data <- read.table(paste0(localDir,"/train/X_train.txt"),       col.names=features$feature)
    
    test.sub  <- read.table(paste0(localDir,"/test/subject_test.txt"), col.names="subject")
    test.act  <- read.table(paste0(localDir,"/test/y_test.txt"),       col.names="activity")
    test.data <- read.table(paste0(localDir,"/test/X_test.txt"),       col.names=features$feature)
    
    x <- rbind(cbind(train.sub, train.act, train.data),
          cbind(test.sub, test.act, test.data))
    
    # convert the values 1, 2, 3, etc. from activity column 
    # to WALKING, WALKING_UPSTAIRS, WALKING_DOWNSTAIRS, etc.
    #
    
    # the activity_label.txt file contains the values 1, 2, etc.
    # and their corresponding strings WALKING, WALKING_UPSTAIRS, etc.
    #
    labels <- read.table(paste0(localDir,"/activity_labels.txt"), 
                         col.names=c("id","name"), stringsAsFactors=FALSE)
    
    # act.factor <- factor(x$activity)
    # levels(act.factor) <- labels$name
    # x$activity <- act.factor
    #
    x$activity <- factor(x$activity,labels=labels$name)
    
    x
}

# extract.mean.std() works on the big data frame and extracts only 
# the subject and activity columns (the primary keys of the data set)
# and the "-mean()" and"-std()" columns (measurements that were the means and standard deviations)
# 
extract.mean.std <- function(x) {
    
    # only want the first 2 columns "subject" and "activity"
    # and any features that has the text "-mean()" or "-std()" in them.
    #
    # note that we are searching for ".mean..", not "-mean()"
    # because R has automatically converted "-mean()" to ".mean.."
    #
    x[,grep("subject|activity|\\.mean\\.\\.|\\.std\\.\\.",names(x))]
}

# gen.tidy.data() computes the average (per subject and activity)
# of the "-mean()" and "-std()" values
# 
gen.tidy.data <- function(x) {
    if (usePlyr & require(plyr)) {
        
        data <- ddply(x, .(subject,activity), colwise(mean))
        
    } else if (useReshape2 & require(reshape2)) {
        
        ids <- names(x)[1:2]
        measures <- names(x)[-(1:2)]
        melted <- melt(x, id.vars=ids, measure.vars=measures)
        data <- dcast(melted, subject + activity ~ variable, mean)          
        
    } else {
        
        data <- aggregate(x[,3:length(x)],by=x[,1:2],mean)
    }
    
    data
}

# rename.columns() illstrates one possible way to rename the data frame columns
# with descriptive variable names.
# 
# Because of the length of the original names, we chose not to convert everything to lower cases
# but rather we are using camel cases to make them clearer to read.
# 
# Also, the riginal data set had some sensible abbreviations (such as Gyro for Gyroscope)
# and so we decided to keep these abbreviations.
# 
# The original feature name "tBodyAcc-mean()-X" is thus converted to "tBodyAccMeanX"
# 
# To emphasize that the tidy data set is the summaries (average of) of the original data,
# the column name is further renamed as "averageOf.tBodyAccMeanX" to clearly distinguish it. 
# 
rename.columns <- function(df) {
    
    # R (read.table) converted column name "tBodyAcc-mean()-X"
    # to "tBodyAcc.mean...X"
    #
    # We will strip out the dots and convert them to camel case
    # and then add "averageOf." in front of it
    #
    # An original column name of ""tBodyAcc-mean()-X" will become 
    # "averageOf.tBodyAccMeanX" in our tidy data set
    #
    new.names <- sapply(names(df), function(x) {
        if (x %in% c("subject", "activity")) {
            x
        } else {
            x <- gsub("\\.mean\\.\\.", "Mean", x)
            x <- gsub("\\.std\\.\\.", "Std", x)
            x <- gsub("\\.([XYZ])$", "\\1", x)
            paste("averageOf.", x, sep="")
        }
    })
    names(df) <- new.names    
    
    df
}

rename.back <- function(name) {
    x <- gsub("^averageOf\\.", "", name)
    x <- gsub("Mean", "-mean()", x)
    x <- gsub("Std", "-std()", x)
    x <- gsub("([XYZ])$", "-\\1", x)
    x
}

gen.code.book <- function(name) {
    if (grepl("Acc",name)) {
        unit <- "in standard gravity units 'g'"
    } else if (grepl("Gyro",name)) {
        unit <- "in units of radians/second for angular velocity"
    } else {
        unit <- " (units unknown)"
    }
    
    cat(name,"\r\n", 
        "\tThe average (for each subject and each activity) of the original",rename.back(name),"values\r\n",
        "\t\tNumeric value", unit, "\r\n\r\n")
}

gen.feature <- function(name) {
    cat(name,rep_len(" ",40-nchar(name)),"# average of ",rename.back(name),"\r\n",sep="")
}

std.name <- function(name) {
    
    # Implements R's (read.table) standard conversion of column names
    # "tBodyAcc-mean()-X" becomes "tBodyAcc.mean...X"
    #
    gsub("[ \\(\\)-]", "\\.", name)
}

orig.name <- function(name) {
    
    # change "tBodyAcc.mean...X" to "tBodyAcc.mean().X"
    t = gsub("(std|mean)\\.\\.", "\\1()", name)
    
    # change "tBodyAcc.mean().X" to "tBodyAcc.mean()-X"
    t = gsub("\\.([XYZ])$", "-\\1", t)
    
    # change "tBodyAcc.mean()-X" to "tBodyAcc-mean()-X"
    gsub("\\.", "-", t)
}
