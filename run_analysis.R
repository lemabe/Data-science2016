## gather all the usable data to the same directory.
## created from UCI HAR DATASET directory a new directory call Uci har with usable files
## set the the workspace to the new directory created ("Uci har").
setwd("Uci har")
## open library(plyr).
library(plyr)
## list all files in Uci har.
file_list <- list.files()
## merge all files in Uci har using join_all.
Ucihar <- join_all(file_list, read.table, header=TRUE, sep="\t")
## calculate the mean using tapply.
Ucihar_mean <- tapply(Ucihar, mean)
## calculating standard deviation using tapply.
Ucihar_stdv <- tapply(ucihar, stdev)
## printing the result.
