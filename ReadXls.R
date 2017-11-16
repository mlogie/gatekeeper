#########################################################################################
#                                                                                       #
#  Code to read in Gatekeeper spreadsheet and parse into a data frame                   #
#                                                                                       #
#########################################################################################
## First, load up the gdata library, for reading xlsx files
#install.packages('gdata')
#install.packages('xlsx')
library('gdata')
#library('xlsx')
rm(list=ls())

## Set the file path
datapath <- file.path('.','Farm_data')
perl <- 'C:/Strawberry/perl/bin/perl5.26.1.exe'
## Discover all the folders in the data folder
FolderList <- list.files(datapath)

## Create a list of all the files in folder 1, and save just those in xls format
FirstList <- list.files(file.path(datapath,FolderList[1]))
FirstList <- FirstList[grepl('.xls$',FirstList)]

mydf <- read.xls(file.path(datapath,FolderList[1],FirstList[1]),
                 sheet = 'Sheet1',
                 perl = perl,header = F)

## Replace awkward NA's with ''
mydf[is.na(mydf)] <- ''

RemoveList <- c()
k <- 0
for(i in mydf){
  ## Remove leading and trailing multiple spaces, and replace multiple spaces with one
  i[1] <- gsub('[ ]+',' ',(gsub('^[ ]+||[ ]+$','',i[1])))

  ## Find how many entries have blank entries.  If column is completely full of data,
  ## this will pass NA, so convert to 0
  BlankSum <- sum(i=='')
  BlankSum[is.na(BlankSum)] <- 0
  
  ## Save column number, then check if every row in this column was blank.
  ## If so, add column number to remove list
  k <- k+1
  if(BlankSum==length(i)){
    RemoveList <- c(RemoveList,k)
  }
}
## Remove all columns with no data
mydf <- mydf[-RemoveList]

## Find row numbers which start with a date
k <- 0
DatesDF <- data.frame()
ErrorList <- c()
for(i in mydf){
  k <- k+1
  RowNum <- which(grepl('^[0-9][0-9]\\/[0-9][0-9]\\/[0-9][0-9]',i))
  if(!is.na(RowNum[1])){
    if(nrow(DatesDF)==0){
      DatesDF <- data.frame(RowNum)
    } else if(length(DatesDF[[1]])==length(RowNum)){
      DatesDF <- data.frame(DatesDF,RowNum)
    } else {
      ErrorList <- c(ErrorList,paste('Wrong number of dates in column',k))
      print(paste('Error: Number of dates do not match in column',k))
    }
  }
}


#V3 startdate V8 start time V13 enddate V18 end time