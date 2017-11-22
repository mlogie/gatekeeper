########################################################################################
#                                                                                      #
#  Code to read in Gatekeeper spreadsheet and parse into a data frame                  #
#                                                                                      #
########################################################################################
rm(list=ls())

## First, load up the libraries and functions
#install.packages('gdata')
#install.packages('tidyr')
library('gdata')
library('tidyr')
source('./ReadDetailed.R')
source('./ReadCondensed.R')
source('./RemoveList.R')

## Set the file path
datapath <- file.path('.','Farm_data')
perl <- 'C:/Strawberry/perl/bin/perl5.26.1.exe'

## Discover all the files in the data folder
FileList <- list.files(file.path(datapath),recursive=T)
FileList <- FileList[grepl('.xls(x)?$',FileList)]

## Set the output
DataOut   <- c()
## Create column headers for the table
ColHeaders  <- c('Parent','Farm','Crop','Variety',
                 'Product','Details','Area','Area Units','Rate','Rate Units',
                 'Start Date','End Date','Start Time','End Time',
                 'Weather','Temp','Wind speed/direction','Soil','Implement',
                 'Reference','Advisor','Operator','Issued By','Source')

DataOut <- data.frame()
for(i in ColHeaders){
  DataOut <- data.frame(DataOut,character())
}
colnames(DataOut) <- ColHeaders

for(i in FileList){
  ## Read in the file
  mydf <- read.xls(file.path(datapath,i),
                   sheet = 'Sheet1',
                   perl = perl,
                   header = F)
  
  ##Find the parent folder for the file
  ParentFolder <- substr(i,1,regexpr('\\/',i)[1]-1)
  
  ## Replace awkward NA's with ''
  mydf[is.na(mydf)] <- ''
  
  ## Check whether file is detailed or condensed, then call the relevant function
  if(grepl('\\/Detailed',i)){
    TmpDF <- read.detailed(mydf,ColHeaders,ParentFolder)
    FileType <- 'detailed'
  } else if(grepl('\\/Condensed',i)){
    TmpDF <- read.condensed(mydf,ColHeaders,ParentFolder)
    FileType <- 'condensed'
  }
  DataOut <- rbind(DataOut,TmpDF)
  print(paste0('Finished reading (',FileType,') file ',
               match(i,FileList),' of ',length(FileList)))
  print(paste('      ',(i)))
}

write.csv(DataOut,file.path(datapath,'Output','DataOut.csv'),row.names = F)