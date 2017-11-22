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
source('./ReadAnalysis.R')
source('./RemoveList.R')

## Set the file path
datapath <- file.path('.','Farm_data')
perl <- 'C:/Strawberry/perl/bin/perl5.26.1.exe'

## Discover all the files in the data folder
FileList <- list.files(file.path(datapath),recursive=T)
FileList <- FileList[grepl('xls(x)?$|csv$',FileList)]

## Set the output
DataOut   <- c()
## Create column headers for the table
ColHeaders  <- c('Farm','Field','Crop','Variety',
                 'Product','Details','Area','Area Units','Rate','Rate Units',
                 'Year','Start Date','End Date','Start Time','End Time',
                 'Weather','Temp','Wind speed/direction','Soil','Implement',
                 'Reference','Advisor','Operator','Issued By','Source')

DataOut <- data.frame()
for(i in ColHeaders){
  DataOut <- data.frame(DataOut,character())
}
colnames(DataOut) <- ColHeaders

for(i in FileList){
  FileType <- ''
  ## Read in the file
  if(grepl('xls(x)?$',i)){
    mydf <- read.xls(file.path(datapath,i),
                     sheet = 'Sheet1',
                     perl = perl,
                     header = F)
  } else if(grepl('csv$',i)){
    mydf <- read.csv(file.path(datapath,i))
  }

  ##Find the parent folder for the file
  FarmFolder <- substr(i,1,regexpr('\\/',i)[1]-1)
  
  ## Replace awkward NA's with ''
  mydf[is.na(mydf)] <- ''

  ## Check whether file is detailed or condensed, then call the relevant function
  if(grepl('\\/Detailed.*xls(x)?$',i)){
    TmpDF <- read.detailed(mydf,ColHeaders,FarmFolder)
    FileType <- 'detailed'
  } else if(grepl('\\/Condensed.*xls(x)?$',i)){
    TmpDF <- read.condensed(mydf,ColHeaders,FarmFolder)
    FileType <- 'condensed'
  } else if(grepl('\\/Analysis.*csv$',i)){
    TmpDF <- read.analysis(mydf,ColHeaders,FarmFolder)
    FileType <- 'analysis'
  }
  if(FileType!=''){
    DataOut <- rbind(DataOut,TmpDF)
    print(paste0('Finished reading (',FileType,') file ',
                 match(i,FileList),' of ',length(FileList),':'))
  } else {
    print(paste0('Did not read file (',FileType,') file ',
                 match(i,FileList),' of ',length(FileList),':'))
  }
  print(paste('      ',(i)))
}

write.csv(DataOut,file.path('.','Output','DataOut.csv'),row.names = F)