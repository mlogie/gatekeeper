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
source('./RemoveList.R')

## Set the file path
datapath <- file.path('.','Farm_data')
perl <- 'C:/Strawberry/perl/bin/perl5.26.1.exe'
## Discover all the folders in the data folder
FolderList <- list.files(datapath)

## Create a list of all the files in folder 1, and save just those in xls format
FirstList <- list.files(file.path(datapath,FolderList[1]))
FirstList <- FirstList[grepl('.xls$',FirstList)]
DataOut   <- c()
m <- FirstList[1]

for(m in 1:length(FirstList)){
  mydf <- read.xls(file.path(datapath,FolderList[1],FirstList[m]),
                   sheet = 'Sheet1',
                   perl = perl,
                   header = F)
  
  ## Replace awkward NA's with ''
  mydf[is.na(mydf)] <- ''
  
  RemoveList <- remove.list(mydf)
  ## Remove all columns with no data
  ## This step is currently commented out, as it's simpler not to do this actually
  #mydf <- mydf[-RemoveList]
  
  
  #####################################################################################
  #                                                                                   #
  #  Section if the file is the awkward 'detailed' spreadsheet format                 #
  #                                                                                   #
  #####################################################################################
  if(grepl('^Detailed',FirstList[m])){
    TmpDF <- read.detailed(mydf)
    DataOut <- c(DataOut,TmpDF)
  }
  print(paste0('Finished reading file: ',FirstList[m],
               ', file ',m,' of ',length(FirstList)))
}
