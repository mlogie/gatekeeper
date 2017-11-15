##########################################################################################
#                                                                                        #
#  Code to read in Gatekeeper spreadsheet and parse into a data frame                    #
#                                                                                        #
##########################################################################################
## First, load up the gdata library, for reading xlsx files
install.packages('gdata')
install.packages('xlsx')
library('gdata')
library('xlsx')

## Set the file path
datapath <- file.path('.','Farm_data')
## Discover all the folders in the data folder
FolderList <- list.files(datapath)
perl <- 'C:/Users/marlog/Downloads'
## Create a list of all the files in folder 1, and save just those in xls format
FirstList <- list.files(file.path(datapath,FolderList[1]))
FirstList <- FirstList[grepl('.xls$',FirstList)]
file.path(datapath,FolderList[1],FirstList[1])
mydf <- read.xls(file.path(datapath,FolderList[1],FirstList[1]),sheet = 'Sheet1')
read.xlsx("./Farm_data/Druids Lodge/Detailed Operations camp and flinty pond 2013.xls")

list.files(path)
