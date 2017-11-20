########################################################################################
#                                                                                      #
#  Code to read in Gatekeeper spreadsheet and parse into a data frame                  #
#                                                                                      #
########################################################################################
## First, load up the gdata library, for reading xlsx files
#install.packages('gdata')
#install.packages('xlsx')
library('gdata')
library('tidyr')

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

for(m in FirstList){}

file.path(datapath,FolderList[1],FirstList[1])
mydf <- read.xls(file.path(datapath,FolderList[1],FirstList[1]),
                 sheet = 'Sheet1',
                 perl = perl,
                 header = F)

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
## This step is currently commented out, as it's simpler not to do this actually
#mydf <- mydf[-RemoveList]

## Find key anchors in the table (start of new farm and dates within each farm)
k <- 0
DatesDF <- data.frame()
FarmList <- data.frame()
ErrorList <- c()
DateCol <- c()
for(i in mydf){
  k <- k+1
  ## Find entries which match a date format
  RowNumDate <- which(grepl('^[0-9][0-9]\\/[0-9][0-9]\\/[0-9][0-9]',i))
  if(!is.na(RowNumDate[1])){
    if(nrow(DatesDF)==0){
      DatesDF <- data.frame(RowNumDate)
      DateCol <- c(k)
    } else if(length(DatesDF[[1]])==length(RowNumDate)){
      DatesDF <- data.frame(DatesDF,RowNumDate)
      DateCol <- c(DateCol,k)
    } else {
      ErrorList <- c(ErrorList,paste('Wrong number of dates in column',k))
      print(paste('Error: Number of dates do not match in column',k))
    }
  }
  ## Find entries which say exactly 'Variety:'.  This should be in only one column
  RowNumVari <- which(grepl('^Variety:$',i))
  if(!is.na(RowNumVari[1])){
    if(nrow(FarmList)==0){
      RowNumFarm <- (RowNumVari - 1)
      FarmList <- data.frame(RowNumFarm)
      colnames(FarmList) <- c(k)
    } else {
      ErrorList <- c(ErrorList,
                     paste('Found additional instance of Variety in column',k))
      print(paste('Found additional instance of Variety in column',k))
    }
  }
}
## Assign column numbers to the dates dataframe
colnames(DatesDF) <- DateCol

## Determine the farm names from the variety column, as farm name is always directly
## above the word variety in the spreadsheet
FarmCol <- as.numeric(colnames(FarmList[1]))
FarmNames <- c()
for(i in 1:length(FarmList[[1]])){
  FarmNames <- c(FarmNames,paste(mydf[FarmList[i,1],FarmCol]))
}
FarmList <- data.frame(FarmList,FarmNames)

## Convert to characters, as splitting does not work so well with vectors
mydf[] <- lapply(mydf, as.character)

## Split the full data frame by farm, giving the preamble df the name 'Intro'
tmp <- split(mydf, cumsum(1:nrow(mydf) %in% FarmList[[1]]))
names(tmp) <- c('Intro',as.character(FarmList$FarmNames))

## Now we have a large list, within which are data frames for each farm.
## We want to split this by date, and populate a table with all the useful information
## First, determine what that useful information is, and where it'll be found
Data  <- c('Start Date','End Date',
           'Start Time','End Time',
           'Weather','Temp','Wind speed/direction','Soil','Implement',
           'Reference','Advisor','Operator')
Regex <- c('^[0-9][0-9]\\/[0-9][0-9]\\/[0-9][0-9]',
           '^[0-9][0-9]\\/[0-9][0-9]\\/[0-9][0-9]',
           '^[0-9][0-9]\\/[0-9][0-9]\\/[0-9][0-9]',
           '^[0-9][0-9]\\/[0-9][0-9]\\/[0-9][0-9]',
           '^Weather:$','^Temp °C:$','^Wind speed/direction:$','^Soil:$','^Implement:',
           '^Reference:$','^Advisor:$','^Operator:$')
Occurrence   <- c(1,1,2,2,1,1,1,1,1,1,1,1)
PlusColumn   <- c(0,0,5,3,3,2,6,1,0,4,3,3)
DataLen      <- length(Data)
PlusRow      <- numeric(DataLen)
Position     <- numeric(DataLen)
Result       <- character(DataLen)
DataToLocate <- data.frame(Data,Regex,Occurrence,PlusColumn,PlusRow,Position,Result,
                           stringsAsFactors = F)

AllData      <- list()
## Now, split each farm by event (date).  For this need to use the relative row number
## rather than the full table row number determined pre-split
for(i in 1:length(tmp)){
  FirstRowTmp <- as.numeric(rownames(tmp[[i]]))[1]
  tmp[[i]] <- split(tmp[[i]],
                    cumsum(1:nrow(tmp[[i]]) %in% (DatesDF[[1]]-FirstRowTmp+1)))
  DateList <- c()
  DateTmp <- c()
  for(j in 1:length(tmp[[i]])){
    ## We've split the data, and we're now just looking at one event in one farm
    nrowtmp <- nrow(tmp[[i]][[j]])
    ## Create a long version of the data.  This makes grepping easier
    tmplong <- gather(tmp[[i]][[j]],column,entry,
                      colnames(tmp[[i]][[j]]),factor_key=TRUE)
    for(k in 1:DataLen){
      PosTmp <- which(grepl(DataToLocate$Regex[k],tmplong$entry))
      if(!is.na(PosTmp[1])){
        DataToLocate$Position[k] <-
          (PosTmp[DataToLocate$Occurrence[k]]+
          (DataToLocate$PlusColumn[k]*nrowtmp) + DataToLocate$PlusRow[k])
        DataToLocate$Result[k]   <- tmplong$entry[DataToLocate$Position[k]]
      } else {
        DataToLocate$Position[k] <- NA
        DataToLocate$Result[k]   <- ''
      }
    }
    if(DataToLocate$Result[1]==''){
      DateTmp <- 'Intro'
    } else {
      DateTmp <- DataToLocate$Result[1]
    }
    DateList <- c(DateList,DateTmp)
  }
  names(tmp[[i]]) <- DateList
  print(DateList)
}

#which(!grepl('Start:|^$',tmp[[3]][[2]][[1]]))
#PosTmp <- which(grepl(DataToLocate$Regex[k],tmplong$entry))

#tmptmp <- c(tmptmp,list(data.frame(c(9,10,10),c(11,12,13))))
