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
AllData   <- list()
m <- FirstList[1]

for(m in 1:length(FirstList)){
  mydf <- read.xls(file.path(datapath,FolderList[1],FirstList[m]),
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
  
  #####################################################################################
  #                                                                                   #
  #  Section if the file is the awkward 'detailed' spreadsheet format                 #
  #                                                                                   #
  #####################################################################################
  if(grepl('^Detailed',FirstList[m])){
    ## Find key anchors in the table (start of new farm and dates within each farm)
    k <- 0
    DatesDF <- data.frame()
    ErrorList <- c()
    DateCol <- c()
    
    ## Remove all rows which just start with when the file was printed or Gatekeeper
    mydf <- mydf[-which(grepl('^Printed:|^Gatekeeper',mydf[[1]])),]
    rownames(mydf) <- seq(length=nrow(mydf))
    
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
        RowNumFarm <- (RowNumVari - 1)
      } else if(k==1){
        ErrorList <- c(ErrorList,
                       paste('Found no instance of Variety',k))
        print(paste('Found no instance of Variety in column',k))
      }
    }
  
    ## Assign column numbers to the dates dataframe
    colnames(DatesDF) <- DateCol
    
    ## Determine the farm names from the variety column, as farm name is always directly
    ## above the word variety in the spreadsheet
    FarmNames <- c()
    VarNames  <- c()
    CropNames <- c()
  
    for(i in 1:length(RowNumFarm)){
      FarmNames <- c(FarmNames,paste(mydf[RowNumFarm[i],1]))
      VarNames  <- c(VarNames,paste(mydf[RowNumFarm[i]+1,4]))
      CropNames <- c(CropNames,paste(mydf[RowNumFarm[i]+2,4]))
    }
  
    FarmList <- data.frame(RowNumFarm,FarmNames,VarNames,CropNames)
  
    ## Convert to characters, as splitting does not work so well with vectors
    mydf[] <- lapply(mydf, as.character)
    
    ## Split the full data frame by farm, giving the preamble df the name 'Intro'
    tmp <- split(mydf, cumsum(1:nrow(mydf) %in% FarmList[[1]]))
    splitnames <- c('Intro',as.character(FarmList$FarmNames))
    names(tmp) <- splitnames
    
    ## Convert the farm list to characters as this causes problems later if you don't
    ## Add a first intro row so numbers match up later
    FarmList[] <- lapply(FarmList, as.character) 
    FarmList   <- rbind(c('Intro','Intro','Intro','Intro'),FarmList)
  
    ## Now we have a large list, within which are data frames for each farm.
    ## We want to split this by date, and populate a table with all the useful information
    ## First, determine what that useful information is, and where it'll be found
    Data  <- c('Start Date','End Date',
               'Start Time','End Time',
               'Weather','Temp','Wind speed/direction','Soil','Implement',
               'Reference','Advisor','Operator','Issued By')
    Regex <- c('^[0-9][0-9]\\/[0-9][0-9]\\/[0-9][0-9]',
               '^[0-9][0-9]\\/[0-9][0-9]\\/[0-9][0-9]',
               '^[0-9][0-9]\\/[0-9][0-9]\\/[0-9][0-9]',
               '^[0-9][0-9]\\/[0-9][0-9]\\/[0-9][0-9]',
               '^Weather:$','^Temp °C:$','^Wind speed/direction:$','^Soil:$',
               '^Implement:','^Reference:$','^Advisor:$','^Operator:$','^Issued by:$')
    Occurrence   <- c(1,2,1,2,1,1,1,1,1,1,1,1,1)
    PlusColumn   <- c(0,0,5,3,3,2,6,1,0,4,3,3,3)
    DataLen      <- length(Data)
    PlusRow      <- numeric(DataLen)
    Position     <- numeric(DataLen)
    Result       <- character(DataLen)
    DataToLocate <- data.frame(Data,Result,Regex,Occurrence,PlusColumn,PlusRow,Position,
                               stringsAsFactors = F)
    
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
  
        ## We now have a neat list of all the data
        ## Problem is there is often more than one additive per event, so separate out
        ## these additives
        addsplit <- which(!grepl('Start:|^$',tmp[[i]][[j]][[1]]))
        AllDataTmp <- DataToLocate[,c(1,2)]
        AllDataTmp <- rbind(c('Farm',FarmList$FarmNames[i]),
                            c('Variety',FarmList$VarNames[i]),
                            c('Crop',FarmList$CropNames[i]),
                            AllDataTmp)
  
        if(DateTmp!='Intro'){
          ## We are looking at a non-intro occurrence
          for(l in addsplit){
            ## Collect the additive and application figures.  Numbers here assume a common
            ## format of spreadsheet.  This is not sophisticated enough to grep for
            ## everything
            Additive    <- tmp[[i]][[j]][[1]][[l]]
            Area        <- tmp[[i]][[j]][[20]][[l]]
            AreaUnits   <- tmp[[i]][[j]][[25]][[l]]
            Volume      <- tmp[[i]][[j]][[29]][[l]]
            VolumeUnits <- tmp[[i]][[j]][[33]][[l]]
            ## The details are a bit more complicated as they are given one row below the
            ## rest of the data, but are sometimes not present, which would mean we would
            ## be trying to get data out of bounds.
            if(nrow(tmp[[i]][[j]])==l){
              Details <- ''
            } else {
              Details     <- tmp[[i]][[j]][[2]][[l+1]]
              if(!grepl('^MAPP',Details)){
                Details <- ''
              }
            }
            AllDataTmp2 <- rbind(AllDataTmp,
                                 c('Additive',Additive),
                                 c('Details',Details),
                                 c('Area',Area),
                                 c('Area Units',AreaUnits),
                                 c('Volume',Volume),
                                 c('Volume Units',VolumeUnits))
            AllData <- c(AllData,list(AllDataTmp2))
          }
        }
      }
      names(tmp[[i]]) <- DateList
    } #End of detailed file reading
  }
  print(paste0('Finished reading file: ',FirstList[m],
               ', file ',m,' of ',length(FirstList)))
}

