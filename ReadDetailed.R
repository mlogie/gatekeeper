#####################################################################################
#                                                                                   #
#  Section if the file is the awkward 'detailed' spreadsheet format                 #
#                                                                                   #
#####################################################################################
read.detailed <- function(xlsdf,columns,farmfolder){
  ## Create a list from which to build the table to return
  TableData <- list()
  for(i in columns){TableData <- c(TableData,list(character()))}

  ## Find key anchors in the table (start of new Field and dates within each Field)
  k <- 0
  DatesDF <- data.frame()
  ErrorList <- c()
  DateCol <- c()
  AllData <- c()
  
  ## Remove all rows which just start with when the file was printed or Gatekeeper
  xlsdf <- xlsdf[-which(grepl('^Printed:|^Gatekeeper',xlsdf[[1]])),]
  rownames(xlsdf) <- seq(length=nrow(xlsdf))
  
  for(i in xlsdf){
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
      RowNumField <- (RowNumVari - 1)
    } else if(k==1){
      ErrorList <- c(ErrorList,
                     paste('Found no instance of Variety',k))
      print(paste('Found no instance of Variety in column',k))
    }
  }
  
  ## Assign column numbers to the dates dataframe
  colnames(DatesDF) <- DateCol
  
  ## Determine the Field names from the variety column, as Field name is always directly
  ## above the word variety in the spreadsheet
  RowNumField <- c('Intro',RowNumField)
  FieldNames  <- c('Intro')
  VarNames    <- c('Intro')
  CropNames   <- c('Intro')

  for(i in 2:length(RowNumField)){
    FieldNames <- c(FieldNames,paste(xlsdf[as.numeric(RowNumField[i]),1]))
    VarNames   <- c(VarNames,paste(xlsdf[as.numeric(RowNumField[i])+1,4]))
    CropNames  <- c(CropNames,paste(xlsdf[as.numeric(RowNumField[i])+2,4]))
  }

  FieldList <- data.frame(RowNumField,FieldNames,VarNames,CropNames)
  
  ## Convert to characters, as splitting does not work so well with vectors
  xlsdf[] <- lapply(xlsdf, as.character)
  
  ## Split the full data frame by Field, giving the preamble df the name 'Intro'
  tmp <- split(xlsdf, cumsum(1:nrow(xlsdf) %in% FieldList[[1]]))
  splitnames <- c(as.character(FieldList$FieldNames))
  names(tmp) <- splitnames
  
  ## Convert the Field list to characters as this causes problems later if you don't
  FieldList[] <- lapply(FieldList, as.character) 

  ## Now we have a large list, within which are data frames for each Field.
  ## We want to split this by date, and populate a table with all the useful information
  ## First, determine what that useful information is, and where it'll be found
  Data  <- c('Start Date','End Date','Start Time','End Time',
             'Weather','Temp','Wind speed/direction',
             'Soil','Implement','Reference','Advisor','Operator','Issued By')
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
  
  ## Now, split each Field by event (date).  For this need to use the relative row
  ## number rather than the full table row number determined pre-split
  for(i in 1:length(tmp)){
    FirstRowTmp <- as.numeric(rownames(tmp[[i]]))[1]
    tmp[[i]] <- split(tmp[[i]],
                      cumsum(1:nrow(tmp[[i]]) %in% (DatesDF[[1]]-FirstRowTmp+1)))
    DateList <- c()
    DateTmp <- c()
    for(j in 1:length(tmp[[i]])){
      ## We've split the data, and we're now just looking at one event in one Field
      nrowtmp <- nrow(tmp[[i]][[j]])
      ## Create a long version of the data.  This makes grepping easier
      tmplong <- gather(tmp[[i]][[j]],column,entry,
                        colnames(tmp[[i]][[j]]),factor_key=TRUE)
      
      for(k in 1:DataLen){
        ## Look for data we want to extract using the regex from the table
        PosTmp <- which(grepl(DataToLocate$Regex[k],tmplong$entry))
        if(!is.na(PosTmp[1])){
          ## Found one.  Work out where the data is to be found.
          DataToLocate$Position[k] <-
            (PosTmp[DataToLocate$Occurrence[k]]+
               (DataToLocate$PlusColumn[k]*nrowtmp) + DataToLocate$PlusRow[k])
          DataToLocate$Result[k]   <- tmplong$entry[DataToLocate$Position[k]]
          if(grepl('^Implement:',DataToLocate$Result[k])){
            DataToLocate$Result[k] <-
              substr(DataToLocate$Result[k],12,nchar(DataToLocate$Result[k]))
          }
        } else {
          ## Not found
          DataToLocate$Position[k] <- NA
          DataToLocate$Result[k]   <- ''
        }
      }
      if(DataToLocate$Result[1]==''){
        DateTmp <- 'Intro'
      } else {
        DateTmp <- DataToLocate$Result[1]
      }
      if(DateTmp!='Intro'){
        ## We are looking at a non-intro occurrence and we have a neat list of data
        ## Problem is there is often more than one Product per event, so separate out
        ## these Products
        addsplit    <- which(!grepl('Start:|^$',tmp[[i]][[j]][[1]]))
        AllDataTmp  <- DataToLocate[,c(1,2)]
        AllDataField <- data.frame(Data=c('Farm','Field','Crop','Variety'),
                                   Result=c(farmfolder,
                                            FieldList$FieldNames[i],
                                            FieldList$CropNames[i],
                                            FieldList$VarNames[i]),
                                   stringsAsFactors = F)

        for(l in addsplit){
          ## Collect the Product and application figures.  Numbers here assume a
          ## common format of spreadsheet.  This is not sophisticated enough to
          ## grep for everything
          Product     <- tmp[[i]][[j]][[1]][[l]]
          Area        <- tmp[[i]][[j]][[20]][[l]]
          AreaUnits   <- tmp[[i]][[j]][[25]][[l]]
          Volume      <- tmp[[i]][[j]][[29]][[l]]
          VolumeUnits <- tmp[[i]][[j]][[33]][[l]]
          ## The details are a bit more complicated as they are given one row below the
          ## rest of the data, but are sometimes not present, which would mean we would
          ## be trying to get data out of bounds.
          if(nrow(tmp[[i]][[j]])==l){
            ## We're at the bottom of the table, so no details present
            Details <- ''
          } else {
            Details     <- tmp[[i]][[j]][[2]][[l+1]]
            if(grepl('^MAPP',Details)){
              ## This is how the details always start.  Take the substr after 'MAPP:'
              Details <- substr(Details,6,nchar(Details))
            } else {
              ## It's not details, so overwrite
              Details <- ''
            }
          }
          AllDataProd <- data.frame(Data=c('Product','Details',
                                           'Area','Area Units',
                                           'Rate','Rate Units'),
                                    Result=c(Product,Details,
                                             Area,AreaUnits,
                                             Volume,VolumeUnits),
                                    stringsAsFactors = F)
          YearTmp <- substr(AllDataTmp$Result[1],
                            regexpr('\\/[0-9]*$',
                                    AllDataTmp$Result[1])[1]+1,
                                    nchar(AllDataTmp$Result[1]))

          AllDataTmp2  <- rbind(AllDataField,
                                AllDataProd,
                                c('Year',YearTmp),
                                AllDataTmp,
                                c('Source','Detailed'))

          ## We've got a table with all the data from one product application.
          ## Add it to the output list
          for(a in 1:length(TableData)){
            TableData[[a]] <- c(TableData[[a]],AllDataTmp2$Result[a])
          }
        }
      }
    }
    names(tmp[[i]]) <- DateList
  } #End of detailed file reading
  
  ## Populate a neat data frame with the data
  FullTable <- data.frame(TableData[[1]])
  for(a in 2:length(TableData)){
    FullTable <- data.frame(FullTable,TableData[[a]])
  }
  ## Name the columns
  colnames(FullTable) <- columns
  
  ## Return the table
  FullTable
}
