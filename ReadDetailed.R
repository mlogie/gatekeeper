#####################################################################################
#                                                                                   #
#  Section if the file is the awkward 'detailed' spreadsheet format                 #
#                                                                                   #
#####################################################################################
read.detailed <- function(xlsdf,columns,farmfolder){
  source('./ParseDetails.R')
  ## Create a list from which to build the table to return
  TableData <- list()
  for(i in columns){TableData <- c(TableData,list(character()))}

  ## Find key anchors in the table (start of new Field and dates within each Field)
  k <- 0
  DatesDF <- data.frame()
  ErrorList <- c()
  DateCol <- c()
  AllData <- c()
  
  ## Find all rows which just start with when the file was printed or 'Gatekeeper'
  RemRow   <- which(grepl('^Printed:|^Gatekeeper',xlsdf[[1]]))
  
  ## Also find rows which have a date but no 'Start' text.  This is because a record
  ## has spilled over two pages resulting in unnecessary repetition in the spreadsheet
  ## Remove this row and the row below it
  DateRow  <- which(grepl('^[0-9][0-9]\\/[0-9][0-9]\\/[0-9][0-9]',xlsdf[[3]]))
  StartRow <- which(!grepl('^Start:',xlsdf[[1]]))
  RemRow2  <- intersect(DateRow,StartRow)
  RemRow3  <- RemRow2+1
  RemRow   <- c(RemRow,RemRow2,RemRow3)

  ## Now remove these rows
  xlsdf    <- xlsdf[-RemRow,]

  ## Resequence the row ID numbers
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
  MapSheet    <- c('Intro')
  NGNumber    <- c('Intro')
  Centroid    <- c('Intro')

  for(i in 2:length(RowNumField)){
    FieldNames <- c(FieldNames,paste(xlsdf[as.numeric(RowNumField[i]),1]))
    VarNames   <- c(VarNames,paste(xlsdf[as.numeric(RowNumField[i])+1,4]))
    CropNames  <- c(CropNames,paste(xlsdf[as.numeric(RowNumField[i])+2,4]))
    MapSheet   <- c(MapSheet,paste(xlsdf[as.numeric(RowNumField[i]+1,30)]))
    NGNumber   <- c(NGNumber,paste(xlsdf[as.numeric(RowNumField[i]+2,30)]))
  }
  ## One of the farms annoyingly has a typo where a 0 is entered as a O.  Fix this here.
  MapSheet <- gsub('([A-Z][A-Z][0-9]?)([Oo])','\\10',MapSheet)
  ## Generate centroid for field from Map Number and NG Number.
  ## Example: MapNumber=SU0937, NGNumber=8859, Centroid=SU09883759
  Centroid <- paste0(substr(MapSheet,1,4),substr(NGNumber,1,2),
                     substr(MapSheet,5,6),substr(NGNumber,3,4))

  FieldList <- data.frame(RowNumField,FieldNames,VarNames,CropNames,
                          MapSheet,NGNumber,Centroid)
  
  ## Convert dataframe to characters, as splitting does not work so well with vectors
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
  Occurrence <- c(1,2,1,2,1,1,1,1,1,1,1,1,1)
  PlusColumn <- c(0,0,5,3,3,2,6,1,0,4,3,3,3)
  DataLen    <- length(Data)
  PlusRow    <- numeric(DataLen)
  Position   <- numeric(DataLen)
  RowTable   <- numeric(DataLen)
  ColTable   <- numeric(DataLen)
  Result     <- character(DataLen)
  DTL        <- data.frame(Data,Result,Regex,Occurrence,PlusColumn,PlusRow,Position,
                           RowTable,ColTable,
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
        PosTmp <- which(grepl(DTL$Regex[k],tmplong$entry))
        if(!is.na(PosTmp[1])){
          ## Found one.  Work out where the data is to be found.
          ## First, find the row using the modulus of the position in the long table
          DTL$RowTable[k] <- PosTmp[DTL$Occurrence[k]] %% nrowtmp
          ## This will resolve to 0 if the data is in the bottom row, so if 0, set row
          ## number to be the number of rows
          if(DTL$RowTable[k]==0){DTL$RowTable[k] <- nrowtmp}
          ## Then find the column using the remainder function
          DTL$ColTable[k] <- ((PosTmp[DTL$Occurrence[k]]-1) %/% nrowtmp)+1
          
          ## Shift rows and columns based on standard table format
          DTL$RowTable[k] <- DTL$RowTable[k]+DTL$PlusRow[k]
          DTL$ColTable[k] <- DTL$ColTable[k]+DTL$PlusColumn[k]
          
          ## Then find the result using the x/y coordinates just found in the table
          DTL$Result[k] <- tmp[[i]][[j]][DTL$RowTable[k],DTL$ColTable[k]]

          if(grepl('^Implement:',DTL$Result[k])){
          ## Get rid of the word 'Implement' to make results more readable
            DTL$Result[k] <-
              substr(DTL$Result[k],12,nchar(DTL$Result[k]))
          }
        } else {
          ## Not found
          DTL$Position[k] <- NA
          DTL$Result[k]   <- ''
        }
      }
      if(DTL$Result[1]==''){
        DateTmp <- 'Intro'
      } else {
        DateTmp <- DTL$Result[1]
      }
      if(DateTmp!='Intro'){
        ## We are looking at a non-intro occurrence and we have a neat list of data
        ## Problem is there is often more than one Product per event, so separate out
        ## these Products
        addsplit    <- which(!grepl('Start:|^$',tmp[[i]][[j]][[1]]))
        AllDataTmp  <- DTL[,c(1,2)]
        AllDataField <- data.frame(Data=c('Farm','Field','Crop','Variety'),
                                   Result=c(farmfolder,
                                            FieldList$FieldNames[i],
                                            FieldList$CropNames[i],
                                            FieldList$VarNames[i],
                                            FieldList$MapSheet[i],
                                            FieldList$NGNumber[i],
                                            FieldList$Centroid[i]),
                                   stringsAsFactors = F)

        for(l in addsplit){
          ## Collect the Product and application figures.  Find the row with the
          ## product then look along the row for the occurrence of numbers and other
          ## letters which denote: Area, Area Units, Volume, Volume Units
          ProductRow <- as.character(tmp[[i]][[j]][l,])
          ## The product is always the first item in the row
          Product    <- ProductRow[1]
          
          ## Find entries with numbers but no letters (some products contain numbers)
          Numbers     <- grep('[0-9]+',ProductRow,value=T)
          NumbersOnly <- grep('[A-Za-z]+',Numbers,invert=T,value=T)
          ## The first number is area, the second is volume
          if(!is.na(NumbersOnly[1])){
            Area <- NumbersOnly[1]
          } else {
            Area <- ''
          }
          if(!is.na(NumbersOnly[2])){
            Volume <- NumbersOnly[2]
          } else {
            Volume <- ''
          }
          
          ## Find entries with letters.  This will include the product (entry one)
          Text <- as.character(grep('[A-Za-z]+',ProductRow,value=T))
          ## The second entry is area units, the third is volume units
          if(!is.na(Text[2])){
            AreaUnits <- Text[2]
          } else {
            AreaUnits <- ''
          }
          if(!is.na(Text[3])){
            VolumeUnits <- Text[3]
          } else {
            VolumeUnits <- ''
          }

          ## The details are a bit more complicated as they are given one row below the
          ## rest of the data, but are sometimes not present, which would mean we would
          ## be trying to get data out of bounds of the table.
          if(nrow(tmp[[i]][[j]])==l){
            ## We're at the bottom of the table, so no details present
            DetailsList <- list(ProductID='',HarvestInterval='',
                                ActiveIngredients='',Manufacturer='',
                                Expires='')
          } else {
            Details <- tmp[[i]][[j]][l+1,2]
            if(grepl('^MAPP',Details)){
              ## This is how the details always start.  Pass this to the details parser
              DetailsList <- parse.details(Details)
            } else {
              ## It's not details, so overwrite
              DetailsList <- list(ProductID='',HarvestInterval='',
                                  ActiveIngredients='',Manufacturer='',
                                  Expires='')
              
            }
          }
          AllDataProd <- data.frame(Data=c('Product','ProductID',
                                           'Harvest Interval','Active Ingredients',
                                           'Manufacturer','Expires',
                                           'Area','Area Units',
                                           'Rate','Rate Units'),
                                    Result=c(Product,
                                             DetailsList$ProductID,
                                             DetailsList$HarvestInterval,
                                             DetailsList$ActiveIngredients,
                                             DetailsList$Manufacturer,
                                             DetailsList$Expires,
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
                                c('Source','Gatekeeper - Detailed'))

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
