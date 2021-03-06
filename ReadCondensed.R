#####################################################################################
#                                                                                   #
#  Section if the file is the basic 'condensed' spreadsheet format                  #
#                                                                                   #
#####################################################################################
read.condensed <- function(xlsdf,columns,farmfolder){
  ## We have a condensed file, with data spread over three table columns
  xlsdf[] <- lapply(xlsdf, as.character)
  ## First, split the file into 3 tables representing three columns and remove blanks
  FirstTable  <- xlsdf[,1:6]
  FirstTable  <- FirstTable[,-c(2)]
  SecondTable <- xlsdf[,8:14]
  SecondTable <- SecondTable[,-c(2,4)]
  ThirdTable  <- xlsdf[,16:22]
  ThirdTable  <- ThirdTable[,-c(2,4)]
  ## Set column headers for all tables
  TmpHeaders  <- c('Date','Product','Area','Rate','QuantUnits')

  colnames(ThirdTable) <- colnames(SecondTable) <- colnames(FirstTable) <- TmpHeaders
  ## Bind the table into one long table
  AllTable   <- rbind(FirstTable,SecondTable,ThirdTable)
  
  ## Remove completely empty rows
  AllTable <- AllTable[!apply(AllTable == "", 1, all),]
  ## Renumber
  rownames(AllTable) <- NULL
  
  ## Find those without a date or blank entry, to start finding Field and other info
  InfoRows <- which(!(grepl('^[0-9][0-9]/[0-9][0-9]/[0-9][0-9]|^$',
                            AllTable$Date)|
                      grepl('^Printed:|^Gatekeeper|^Date|^Condensed|^Main Business',
                            AllTable$Date)))
  
  ## Find the units for area
  UnitRows <- which(grepl('^Date',AllTable$Date))
  UnitArea <- as.character(AllTable$Area[UnitRows[1]])
  
  ## Convert vectors to characters
  AllTable[] <- lapply(AllTable, as.character)
  
  ## Find Field row numbers
  RowNumField <- c()
  for(i in 1:length(InfoRows)){
    if((i %% 2)==1){
      RowNumField <- c(RowNumField,InfoRows[i])
    }
  }

  ## Split the full data frame by Field
  tmp <- split(AllTable, cumsum(1:nrow(AllTable) %in% RowNumField))

  AllTable <- data.frame()
  for(i in 2:length(tmp)){
    tmp[[i]]$Field  <- tmp[[i]]$Date[1]
    tmp[[i]]$Crop   <- tmp[[i]]$Date[2]
    tmp[[i]]$Source <- 'Gatekeeper - Condensed'
    tmp[[i]] <- tmp[[i]][-c(1,2),]
    AllTable <- rbind(AllTable,tmp[[i]])
  }
  
  ## Remove all extra non-data rows
  DateRows <- which(!grepl('^[0-9][0-9]/[0-9][0-9]/[0-9][0-9]',AllTable$Date))
  AllTable <- AllTable[-DateRows,]
  
  ## Reshuffle table to match the format obtained from detailed data
  TableLen  <- nrow(AllTable)
  
  ## Use the farm folder passed to function to populate column of data
  Farm <- rep(farmfolder,TableLen)
  
  ## Create the area units column and append to rate unit column
  AreaUnits <- rep(UnitArea,TableLen)
  RateUnits <- paste0(AllTable$QuantUnits,'/',UnitArea)
  Quantity  <- as.numeric(AllTable$Area)*as.numeric(AllTable$Rate)
  
  ## Generate the year column
  Year <- substr(AllTable$Date,
                 regexpr('\\/[0-9]*$',AllTable$Date)[1]+1,nchar(AllTable$Date))
  
  ## Generate blank data missing from condensed operations summary
  Blank <- character(TableLen)
  
  ## Build the table
  ## As of 11/12/17, headers are:
  ## Farm,Field,Crop,Variety,MapSheet,NGNumber,Centroid,Product,ProductID,
  ## Harvest Interval,Active Ingredients,Manufacturer,Expires,
  ## Area,Area Units,Rate,Rate Units,Quantity,Quantity Units,
  ## Year,Start Date,End Date,Start Time,End Time,Weather,Temp,
  ## Wind speed/direction,Soil,Implement,Reference,Advisor,Operator,Issued By,Source
  AllTable <- data.frame(Farm,AllTable$Field,AllTable$Crop,Blank,Blank,Blank,Blank,
                         AllTable$Product,Blank,Blank,Blank,Blank,Blank,
                         AllTable$Area,AreaUnits,
                         AllTable$Rate,RateUnits,
                         Quantity,AllTable$QuantUnits,
                         Year,AllTable$Date,Blank,Blank,Blank,
                         Blank,Blank,Blank,Blank,Blank,
                         Blank,Blank,Blank,Blank,AllTable$Source)
  ## Name the columns
  colnames(AllTable) <- columns

  ## Return the table
  AllTable[] <- lapply(AllTable, as.character)
  AllTable
}
