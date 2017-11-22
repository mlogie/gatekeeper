read.condensed <- function(xlsdf,columns,pfolder){
  ## We have a condensed file, with data spread over three table columns
  ## First, split the file into 3 tables representing the three columns and remove blanks
  FirstTable  <- xlsdf[,1:6]
  FirstTable  <- FirstTable[,-c(2)]
  SecondTable <- xlsdf[,8:14]
  SecondTable <- SecondTable[,-c(2,4)]
  ThirdTable  <- xlsdf[,16:22]
  ThirdTable  <- ThirdTable[,-c(2,4)]
  ## Set column headers for all tables
  TmpHeaders  <- c('Date','Product','Area','Rate','RateUnits')

  colnames(ThirdTable) <- colnames(SecondTable) <- colnames(FirstTable) <- TmpHeaders
  ## Bind the table into one long table
  AllTable   <- rbind(FirstTable,SecondTable,ThirdTable)
  
  ## Remove completely empty rows
  AllTable <- AllTable[!apply(AllTable == "", 1, all),]
  ## Renumber
  rownames(AllTable) <- NULL
  
  ## Find those without a date or blank entry, to start finding farm and other info
  InfoRows <- which(!(grepl('^[0-9][0-9]/[0-9][0-9]/[0-9][0-9]|^$',
                            AllTable$Date)|
                      grepl('^Printed:|^Gatekeeper|^Date|^Condensed|^Main Business',
                            AllTable$Date)))
  
  ## Find the units for area
  UnitRows <- which(grepl('^Date',AllTable$Date))
  UnitArea <- as.character(AllTable$Area[UnitRows[1]])
  
  ## Convert vectors to characters
  AllTable[] <- lapply(AllTable, as.character)
  
  ## Find farm row numbers
  RowNumFarm <- c()
  for(i in 1:length(InfoRows)){
    if((i %% 2)==1){
      RowNumFarm <- c(RowNumFarm,InfoRows[i])
    }
  }

  ## Split the full data frame by farm
  tmp <- split(AllTable, cumsum(1:nrow(AllTable) %in% RowNumFarm))

  AllTable <- data.frame()
  for(i in 2:length(tmp)){
    tmp[[i]]$Farm <- tmp[[i]]$Date[1]
    tmp[[i]]$Crop <- tmp[[i]]$Date[2]
    tmp[[i]]$Source <- 'Condensed'
    tmp[[i]] <- tmp[[i]][-c(1,2),]
    AllTable <- rbind(AllTable,tmp[[i]])
  }
  
  ## Remove all extra non-data rows
  DateRows <- which(!grepl('^[0-9][0-9]/[0-9][0-9]/[0-9][0-9]',AllTable$Date))
  AllTable <- AllTable[-DateRows,]
  
  ## Reshuffle table to match the format obtained from detailed data
  TableLen  <- nrow(AllTable)
  
  ## Use the parent folder passed to function to populate column of data
  Parent <- rep(pfolder,TableLen)
  
  ## Create the area units column and append to rate unit column
  AreaUnits <- rep(UnitArea,TableLen)
  AllTable$RateUnits <- paste0(AllTable$RateUnits,'/',UnitArea)
  
  ## Generate blank data missing from condensed operations summary
  StartTime <- EndTime <- EndDate <- Weather <- Temp <- Windspeed <- character(TableLen)
  Soil <- Implement <- Reference <- Advisor <- Operator <- character(TableLen)
  IssuedBy <- Details <- Variety <- character(TableLen)
  
  ## Build table
  AllTable <- data.frame(Parent,AllTable$Farm,AllTable$Crop,Variety,
                         AllTable$Product,Details,AllTable$Area,AreaUnits,
                         AllTable$Rate,AllTable$RateUnits,
                         AllTable$Date,EndDate,StartTime,EndTime,
                         Weather,Temp,Windspeed,Soil,Implement,
                         Reference,Advisor,Operator,IssuedBy,AllTable$Source)
  ## Name the columns
  colnames(AllTable) <- columns
  
  ## Return the table
  AllTable
}
