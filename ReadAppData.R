#####################################################################################
#                                                                                   #
#  Section if the file is the basic 'ApplicationData' spreadsheet format            #
#                                                                                   #
#####################################################################################
read.appdata <- function(xlsdf,columns,farmfolder,fullfilename){
  ## Convert factors to characters
  xlsdf[] <- lapply(xlsdf, as.character)
  ## We have an application data file
  if(grepl('\\.csv$',fullfilename)){
    ## Remove all extra non-data rows
    if(length(which(xlsdf[1]==""))>0){
      xlsdf <- xlsdf[-which(xlsdf[1]==""),]
    }
    ## Capture required variables
    QuantUnits <- xlsdf$Units
    Quantity   <- xlsdf$Quantity
    if(is.null(Quantity)){
      Quantity <- xlsdf$Applied.Quantity
    }
    Product    <- xlsdf$Product.Name
    if(is.null(Product)){
      Product  <- xlsdf$Product
    }
    Variety    <- xlsdf$Variety
    Area       <- xlsdf$Applied.Area.ha
    Rate       <- xlsdf$Applied.Rate
    Date       <- xlsdf$Applied.Date
    TableLen   <- nrow(xlsdf)
    OddData    <- list(Variety,Area,Rate,Date)
    names(OddData) <- c('Variety','Area','Rate','Date')
    for(i in 1:length(OddData)){
      if(is.null(OddData[[i]])){OddData[[i]] <- character(TableLen)}
    }
    Variety <- OddData$Variety
    Area    <- OddData$Area
    Rate    <- OddData$Rate
    Date    <- OddData$Date
    Date    <- as.character(Date)
    AreaUnits <- rep('ha',TableLen)
    RateUnits <- paste0(QuantUnits,'/',AreaUnits)
    
    ## Farm name needs to be extracted from file name
    FieldName  <- substr(fullfilename,
                         gregexpr('_',fullfilename)[[1]][1]+1,
                         gregexpr('_',fullfilename)[[1]][2]-1)
    Field <- rep(FieldName,TableLen)
    ## Generate the year column
    if(Date[1]==""){
      YearTmp <- substr(fullfilename,
                        gregexpr('_',fullfilename)[[1]][2]+1,
                        gregexpr('_',fullfilename)[[1]][3]-1)
      Year <- rep(YearTmp,TableLen)
    } else {
      Year <- substr(Date,regexpr('\\/[0-9]*$',Date)[1]+1,nchar(Date))
    }
  } else if(grepl('\\.xls(x)?$',fullfilename)){
    ## Find field name then remove first two pre-data rows from spreadsheet
    FieldName  <- xlsdf[1,1]
    HeadersRow <- xlsdf[2,]
    xlsdf      <- xlsdf[-c(1:2),]
    Product    <- xlsdf[,which(grepl('Product',HeadersRow))]
    Rate       <- xlsdf[,which(grepl('Applied Rate',HeadersRow))]
    QuantUnits <- xlsdf[,which(grepl('Units',HeadersRow))]
    Date       <- xlsdf[,which(grepl('Applied Date',HeadersRow))]
    Area       <- xlsdf[,which(grepl('Applied Area ha',HeadersRow))]
    Quantity   <- xlsdf[,which(grepl('Applied Quantity',HeadersRow))]
    ## Create the area units column and append to rate unit column
    RateUnits <- paste0(QuantUnits,'/ha')
    ## Generate the year column
    Year      <- substr(Date,regexpr('\\/[0-9]*$',Date)[1]+1,nchar(Date))
    TableLen  <- nrow(xlsdf)
    Field     <- rep(FieldName,TableLen)
    AreaUnits <- rep('ha',TableLen)
    Variety   <- Date <- character(TableLen)
  }
  
  Blank <- character(TableLen)
  ## Populate column of data for source
  SourceData <- rep('ApplicationData',TableLen)

  ## Build the table
  ## As of 11/12/17, headers are:
  ## Farm,Field,Crop,Variety,MapSheet,NGNumber,Centroid,Product,ProductID,
  ## Harvest Interval,Active Ingredients,Manufacturer,Expires,
  ## Area,Area Units,Rate,Rate Units,Quantity,Quantity Units,
  ## Year,Start Date,End Date,Start Time,End Time,Weather,Temp,
  ## Wind speed/direction,Soil,Implement,Reference,Advisor,Operator,Issued By,Source
  AllTable <- data.frame(Blank,Field,Blank,Variety,Blank,Blank,Blank,
                         Product,Blank,Blank,Blank,Blank,Blank,
                         Area,AreaUnits,
                         Rate,RateUnits,Quantity,QuantUnits,
                         Year,Date,Blank,Blank,Blank,
                         Blank,Blank,Blank,Blank,Blank,
                         Blank,Blank,Blank,Blank,SourceData)
  ## Name the columns
  colnames(AllTable) <- columns
  
  ## Return the table
  AllTable[] <- lapply(AllTable, as.character)
  AllTable
}