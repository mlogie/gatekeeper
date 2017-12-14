#####################################################################################
#                                                                                   #
#  Section if the file is the simple 'analysis' spreadsheet format                  #
#                                                                                   #
#####################################################################################
read.analysis <- function(csvdf,columns,farmfolder){
  ## Subset the table to only show rows which start with a date
  csvdf <- csvdf[which(grepl('^[0-9]{4}$',csvdf[[1]])),]
  
  ## Generate columns with repeated data
  TableLen  <- nrow(csvdf)
  Farm      <- rep(farmfolder,TableLen)
  AreaUnits <- rep('ha',TableLen)
  Source    <- rep('Gatekeeper - Analysis',TableLen)
  RateUnits <- paste0(csvdf$Units,'/ha')
  Date      <- csvdf$Actual.Issued.Date
  Year      <- substr(Date,regexpr('\\/[0-9]*$',Date)[1]+1,length(Date))
  
  ## Create blank column for data not present in this format
  Blank <- character(TableLen)
  
  ## As of 11/12/17, headers are:
  ## Farm,Field,Crop,Variety,MapSheet,NGNumber,Centroid,Product,ProductID,
  ## Harvest Interval,Active Ingredients,Manufacturer,Expires,
  ## Area,Area Units,Rate,Rate Units,Quantity,Quantity Units,
  ## Year,Start Date,End Date,Start Time,End Time,Weather,Temp,
  ## Wind speed/direction,Soil,Implement,Reference,Advisor,Operator,Issued By,Source
  AllTable <- data.frame(Farm,csvdf$Field.Defined.Name,Blank,Blank,Blank,Blank,Blank,
                         csvdf$Product.Name,Blank,Blank,Blank,Blank,Blank,
                         csvdf$Application.Area.ha,AreaUnits,
                         csvdf$Rate.per.Application.Area.ha,RateUnits,
                         csvdf$Quantity,csvdf$Units,
                         Year,Date,Blank,Blank,Blank,
                         Blank,Blank,Blank,Blank,Blank,
                         Blank,Blank,Blank,Blank,Source)

  colnames(AllTable) <- columns
  
  ## Return the table
  AllTable[] <- lapply(AllTable, as.character)
  AllTable
}
