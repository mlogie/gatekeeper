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
  
  ## Create blank column for data not present in this format
  Blank <- character(TableLen)
  
  AllTable <- data.frame(Farm,csvdf$Field.Defined.Name,Blank,Blank,
                         csvdf$Product.Name,Blank,Blank,Blank,Blank,Blank,
                         csvdf$Application.Area.ha,AreaUnits,
                         csvdf$Rate.per.Application.Area.ha,RateUnits,
                         csvdf$Year,csvdf$Actual.Issued.Date,Blank,Blank,Blank,
                         Blank,Blank,Blank,Blank,Blank,
                         Blank,Blank,Blank,Blank,Source)

  colnames(AllTable) <- columns
  
  ## Return the table
  AllTable
}