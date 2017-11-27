#######################################################################################
#                                                                                     #
#   Short code to create a lookup table                                               #
#                                                                                     #
#######################################################################################
source('./OutputRepro.R')
lookup.table <- function(dataoutdf){
  ## Call field summary function to create a summary table
  SummaryCols <- c('Product','ProductID','Active Ingredients','Manufacturer')
  ReturnList  <- repro.field.summary(DataOut,SummaryCols)
  ## Remove rows with no details
  LookupTable <- ReturnList$df[ReturnList$df$`Active Ingredients`!='',]
  ## Convert to character strings
  LookupTable[] <- lapply(LookupTable, as.character)
  ## Find if any rows are duplicated - this would be a problem in a lookup table
  if(!is.na(LookupTable$Product[which(duplicated(LookupTable$Product))][1])){
    ## There are duplicates - output them as an error message
    stop('Duplicated products found: ',
         paste0(LookupTable$Product[which(duplicated(LookupTable$Product))],' '))
  } 
  ## Return the Lookup Table
  LookupTable
}

