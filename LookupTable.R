#######################################################################################
#                                                                                     #
#   Short code to create a lookup table                                               #
#                                                                                     #
#######################################################################################
source('./OutputRepro.R')
lookup.table <- function(dataoutdf){
  ## Call field summary function to create a summary table
  SummaryCols <- c('Product','ProductID','Active Ingredients','Manufacturer')
  ReturnList  <- repro.field.summary(dataoutdf,SummaryCols)
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
  ## Find products in the Data Out table with no details
  Blanks  <- which(dataoutdf$`Active Ingredients`=='')
  ## Match all these products with the products in lookup table which we do have data for
  Matches <- match(dataoutdf$Product[Blanks],LookupTable$Product)
  ## Remove all entries which there is no match for
  Blanks  <- Blanks[which(!is.na(Matches))]
  Matches <- Matches[which(!is.na(Matches))]
  ## Set all the product IDs, Active Ingredients and Manufacturers using the Lookup Table
  dataoutdf$ProductID[Blanks]            <- LookupTable$ProductID[Matches]
  dataoutdf$`Active Ingredients`[Blanks] <- LookupTable$`Active Ingredients`[Matches]
  dataoutdf$Manufacturer[Blanks]         <- LookupTable$Manufacturer[Matches]
  ## Return the Lookup Table and data out DF
  returnlist <- list(dataoutdf,LookupTable)
  names(returnlist) <- c('df','lookup')
  returnlist
}