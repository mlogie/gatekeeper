#########################################################################################
#                                                                                       #
#  Code to look at the parsing outputs and create summary tables or output useful       #
#  subsets                                                                              #
#                                                                                       #
#########################################################################################
## Produce summary of unique field specific terms, passed in as a list
field.summary <- function(dataoutdf,columnsummary){
  summarydf  <- unique(dataoutdf[,columnsummary])
  returnlist <- list(summarydf,'FieldSummary.csv')
  returnlist
}

## Create list for summary and run summary function
SummaryCols <- c('Farm','Field','Crop','Variety','Product')
OutputTmp <- field.summary(DataOut,SummaryCols)

## Check if we have the output folder, and if not, create one for the output file
if(!dir.exists(file.path('.','Output'))){
  dir.create(file.path('.','Output'))
}

write.csv(OutputTmp[[1]],file.path('.','Output',OutputTmp[[2]]),row.names = F)
print(paste('File saved to:',file.path('.','Output',OutputTmp[[2]])))
