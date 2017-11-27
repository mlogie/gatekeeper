#########################################################################################
#                                                                                       #
#  Code to look at the parsing outputs and create summary tables or output useful       #
#  subsets                                                                              #
#                                                                                       #
#########################################################################################
## Produce summary of unique field specific terms, passed in as a list
repro.field.summary <- function(dataoutdf,columnsummary,filename){
  summarydf  <- unique(dataoutdf[,columnsummary])
  ## Check if we have the output folder, and if not, create one for the output file
  if(!dir.exists(file.path('.','Output'))){
    dir.create(file.path('.','Output'))
  }
  ## Write to file.  The first item in the list is the data, the second is the name
  write.csv(summarydf,file.path('.','Output',filename),row.names = F)
  
  returnmsg  <- paste('File saved to:',file.path('.','Output',filename))
  returnlist <- list(returnmsg,summarydf)
  names(returnlist) <- c('msg','df')
  returnlist
}




