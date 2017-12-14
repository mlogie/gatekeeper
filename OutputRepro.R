#########################################################################################
#                                                                                       #
#  Code to look at the parsing outputs and create summary tables or output useful       #
#  subsets                                                                              #
#                                                                                       #
#########################################################################################
## Produce summary of unique field specific terms, passed in as a list
repro.field.summary <- function(dataoutdf,columnsummary,filename='',writetofile=F){
  summarydf  <- unique(dataoutdf[,columnsummary])
  ## Check if we have the output folder, and if not, create one for the output file
  if(!dir.exists(file.path('.','Output'))){
    dir.create(file.path('.','Output'))
  }

  if(writetofile){
    if(filename==''){
      returnmsg <- 'File requested, but no filename passed'
      stop('File requested, but no filename passed')
    } else {
    ## File requested, so write file.
    write.csv(summarydf,file.path('.','Output',filename),row.names = F)
    returnmsg  <- paste('File saved to:',file.path('.','Output',filename))
    }
  } else {
    ## No file requested
    returnmsg <- 'Dataframe created and returned using columns'
  }
  returnlist <- list(returnmsg,summarydf)
  names(returnlist) <- c('msg','df')
  returnlist
}