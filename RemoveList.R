remove.list <- function(df){
  RemoveList <- c()
  k <- 0
  for(i in df){
    ## Remove leading and trailing multiple spaces, and replace multiple spaces with one
    i[1] <- gsub('[ ]+',' ',(gsub('^[ ]+||[ ]+$','',i[1])))
    
    ## Find how many entries have blank entries.  If column is completely full of data,
    ## this will pass NA, so convert to 0
    BlankSum <- sum(i=='')
    BlankSum[is.na(BlankSum)] <- 0
    
    ## Save column number, then check if every row in this column was blank.
    ## If so, add column number to remove list
    k <- k+1
    if(BlankSum==length(i)){
      RemoveList <- c(RemoveList,k)
    }
  }
  RemoveList
}