#########################################################################################
#                                                                                       #
#  Section to look at the 'details' output and parse it into more information           #
#                                                                                       #
#########################################################################################
parse.details <- function(details){
  tmp <- gsub('\t','',details)
  ## Find relative positions of key pieces of information in details
  IDRegex <- regexpr('MAPP:',tmp)
  HarvestRegex <- regexpr('Harvest interval:',tmp)
  ActiveRegex <- regexpr('Active Ingredients:',tmp)
  ManuRegex <- regexpr('Manufacturer:',tmp)
  ExpireRegex <- regexpr('Expires:',tmp)
  ## Put these regex's in a list, then put in order
  RegexList <- list(IDRegex,HarvestRegex,ActiveRegex,ManuRegex,ExpireRegex)
  names(RegexList) <- c('ID','Harvest','Active','Manu','Expire')
  RegexList <- RegexList[order(sapply(RegexList, function(x) x[1], simplify=TRUE))]
  
  #tmp <- 'MAPP:16239, Harvest interval:14 Days 00 Hours'
  RegexLength <- length(RegexList)
  for(i in 1:RegexLength){
    if(RegexList[[i]][1]==-1){
      RegexList[[i]][2] <- ''
    } else if(i<RegexLength){
      RegexList[[i]][2] <- substr(tmp,
                                  RegexList[[i]][1]+attr(RegexList[[i]],"match.length"),
                                  RegexList[[i+1]][1]-3)
    } else {
      RegexList[[i]][2] <- substr(tmp,
                                  RegexList[[i]][1]+attr(RegexList[[i]],"match.length"),
                                  nchar(tmp))
    }
  }

  returnlist <- list(RegexList$ID[2],
                     RegexList$Harvest[2],
                     RegexList$Active[2],
                     RegexList$Manu[2],
                     RegexList$Expire[2])
  names(returnlist) <- c('ProductID','HarvestInterval',
                         'ActiveIngredients','Manufacturer',
                         'Expires')
  returnlist
}
