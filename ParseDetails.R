#########################################################################################
#                                                                                       #
#  Section to look at the 'details' output and parse it into more information           #
#                                                                                       #
#########################################################################################
parse.details <- function(details){
  ## Take all characters from the 6th onwards(following 'MAPP: ')
  tmp <- substr(details,6,nchar(details))
  
  ## Find relative positions of key pieces of information in details
  IDRegex <- regexpr('[0-9]+\\,',tmp)
  HarvestRegex <- regexpr('[ ]*Harvest interval:',tmp)
  ActiveRegex <- regexpr('[ ]*Active Ingredients:',tmp)
  ManuRegex <- regexpr('[ ]*Manufacturer:',tmp)
  ExpireRegex <- regexpr('[ ]*Expires:',tmp)

  ## Check the regex worked, then parse
  if((ActiveRegex[1]==-1)|(ManuRegex[1]==-1)|(IDRegex[1]==-1)){
    returnlist <- list(NA,NA,NA,NA,NA)
  } else {
    IDNumber <- substr(tmp,IDRegex[1],IDRegex[1]+attr(IDRegex, "match.length")-2)
    if(HarvestRegex[1]==-1){
      Harvest <- ''
    } else {
      Harvest <- substr(tmp,
                        HarvestRegex[1]+attr(HarvestRegex, "match.length"),
                        ActiveRegex[1]-2)
    }
    Active <- substr(tmp,
                     ActiveRegex[1]+attr(ActiveRegex, "match.length"),
                     ManuRegex[1]-2)
    if(ExpireRegex[1]==-1){
      Manufacturer <- substr(tmp,
                             ManuRegex[1]+attr(ManuRegex, "match.length"),
                             nchar(tmp))
      Expire <- ''
    } else {
      Manufacturer <- substr(tmp,
                             ManuRegex[1]+attr(ManuRegex, "match.length"),
                             ExpireRegex[1]-2)
      Expire <- substr(tmp,
                       ExpireRegex[1]+attr(ExpireRegex, "match.length"),
                       nchar(tmp))
    }
  }

  returnlist <- list(IDNumber,Harvest,Active,Manufacturer,Expire)
  names(returnlist) <- c('ProductID','HarvestInterval',
                         'ActiveIngredients','Manufacturer',
                         'Expires')
  returnlist
}