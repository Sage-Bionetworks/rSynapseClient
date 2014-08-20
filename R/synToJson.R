# Convert R object to JSON, taking care of edge cases to ensure correctness
# 
# Author: brucehoff
###############################################################################


synToJson<-function(toEncode) {
  if(!is.list(toEncode)) {
    stop("input must be supplied of R type list")
  }
  
  if((any(names(toEncode) == "") || is.null(names(toEncode))) && length(toEncode) > 0){
    stop("all elements must be named")
  }
  
  if(length(toEncode) == 0)
    toEncode <- emptyNamedList
  
  ## change dates to characters
  toEncode<-convertDatesToCharacters(toEncode)
  toEncode<-convertIntegersToCharacters(toEncode)
  toJSON(toEncode)
}

# warning:  This is designed specifically for date annotations objects yet is embedded in a low level library
convertDatesToCharacters<-function(toEncode) {
  indx <- grep("date", tolower(names(toEncode)))
  indx <- indx[as.character(toEncode[indx]) != "NULL"]
  indx <- indx[names(toEncode)[indx] != "dateAnnotations"]
  for(ii in indx)
    toEncode[ii] <- as.character(toEncode[ii])
  
  toEncode
}

## convert integers to characters
convertIntegersToCharacters<-function(toEncode) {
  if(length(toEncode) > 0) {
    for(ii in 1:length(toEncode)) {
      elem<-toEncode[[ii]]
      if (length(elem)>1) {
        toEncode[[ii]]<-convertIntegersToCharacters(elem)
      } else {
        if(all(synapseClient:::checkInteger(elem))) {
          as_char <- as.character(as.integer(elem))
          toEncode[[ii]] <-as_char
        }
      }
    }
  }
  toEncode
}
