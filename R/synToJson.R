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
  
  if(length(toEncode) == 0) {
    emptyNamedList<-structure(list(), names = character()) # copied from RJSONIO
    toEncode <- emptyNamedList
  }
  
  ## change dates to characters
  toEncode<-handleNAs(toEncode)
  toEncode<-convertDatesToCharacters(toEncode)
  toEncode<-convertIntegersToCharacters(toEncode)
  toJSON(toEncode, method="C") # if method="R", will generate spurious \\/ sequences
}

## Note: the RJSONIO library converted NAs to nulls
## Switching to rjson, for backwards compatibility we must do the same
handleNAs<-function(toEncode) {
  result<-list()
  if (length(toEncode) > 0) {
    for (ii in 1:length(toEncode)) {
      elemIndex<-names(toEncode)[[ii]]
      if (is.null(elemIndex) || nchar(elemIndex)==0) elemIndex<-ii
      elem<-toEncode[[elemIndex]]
      modifiedElem<-elem
      if (length(elem)>1) {
        modifiedElem<-handleNAs(elem)
      } else if (length(elem)==1 && is.na(elem)) {
          modifiedElem<-NULL
      }
      result[[elemIndex]]<-modifiedElem
    }
  }
  result
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
        if(all(checkInteger(elem))) {
          as_char <- as.character(as.integer(elem))
          toEncode[[ii]] <-as_char
        }
      }
    }
  }
  toEncode
}


