# create a list version of an S4 object, suitable for serialization to JSON.
# 
# Author: brucehoff
###############################################################################

createListFromS4Object<-function(obj) {
  if (is.null(obj) || is(obj, "NullS4Object")) return(NULL)
  if (isPrimitiveType(class(obj))) {
    if (length(obj)==0) return(NULL)
    return(obj)
  }
  result <-list()
  if (is(obj, "TypedList")) {
    if (length(obj)>0) {
      for (i in 1:length(obj)) {
        result[[1+length(result)]]<-createListFromS4Object(obj[[i]])
      }
    }
    return(result)
  }
  
  # at this point we know obj is an S4 class
  for (slotName in slotNames(obj)) {
    value<-slot(obj, slotName)
    result[[slotName]]<-createListFromS4Object(value)
  }
  
  # An object with no field values becomes an empty list.
  # To keep RJSONIO from erroneously encoding as an empty _JSON_ list ("[]")
  # we replace it with NULL
  if (length(result)==0) {
    NULL
  } else {
    result
  }
}
