# synGetUserProfile
# 
# Author: brucehoff
###############################################################################


synGetUserProfile<-function(id) {
  if (missing(id)) {
    getOrUpdateUri<-"/userProfile"
  } else {
    getOrUpdateUri<-sprintf("/userProfile/%s", id)
  }
  response<-synRestGET(getOrUpdateUri)  
  objectResult<-createS4ObjectFromList("UserProfile", NULL, response)
  objectResult@updateUri<-getOrUpdateUri
  objectResult
}

synUpdateUserProfile<-function(userProfile) {
  synUpdate(userProfile)
}




