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
  result<-UserProfile(synRestGET(getOrUpdateUri))
  result@updateUri<-getOrUpdateUri
  result
}


