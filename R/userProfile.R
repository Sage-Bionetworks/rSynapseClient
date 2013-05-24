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
  populateUserProfile(response, getOrUpdateUri)
}

synUpdateUserProfile<-function(userProfile) {
  listResult<-synRestPUT(userProfile@updateUri, userProfile)
  populateUserProfile(listResult, userProfile@updateUri)
}

populateUserProfile<-function(listResult, getOrUpdateUri) {
  if (!is.null(listResult$pic)) listResult$pic<-as.list(listResult$pic)
  result<-UserProfile(listResult)
  result@updateUri<-getOrUpdateUri
  result
}


