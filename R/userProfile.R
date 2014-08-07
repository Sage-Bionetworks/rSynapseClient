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
  objectResult<-createS4ObjectFromList(response, "UserProfile")
  objectResult
}

userProfileUpdateAndDeleteUri<-function(obj) {
  sprintf("/userProfile",obj$ownerId)
}

setMethod(
  f = "synStore",
  signature = "UserProfile",
  definition = function(entity) {
    # note, user can't create a UserProfile, only update one
    updateS4Object(entity, userProfileUpdateAndDeleteUri(entity))
  }
)

synUpdateUserProfile<-function(userProfile) {
  updateS4Object(userProfile, userProfileUpdateAndDeleteUri(userProfile))
}




