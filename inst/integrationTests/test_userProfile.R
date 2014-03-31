# Integration test for user profile
# 
# Author: brucehoff
###############################################################################


integrationTestUserProfile<-function() {
  profile<-synGetUserProfile()
  origSummary<-propertyValue(profile, "summary")
  
  # will be restored under SYNR-671
  #propertyValue(profile, "summary")<-"test summary text"
  # profile2<-synStore(profile)
  # propertyValue(profile, "etag")<-propertyValue(profile2, "etag")
  # checkEquals(properties(profile2), properties(profile))
  # checkEquals(profile2@updateUri, profile@updateUri)
  
  profile3<-synGetUserProfile()
  checkEquals(properties(profile3), properties(profile))
  checkEquals(profile3@updateUri, profile@updateUri)
  
  profile4<-synGetUserProfile(propertyValue(profile, "ownerId"))
  checkEquals(properties(profile4), properties(profile))
  
  # restore to original settings
  propertyValue(profile, "summary")<-origSummary
  # synStore(profile) will be restored under SYNR-671
}
