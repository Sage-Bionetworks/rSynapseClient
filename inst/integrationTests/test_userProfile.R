# Integration test for user profile
# 
# Author: brucehoff
###############################################################################


integrationTestUserProfile<-function() {
  profile<-synGetUserProfile()
  origSummary<-propertyValue(profile, "summary")
  
  propertyValue(profile, "summary")<-"test summary text"
  profile2<-synStore(profile)
  checkEquals(profile2, profile)
  profile3<-synGetUserProfile()
  checkEquals(profile3, profile)
  
  profile4<-synGetUserProfile(propertyValue(profile, "ownerId"))
  checkEquals(profile4, profile)
  
  # restore to original settings
  propertyValue(profile, "summary")<-origSummary
  synStore(profile)
}
