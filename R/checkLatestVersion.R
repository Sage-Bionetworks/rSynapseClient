# Retrieves version information.
# Compares own version to latest
# 
# Author: brucehoff
###############################################################################

checkLatestVersion<-function() {
  # get my own version
  myOwnVersion<-packageDescription("synapseClient", fields="Version")
  .checkLatestVersionGivenMyVersion(myOwnVersion)
}

# for testing purposes returns the printed message
.checkLatestVersionGivenMyVersion<-function(myOwnVersion) {
  if (is.null(myOwnVersion) || myOwnVersion=="") return("")
  
  # get the latest version, release notes, black list, and optional user message
  versionInfo <- fromJSON(getURL(.getVersionsEndpoint()))
  
  if (myOwnVersion!=versionInfo$latestVersion) {
    message = sprintf("Please upgrade to the latest version of the Synapse Client, %s, having the following changes/features:\n%s\n\n%s\n",
      versionInfo$latestVersion, versionInfo$releaseNotes, versionInfo$message)
  } else {
    message = sprintf("\n\n%s\n", versionInfo$message)
  }
  cat(message)
  message
}


