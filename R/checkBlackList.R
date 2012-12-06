# Retrieves version information.
# Compares own version to blacklist
# 
# Author: brucehoff
###############################################################################

checkBlackList<-function() {
  # get my own version
  myOwnVersion<-packageDescription("synapseClient", fields="Version")
  .checkBlackListGivenMyVersion(myOwnVersion)
}

# provided for integration testing
.checkBlackListGivenMyVersion<-function(myOwnVersion) {
  if (is.null(myOwnVersion) || myOwnVersion=="") return
  
  # get the latest version, release notes, black list, and optional user message
  versionInfo <- fromJSON(getURL(.getVersionsEndpoint()))
  
  if (any(versionInfo$blacklist==myOwnVersion)) {
    stop(sprintf("This version of the Synapse Client, %s, has been disabled.  Please upgrade to the latest version, %s.\n%s",
        myOwnVersion, versionInfo$latestVersion, versionInfo$message))
  }
}

