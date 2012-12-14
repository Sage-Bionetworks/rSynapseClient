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
.checkLatestVersionGivenMyVersion<-function(myOwnVersion, serverVersion) {
  if (is.null(myOwnVersion) || myOwnVersion=="") return("")
  
  # get the latest version, release notes, black list, and optional user message
  versionInfo <- fromJSON(getURL(.getVersionsEndpoint()))
  
  # in the unlikely/transient event that there is a more up-to-date client but that client is *blacklisted* for this server, 
  # then we suppress telling the user to update their client.  They will receive the message once the latest client works with the server
  if (myOwnVersion!=versionInfo$latestVersion && !.versionIsBlackListed(versionInfo$latestVersion, serverVersion, versionInfo$blacklist)) {
    msg <- sprintf("Please upgrade to the latest version of the Synapse Client, %s, having the following changes/features:\n%s\n\n%s\n",
      versionInfo$latestVersion, versionInfo$releaseNotes, versionInfo$message)
  } else {
    msg <- sprintf("\n\n%s\n", versionInfo$message)
  }
  message(msg)
  msg
}


