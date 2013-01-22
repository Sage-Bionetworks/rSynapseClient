# Retrieves version information.
# Compares own version to blacklist
# 
# Author: brucehoff
###############################################################################

checkBlackList<-function() {
  # get my own version
  myOwnVersion<-packageDescription("synapseClient", fields="Version")
  serverVersion<-getServerVersion()
  .checkBlackListGivenMyVersion(myOwnVersion, serverVersion)
}

# provided for integration testing
.checkBlackListGivenMyVersion<-function(myOwnVersion, serverVersion) {
  if (is.null(myOwnVersion) || myOwnVersion=="") return
  
  # get the latest version, release notes, black list, and optional user message
  response<-getURLWithRetries(.getVersionsEndpoint(), opts=.getCache("curlOpts"))$response
  versionInfo <- fromJSON(response$body)
  
  if (.versionIsBlackListed(myOwnVersion, serverVersion, versionInfo$blacklist)) {
    # check whether the *latest* version is also blacklisted for the server we're using
    if (.versionIsBlackListed(versionInfo$latestVersion, serverVersion, versionInfo$blacklist)) {
      stop(sprintf("This version of the Synapse Client, %s, has been disabled.  Please contact Synapse support to access an enabled client.\n%s",
          myOwnVersion, versionInfo$message))
    } else {
      stop(sprintf("This version of the Synapse Client, %s, has been disabled.  Please upgrade to the latest version, %s.\n%s",
          myOwnVersion, versionInfo$latestVersion, versionInfo$message))
    }
  }
}

.versionIsBlackListed<-function(clientVersion, serverVersion, blackList) {
  for (entry in blackList) {
    if (entry[["server"]]=="*" || entry[["server"]]==serverVersion) {
      if (.versionMeetsConstraint(clientVersion, entry[["client"]])) {
        return(TRUE)
      }
    }
  }
  FALSE
}

  # given a version and a version constraint (e.g. "0.10-1", "<1.5", ">=0.09")
# does the version meet the constraint?
# Note: use compareVersion from utils package to do the comparison
  .versionMeetsConstraint<-function(version, constraint) {
    if (substring(constraint, 1, 1)=="<") {
      if (substring(constraint, 1, 2)=="<=") {
        # <=
        constraintVersion <- substring(constraint, 3)
        comparison <- compareVersion(version, constraintVersion)
        comparison==-1 || comparison==0
      } else {
        # <
        constraintVersion <- substring(constraint, 2)
        comparison <- compareVersion(version, constraintVersion)
        comparison==-1
      }
    } else if (substring(constraint, 1, 1)==">") {
      if (substring(constraint, 1, 2)==">=") {
        # >=
        constraintVersion <- substring(constraint, 3)
        comparison <- compareVersion(version, constraintVersion)
        comparison==1 || comparison==0
      } else {
        # >
        constraintVersion <- substring(constraint, 2)
        comparison <- compareVersion(version, constraintVersion)
        comparison==1
      }
    } else {
      # == (implicitly)
      constraintVersion <- constraint
      comparison <- compareVersion(version, constraintVersion)
      comparison==0
    }
  }
  
  
  