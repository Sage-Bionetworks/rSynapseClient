# Retrieves version information.
# Compares own version to blacklist
# 
# Author: brucehoff
###############################################################################

checkBlackList<-function(logErrorsToSynapse=TRUE) {
  # get my own version
  myOwnVersion<-packageDescription("synapseClient", fields="Version")
  serverVersion<-getServerVersion(logErrorsToSynapse)
  .checkBlackListGivenMyVersion(myOwnVersion, serverVersion, logErrorsToSynapse)
}


# provided for integration testing
.checkBlackListGivenMyVersion<-function(myOwnVersion, serverVersion, logErrorsToSynapse=TRUE) {
  if (is.null(myOwnVersion) || myOwnVersion=="") return
  
  # get the latest version, release notes, black list, and optional user message
  # a local cache is used to avoid making too many web service calls for this static info
  versionInfo <- getVersionInfo(logErrorsToSynapse)
  
  if (.versionIsBlackListed(myOwnVersion, serverVersion, versionInfo$blacklist)) {
    # check whether the *latest* version is also blacklisted for the server we're using
    if (.versionIsBlackListed(versionInfo$latestVersion, serverVersion, versionInfo$blacklist)) {
      stop(sprintf("This version of the Synapse Client, %s, has been disabled.  To upgrade:\n\tsource('http://depot.sagebase.org/CRAN.R')\n\tpkgInstall('synapseClient')\n%s",
          myOwnVersion, versionInfo$message))
    } else {
      stop(sprintf("This version of the Synapse Client, %s, has been disabled.  Please upgrade to the latest version, %s.\nTo upgrade:\n\tsource('http://depot.sagebase.org/CRAN.R')\n\tpkgInstall('synapseClient')\n%s",
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
  
  
  