# Server side logging of exceptions encountered in the client
# 
# Author: brucehoff
#
# 
###############################################################################

logErrorToSynapse<-function(label, message) {
  logEntry<-LogEntry(label=label, message=message)
  synapsePost("/log",
    createListFromS4Object(logEntry), 
    anonymous = TRUE, 
    checkHttpStatus=FALSE,
	logErrorsToSynapse=FALSE # if an error occurs while logging, don't recursively generate the same error
  )
}
