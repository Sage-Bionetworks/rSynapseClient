# synFromJson:  translates JSON returned from Synapse web requests into list objects
# 
# Author: bhoff
###############################################################################



synFromJson<-function(content) {
#	fromJSON(content, method="R", unexpected.escape="skip")
	result<-try(fromJSON(content, method="R", unexpected.escape="skip"), silent=TRUE)
	if (class(result)=="try-error") {
		cleanedContent<-gsub("\\/", "/", content, fixed=TRUE)
		fromJSON(cleanedContent, method="R", unexpected.escape="skip")		
	} else {
		result
	}
}

