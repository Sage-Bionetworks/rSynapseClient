# method for parsing JSON returned by Synapse
# 
# Author: bhoff
###############################################################################


synFromJson<-function(content) {
	tryCatch(
			fromJSON(content, method="R", unexpected.escape="skip"),
			error=function(e) {
				if (length(grep("unrecognized escape", e))>0) {
					cleanedContent<-gsub("\\\\/", content, fixed=TRUE)
					fromJSON(cleanedContent, method="R", unexpected.escape="skip")
				} else {
					stop(e)
				}
			}
	)
}
