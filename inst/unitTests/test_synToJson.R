# unit tests for synToJson.R
# 
# Author: brucehoff
###############################################################################

unitTest_parseRowAndVersion<-function() {
	orig<-list(foo="bar", baz=NA, bar=list(a="A",b=NA))
	expected<-list(foo="bar", bar=list(a="A"))
	checkEquals(synapseClient:::synToJson(orig), toJSON(expected))
}

