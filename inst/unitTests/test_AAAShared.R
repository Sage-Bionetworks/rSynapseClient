
# 
# Author: brucehoff
###############################################################################



unitTestGetEffectivePropertySchemas<-function() {
  # this schema implements *two* interfaces
  schemaName<-"org.sagebionetworks.repo.model.file.S3FileHandle"
  eps<-synapseClient:::getEffectivePropertySchemas(schemaName, synapseClient:::getSchemaPath())
  checkTrue(any(names(eps)=="previewId"))
  checkTrue(any(names(eps)=="id"))
}

unitTest_ProblemString <- function(){
  require(rjson)
  problemString <- "{\"foo\":\"b\\ar\"}"
  expected <- "b\ar"
  checkEquals(fromJSON(problemString, method = "R")$foo, expected)
  checkException(fromJSON(problemString))
}
