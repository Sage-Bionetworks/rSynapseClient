#
# 
# Author: furia
###############################################################################

# disabled, per SYNR-467
#.setUp <-
#  function()
#{
#  synapseClient:::.setCache('testProject', createEntity(Project()))
#}
#
#.tearDown <-
#  function()
#{
#  deleteEntity(synapseClient:::.getCache("testProject"))
#  synapseClient:::.deleteCache("testProject")
#}
#
#integrationTestBinFiles <-
#  function()
#{
#  file <- tempfile()
#  cat(sprintf("THIS IS A TEST %s", Sys.time()), file = file)
#  
#  for(cc in c("Data", "Study", "ExpressionData", "RObject", "PhenotypeData", "GenotypeData")){
#    pp <- synapseClient:::.getCache("testProject")
#    ee <-  do.call(cc, args=list(entity=list(parentId=propertyValue(pp, "id"))))
#    addObject(ee, "foo", "bar")
#    addFile(ee, file)
#    checkEquals(length(ee$files), 1L)
#    copy <- storeEntity(ee)
#    checkEquals(length(ee$files), 1L)
#    checkEquals(length(copy$files), 1L)
#    copy <- loadEntity(propertyValue(copy, "id"))
#    checkEquals(length(copy$files), 1L)
#    
#    ## add an annotation value
#    annotValue(ee, "testAnnotName") <- "testAnnotValue"
#    ee <- storeEntity(ee)
#    checkEquals(length(ee$files), 1L)
#    checkEquals(annotValue(ee, "testAnnotName"), "testAnnotValue")
#    
#    copy <- loadEntity(propertyValue(ee, "id"))
#    checkEquals(length(copy$files), 1L)
#    checkEquals(annotValue(copy, "testAnnotName"), "testAnnotValue")
#  }
#}