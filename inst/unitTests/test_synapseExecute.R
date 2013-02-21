# 
# 
# Author: brucehoff
###############################################################################



unitTestIsSynapseId <- function() {
  checkTrue(synapseClient:::isSynapseId("syn123456"))
  checkTrue(!synapseClient:::isSynapseId("123456"))
  checkTrue(!synapseClient:::isSynapseId("SYN123456"))
  checkTrue(!synapseClient:::isSynapseId("syn 123456"))
  checkTrue(!synapseClient:::isSynapseId(123445))
  checkTrue(!synapseClient:::isSynapseId(c("syn123456", "syn654321")))
  checkTrue(!synapseClient:::isSynapseId("syn1234X56"))
}

unitTestCreateUsedEntitiesList <- function() {
  data<-Data()
  propertyValue(data, "id")<-"syn987"
  args<-list(c("123", "456"), list("syn123", "SYN 456"), data, 1, "my dog has fleas")
  checkEquals(list(list(entity="syn123", wasExecuted=F), list(entity="syn987", wasExecuted=F)), synapseClient:::createUsedEntitiesList(args))
}

unitTestScrubEntityName<-function() {
  checkEquals("++++++++++()_-+++++++++++++++.+abc 123+ABC", synapseClient:::scrubEntityName("~`!@#$%^&*()_-+={}[]|\\;:\"'<,>./abc 123'ABC"), replChar="+")
  checkEquals("..........()_-+...............+abc 123+ABC", synapseClient:::scrubEntityName("~`!@#$%^&*()_-+={}[]|\\;:\"'<,>./abc 123'ABC"))
}

unitTestsplitByLines<-function() {
  checkEquals(synapseClient:::splitByLines("foo\r\nbar\r\nbas\r\n\r\nfoo2\n\rfoo3"), c("foo","bar","bas","","foo2","", "foo3"))
}

unitTestIndent<-function() {
  checkEquals("\tfoo\n\tbar\n\tbas\n\t\n\tfoo2\n\t\n\tfoo3", synapseClient:::indent("foo\r\nbar\r\nbas\r\n\r\nfoo2\n\rfoo3"))
}

unitTestStringMd5<-function() {
  checkEquals("531bde74cd24f4eddc23a5a1b49dfd0f", synapseClient:::stringMd5("my dog has fleas"))
}

unitTestHasRSuffix<-function() {
  checkTrue(synapseClient:::hasRSuffix("function.R"))
  checkTrue(!synapseClient:::hasRSuffix("functionR"))
  checkTrue(!synapseClient:::hasRSuffix("function."))
  checkTrue(!synapseClient:::hasRSuffix("."))
  checkTrue(!synapseClient:::hasRSuffix(""))
}

