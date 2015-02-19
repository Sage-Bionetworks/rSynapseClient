# 
# 
# Author: brucehoff
###############################################################################
library(RJSONIO)


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
  checkEquals(list("syn123", data), synapseClient:::createUsedEntitiesList(args))
}

unitTestScrubEntityName<-function() {
  checkEquals("++++++++++()_-+++++++++++++++.+abc 123+ABC", synapseClient:::scrubEntityName("~`!@#$%^&*()_-+={}[]|\\;:\"'<,>./abc 123'ABC", replaceChar="+"))
  checkEquals("..........()_-+................abc 123.ABC", synapseClient:::scrubEntityName("~`!@#$%^&*()_-+={}[]|\\;:\"'<,>./abc 123'ABC"))
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

unitTestConvertAnnotation<-function() {
  checkTrue(synapseClient:::isScalar(1))
  checkTrue(synapseClient:::isScalar("abc"))
  checkTrue(!synapseClient:::isScalar(list(1)))
  checkTrue(!synapseClient:::isScalar(list("abc")))
  checkTrue(!synapseClient:::isScalar(c(a=1, b=2)))
  checkTrue(!synapseClient:::isScalar(list(a=1, b=2)))
  
  checkEquals(1, synapseClient:::convertAnnotation(1))
  checkEquals("abc", synapseClient:::convertAnnotation("abc"))
  checkEquals("abc", synapseClient:::convertAnnotation("abc"))
  map<-c(a=1, b=2)
  checkEquals(toJSON(map), synapseClient:::convertAnnotation(map))
  map<-list(a=1, b=2)
  checkEquals(toJSON(map), synapseClient:::convertAnnotation(map))
  list<-list(1,2)
  checkEquals(list, synapseClient:::convertAnnotation(list))
  checkEquals(list, synapseClient:::convertAnnotation(c(1,2)))
  list<-list(1,map)
  converted<-list(1, toJSON(map))
  checkEquals(converted, synapseClient:::convertAnnotation(list))
  sublist<-c(1,2)
  list<-list("ab", "cd", sublist)
  converted<-list("ab", "cd", toJSON(sublist))
  checkEquals(converted, synapseClient:::convertAnnotation(list))
}

