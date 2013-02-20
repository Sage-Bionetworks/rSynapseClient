# 
# 
# Author: brucehoff
###############################################################################



unitTestIsSynapseId <- function() {
  checkTrue(synapseClient:::isSynapseId("syn123456"))
  checkFalse(synapseClient:::isSynapseId("123456"))
  checkFalse(synapseClient:::isSynapseId("SYN123456"))
  checkFalse(synapseClient:::isSynapseId("syn 123456"))
  checkFalse(synapseClient:::isSynapseId(123445))
  checkFalse(synapseClient:::isSynapseId(c("syn123456", "syn654321")))
  checkFalse(synapseClient:::isSynapseId("syn1234X56"))
}

unitTestScrubEntityName<-function() {
  checkEquals("++++++++++()_-+++++++++++++++.+abc 123+ABC", synapseClient:::scrubEntityName("~`!@#$%^&*()_-+={}[]|\\;:\"'<,>./abc 123'ABC"))
}

unitTestsplitByLines<-function() {
  checkEquals(synapseClient:::.splitByLines("foo\r\nbar\r\nbas\r\n\r\nfoo2\n\rfoo3"), c("foo","bar","bas","","foo2","", "foo3"))
# alternative is: checkTrue(identical(.splitByLines("foo\r\nbar\r\nbas\r\n\r\nfoo2\n\rfoo3"), c("foo","bar","bas","","foo2","", "foo3")))
}

unitTestIndent<-function() {
  checkEquals("\tfoo\r\n\tbar\r\n\tbas\r\n\t\r\n\tfoo2\r\n\t\r\n\tfoo3", synapseClient:::.indent("foo\r\nbar\r\nbas\r\n\r\nfoo2\n\rfoo3"))
}