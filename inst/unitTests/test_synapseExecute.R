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