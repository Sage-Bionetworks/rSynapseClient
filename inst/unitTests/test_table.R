# unit tests for supporting functions in Table.R
# 
# Author: brucehoff
###############################################################################

unitTest_parseRowAndVersion<-function() {
  checkEquals(synapseClient:::parseRowAndVersion(c("1-2", "2-3", "3-2")), rbind(c(1,2,3), c(2,3,2)))
  checkException(synapseClient:::parseRowAndVersion(c("1-2", "2-3", "3")))
  checkException(synapseClient:::parseRowAndVersion(c("1-2", "2-3", "3-x")))
  checkException(synapseClient:::parseRowAndVersion(c("1-2", "2-3", "3-3.5")))
}

unitTest_findSynIdInSql<-function() {
  checkEquals("syn123", synapseClient:::findSynIdInSql("select * from syn123"))
  checkEquals("syn123", synapseClient:::findSynIdInSql("select * from syn123 where foo=bar"))
  checkEquals("syn123", synapseClient:::findSynIdInSql("select * FrOm SyN123 where foo=bar"))
  checkEquals("syn123", synapseClient:::findSynIdInSql("select * from\tsyn123\twhere foo=bar"))
}

unitTest_isAggregationQuery<-function() {
  checkTrue(!synapseClient:::isAggregationQuery("select * from syn123"))
  checkTrue(synapseClient:::isAggregationQuery("select count(*) from syn123"))
  checkTrue(synapseClient:::isAggregationQuery("select\t\tcount(*) from syn123"))
  checkTrue(synapseClient:::isAggregationQuery("select\nsum(*) from syn123"))
  checkTrue(synapseClient:::isAggregationQuery("select\nAVG (*) from syn123"))
  checkTrue(synapseClient:::isAggregationQuery("select min(*) from syn123"))
  checkTrue(synapseClient:::isAggregationQuery("select MAX(distinct foo) from syn123"))
  checkTrue(!synapseClient:::isAggregationQuery("select * from syn123 where count in (1,2,3)"))
  checkTrue(synapseClient:::isAggregationQuery("select foo, count(foo) from syn123"))
}