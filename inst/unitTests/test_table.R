# unit tests for supporting functions in Table.R
# 
# Author: brucehoff
###############################################################################

test_parseRowAndVersion-function() {
  checkEquals(synapseClient:::parseRowAndVersion(c("1-2", "2-3", "3-2")), rbind(c(1,2,3), c(2,3,2)))
  checkException(synapseClient:::parseRowAndVersion(c("1-2", "2-3", "3")))
  checkException(synapseClient:::parseRowAndVersion(c("1-2", "2-3", "3-x")))
  checkException(synapseClient:::parseRowAndVersion(c("1-2", "2-3", "3-3.5")))
}