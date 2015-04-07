# unit tests for supporting functions in Table.R
# 
# Author: brucehoff
###############################################################################

unitTest_parseRowAndVersion<-function() {
  checkEquals(synapseClient:::parseRowAndVersion(c("1_2", "2_3", "3_2")), rbind(c(1,2,3), c(2,3,2)))
  checkException(synapseClient:::parseRowAndVersion(c("1_2", "2_3", "3")))
  checkException(synapseClient:::parseRowAndVersion(c("1_2", "2_3", "3_x")))
  checkException(synapseClient:::parseRowAndVersion(c("1_2", "2_3", "3_3.5")))
}

unitTest_findSynIdInSql<-function() {
  checkEquals("syn123", synapseClient:::findSynIdInSql("select * from syn123"))
  checkEquals("syn123", synapseClient:::findSynIdInSql("select * from syn123 where foo=bar"))
  checkEquals("syn123", synapseClient:::findSynIdInSql("select * FrOm SyN123 where foo=bar"))
  checkEquals("syn123", synapseClient:::findSynIdInSql("select * from\tsyn123\twhere foo=bar"))
}

# checks values and column labels, but not row labels
dataFramesAreSame<-function(df1, df2) {
  if (nrow(df1)!=nrow(df2) || ncol(df1)!=ncol(df2)) return(FALSE)
  if (any(names(df1)!=names(df2))) return(FALSE)
  if (nrow(df1)==0 || ncol(df1)==0) return(TRUE)
  for (c in 1:ncol(df1)) {
	  if ((is.numeric(df1[[c]]) && is.numeric(df2[[c]])) ||
			(is(df1[[c]], "POSIXct") && is(df2[[c]], "POSIXct"))) {
		  if (!all.equal(df1[[c]], df2[[c]])) return (FALSE)
	  } else {
		  for (r in 1:nrow(df1)) {
			  if (!identical(df1[r,c], df2[r,c])) return(FALSE)
		  }
	  }
  }
  TRUE
}

unitTest_csvRoundTrip<-function() {
  dataFrame <- data.frame(sweet=c(1:5, 1.234e-10, 5.678e+10, NA), sweet2=c(NA, 6:10, 1.234567, 9.876543))
  filePath<-tempfile()
  synapseClient:::writeDataFrameToCSV(dataFrame, filePath)
  readBackIn<-synapseClient:::readDataFrameFromCSV(filePath)
  checkTrue(dataFramesAreSame(dataFrame,readBackIn))
}

unitTest_csvRoundTripWithDates<-function() {
	dataFrame <- data.frame(col1=(1:5), col2=Sys.time()+(1:5))
	filePath<-tempfile()
	synapseClient:::writeDataFrameToCSV(dataFrame, filePath)
	readBackIn<-synapseClient:::readDataFrameFromCSV(filePath)
	headers<-synapseClient:::TableColumnList(TableColumn(name="col1", columnType="INTEGER"), 
			TableColumn(name="col2", columnType="DATE"))
	converted<-synapseClient:::convertDataFrameTypeToSchemaType(readBackIn, headers)
	checkTrue(dataFramesAreSame(dataFrame,converted))
}
