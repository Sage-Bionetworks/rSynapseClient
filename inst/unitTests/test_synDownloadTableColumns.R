# unit tests for synDownloadTableColumns.R
# 
# Author: brucehoff
###############################################################################

.setUp <- function() {

}

.tearDown <- function() {
	synapseClient:::.unmockAll()
}


unitTest_MaximumBulkDownloadRequest<-function() {
	fhaList<-list()
	for (i in 1:100) {
		fhaList<-append(fhaList, synapseClient:::FileHandleAssociation(fileHandleId="123456", 
				associateObjectId="987654", associateObjectType="TableEntity"))
	}
	legalRequest<-synapseClient:::getMaxmimumBulkDownloadRequest(fhaList, 2000)
	# we did indeed have to subselect:
	checkTrue(length(legalRequest$requestedFiles)<100)
	# we got _something_ back
	checkTrue(length(legalRequest$requestedFiles)>1)
	# it's not too big
	checkTrue(nchar(synapseClient:::synToJson(legalRequest))<2000)
	# it's a valid request
	synapseClient:::createS4ObjectFromList(legalRequest, "BulkFileDownloadRequest")
}

unitTest_synDownloadTableColumnsHappyPath<-function() {
	fileHandleIds<-as.character(sample(1000000, 3))
	# make sure there is no cached information
	for (fileHandleId in fileHandleIds) {
		unlink(synapseClient:::defaultDownloadLocation(fileHandleId))
	}
	df<-data.frame(string=c("a", "b", "c"), files=fileHandleIds, stringsAsFactors=FALSE)
	table<-Table("syn123", df)
	expectedDownloadResult<-c()
	synapseClient:::.mock("downloadTableFileHandles",
			function(fhasToDownload) {
				successes<-list()
				fhIds<-c()
				for (fha in fhasToDownload) {
					fhId<-fha@fileHandleId
					filePath<- tempfile(tmpdir=synapseClient:::defaultDownloadLocation(fhId))
					expectedDownloadResult<-c(expectedDownloadResult, filePath)
					connection<-file(filePath)
					writeChar("this is a test", connection, eos=NULL)
					close(connection)  
					synapseClient:::addToCacheMap(fhId, filePath)
					fhIds<-c(fhIds, fhId)
					successes<-append(successes, filePath)
				}
				names(successes)<-fhIds
				list() # return the permanent failures (NONE)
			}
	)
	downloadResult<-synDownloadTableColumns(table, "files")
	
	names(expectedDownloadResult)<-fileHandleIds
	checkEquals(downloadResult, expectedDownloadResult)
}

unitTest_synDownloadTableColumnsIllegalFile<-function() {
	fileHandleIds<-as.character(sample(1000000, 3))
	illegalFHID<-fileHandleIds[length(fileHandleIds)] # let's say the last one is illegal (invalid or forbidden)
	# make sure there is no cached information
	for (fileHandleId in fileHandleIds) {
		unlink(synapseClient:::defaultDownloadLocation(fileHandleId))
	}
	df<-data.frame(string=c("a", "b", "c"), files=fileHandleIds, stringsAsFactors=FALSE)
	table<-Table("syn123", df)
	expectedDownloadResult<-c()
	synapseClient:::.mock("downloadTableFileHandles",
			function(fhasToDownload) {
				successes<-list()
				fhIds<-c()
				permanentFailures<-list() 
				for (fha in fhasToDownload) {
					fhId<-fha@fileHandleId
					if (fhId==illegalFHID) {
						permanentFailures[[fhId]]<-"NOT FOUND"
					} else {
						filePath<- tempfile(tmpdir=synapseClient:::defaultDownloadLocation(fhId))
						expectedDownloadResult<-c(expectedDownloadResult, filePath)
						connection<-file(filePath)
						writeChar("this is a test", connection, eos=NULL)
						close(connection)  
						synapseClient:::addToCacheMap(fhId, filePath)
						fhIds<-c(fhIds, fhId)
					}
				}
				permanentFailures
			}
	)
	downloadResult<-synDownloadTableColumns(table, "files")
	
	checkEquals(2, length(expectedDownloadResult)) # we only get two of three
	names(expectedDownloadResult)<-fileHandleIds
	checkEquals(downloadResult, expectedDownloadResult)

}

