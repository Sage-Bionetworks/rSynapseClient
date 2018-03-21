## Unit test fileNameFromHeaders
## 
## 
###############################################################################



unitTestMissingContentDispositionHeader <-function() {
	headers<-"Content-Type: text/html; charset=UTF-8\r\nDate: Sat, 28 Mar 2015 19:43:25 GMT\r\nExpires: Mon, 27 Apr 2015 19:43:25 GMT"
	checkTrue(is.null(synapseClient:::fileNameFromHeaders(headers)))
}

unitTestWrongHeaderName <- function() {
	headers<-"x-amz-id-2: +iB0K6cFV3I1eUO7YIGJlF3Wn7GVxLKZEPsO1dbo5DYzwwo0lBVBlCqKV4W8S4w5\r\nx-amz-request-id: A2EA02790F1D642A\r\nDate: Thu, 26 Mar 2015 17:11:54 GMT\r\nNOT-Content-Disposition: attachment; filename=file7697fb85a1e.rbin\r\nLast-Modified: Tue, 13 Jan 2015 02:03:02 GMT\r\n"
	checkTrue(is.null(synapseClient:::fileNameFromHeaders(headers)))
}

unitTestLegalContentDisposition <- function() {
	headerListTemplate<-"x-amz-id-2: +iB0K6cFV3I1eUO7YIGJlF3Wn7GVxLKZEPsO1dbo5DYzwwo0lBVBlCqKV4W8S4w5\r\nx-amz-request-id: A2EA02790F1D642A\r\nDate: Thu, 26 Mar 2015 17:11:54 GMT\r\nContent-Disposition: %s\r\nLast-Modified: Tue, 13 Jan 2015 02:03:02 GMT\r\n"

	# simple case
	headers<-sprintf(headerListTemplate, "attachment; filename=file7697fb85a1e.rbin")
	checkEquals(synapseClient:::fileNameFromHeaders(headers), "file7697fb85a1e.rbin")
	
	# no 'attachment'
	headers<-sprintf(headerListTemplate, "filename=file7697fb85a1e.rbin")
	checkEquals(synapseClient:::fileNameFromHeaders(headers), "file7697fb85a1e.rbin")
	
	# quoted file name - double quotes
	headers<-sprintf(headerListTemplate, "attachment; filename=\"file7697fb85a1e.rbin\"")
	checkEquals(synapseClient:::fileNameFromHeaders(headers), "file7697fb85a1e.rbin")
	
	# quoted file name - single quotes
	headers<-sprintf(headerListTemplate, "attachment; filename='file7697fb85a1e.rbin'")
	checkEquals(synapseClient:::fileNameFromHeaders(headers), "file7697fb85a1e.rbin")
	
	# embedded comma / quoted file name
	headers<-sprintf(headerListTemplate, "attachment; filename=\"foo,bar.txt\"")
	checkEquals(synapseClient:::fileNameFromHeaders(headers), "foo,bar.txt")
	
	# file name is filename
	headers<-sprintf(headerListTemplate, "attachment; filename=filename.txt")
	checkEquals(synapseClient:::fileNameFromHeaders(headers), "filename.txt")
}



