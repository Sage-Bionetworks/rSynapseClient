# parses response headers from RCurl package
# The value has the form
# HTTP/1.1 <http-status-number> <http-status-string>
# <header name>: <header value>
# <header name>: <header value>
# 
# Author: brucehoff
###############################################################################

statusLines<-function(headers) {
	grep("^HTTP/1.(0|1) \\d\\d\\d ", headers)
}

# return the index of the row in 'headers' which is the last row 
# meeting the following criteria: (1) it is a blank row, (2) it
# is followed by a status line
findLastResponseBreak<-function(headers) {
	matchingIndices<-intersect(which(headers==""), statusLines(headers)-1)
	if (length(matchingIndices)==0) {
		integer(0)
	} else {
		max(matchingIndices)
	}
}

parseHttpHeaders<-function(r) {
  parsedResult<-strsplit(r, "\r\n", fixed=TRUE)[[1]]
  # sometimes the response has two extra lines:  "100 Continue", followed by a blank line
  # I believe this is RCurl returning an intermediate response concatenated with the final response
  # Update:  Sometimes the first of these two lines is "HTTP/1.0 200 Connection established"
	# A corporate proxy will add additional lines following the 'Connection established' which
	# we must skip
	lastResponseBreak<-findLastResponseBreak(parsedResult)
	if (length(lastResponseBreak)>0) {
		parsedResult<-parsedResult[(lastResponseBreak+1):length(parsedResult)]
	}
	
  firstRow<-strsplit(parsedResult[1], " ")[[1]]
  if (substr(firstRow[1],1,4)!="HTTP") stop(sprintf("Expected 'HTTP' but found %s", substr(firstRow[1],1,4)))
  # in practice we find the response code found here is 100 when the curl handle has the expected 200 value
  responseStatusCode<-as.integer(firstRow[2])
  responseStatusString<-paste(firstRow[3:length(firstRow)], collapse=" ")
  # create list of headers
  headers<-list()
  if (length(parsedResult)>1) {
    for (j in 2:length(parsedResult)) {
      headerRow<-parsedResult[j]
	  	if (nchar(headerRow)>0) {
		 	 i <- regexpr(":", headerRow, fixed=T)
		 	 if (length(i)<1 | i<0) stop(sprintf("Unexpected format for header %s", headerRow))
		  	headers[[substr(headerRow, 1, i-1)]]<-trimWhitespace(substr(headerRow, i+1, nchar(headerRow)))
	  	}
    }
  }
  # return list of response code, response string, headers
  list(statusCode=responseStatusCode, statusString=responseStatusString, headers=headers)
}

# from http://r.789695.n4.nabble.com/Remove-leading-and-trailing-white-spaces-td907851.html
trimWhitespace<-function(x) sub("^[[:space:]]*(.*?)[[:space:]]*$", "\\1", x, perl=TRUE) 

