# parses response headers from RCurl package
# The value has the form
# HTTP/1.1 <http-status-number> <http-status-string>
# <header name>: <header value>
# <header name>: <header value>
# 
# Author: brucehoff
###############################################################################

parseHttpHeaders<-function(r) {
  parsedResult<-strsplit(r, "\r\n", fixed=TRUE)[[1]]
  # sometimes the response has two extra lines:  "100 Continue", followed by a blank line
  # I believe this is RCurl returning an intermediate response concatenated with the final response
  # Update:  Sometimes the first of these two lines is "HTTP/1.0 200 Connection established"
  while (length(parsedResult)>2 && 
    (parsedResult[1]=="HTTP/1.1 100 Continue" || parsedResult[1]=="HTTP/1.0 200 Connection established")
    && parsedResult[2]=="") {
    parsedResult<-parsedResult[3:length(parsedResult)]
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
		  if (i<0) stop(sprintf("Unexpected format for header %s", headerRow))
		  headers[[substr(headerRow, 1, i-1)]]<-trimWhitespace(substr(headerRow, i+1, nchar(headerRow)))
	  }
    }
  }
  # return list of response code, response string, headers
  list(statusCode=responseStatusCode, statusString=responseStatusString, headers=headers)
}

# from http://r.789695.n4.nabble.com/Remove-leading-and-trailing-white-spaces-td907851.html
trimWhitespace<-function(x) sub("^[[:space:]]*(.*?)[[:space:]]*$", "\\1", x, perl=TRUE) 

