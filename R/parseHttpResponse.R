# parses value returned by getURL in RCurl package
# The value has the form
# HTTP/1.1 <http-status-number> <http-status-string>
# <header name>: <header value>
# <header name>: <header value>
# ...
# <blank line>
# <resonse body>
# 
# Author: brucehoff
###############################################################################

parseHttpResponse<-function(r) {
  parsedResult<-strsplit(r, "\r\n", fixed=TRUE)[[1]]
  # sometimes the response has two extra lines:  "100 Continue", followed by a blank line
  # I believe this is RCurl returning an intermediate response concatenated with the final response
  while (length(parsedResult)>2 && parsedResult[1]=="HTTP/1.1 100 Continue" && parsedResult[2]=="") {
    parsedResult<-parsedResult[3:length(parsedResult)]
	}
  firstRow<-strsplit(parsedResult[1], " ")[[1]]
  if (substr(firstRow[1],1,4)!="HTTP") stop(sprintf("Expected 'HTTP' but found %s", substr(firstRow[1],1,4)))
  # in practice we find the response code found here is 100 when the curl handle has the expected 200 value
  responseStatusCode<-as.integer(firstRow[2])
  responseStatusString<-paste(firstRow[3:length(firstRow)], collapse=" ")
  zeroLengthElement<-.zeroLengthElement(parsedResult)
  headerCount<-zeroLengthElement-2
  if (headerCount<0) stop(sprintf("unexpected http response: %s", r))
  # create list of headers
  headers<-list()
  if (headerCount>0) {
    for (j in 2:(headerCount+1)) {
      headerRow<-parsedResult[j]
      i <- regexpr(": ", headerRow, fixed=T)
      if (i<0) stop(sprintf("Unexpected format for header %s", headerRow))
      headers[[substr(headerRow, 1, i-1)]]<-substr(headerRow, i+2, nchar(headerRow))
    }
  }
  if (zeroLengthElement<length(parsedResult)) {
    responseBody<-paste(parsedResult[(zeroLengthElement+1):length(parsedResult)], collapse="\r\n")
  } else {
    responseBody<-""
  }
  # return list of response code, response string, headers, response body
  return(list(statusCode=responseStatusCode, statusString=responseStatusString, headers=headers, body=responseBody))
}

# returns the zero length character string from an array of character strings
# stops if no zero length character string is found
.zeroLengthElement<-function(stringArray) {
  for (i in 1:length(stringArray)) {
    if (nchar(stringArray[i])==0) return (i)
  }
  stop("zero length element not found")
}
