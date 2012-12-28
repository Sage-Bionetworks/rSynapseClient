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
  firstRow<-strsplit(parsedResult[1], " ")[[1]]
  if (firstRow[1]!="HTTP/1.1") stop(sprintf("Expected 'HTTP/1.1' but found %s", firstRow[1]))
  responseStatusCode<-as.integer(firstRow[2])
  responseStatusString<-paste(firstRow[3:length(firstRow)], collapse=" ")
  zeroLengthElement<-.zeroLengthElement(parsedResult)
  headerCount<-zeroLengthElement-2
  if (headerCount<0) stop(sprintf("unexpected http response: %s", r))
  # create list of headers
  headers<-list()
  if (headerCount>0) {
    for (headerRow in parsedResult[2:headerCount+1]) {
      i <- regexpr(": ", headerRow, fixed=T)
      if (i<0) stop(sprintf("Unexpected format for header %s", headerRow))
      headers[[substr(headerRow, 1, i-1)]]<-substr(headerRow, i+2, nchar(headerRow))
    }
  }
  if (zeroLengthElement<length(parsedResult)) {
    responseBody<-parsedResult[length(parsedResult)]
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
