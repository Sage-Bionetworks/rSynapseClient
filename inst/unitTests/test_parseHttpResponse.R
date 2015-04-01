# 
# 
# Author: brucehoff
###############################################################################



unitTestBasicCase <- 
  function()
{
  # this is the response to an invalid log in
  # Note, it doesn't matter what version of HTTP is in the string (here we use A.B)
  s<-"HTTP/A.B 500 Internal Server Error\r\nContent-Type: application/json\r\nDate: Wed, 02 Jan 2013 17:42:18 GMT\r\nServer: Apache-Coyote/1.1\r\ntransfer-encoding: chunked\r\nConnection: keep-alive\r\n\r\n"
  ans <- synapseClient:::parseHttpHeaders(s)
  # parsed response has (1) status code, (2) status string, (3) headers
  checkEquals(3, length(ans))
  checkEquals(500, ans$statusCode)
  checkEquals("Internal Server Error", ans$statusString)
  checkEquals(5, length(ans$headers))
  checkEquals("Wed, 02 Jan 2013 17:42:18 GMT", ans$headers$Date)
  checkEquals("Apache-Coyote/1.1", ans$headers$Server)
  checkEquals("chunked", ans$headers$`transfer-encoding`)
  checkEquals("keep-alive", ans$headers$Connection)
}


unitTestRedirect <-
  function()
{
  # this is the response indicating a redirect.  there is no response body
  s<-"HTTP/1.1 301 Moved Permanently\r\nContent-Type: text/plain; charset=UTF-8\r\nDate: Wed, 02 Jan 2013 17:50:10 GMT\r\nLocation: https://auth-dev-xschildw.dev.sagebase.org/auth/v1/session\r\nServer: Apache-Coyote/1.1\r\nContent-Length: 0\r\nConnection: keep-alive\r\n\r\n"
  ans <- synapseClient:::parseHttpHeaders(s)
  # parsed response has (1) status code, (2) status string, (3) headers
  checkEquals(3, length(ans))
  checkEquals(301, ans$statusCode)
  checkEquals("Moved Permanently", ans$statusString)
  checkEquals(6, length(ans$headers))
  checkEquals("Wed, 02 Jan 2013 17:50:10 GMT", ans$headers$Date)
  checkEquals("https://auth-dev-xschildw.dev.sagebase.org/auth/v1/session", ans$headers$Location)
  checkEquals("Apache-Coyote/1.1", ans$headers$Server)
  checkEquals("0", ans$headers$`Content-Length`)
  checkEquals("keep-alive", ans$headers$Connection) 
}

unitTestConnectionEstablished <-
  function()
{
  s<-"HTTP/1.0 200 Connection established\r\n\r\nHTTP/1.1 200 OK\r\nAccess-Control-Allow-Origin: *\r\nContent-Type: application/json\r\nDate: Thu, 20 Feb 2014 00:44:53 GMT\r\nServer: Apache-Coyote/1.1\r\nContent-Length: 29\r\nConnection: keep-alive\r\n\r\n"
  ans <- synapseClient:::parseHttpHeaders(s)
  # parsed response has (1) status code, (2) status string, (3) headers
  checkEquals(3, length(ans))
  checkEquals(200, ans$statusCode)
  checkEquals("OK", ans$statusString)
  checkEquals(6, length(ans$headers))
  checkEquals("Thu, 20 Feb 2014 00:44:53 GMT", ans$headers$Date)
  checkEquals("Apache-Coyote/1.1", ans$headers$Server)
  checkEquals("29", ans$headers$`Content-Length`)
  checkEquals("keep-alive", ans$headers$Connection)
}

