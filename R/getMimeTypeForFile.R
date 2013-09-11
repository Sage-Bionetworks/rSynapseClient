#
# getMimeTypeForFile
# given a file name, determine the MIME type using the publicly available Apache mapping
#

# get the file's extension and then look up the MIME type, if there is no extension, apply a default
getMimeTypeForFile<-function(fileName) {
  defaultMimeType<-"application/octet-stream"
  extension<-getExtension(fileName)
  if (nchar(extension)==0) return(defaultMimeType) # default mime type
  mimeTypeMap<-getMimeTypeMap()
  mimeType<-mimeTypeMap[[extension]]
  if (is.null(mimeType)) mimeType<-defaultMimeType
  return(mimeType)
}

# get the substring following the LAST "." in fname, or "" if there is no "."
# this code takes into account that there may be multiple "."s in fname
getExtension<-function(fname) {
  suffix<-fname
  dot<-0
  while (dot>=0) {
    lastDot<-dot
    suffix<-substr(suffix, dot+1, nchar(suffix))
    dot<-regexpr(".", suffix, fixed=T)[[1]]
    if (dot==0) stop("Illegal state, dot==0")
  }
  if (lastDot==0) {
    return("")
  } else {
    return(tolower(suffix))
  }
}

# use the global cache to avoid computing the (static) map more than once
getMimeTypeMap<-function() {
  mtmCacheLabel<-"mimeTypeMap"
  mimeTypeMap<-.getCache(mtmCacheLabel)
  if (is.null(mimeTypeMap)) {
    mimeTypeMap<-createMimeTypeMap()
    .setCache(mtmCacheLabel, mimeTypeMap)
  }
  mimeTypeMap
}

# read the Apache MIME type list from the 'net and build a map in memory
createMimeTypeMap<-function() {
  apacheMimeTypeURL<-"http://svn.apache.org/repos/asf/httpd/httpd/branches/2.0.x/docs/conf/mime.types";
  parsedResult<-strsplit(getURLContent(apacheMimeTypeURL), "\n", fixed=TRUE)[[1]]
  ans<-list()
  for (line in parsedResult) {
    # skip blank lines and comments
    if (nchar(line)>0 && substr(line,1,1)!="#") {
      tokens<-strsplit(line, "[ \t]")[[1]]
      if (length(tokens)>1) {
       mimeType<-tokens[1]
       for (token in tokens[2:length(tokens)]) if (nchar(token)>0) ans[[token]]<-mimeType
      }
    }
  }
  # The online list does not contain a mime type for .R files
  ans[['r']] <- 'text/x-r'
  return(ans)
}