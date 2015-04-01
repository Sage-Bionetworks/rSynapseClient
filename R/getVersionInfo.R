#
# getVersionInfo
# retrieves the black list / latest version info for the R client, using a cache to avoid repeated web calls
#
#

getVersionInfo<-function() {
  cacheTimestampName<-"versionsInfoTimestamp"
  cacheVersionInfoName<-"versionsInfo"
  cacheRefreshSeconds<-300 # 5 minutes
  now<-Sys.time()
  cacheTimestamp<-.getCache(cacheTimestampName)
  versionInfo<-.getCache(cacheVersionInfoName)
  if (is.null(versionInfo) || is.null(cacheTimestamp) || Sys.time()-now>cacheRefreshSeconds) {
    response<-getURLWithRetries(.getVersionsEndpoint(), opts=.getCache("curlOpts"))
    versionInfo <- fromJSON(response$body)
    .setCache(cacheTimestampName, now)
    .setCache(cacheVersionInfoName, versionInfo)
  }
  versionInfo
}
