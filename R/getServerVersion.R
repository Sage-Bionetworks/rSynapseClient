#
# returns the version of the Synapse server
#
getServerVersion<-function() {
  cacheTimestampName<-"serverVersionTimestamp"
  cacheVersionInfoName<-"serverVersion"
  cacheRefreshSeconds<-300 # 5 minutes
  now<-Sys.time()
  cacheTimestamp<-.getCache(cacheTimestampName)
  versionInfo<-.getCache(cacheVersionInfoName)
  if (is.null(versionInfo) || is.null(cacheTimestamp) || now-cacheTimestamp>cacheRefreshSeconds) {
    versionInfo<-synapseGet("/version", anonymous=T)$version
    .setCache(cacheTimestampName, now)
    .setCache(cacheVersionInfoName, versionInfo)
  }
  versionInfo
}
