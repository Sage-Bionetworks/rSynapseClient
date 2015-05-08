## get the current time
## 
## Author: Nicole Deflaxu <nicole.deflaux@sagebase.org>
###############################################################################

# All dates sent to Synapse should be in UTC
.now <- function() {
  as.POSIXlt(Sys.time(), 'UTC')
}

.formatAsISO8601<-function(t) {
  format(as.POSIXlt(t, 'UTC', usetz=TRUE), "%Y-%m-%dT%H:%M:%S.000Z")
}

# All dates sent to Synapse as strings should be formatted as ISO8601 dates in timezone UTC
.nowAsString <- function() {
  .formatAsISO8601(Sys.time())
}