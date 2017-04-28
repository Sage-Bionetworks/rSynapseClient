# execute exponential backoff
# 
# Author: bhoff
#
# fcn - the function to call, retrying on error
# isRetryable - returns true iff the value returned by 'try' indicates an error which can be retried
# finalizeResult - checks value returned by 'try' for errors and constructs the final result of this function
#
withRetries<-function(fcn,
		isRetryable,
		finalizeResult) {
	backoff<-initialBackOffSeconds()
	startTime<-Sys.time()
	
	repeat {
		fcnResult<-try(fcn(), silent=T)
		
		if (isRetryable(fcnResult)) {
			# retry
			if (is(fcnResult, "try-error")) {
				reportableResult<-fcnResult[[1]]
			} else {
				reportableResult<-fcnResult
			}
			sleepTime<-sleepTime(startTime, Sys.time(), backoff)
			if (sleepTime>0) {
				message(sprintf("Error encountered: %s. Will wait for %.1f seconds then retry. Press CTRL+C to quit.", 
								reportableResult, sleepTime))
				Sys.sleep(sleepTime)
			}
			backoff <- increaseBackoff(backoff)
		} else {
			break
		}
		if (maxWaitTimeExceeded(startTime, Sys.time()))  break
	} # end 'repeat' loop
	finalizeResult(fcnResult)
}

initialBackOffSeconds<-function() {0.5} # half second

maxWaitTimeExceeded<-function(startTime, currentTime) {
	maxWaitTime<-.getCache("maxWaitDiffTime")
	if (is.null(maxWaitTime)) stop("Missing value for maxWaitDiffTime.")
	currentTime-startTime>=maxWaitTime
}

sleepTime<-function(startTime, currentTime, backoff) {
	maxWaitTime<-.getCache("maxWaitDiffTime")
	if (is.null(maxWaitTime)) stop("Missing value for maxWaitDiffTime.")
	maxWaitTimeRemaining<-max(0,maxWaitTime-(currentTime-startTime))
	min(backoff, maxWaitTimeRemaining)
}

increaseBackoff<-function(currentBackOffSeconds) {
	BACKOFF_MULTIPLIER <- 2
	MAX_BACKOFF_SECONDS<-30
	min(MAX_BACKOFF_SECONDS, BACKOFF_MULTIPLIER*currentBackOffSeconds)
}

