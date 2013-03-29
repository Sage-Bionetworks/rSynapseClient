#
# these utilities are for providing exclusive access to a file
# they are appropriate for small files and brief (<1 sec) locks
#

lockdirPath<-function(filePath) {sprintf("%s.lock", filePath)}

# lock a file by creating an empty directory with a ".lock" suffix and capturing the time stamp
# returns the expiration time on the lock, which lets the caller know how long
# they have exclusive access to the file
# Note: It is possible, and in fact proper, to lock a file that doesn't exist yet
# This gives the caller exclusive access for creating the file.
lockFile<-function(filePath, maxWaitSeconds=10) {
  lockdirPath <- lockdirPath(filePath)
  startTime<-Sys.time()
  ageTimeoutSeconds<-1.0
  agePaddingSeconds <- 1.0 # extra time to wait after expiration before breaking the lock
  while ((Sys.time()-startTime) < maxWaitSeconds) {
    success<-dir.create(lockdirPath, showWarnings=FALSE, recursive=TRUE)
    if (success) return(lastModifiedTimestamp(lockdirPath)+ageTimeoutSeconds)
    # can't access the file, maybe someone else is using it
    Sys.sleep(0.5)
    lockdirTimestamp<-lastModifiedTimestamp(lockdirPath)
    if (!is.na(lockdirTimestamp)) { # the directory is there already...is the lock stale?
      modifiedAgeSeconds<-Sys.time()-lockdirTimestamp
      # if the lock is stale -- and has been for a while -- break the lock
      if (modifiedAgeSeconds > ageTimeoutSeconds + agePaddingSeconds) {
        unlockFile(filePath)
      }
    }
    # loop around and try again
  } # end of while loop
  stop(sprintf("Timed out trying to acquire lock on %s for %f seconds.", filePath, maxWaitSeconds))
}

unlockFile<-function(filePath) {
  lockdirPath <- lockdirPath(filePath)
  file.remove(lockdirPath)
}

# return content of file
readFile<-function(filePath) {
  readChar(filePath, file.info(filePath)$size)
}

writeFileAndUnlock<-function(filePath, content, lockExpiration) {
  # if I don't actually have the lock, then release it
  if (Sys.time() > lockExpiration) {
    unlockFile(filePath) # (not critical, just keeps things clean)
    stop(sprintf("File lock exceeded for %s", filePath))
  }
  # write content to filePath
  writeChar(content, filePath)
  unlockFile(filePath) # (not critical, just keeps things clean)
  # we should be able to do all this well within the allocated lock time
  if (Sys.time() > lockExpiration) {
    warning("Warning, file write operation exceeded file lock.  To ensure exclusive access, longer file lock should be used.")
  }
}