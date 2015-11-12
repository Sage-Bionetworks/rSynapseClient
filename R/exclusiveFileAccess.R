#
# these utilities are for providing exclusive access to a file
# they are appropriate for small files and brief (<1 sec) locks
#

lockdirPath<-function(filePath) {file.path(dirname(filePath), sprintf("%s.lock", basename(filePath)))}

# lock a file by creating an empty directory with a ".lock" suffix and capturing the time stamp
# returns the expiration time on the lock, which lets the caller know how long
# they have exclusive access to the file
# Note: It is possible, and in fact proper, to lock a file that doesn't exist yet
# This gives the caller exclusive access for creating the file.
#
# Note:  The ageTimeoutSeconds parameter is provided for testing purposes only
# Other callers are not to change it.
#
lockFile<-function(filePath, maxWaitSeconds=70, ageTimeoutSeconds=10.0) {
  lockdirPath <- lockdirPath(filePath)
  startTime<-Sys.time()
  while ((Sys.time()-startTime) < maxWaitSeconds) {
    success<-dir.create(lockdirPath, showWarnings=FALSE, recursive=TRUE)
    if (success) return(lastModifiedTimestampNonexistentOK(lockdirPath)+ageTimeoutSeconds)
    # can't access the file, maybe someone else is using it
    Sys.sleep(0.5)
    lockdirTimestamp<-lastModifiedTimestampNonexistentOK(lockdirPath)
    if (!is.na(lockdirTimestamp)) { # the directory is there already...is the lock stale?
      modifiedAgeSeconds<-difftime(Sys.time(), lockdirTimestamp, units="secs")
      # if the lock is stale -- and has been for a while -- break the lock
      if (modifiedAgeSeconds > ageTimeoutSeconds) {
        unlockFile(filePath)
      }
    }
    # loop around and try again
  } # end of while loop
  stop(sprintf("Timed out trying to acquire lock on %s for %f seconds.", filePath, maxWaitSeconds))
}

unlockFile<-function(filePath) {
  lockdirPath <- lockdirPath(filePath)
  if (file.exists(lockdirPath)) {
    # This won't work on Windows (since empty directories are not files)
    # It'll throw a warning saying that "Permission denied"
    suppressWarnings(file.remove(lockdirPath))
  }
  if (file.exists(lockdirPath)) unlink(lockdirPath, recursive=T, force=T)
  if (file.exists(lockdirPath)) system(sprintf("rm -r %s", lockdirPath))
}

# return content of file
readFile<-function(filePath) {
  connection<-file(filePath)
  result<-paste(readLines(connection, warn=FALSE), collapse="\n")
  close(connection)
  result
}

writeFileAndUnlock<-function(filePath, content, lockExpiration) {
  # if I don't actually have the lock, then release it
  # This check should really be unnecessary.  In practice the client should call
  # this method immediately after obtaining the lock and so should have LOTS of
  # time left before expiration.
  if (Sys.time() >= lockExpiration) {
    unlockFile(filePath) # (not critical, just keeps things clean)
    stop(sprintf("File lock exceeded for %s", filePath))
  }
  # write content to filePath
  connection<-file(filePath)
  writeLines(content, connection)
  close(connection)
  unlockFile(filePath) # (not critical, just keeps things clean)
  # we should be able to do all this well within the allocated lock time
  if (Sys.time() >= lockExpiration) {
    warning("Warning, file write operation exceeded file lock.  To ensure exclusive access, longer file lock should be used.")
  }
}