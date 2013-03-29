.setUp <- 
  function()
{

}

.tearDown <-
  function()
{
}

integrationTestHappyPath <-
  function()
{
  # lock file
  dir<-tempdir()
  filePath<-sprintf("%s/eaftest.txt", dir)
  lockExpirationTimeStamp<-synapseClient:::lockFile(filePath, maxWaitSeconds=2)
  checkTrue(!is.na(lockExpirationTimeStamp))
  # expiration is in a second, so should be less than five seconds from now
  checkTrue(lockExpirationTimeStamp<Sys.time()+5)
  checkTrue(file.exists(sprintf("%s.lock", filePath)))
  # write file
  content<-"my dog has fleas!"
  synapseClient:::writeFileAndUnlock(filePath, content, lockExpirationTimeStamp)
  # read file
  checkEquals(content, synapseClient:::readFile(filePath))
  # check that directory is gone
  checkTrue(!file.exists(sprintf("%s.lock", filePath)))
}

# test locking a file which already exists
integrationTestLockTimeout <-
  function()
{
  # lock file
  dir<-tempdir()
  filePath<-sprintf("%s/eaftest.txt", dir)
  lockExpirationTimeStamp<-synapseClient:::lockFile(filePath, maxWaitSeconds=2)
  # assume the lock is held by a process that died
  # second "process" should be able to get the lock
  lockExpirationTimeStamp2<-synapseClient:::lockFile(filePath)
  checkTrue(lockExpirationTimeStamp2>=lockExpirationTimeStamp)
  synapseClient:::unlockFile(filePath)
  # check that directory is gone
  checkTrue(!file.exists(sprintf("%s.lock", filePath)))
}

integrationTestAcquireLockFailure <-
  function()
{
  # lock file
  dir<-tempdir()
  filePath<-sprintf("%s/eaftest.txt", dir)
  lockExpirationTimeStamp<-synapseClient:::lockFile(filePath, maxWaitSeconds=2)
  # assume the lock is held by a process that died
  # second "process" fails to get the lock because it doesn't wait long enough
  result<-try(synapseClient:::lockFile(filePath, maxWaitSeconds=.5), silent=TRUE)
  checkTrue(class(result)=="try-error")
  synapseClient:::unlockFile(filePath)
  # check that directory is gone
  checkTrue(!file.exists(sprintf("%s.lock", filePath)))
}