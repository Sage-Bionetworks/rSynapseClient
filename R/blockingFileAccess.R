#
# these utilities are for providing exclusive access to a file
# they are appropriate for small files and brief (<1 sec) locks
#

lockFile<-function(filePath, maxWaitSeconds=20) {
  # check that the file exists
  # define a lock file in the same directory
  # while <maxWaitSeconds
  # 	try to create the dir
  # 	if successful, return timestamp
  # 	if timestamp more than 1 sec old, delete dir, else sleep for 1 sec
}

readFile<-function(filePath) {
  # return content of file
}

writeFile<-function(filePath, content, lockTimestamp) {
  # if currenttime - lockTimestamp > 1 sec minus epsilon stop (don't have lock anymore)
  # write content to filePath
  # delete lock file (not critical, just keeps things clean)
}