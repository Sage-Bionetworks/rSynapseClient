## test QueryResult object
## 
## Author: J. Christopher Bare <chris.bare@sagebase.org>
###############################################################################

## to run:
## synapseClient:::.integrationTest(testFileRegexp="test_QueryResult.R")

.setUp <-
  function()
{
  ## create a project to fill with entities
  project <- createEntity(Project())
  synapseClient:::.setCache("testProject", project)
}

.tearDown <-
  function()
{
  ## delete the test project
  deleteEntity(synapseClient:::.getCache("testProject"))
}

timeToWait<-"00:10:00" # wait for up to ten minutes

integrationTestQueryResult_Fetch <- function() {
  project <- synapseClient:::.getCache("testProject")$properties$id

  ## add some children
  folder <- Folder(parentId=project)
  lapply(1:10, function(i) createEntity(folder))

	startTime<-Sys.time()
	while (Sys.time()-startTime<as.difftime(timeToWait)) {
  	qr <- synapseClient:::QueryResult$new(sprintf("select id, name, parentId from entity where parentId=='%s'", project), blockSize=5)
  	df <- qr$fetch()
		if (nrow(df)==5) break
		message("Expected 5 rows but found ", nrow(df))
		Sys.sleep(5)
	}
	message("Waited ", difftime(Sys.time(), startTime, units="mins"), " minutes for query results.")
	
  checkEquals(nrow(df),5)
  checkEquals(ncol(df),3)
	
	startTime<-Sys.time()
	while (Sys.time()-startTime<as.difftime(timeToWait)) {
		df <- qr$fetch()
		if (nrow(df)==5) break
		message("Expected 5 rows but found ", nrow(df))
		Sys.sleep(5)
	}
	message("Waited ", difftime(Sys.time(), startTime, units="mins"), " minutes for query results.")
	
  checkEquals(nrow(df),5)
  checkEquals(ncol(df),3)

  # test length and names functions
  checkEquals(length(qr), 5)
  checkEquals(class(names(qr)), "character")
  checkEquals(length(names(qr)), 3)
}

integrationTestQueryResult_Collect <- function() {
  project <- synapseClient:::.getCache("testProject")$properties$id

  ## add some children
  folder <- Folder(parentId=project)
  lapply(1:10, function(i) createEntity(folder))

	startTime<-Sys.time()
	while (Sys.time()-startTime<as.difftime(timeToWait)) {
		qr <- synapseClient:::QueryResult$new(sprintf("select id, name, parentId from entity where parentId=='%s'", project), blockSize=3)
		df <- qr$collect()
		if (nrow(df)==3) break
		message("Expected 3 rows but found ", nrow(df))
		Sys.sleep(5)
	}
	message("Waited ", difftime(Sys.time(), startTime, units="mins"), " minutes for query results.")
	
  checkEquals(nrow(df),3)
  checkEquals(ncol(df),3)
	
	startTime<-Sys.time()
	while (Sys.time()-startTime<as.difftime(timeToWait)) {
		df <- qr$collect()
		if (nrow(df)==3) break
		message("Expected 3 rows but found ", nrow(df))
		Sys.sleep(5)
	}
	message("Waited ", difftime(Sys.time(), startTime, units="mins"), " minutes for query results.")
	
  checkEquals(nrow(df),3)
  checkEquals(ncol(df),3)
	
	
	startTime<-Sys.time()
	while (Sys.time()-startTime<as.difftime(timeToWait)) {
		df <- qr$collect()
		if (nrow(df)==3) break
		message("Expected 3 rows but found ", nrow(df))
		Sys.sleep(5)
	}
	message("Waited ", difftime(Sys.time(), startTime, units="mins"), " minutes for query results.")
	
  checkEquals(nrow(df),3)
  checkEquals(ncol(df),3)

  df <- qr$as.data.frame()
  checkEquals(nrow(df),9)
  checkEquals(ncol(df),3)
}

integrationTestQueryResult_CollectAll <- function() {
  project <- synapseClient:::.getCache("testProject")$properties$id

  ## add some children
  folder <- Folder(parentId=project)
  lapply(1:10, function(i) createEntity(folder))
	
	
	startTime<-Sys.time()
	while (Sys.time()-startTime<as.difftime(timeToWait)) {
		qr <- synapseClient:::QueryResult$new(sprintf("select id, name, parentId from entity where parentId=='%s' LIMIT 10", project), blockSize=7)
		qr$collect()
		df <- qr$collectAll()
		if (nrow(df)==10) break
		message("Expected 10 rows but found ", nrow(df))
		Sys.sleep(5)
	}
	message("Waited ", difftime(Sys.time(), startTime, units="mins"), " minutes for query results.")
	
  checkEquals(nrow(df), 10)
}

integrationTestQueryResult_EmptyResult <- function() {
  # query that should have no results
  qr <- synapseClient:::QueryResult$new(
    'select id, name, parentId from entity where parentId=="-1" limit 100', blockSize=25)
  checkEquals(length(qr), 0)
  df <- qr$collect()
  # should result in an empty data frame
  checkEquals(class(df), "data.frame")
  checkEquals(nrow(df), 0)
  checkEquals(ncol(df), 0)
}


