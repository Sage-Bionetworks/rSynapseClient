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

timeToWait<-"00:10:00" # wait for up to 10 min

integrationTestQueryResult_Fetch <- function() {
  project <- synapseClient:::.getCache("testProject")$properties$id

  ## add some children
  folder <- Folder(parentId=project)
  lapply(1:10, function(i) createEntity(folder))

	startTime<-Sys.time()
	qr <- synapseClient:::QueryResult$new(sprintf("select id, name, parentId from entity where parentId=='%s'", project), blockSize=5)
	nrows<-0
	ncols<-0
	while (Sys.time()-startTime<as.difftime(timeToWait)) {
  	df <- qr$fetch()
		nrows <- nrows + nrow(df)
		ncols <- max(ncols, ncol(df))
		message("Fetch: Number of accumulated rows: ", nrows)
		if (nrows==10) break # got all the results
		Sys.sleep(5)
	}
	message("Waited ", difftime(Sys.time(), startTime, units="mins"), " minutes for query results.")
	
  checkEquals(nrows,10)
  checkEquals(ncols,3)

  # test length and names functions
  checkEquals(length(qr), nrow(df))
  checkEquals(class(names(qr)), "character")
  checkEquals(length(names(qr)), 3)
}

integrationTestQueryResult_Collect <- function() {
  project <- synapseClient:::.getCache("testProject")$properties$id

  ## add some children
  folder <- Folder(parentId=project)
  lapply(1:10, function(i) createEntity(folder))

	startTime<-Sys.time()
	qr <- synapseClient:::QueryResult$new(sprintf("select id, name, parentId from entity where parentId=='%s'", project), blockSize=3)
	while (Sys.time()-startTime<as.difftime(timeToWait)) {
		df <- qr$collect()
		message("Collect: Number of accumulated rows: ", nrow(qr$results))
		if (nrow(qr$results)==10) break # got all the results
		Sys.sleep(5)
	}
	message("Waited ", difftime(Sys.time(), startTime, units="mins"), " minutes for query results.")
	
	checkEquals(nrow(qr$results),10)
	checkEquals(ncol(qr$results),3)
	
  df <- qr$as.data.frame()
  checkEquals(nrow(df),10)
  checkEquals(ncol(df),3)
}

integrationTestQueryResult_CollectAll <- function() {
  project <- synapseClient:::.getCache("testProject")$properties$id

  ## add some children
  folder <- Folder(parentId=project)
  lapply(1:10, function(i) createEntity(folder))
	
	
	startTime<-Sys.time()
	qr <- synapseClient:::QueryResult$new(sprintf("select id, name, parentId from entity where parentId=='%s' LIMIT 10", project), blockSize=7)
	while (Sys.time()-startTime<as.difftime(timeToWait)) {
		qr$collect()
		df <- qr$collectAll()
		message("Collect All: Number of accumulated rows: ", nrow(qr$results))
		if (nrow(qr$results)==10) break # got all the results
		Sys.sleep(5)
	}
	message("Waited ", difftime(Sys.time(), startTime, units="mins"), " minutes for query results.")
	
	checkEquals(nrow(qr$results),10)
	checkEquals(ncol(qr$results),3)
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


