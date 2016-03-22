# Download the multiple file attachments in a Synapse Table,
# avoiding downloading files already in the local file cache.
# Returns a vector whose names are the fileHandle IDs found in the
# specified columns of the given Table and each value of which is
# the path to the requested file or NULL if the file was not able
# to be downloaded.
# 
# Author: Brian Bot
###############################################################################


synDownloadTableColumns <- function(synTable, tableColumns, verbose=FALSE) {
	timeProfile<-timePoint("start")
	
	if(!class(synTable) == "TableDataFrame") {
		stop("synTable must be a TableDataFrame object returned from synTableQuery")
	}
	
	if(!is.character(tableColumns)) {
		stop("tableColumns must be a character object listing the column(s) to be retrieved")
	}
	
	if(!all(tableColumns %in% names(synTable@values))) {
		stop("at least one column specified in tableColumns was not in synTable")
	}
	
	schema<-synTable@schema
	if (is(schema, "TableSchema")) {
		tableId<-propertyValue(schema, "id")
	} else if (is (schema, "character")) {
		tableId<-schema
	} else {
		stop(sprintf("Unexpected type %s", class(schema)))
	}
	
	# list the file handle Ids in the specified columns, in the form
	# of FileHandleAssociations
	fhasInTable <- lapply(as.list(tableColumns), function(i) {
				lapply(as.list(synTable@values[[i]]), function(j) {
							if(is.na(j)) {
								NULL
							} else{
								FileHandleAssociation(associateObjectType="TableEntity", 
										fileHandleId=j, 
										associateObjectId=tableId)
							}
						})
			})
	fhasInTable <- unlist(fhasInTable, recursive=FALSE)
	fhasInTable <- fhasInTable[ !sapply(fhasInTable, is.null) ]
	
	# this is a list of just the file handle Ids
	fileHandleIdsInTable <- sapply(fhasInTable, "slot", "fileHandleId")
	
	permanentFailures<-list() # this map has file handle Ids for names, and failure messages for values
	MAX_DOWNLOAD_TRIES<-100
	
	for (i in 1:(MAX_DOWNLOAD_TRIES+1)) {
		trackRequested <- sapply(as.list(fileHandleIdsInTable), function(x) {
					tmp <- getCachedInLocation(x, defaultDownloadLocation(x))
					if (length(tmp) > 0) {
						tmp$unchanged
					} else{
						NULL
					}
				})
		names(trackRequested) <- fileHandleIdsInTable
		# 'trackRequested' is a map whose names/keys are file handle IDs and
		# whose values are file paths -- if present -- or NULL if absent from
		# the local file system
		
		stillNeeded <- sapply(names(trackRequested), function(name, trackRequested) {
					is.null(trackRequested[[name]]) && !any(name==names(permanentFailures))
			}, trackRequested)
		fhasToDownload <- fhasInTable[stillNeeded]
		
		if (length(fhasToDownload) == 0) {
			# we're done, so break out of the loop.  'trackRequested' is the returned result
			break
		}
		if (i==(MAX_DOWNLOAD_TRIES+1)) {
			stop(sprintf("Unable to download all files after %s iterations.  The collection of files is much larger than expected.", MAX_DOWNLOAD_TRIES))
		}
		
		cat(paste0(length(fileHandleIdsInTable), " total files requested:  ", 
						length(fileHandleIdsInTable)-length(fhasToDownload), " files are in cache;  ", 
						length(fhasToDownload), " files left to retrieve\n"))
		
		# download as many as possible.  No guarantee that all requested files
	  # will be downloaded.
		dtfResult<-downloadTableFileHandles(fhasToDownload)
		permanentFailuresBatch<-dtfResult$permanentFailuresBatch
		timeProfile<-c(timeProfile, dtfResult$timeProfile)
		# collect the permanent failures, both for reporting and to avoid retrying
		permanentFailures<-append(permanentFailures, permanentFailuresBatch)
	}
	
	
	for (x in names(permanentFailures)) {
		cat(paste0("filehandle", x, " failed:  ", permanentFailures[[x]], "\n"))
	}
	

	timeProfile<-c(timeProfile, timePoint("end"))
	if (verbose) {
		df<-NULL
		df$names<-names(timeProfile)
		df$times<-timeProfile
		write.csv(df, row.names=F)
	}
	trackRequested
}

timePoint<-function(label) {
	result<-Sys.time()
	names(result)<-label
	result
}

createBulkDownloadRequest<-function(fhaList) {
	createListFromS4Object(BulkFileDownloadRequest(requestedFiles=as.FileHandleAssociationList(fhaList)))
}

# Take the json string for any asynchronous request body. 
# Convert the string to 'UTF-8' bytes and count the resulting bytes.
# For now the request must be less than 262144 bytes due to a limit
# in Amazon SQS
requestSize<-function(request) {
	string<-synToJson(request)
	# all the fields in BulkFileDownloadRequest are simple ascii (IDs and enum values,
	# no user supplied text fields) so the legnth of the string is the same as the length
	# of the underlying byte array
	nchar(string)
}

getMaxmimumBulkDownloadRequest<-function(fhaList, requestSizeLimit) {
	maximumRequest<-createBulkDownloadRequest(fhaList)
	maximumRequestSize<-requestSize(maximumRequest)
	if (maximumRequestSize<requestSizeLimit) return(maximumRequest)
	emptyRequestSize<-requestSize(createBulkDownloadRequest(list()))
	numFhas<-as.integer((requestSizeLimit-emptyRequestSize)*length(fhaList)/(maximumRequestSize-emptyRequestSize))
	if (numFhas<1) stop(sprintf("Number of file handles which can be accommodated in a bulk download request is %s", numFhas))
	createBulkDownloadRequest(fhaList[1:numFhas])
}

# returns permanent failures (not to retry)
downloadTableFileHandles <- function(fhasToDownload) {
	timeProfile<-timePoint("start dtfh")
	
	bulkDownloadRequestBody<-getMaxmimumBulkDownloadRequest(fhasToDownload, 260000)
	result<-synRestPOST('/file/bulk/async/start', bulkDownloadRequestBody, synapseFileServiceEndpoint())
	asyncJobId <- createS4ObjectFromList(result, "AsyncJobId")
	bulkAsyncGetUri<-paste0('/file/bulk/async/get/', asyncJobId@token)
	timeProfile<-c(timeProfile, timePoint("before 'track progress'"))
	responseBodyAsList <- trackProgress(bulkAsyncGetUri, endpoint="FILE")
	timeProfile<-c(timeProfile, timePoint("after 'track progress'"))
	responseBody <- createS4ObjectFromList(responseBodyAsList, "BulkFileDownloadResponse")
	
	## CHECK FOR FAILURES
	filehandles <- sapply(responseBody@fileSummary@content, function(x) {
				if(x@status == "SUCCESS") {
					tmp <-list(x@zipEntryName)
					names(tmp) <- x@fileHandleId
					tmp
				} else {
					NULL
				}
	})
	
	# by 'permanent' we mean those failure that can't be handled by retrying
  permanentFailures<-list()
  for (x in responseBody@fileSummary@content) {
		if(x@status != "SUCCESS" && x@failureCode!="EXCEEDS_SIZE_LIMIT") {
			permanentFailures[[x@fileHandleId]]<-x@failureMessage
		}
	}
	
	if (length(responseBody@resultZipFileHandleId)==0) {
		if (length(bulkDownloadRequestBody$requestedFiles)-length(permanentFailures)>0) {
			stop("Server failed to return zip file.")
		} else {
			return(permanentFailures)
		}
	}
	
	timeProfile<-c(timeProfile, timePoint("before 'downloadFromService'"))
	## DOWNLOAD THE ZIP FILE
	downloadUri <- sprintf("/fileHandle/%s/url?redirect=FALSE", responseBody@resultZipFileHandleId)
	zipFilePath <- downloadFromService(downloadUri, "FILE")
	timeProfile<-c(timeProfile, timePoint("after 'downloadFromService'"))
	
	## CREATE A TEMPORARY FOLDER TO EXPAND ZIP INTO
	zipPath <- tempfile(pattern="zipPath")
	if(!dir.create(zipPath)) {
		stop("Couldn't create temporary directory of unzip files into.")
	}
	unzippedFilePaths <- unzip(zipFilePath$downloadedFile, exdir=zipPath)
	names(unzippedFilePaths) <- basename(dirname(unzippedFilePaths))
	
	if(!all(sapply(filehandles, function(x) {is.null(x) || any(grepl(x, unzippedFilePaths))}))) {
		stop("Some file handles are missing from downloaded zip file.")
	}
	
	newPaths <- sapply(as.list(names(unzippedFilePaths)), defaultDownloadLocation)
	createSuccess <- sapply(as.list(newPaths), function(x) {
				res <- file.exists(x)
				if(!res) {
					res <- dir.create(x, recursive = TRUE)
				}
				res
			})
	if(!all(createSuccess)) {
		stop("Couldn't create cache directories.")
	}
	
	cachedPath <- file.path(newPaths, basename(unzippedFilePaths))
	cachedFilehandle <- basename(dirname(cachedPath))
	copySuccess <- file.copy(unzippedFilePaths, cachedPath, overwrite = TRUE)
	if(!all(copySuccess)) {
		stop("Failed to copy all files.")
	} else{
		unlink(zipPath)
	}
	cachedPath <- as.list(cachedPath)
	names(cachedPath) <- cachedFilehandle
	
	for(i in 1:length(cachedPath)) {
		addToCacheMap(names(cachedPath)[i], cachedPath[[i]])
	}
	
	timeProfile<-c(timeProfile, timePoint("end dtfh"))
	list(permanentFailures=permanentFailures, timeProfile=timeProfile)
}

