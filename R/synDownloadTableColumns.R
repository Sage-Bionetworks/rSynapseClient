# Download the multiple file attachments in a Synapse Table,
# avoiding downloading files already in the local file cache
# returns a list whose names are the downloaded fileHandle IDs
# and whose values are the paths to the requested files
# 
# Author: Brian Bot
###############################################################################


synDownloadTableColumns <- function(synTable, tableColumns) {
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
		
		stillNeeded <- sapply(trackRequested, function(x) {
					is.null(x) && !any(names(x)==names(permanentFailures))
			})
		fhasToDownload <- fhasInTable[stillNeeded]
		
		if (length(fhasToDownload) == 0) {
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
		permanentFailuresBatch<-downloadTableFileHandles(fhasToDownload)
		# collect the permanent failures, both for reporting and to avoid retrying
		permanentFailures<-append(permanentFailures, permanentFailuresBatch)
	}
	
	
	for (x in names(permanentFailures)) {
		cat(paste0("filehandle", x, " failed:  ", permanentFailures[[x]], "\n"))
	}
	
	trackRequested
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
	if (numFhas<1) stop(sprintf("Number of file handles which can be accomodated in a bulk download request is %s", numFhas))
	createBulkDownloadRequest(fhaList[1:numFhas])
}

# returns permanent failures (not to retry)
downloadTableFileHandles <- function(fhasToDownload) {
	bulkDownloadRequestBody<-getMaxmimumBulkDownloadRequest(fhasToDownload, 262000)
	result<-synRestPOST('/file/bulk/async/start', bulkDownloadRequestBody, synapseFileServiceEndpoint())
	asyncJobId <- createS4ObjectFromList(result, "AsyncJobId")
	responseBodyAsList <- trackProgress(paste0('/file/bulk/async/get/', asyncJobId@token), endpoint="FILE")
	responseBody <- createS4ObjectFromList(responseBodyAsList, "BulkFileDownloadResponse")
	
	permanentFailures <- list() # by permanent we mean those failure that can't be handled by retrying
	## CHECK FOR FAILURES
	filehandles <- sapply(responseBody@fileSummary@content, function(x) {
				if(x@status == "SUCCESS") {
					tmp <-list(x@zipEntryName)
					names(tmp) <- x@fileHandleId
					tmp
				} else {
					if (x@failureCode!="EXCEEDS_SIZE_LIMIT") {
						permanentFailures[[x@fileHandleId]]<-x@failureMessage
					}
					NULL
				}
			})
	
	## DOWNLOAD THE ZIP FILE
	downloadUri <- sprintf("/fileHandle/%s/url?redirect=FALSE", responseBody@resultZipFileHandleId)
	zipFilePath <- downloadFromService(downloadUri, "FILE")
	
	
	## CREATE A TEMPORARY FOLDER TO EXPAND ZIP INTO
	zipPath <- tempfile(pattern="zipPath")
	if(!dir.create(zipPath)) {
		stop("couldn't create temporary directory of unzip files into")
	}
	unzippedFilePaths <- unzip(zipFilePath$downloadedFile, exdir=zipPath)
	names(unzippedFilePaths) <- basename(dirname(unzippedFilePaths))
	
	if(!all(sapply(filehandles, function(x) {any(grepl(x, unzippedFilePaths))}))) {
		stop("some file handles are missing from downloaded zip file")
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
		stop("couldn't create cache directories")
	}
	
	cachedPath <- file.path(newPaths, basename(unzippedFilePaths))
	cachedFilehandle <- basename(dirname(cachedPath))
	copySuccess <- file.copy(unzippedFilePaths, cachedPath, overwrite = TRUE)
	if(!all(copySuccess)) {
		stop("Failed to copy all files")
	} else{
		unlink(zipPath)
	}
	cachedPath <- as.list(cachedPath)
	names(cachedPath) <- cachedFilehandle
	
	for(i in 1:length(cachedPath)) {
		addToCacheMap(names(cachedPath)[i], cachedPath[[i]])
	}
	
	permanentFailures
}

