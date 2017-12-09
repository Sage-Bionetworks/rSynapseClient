# Download the multiple file attachments in a Synapse Table,
# avoiding downloading files already in the local file cache.
# Returns a list whose names are the fileHandle IDs found in the
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
	fileHandleIdsInTable <- lapply(as.list(tableColumns), function(i) {
				lapply(as.list(synTable@values[[i]]), function(j) {
							if(is.na(j)) {
								NULL
							} else{
								j
							}
						})
			})
	
	fileHandleIdsInTable<-unlist(fileHandleIdsInTable, recursive=FALSE)
	fileHandleIdsInTable <- fileHandleIdsInTable[ !sapply(fileHandleIdsInTable, is.null) ]
	fileHandleIdsInTable <- unique(fileHandleIdsInTable)
	
	permanentFailures<-list() # this map has file handle Ids for names, and failure messages for values
	MAX_DOWNLOAD_TRIES<-100
	
	# This is the returned value, a list whose names are fh ids and whose
	# values are file paths or NULL if not downloaded
	# initialize to all NULL and then fill in as we download
	cumulativeDownloadResults<-sapply(1:length(fileHandleIdsInTable), function(x){NULL})
	names(cumulativeDownloadResults)<-fileHandleIdsInTable
	
	# keep track of the earliest place to start downloading
	# there may be files with higher indices that have been downloaded,
	# this just gives us a 'lower bound' position in cumulativeDownloadResults:
	# we need never scan below this index
	firstFHToDownload<-1 
	
	for (i in 1:(MAX_DOWNLOAD_TRIES+1)) {
		
		# this is the maximum number of files which will be requested to be zipped
		# in any single batch
		max_batch_size<-10000
		
		fhasToDownload<-list()
		if (firstFHToDownload<=length(cumulativeDownloadResults)) {
			for (j in firstFHToDownload:length(cumulativeDownloadResults)) {
				if (length(fhasToDownload)>=max_batch_size) break
				if (is.null(cumulativeDownloadResults[[j]])) {
					fhId<-names(cumulativeDownloadResults)[j]
					if (any(names(permanentFailures)==fhId)) {
						if (j==firstFHToDownload) firstFHToDownload<-firstFHToDownload+1
					} else {
						tmp <- getCachedInLocation(fhId, defaultDownloadLocation(fhId))
						if (length(tmp) > 0 && !is.null(tmp$unchanged)) {
							# the file is already downloaded
							cumulativeDownloadResults[[j]]<-tmp$unchanged
							if (j==firstFHToDownload) firstFHToDownload<-firstFHToDownload+1
						} else {
							# we haven't downloaded this one yet and it's not a 'permanent failure' so let's download it
							fhasToDownload<-append(fhasToDownload, 
									synapseClient:::FileHandleAssociation(associateObjectType="TableEntity", 
											fileHandleId=fhId, associateObjectId=tableId))
						}
					}
				} else {
					if (j==firstFHToDownload) firstFHToDownload<-firstFHToDownload+1
				}
			}
		}
	
		
		if (length(fhasToDownload) == 0) {
			# we're done, so break out of the loop.  'cumulativeDownloadResults' is the returned result
			break
		}
		if (i==(MAX_DOWNLOAD_TRIES+1)) {
			stop(sprintf("Unable to download all files after %s iterations.  The collection of files is much larger than expected.", MAX_DOWNLOAD_TRIES))
		}
		
		cat(paste0(length(fileHandleIdsInTable), " total files requested:  At least ", 
						firstFHToDownload-1, " are downloaded;  ", 
						length(fhasToDownload), " will be retrieved in the next batch.\n"))
		
		# download as many as possible.  No guarantee that all requested files
	  # will be downloaded.
		dtfResult<-downloadTableFileHandles(fhasToDownload)
		timeProfile<-c(timeProfile, dtfResult$timeProfile)
		# collect the permanent failures, both for reporting and to avoid retrying
		permanentFailures<-append(permanentFailures, dtfResult$permanentFailures)
		
		# now merge the successes back into the cumulative results
		cumulativeDownloadResults[unlist(names(dtfResult$successes))]<-dtfResult$successes
		
		if (verbose) {
			df<-NULL
			df$names<-names(timeProfile)
			df$times<-timeProfile
			write.csv(df, row.names=F)
			cat("\n")
		}
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

	cumulativeDownloadResults
}

timePoint<-function(label) {
	result<-Sys.time()
	names(result)<-label
	result
}

createBulkDownloadRequest<-function(fhaList) {
	createListFromS4Object(BulkFileDownloadRequest(requestedFiles=as.FileHandleAssociationList(fhaList)))
}

# returns permanent failures (not to retry)
downloadTableFileHandles <- function(fhasToDownload) {
	timeProfile<-timePoint("start dtfh")
	
	bulkDownloadRequestBody<-createBulkDownloadRequest(fhasToDownload)
	result<-synRestPOST('/file/bulk/async/start', bulkDownloadRequestBody, synapseFileServiceEndpoint())
	asyncJobId <- createS4ObjectFromList(result, "AsyncJobId")
	bulkAsyncGetUri<-paste0('/file/bulk/async/get/', asyncJobId@token)
	timeProfile<-c(timeProfile, timePoint("before 'track progress'"))
	responseBodyAsList <- trackProgress(bulkAsyncGetUri, endpoint="FILE")
	timeProfile<-c(timeProfile, timePoint("after 'track progress'"))
	responseBody <- createS4ObjectFromList(responseBodyAsList, "BulkFileDownloadResponse")
	
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
	zipFilePath <- downloadFromService(downloadUri=downloadUri, endpointName="FILE", destdir=synapseCacheDir())
	timeProfile<-c(timeProfile, timePoint("after 'downloadFromService'"))
	
	## CREATE A TEMPORARY FOLDER TO EXPAND ZIP INTO
	zipPath <- tempfile(pattern="zipPath")
	if(!dir.create(zipPath)) {
		stop("Couldn't create temporary directory of unzip files into.")
	}
	unzippedFilePaths <- unzip(zipFilePath$downloadedFile, exdir=zipPath)
	names(unzippedFilePaths) <- basename(dirname(unzippedFilePaths))
	
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
		unlink(zipFilePath$downloadedFile)
	}
	
	cachedPath <- as.list(cachedPath)
	names(cachedPath) <- cachedFilehandle
	
	for(i in 1:length(cachedPath)) {
		addToCacheMap(names(cachedPath)[i], cachedPath[[i]])
	}
	
	timeProfile<-c(timeProfile, timePoint("end dtfh"))
	list(permanentFailures=permanentFailures, timeProfile=timeProfile, successes=cachedPath)
}

