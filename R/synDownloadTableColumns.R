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
	requestedFiles <- lapply(as.list(tableColumns), function(i) {
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
	
	requestedFiles <- unlist(requestedFiles, recursive=FALSE)
	requestedFiles <- requestedFiles[ !sapply(requestedFiles, is.null) ]
	
	# this is a list of just the file handle Ids
	allRequested <- sapply(requestedFiles, "slot", "fileHandleId")
	
	trackRequested <- sapply(as.list(allRequested), function(x) {
				tmp <- getCachedInLocation(x, defaultDownloadLocation(x))
				if (length(tmp) > 0) {
					tmp$unchanged
				} else{
					NULL
				}
			})
	
	names(trackRequested) <- allRequested
	stillNeeded <- sapply(trackRequested, is.null)
	requestedFiles <- requestedFiles[stillNeeded]
	
	if (length(requestedFiles) == 0) {
		return(trackRequested)
	}
	
	message(paste0(length(allRequested), " total files requested:  ", 
					length(allRequested)-length(requestedFiles), " files found in cache;  ", 
					length(requestedFiles), " files left to retrieve"))

	result<-synRestPOST('/file/bulk/async/start', 
			createListFromS4Object(BulkFileDownloadRequest(requestedFiles=as.FileHandleAssociationList(requestedFiles))),
					synapseFileServiceEndpoint())
	asyncJobId <- createS4ObjectFromList(result, "AsyncJobId")
	responseBodyAsList <- trackProgress(paste0('/file/bulk/async/get/', asyncJobId@token), endpoint="FILE")
	responseBody <- createS4ObjectFromList(responseBodyAsList, "BulkFileDownloadResponse")
	
	## CHECK FOR FAILURES
	filehandles <- sapply(responseBody@fileSummary@content, function(x) {
				if(x@status == "SUCCESS") {
					tmp <-list(x@zipEntryName)
					names(tmp) <- x@fileHandleId
					tmp
				} else {
					message(paste0("filehandle", x@fileHandleId, " failed:  ", x@failureMessage))
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
				res <- dir.exists(x)
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
	
	trackRequested[names(cachedPath)] <- cachedPath
	trackRequested
}
