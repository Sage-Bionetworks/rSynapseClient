.unpack <- 
		function(filename)
{
	filename <- path.expand(filename)
	splits <- strsplit(filename, "\\.")
	extension <- splits[[1]][length(splits[[1]])]
	destdir <- gsub(paste("[\\.]", extension, sep=""), paste("_", .getCache("downloadSuffix"), sep=""), filename)
	extension <- tolower(extension)
	
	switch(extension,
		zip = {unlink(destdir); unzip(filename, exdir = destdir)},
		gz = {unlink(destdir); untar(filename, exdir = destdir)},
		tar = {unlink(destdir); untar(filename, exdir = destdir)},
		{ ## default
			splits <- strsplit(filename, .Platform$file.sep)
			destdir <- paste(splits[[1]][-length(splits[[1]])], collapse=.Platform$file.sep)
			attr(filename, "rootDir") <- destdir
			return(filename)
		}
	)	
	files <- list.files(destdir, full.names = TRUE, recursive=TRUE, all.files = TRUE)
	files <- setdiff(files, c(".",".."))
	attr(files, "rootDir") <- destdir
	files
}


