fixFilePath <- function(path, mustWork=FALSE){
	path <- normalizePath(path, mustWork=mustWork)
    path <- gsub("[\\/]+", "/", path)
    gsub("/+$", "", path)
}