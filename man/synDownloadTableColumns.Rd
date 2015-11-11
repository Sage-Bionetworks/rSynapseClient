\name{synDownloadTableColumns}
\alias{synDownloadTableColumns}
\docType{methods}
\title{
synDownloadTableColumns
}
\description{
Download files referenced in a Table
}
\usage{
  synDownloadTableColumns(synTable, tableColumns)
}
\arguments{
  \item{synTable}{
	 The tabular data containing file references to download.  Must be a TableDataFrame object returned from synTableQuery
  }
  \item{tableColumns}{
  	A character object listing the column(s) to be retrieved.
  }
}
\value{
A list whose names are the downloaded fileHandle IDs
and whose values are the paths to the requested files.
}
\details{
Downloads the files corresponding to the references in the selected rows of the 
given Table, while avoiding re-downloading those files which are already downloaded
to the default download location.
}
\seealso{
\code{\link{synTableQuery}}
}
