\name{as.tableColumns}
\alias{as.tableColumns}
\docType{methods}
\title{
as.tableColumns
}
\description{
Create Table Columns based on a data frame
}
\usage{
as.tableColumns(dataframe)
as.tableColumns(filePath)
}
\arguments{
	\item{dataframe}{
	The data frame of interest.
	}
	\item{filePath}{
	The path to a file containing the data frame of interest.
	}
}
\value{
A list containing two fields, 'tableColumns' and 'fileHandleId'.

tableColumns: A list of TableColumns matching the columns in the given data frame in terms of name, type and (for 'factor' columns), enumeration of permitted values.

fileHandleId: The ID of the uploaded file, suitable for passing to the Table constructor along with a schema. (See 'Table'.)
}
\seealso{
\code{\link{TableColumn}}
}
