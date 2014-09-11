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
A list of TableColumns matching the columns in the given data frame in terms of name, type and (for 'factor' columns), enumeration of permitted values.
}
\seealso{
\code{\link{TableColumn}}
}
