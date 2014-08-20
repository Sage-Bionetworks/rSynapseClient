\name{Table}
\alias{Table}
\docType{methods}
\title{
Table Constructor
}
\description{Constructor for a table, which can subsequently be stored in Synapse}
\usage{Table(tableSchema, values)}
\arguments{
  \item{tableSchema}{
  the schema for the table, an object of the type TableSchema or its ID
  }
  \item{values}{
  the values to be stored in the table, either a data frame, a matrix, a RowList, or the (character) file path to a CSV file
  }
}
\seealso{
\code{\link{TableSchema},\link{RowList},\link{synStore}}
}
