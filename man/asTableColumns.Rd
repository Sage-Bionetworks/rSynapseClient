\name{as.TableColumns}
\alias{as.TableColumns}
\docType{methods}
\title{
as.TableColumns
}
\description{
Create Table Columns based on a data frame
}
\usage{as.TableColumns(datafame)}
\arguments{
	\item{datafame}{
	The data frame of interest.
	}
}
\usage{as.TableColumns(filePath)}
\arguments{
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
