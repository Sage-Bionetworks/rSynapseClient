## Merge new columns into a data.frame
## 
## Author: J. Christopher Bare <chris.bare@sagebase.org>
###############################################################################

# Note: This might be a duplication of what's in rowMerge, so we
#       might want to remove one or the other.

## Given a data.frame and some potentially new field names, make sure
## that all the new fields are in the data.frame. This is done so that
## we can rbind together data.frames with potentially mismatched columns.
##
## Returns the new data.frame that results from cbinding any missing
## columns, with values set to NA.
.mergeColumns <- function(df, newFieldNames) {

	# if df is null or empty, just return it
	if (is.null(df) || nrow(df)==0)
	  return(df)

	# what fields are we missing?
	newFieldNames <- setdiff(newFieldNames, names(df))
  
  # if df already has all needed fields, just return it
  if (length(newFieldNames)==0)
 	  return(df)

  # fill in missing fields with NAs and cbind to df
  newFields <- rep(list(NA),length(newFieldNames))
  names(newFields) <- newFieldNames
  cbind(df, newFields)
}

## Given two data.frames with possibly different columns, returns
## a new data.frame with all columns from both original data.frames
## and rows from the first data.frame stacked on top of rows from
## second data.frame.
.mergeDataFrames <- function(df1, df2) {
  if (is.null(df1)) {
    return(df2)
  }

  if (is.null(df2)) {
    return(df1)
  }

  # There is no guarantee of getting the same schema in each
  # block, so we make sure to add any missing columns, before
  # we do rbind.

  # add new columns to first data.frame
  df1 <- .mergeColumns(df1, colnames(df2))

  # add new columns to second data.frame
  df2 <- .mergeColumns(df2, colnames(df1))

  # now that both data.frames have all columns, rbind them
  return(rbind(df1,df2))
}
