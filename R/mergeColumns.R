## Merge new columns into a data.frame
## 
## Author: J. Christopher Bare <chris.bare@sagebase.org>
###############################################################################

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
