## test QueryResult object
## 
## Author: J. Christopher Bare <chris.bare@sagebase.org>
###############################################################################

integrationTestQueryResult_Fetch <- function() {
  qr <- synapseClient:::QueryResult$new('select id, name, parentId from dataset', blockSize=25)
  df <- qr$fetch()
  checkEquals(nrow(df),25)
  checkEquals(ncol(df),3)
  df <- qr$fetch()
  checkEquals(nrow(df),25)
  checkEquals(ncol(df),3)
}

integrationTestQueryResult_Collect <- function() {
  qr <- synapseClient:::QueryResult$new('select id, name, parentId from dataset', blockSize=25)
  df <- qr$collect()
  checkEquals(nrow(df),25)
  checkEquals(ncol(df),3)
  df <- qr$collect()
  checkEquals(nrow(df),50)
  checkEquals(ncol(df),3)
  df <- qr$collect()
  checkEquals(nrow(df),75)
  checkEquals(ncol(df),3)
}

integrationTestQueryResult_CollectAll <- function() {
  qr <- synapseClient:::QueryResult$new('select id, name from dataset limit 40', blockSize=15)
  df2 <- qr$collect()
  df3 <- qr$collectAll()
  checkEquals(nrow(df3), 40)
}


