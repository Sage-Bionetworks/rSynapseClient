## test QueryResult object
## 
## Author: J. Christopher Bare <chris.bare@sagebase.org>
###############################################################################

integrationTestQueryResult_nextBlock <- function() {
  qr <- synapseClient:::QueryResult$new('select id, name, parentId from dataset', blockSize=25)
  df <- qr$nextBlock()
  checkEquals(nrow(df),25)
  checkEquals(ncol(df),3)
  df <- qr$nextBlock()
  checkEquals(nrow(df),25)
  checkEquals(ncol(df),3)
}

integrationTestQueryResult_Fetch <- function() {
  qr <- synapseClient:::QueryResult$new('select id, name, parentId from dataset', blockSize=25)
  df <- qr$fetch()
  checkEquals(nrow(df),25)
  checkEquals(ncol(df),3)
  df <- qr$fetch()
  checkEquals(nrow(df),50)
  checkEquals(ncol(df),3)
  df <- qr$fetch()
  checkEquals(nrow(df),75)
  checkEquals(ncol(df),3)
}

integrationTestQueryResult_FetchAll <- function() {
	## as of now, there's only 35 code entities. This test will
	## likely have to change if that number gets really large
  qr <- synapseClient:::QueryResult$new('select id from code', blockSize=20)
  df <- qr$nextBlock()
  checkTrue(nrow(df) > 20)

  qr <- synapseClient:::QueryResult$new('select id from code', blockSize=10)
  df2 <- qr$fetch()
  df3 <- qr$fetchAll()
  checkEquals(nrow(df3), nrow(df))
  checkEquals(df3$dataset.id, df$dataset.id)
}


