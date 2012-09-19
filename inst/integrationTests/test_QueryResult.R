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

  # test length and names functions
  checkEquals(length(qr), 25)
  checkEquals(class(names(qr)), "character")
  checkEquals(length(names(qr)), 3)
}

integrationTestQueryResult_Collect <- function() {
  qr <- synapseClient:::QueryResult$new('select id, name, parentId from dataset limit 100', blockSize=25)
  df <- qr$collect()
  checkEquals(nrow(df),25)
  checkEquals(ncol(df),3)
  df <- qr$collect()
  checkEquals(nrow(df),25)
  checkEquals(ncol(df),3)
  df <- qr$collect()
  checkEquals(nrow(df),25)
  checkEquals(ncol(df),3)

  df <- qr$as.data.frame()
  checkEquals(nrow(df),75)
  checkEquals(ncol(df),3)
}

integrationTestQueryResult_CollectAll <- function() {
  qr <- synapseClient:::QueryResult$new('select id, name from dataset limit 40', blockSize=15)
  qr$collect()
  df <- qr$collectAll()
  checkEquals(nrow(df), 40)
}


