# TODO: Add comment
# 
# Author: furia
###############################################################################

unitTestPropertyNames <-
  function()
{
  ps <- TypedPropertyStore()
  ps <- setProperty(ps, "aProp", "aVal")
  
  checkEquals(propertyNames(ps), "aProp")
}

unitTestClearProperty <-
  function()
{
  ps <- TypedPropertyStore()
  ps <- setProperty(ps, "aProp", "aVal")
  
  ps <- setUpdatePropValue(ps, "aProp")
  checkEquals(propertyNames(ps), character()) 
}

unitTestGetSetProperty <-
  function()
{
  ps <- TypedPropertyStore()
  ps <- setProperty(ps, "aProp", "aVal")
  checkEquals(propertyType(ps, "aProp"), "stringAnnotations")
  checkEquals(getProperty(ps, "aProp"), "aVal")
  checkEquals(class(getProperty(ps, "aProp")), "character")
  
  ps <- setProperty(ps, "aProp", 1L)
  checkEquals(propertyType(ps, "aProp"), "longAnnotations")
  checkEquals(getProperty(ps, "aProp"), 1L)
  checkEquals(class(getProperty(ps, "aProp")), "integer")
  
  ps <- setProperty(ps, "aProp", 1.0)
  checkEquals(propertyType(ps, "aProp"), "doubleAnnotations")
  checkEquals(getProperty(ps, "aProp"), 1.0)
  checkEquals(class(getProperty(ps, "aProp")), "numeric")
  
  ps <- setProperty(ps, "aProp", 2.0)
  checkEquals(propertyType(ps, "aProp"), "doubleAnnotations")
  checkEquals(getProperty(ps, "aProp"), 2.0)
  checkEquals(class(getProperty(ps, "aProp")), "numeric")
  
  now <- Sys.time()
  ps <- setProperty(ps, "aProp", now)
  checkEquals(propertyType(ps, "aProp"), "dateAnnotations")
  checkTrue("POSIXct" %in% class(getProperty(ps, "aProp")))
}

unitTestConstructors <-
  function()
{
  warning("need tests for PropertyStore Constructors")
}

unitTestPassByCopy <-
  function()
{
  warning("need tests for PropertyStore Pass by copy")
}



