# TODO: Add comment
# 
# Author: furia
###############################################################################

unitTestPropertyNames <-
  function()
{
  ps <- synapseClient:::TypedPropertyStore()
  ps <- synapseClient:::setProperty(ps, "aProp", "aVal")
  
  checkEquals(propertyNames(ps), "aProp")
}

unitTestClearProperty <-
  function()
{
  ps <- synapseClient:::TypedPropertyStore()
  ps <- synapseClient:::setProperty(ps, "aProp", "aVal")
  
  ps <- synapseClient:::setUpdatePropValue(ps, "aProp")
  checkEquals(propertyNames(ps), character()) 
}

unitTestGetSetProperty <-
  function()
{
  ps <- synapseClient:::TypedPropertyStore()
  ps <- synapseClient:::setProperty(ps, "aProp", "aVal")
  checkEquals(synapseClient:::propertyType(ps, "aProp"), "stringAnnotations")
  checkEquals(synapseClient:::getProperty(ps, "aProp"), "aVal")
  checkEquals(class(synapseClient:::getProperty(ps, "aProp")), "character")
  
  ps <- synapseClient:::setProperty(ps, "aProp", 1L)
  checkEquals(synapseClient:::propertyType(ps, "aProp"), "longAnnotations")
  checkEquals(synapseClient:::getProperty(ps, "aProp"), 1L)
  checkEquals(class(synapseClient:::getProperty(ps, "aProp")), "integer")
  
  ps <- synapseClient:::setProperty(ps, "aProp", 1.0)
  checkEquals(synapseClient:::propertyType(ps, "aProp"), "doubleAnnotations")
  checkEquals(synapseClient:::getProperty(ps, "aProp"), 1.0)
  checkEquals(class(synapseClient:::getProperty(ps, "aProp")), "numeric")
  
  ps <- synapseClient:::setProperty(ps, "aProp", 2.0)
  checkEquals(synapseClient:::propertyType(ps, "aProp"), "doubleAnnotations")
  checkEquals(synapseClient:::getProperty(ps, "aProp"), 2.0)
  checkEquals(class(synapseClient:::getProperty(ps, "aProp")), "numeric")
  
  now <- Sys.time()
  ps <- synapseClient:::setProperty(ps, "aProp", now)
  checkEquals(synapseClient:::propertyType(ps, "aProp"), "dateAnnotations")
  checkTrue("POSIXct" %in% class(synapseClient:::getProperty(ps, "aProp")))
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



