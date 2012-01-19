unitTestCheckSingleValue <-
	function()
{
	val <- 5
	checkTrue(synapseClient:::checkInteger(val))

	val <- 5L
	checkTrue(synapseClient:::checkInteger(val))

	val <- "5"
	checkTrue(synapseClient:::checkInteger(val))

	val <- 5.0000000
	checkTrue(synapseClient:::checkInteger(val))

	val <- "5L"
	checkTrue(synapseClient:::checkInteger(val))

	val <- 5 + .Machine$double.eps
	checkTrue(synapseClient:::checkInteger(val))

	val <- 5 - .Machine$double.eps
        checkTrue(synapseClient:::checkInteger(val))

	val <- -5
        checkTrue(synapseClient:::checkInteger(val))


	val <- "5l"
        checkTrue(!synapseClient:::checkInteger(val))

	val <- "A"
        checkTrue(!synapseClient:::checkInteger(val))

	val <- Dataset(list(name="foo"))
        checkTrue(!synapseClient:::checkInteger(val))

        val <- sum
        checkTrue(!synapseClient:::checkInteger(val))

	val <- 5.00001
	checkTrue(!synapseClient:::checkInteger(val))
}

unitTestFactors <-
	function()
{
	val <- factor(c('a','b','c'))
	checkTrue(all(!synapseClient:::checkInteger(val)))
}

unitTestS4Class <-
	function()
{
	checkTrue(!synapseClient:::checkInteger(new("Dataset")))
}

unitTestMultipleValues <-
	function()
{
	checkTrue(all(synapseClient:::checkInteger(1:1000)))
	checkTrue(all(!synapseClient:::checkInteger(c('a','ab','foo','d'))))
	val <- factor(c('a','b','c'))
		
}

unitTestMultipleValuesMixedTypes <-
	function()
{
	val <- c(1, 3.001, "a", 6, 7, sum, Dataset(list(name="foo")))
	res <- c(TRUE, FALSE, FALSE, TRUE, TRUE, FALSE, FALSE)
	checkTrue(all(synapseClient:::checkInteger(val) == res))
}

unitTestNamedList <-
	function()
{
	checkTrue(!synapseClient:::checkInteger(RJSONIO:::emptyNamedList))

	val <- list()
	val[[2]] <- RJSONIO::emptyNamedList
	res <- c(FALSE, FALSE)
	checkTrue(all(res == synapseClient:::checkInteger(val)))


}
