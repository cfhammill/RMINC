

if(!exists("dataPath")) dataPath <- "/tmp/rminctestdata/"
gf <- read.csv(sprintf("%s/CIVET_TEST.csv", dataPath))
gf <- civet.getAllFilenames(gf,"ID","POND",sprintf("%s/CIVET", dataPath),"TRUE","1.1.12")
gf <- civet.readAllCivetFiles(sprintf("%s/AAL.csv", dataPath),gf)


context("anatMean")

#Calculate mean

vm <- verboseRun("anatMean(gf$lobeThickness)",getOption("verbose"))


test_that("anatMean", {
    for (j in 1:dim(gf$lobeThickness)[2]) {
    		expect_that(mean(gf$lobeThickness[,j]),is_equivalent_to(vm[j]))
	}
})

context("anatSum")

#Calculate sum
vs <- verboseRun("anatSum(gf$lobeThickness)",getOption("verbose"))

test_that("anatSum", {
    for (j in 1:dim(gf$lobeThickness)[2]) {
    		expect_that(sum(gf$lobeThickness[,j]),is_equivalent_to(vs[j]))
	}	
})

context("anatVar")

#Calculate variance
vv <- verboseRun("anatVar(gf$lobeThickness)",getOption("verbose"))

test_that("anatVar", {
    for (j in 1:dim(gf$lobeThickness)[2]) {
    		expect_that(var(gf$lobeThickness[,j]),is_equivalent_to(vv[j]))
	}	
})


context("anatSd")

#Calculate standard deviation
vsd <- verboseRun("anatSd(gf$lobeThickness)",getOption("verbose"))

test_that("anatSd", {
    for (j in 1:dim(gf$lobeThickness)[2]) {
    		expect_that(sd(gf$lobeThickness[,j]),is_equivalent_to(vsd[j]))
	}	
})

