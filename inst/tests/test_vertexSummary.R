#testthat test script for vertex summary functions

if(!exists("dataPath")) dataPath <- "/tmp/rminctestdata/"
# vertexMean, vertexSd, vertexVar, vertexSum 

gftest <- read.csv(sprintf('%s/subject.csv', dataPath))
subjectFile = matrix(data=NA,nrow=10,1)
subjectFile[1,1]  = sprintf('%s/vertex2.txt', dataPath)
subjectFile[2,1]  = sprintf('%s/vertex3.txt', dataPath)
subjectFile[3,1]  = sprintf('%s/vertex4.txt', dataPath)
subjectFile[4,1]  = sprintf('%s/vertex3.txt', dataPath)
subjectFile[5,1]  = sprintf('%s/vertex1.txt', dataPath)
subjectFile[6,1]  = sprintf('%s/vertex2.txt', dataPath)
subjectFile[7,1]  = sprintf('%s/vertex4.txt', dataPath)
subjectFile[8,1]  = sprintf('%s/vertex2.txt', dataPath)
subjectFile[9,1]  = sprintf('%s/vertex3.txt', dataPath)
subjectFile[10,1] = sprintf('%s/vertex1.txt', dataPath)
gftest$testFilesLeft <- (subjectFile)
gftest$testLeft <- t(vertexTable(gftest$testFilesLeft))

context("vertexMean")

#Calculate mean
vm <- verboseRun("vertexMean(gftest$testFilesLeft)",getOption("verbose"))

test_that("vertexMean", {
    expect_equal(mean(gftest$testLeft[,1]), vm[1])
    expect_equal(mean(gftest$testLeft[,2]), vm[2])
    expect_equal(mean(gftest$testLeft[,3]), vm[3])	
})

context("vertexSum")

#Calculate sum

vs <- verboseRun("vertexSum(gftest$testFilesLeft)",getOption("verbose"))

test_that("vertexSum", {
    expect_equal(sum(gftest$testLeft[,1]), vs[1])
    expect_equal(sum(gftest$testLeft[,2]), vs[2])
    expect_equal(sum(gftest$testLeft[,3]), vs[3])	
})

context("vertexVar")

#Calculate variance
vv <- verboseRun("vertexVar(gftest$testFilesLeft)",getOption("verbose"))

test_that("vertexVar", {
    expect_equal(var(gftest$testLeft[,1]), vv[1])
    expect_equal(var(gftest$testLeft[,2]), vv[2])
    expect_equal(var(gftest$testLeft[,3]), vv[3])	
})

context("vertexSd")

#Calculate standard deviation
vsd <- verboseRun("vertexSd(gftest$testFilesLeft)",getOption("verbose"))

test_that("vertexSd", {
    expect_equal(sd(gftest$testLeft[,1]), vsd[1])
    expect_equal(sd(gftest$testLeft[,2]), vsd[2])
    expect_equal(sd(gftest$testLeft[,3]), vsd[3])	
})
