context("vertexApply")

if(!exists("dataPath")) dataPath <- "/tmp/rminctestdata/"

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


mm <- verboseRun("vertexMean(gftest$testFilesLeft)",getOption("verbose"))
ma <- verboseRun("vertexApply(gftest$testFilesLeft,quote(mean(x)))",getOption("verbose"))

test_that("vertexApply one output",{
     for (nVox in 1:length(mm)) {
           expect_equal(ma[nVox], mm[nVox]) }
 })
 
# Need to define global variable when running tests, but normally do not...
testFunc <<- function (x) { return(c(1,2))}
ma <- verboseRun("vertexApply(gftest$testFilesLeft,quote(testFunc(x)))",getOption("verbose"))

test_that("vertexApply two output",{
    for (nVox in 1:dim(ma)[1]) {
 	   expect_equal(ma[nVox,1], 1) 
	   expect_equal(ma[nVox,2], 2)}
})

