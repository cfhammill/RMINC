context("vertexAnova")

if(!exists("dataPath")) dataPath <- "/tmp/rminctestdata/"

gftest <- read.csv(sprintf('%s/subject.csv', dataPath))
subjectFile = matrix(data=NA,nrow=10,1)
subjectFile[1,1] = sprintf('%s/vertex2.txt', dataPath)
subjectFile[2,1] = sprintf('%s/vertex3.txt', dataPath)
subjectFile[3,1] = sprintf('%s/vertex4.txt', dataPath)
subjectFile[4,1] = sprintf('%s/vertex3.txt', dataPath)
subjectFile[5,1] = sprintf('%s/vertex1.txt', dataPath)
subjectFile[6,1] = sprintf('%s/vertex2.txt', dataPath)
subjectFile[7,1] = sprintf('%s/vertex4.txt', dataPath)
subjectFile[8,1] = sprintf('%s/vertex2.txt', dataPath)
subjectFile[9,1] = sprintf('%s/vertex3.txt', dataPath)
subjectFile[10,1] = sprintf('%s/vertex1.txt', dataPath)
gftest$testFilesLeft <- (subjectFile)


rmincAnova <- verboseRun("vertexAnova(testFilesLeft ~ Sex,gftest)",getOption("verbose"))
gftest$testLeft = t(vertexTable(gftest$testFilesLeft))
rAnova = anova(lm(testLeft[,1]~Sex,gftest))

test_that("vertexAnova Two Factors",{
	expect_that(rmincAnova[1,1],is_equivalent_to(rAnova$F[1]))
	expect_that(attr(rmincAnova,"df")[[1]][2],is_equivalent_to(rAnova$Df[2]))
	expect_that(attr(rmincAnova,"df")[[1]][1],is_equivalent_to(rAnova$Df[1]))
})

rmincAnova <- verboseRun("vertexAnova(testFilesLeft ~ Age*Sex,gftest)",getOption("verbose"))
gftest$testLeft = t(vertexTable(gftest$testFilesLeft))
rAnova = anova(lm(testLeft[,1]~Age*Sex,gftest))

test_that("vertexAnova Interaction",{
	expect_that(rmincAnova[1,1],is_equivalent_to(rAnova$F[1]))
	expect_that(rmincAnova[1,2],is_equivalent_to(rAnova$F[2]))
	expect_that(rmincAnova[1,3],is_equivalent_to(rAnova$F[3]))
	expect_that(attr(rmincAnova,"df")[[1]][1],is_equivalent_to(rAnova$Df[1]))
	expect_that(attr(rmincAnova,"df")[[1]][2],is_equivalent_to(rAnova$Df[4]))
	expect_that(attr(rmincAnova,"df")[[2]][1],is_equivalent_to(rAnova$Df[2]))
	expect_that(attr(rmincAnova,"df")[[2]][2],is_equivalent_to(rAnova$Df[4]))
	expect_that(attr(rmincAnova,"df")[[3]][1],is_equivalent_to(rAnova$Df[3]))
	expect_that(attr(rmincAnova,"df")[[3]][2],is_equivalent_to(rAnova$Df[4]))
})

rmincAnova <- verboseRun("vertexAnova(testFilesLeft ~ Group,gftest)",getOption("verbose"))
gftest$testLeft = t(vertexTable(gftest$testFilesLeft))
rAnova = anova(lm(testLeft[,1]~Group,gftest))

test_that("vertexAnova Three Factors",{
	expect_that(attr(rmincAnova,"df")[[1]][2],is_equivalent_to(rAnova$Df[2]))
	expect_that(attr(rmincAnova,"df")[[1]][1],is_equivalent_to(rAnova$Df[1]))
})
rmincAnova <- verboseRun("vertexAnova(testFilesLeft ~ Age*Group,gftest)",getOption("verbose"))

gftest$testLeft = t(vertexTable(gftest$testFilesLeft))
rAnova = anova(lm(testLeft[,1]~Age*Group,gftest))

test_that("vertexAnova Three Factors Interaction",{
         expect_that(rmincAnova[1,1],is_equivalent_to(rAnova$F[1]))
         expect_that(rmincAnova[1,2],is_equivalent_to(rAnova$F[2]))
         expect_that(rmincAnova[1,3],is_equivalent_to(rAnova$F[3]))
         expect_that(attr(rmincAnova,"df")[[1]][1],is_equivalent_to(rAnova$Df[1]))
         expect_that(attr(rmincAnova,"df")[[1]][2],is_equivalent_to(rAnova$Df[4]))
         expect_that(attr(rmincAnova,"df")[[2]][1],is_equivalent_to(rAnova$Df[2]))
         expect_that(attr(rmincAnova,"df")[[2]][2],is_equivalent_to(rAnova$Df[4]))
         expect_that(attr(rmincAnova,"df")[[3]][1],is_equivalent_to(rAnova$Df[3]))
         expect_that(attr(rmincAnova,"df")[[3]][2],is_equivalent_to(rAnova$Df[4])) 
})





