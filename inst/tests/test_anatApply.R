context("anatApply")

if(!exists("dataPath")) dataPath <- "/tmp/rminctestdata/"

gf <- read.csv(sprintf("%s/CIVET_TEST.csv", dataPath))
gf <- civet.getAllFilenames(gf,"ID","TEST",sprintf("%s/CIVET", dataPath),"TRUE","1.1.12")
gf <- civet.readAllCivetFiles(sprintf("%s/AAL.csv", dataPath),gf)

mm = subset(gf$lobeThickness,gf$Primary.Diagnosis=="ADHD")
mm = mean(mm[,1])

ma <- verboseRun("anatApply(gf$lobeThickness,gf$Primary.Diagnosis)",getOption("verbose"))

test_that("anatApply one output", {
       expect_equal(ma[1], mm) 
})
