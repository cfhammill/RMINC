context("civet.readAllCivetFiles")

if(!exists("dataPath")) dataPath <- "/tmp/rminctestdata/"

gf = read.csv(sprintf("%s/CIVET_TEST.csv", dataPath))
gf = read.csv(sprintf("%s/CIVET_TEST.csv", dataPath))
gf = civet.getAllFilenames(gf,"ID","TEST",sprintf("%s/CIVET", dataPath),"TRUE","1.1.12")
gf = civet.getAllFilenames(gf,"ID","TEST",sprintf("%s/CIVET", dataPath),"TRUE","1.1.12")
gf = civet.readAllCivetFiles(sprintf("%s/AAL.csv", dataPath),gf)
gf = civet.readAllCivetFiles(sprintf("%s/AAL.csv", dataPath),gf)
dataFile = gf$lobeThickness
AALAtlas = read.csv(sprintf("%s/AAL.csv", dataPath))
AALAtlas = read.csv(sprintf("%s/AAL.csv", dataPath))
verticesL = read.csv(sprintf("%s/AAL_atlas_left.txt", dataPath),header = FALSE)
verticesL = read.csv(sprintf("%s/AAL_atlas_left.txt", dataPath),header = FALSE)
verticesR = read.csv(sprintf("%s/AAL_atlas_right.txt", dataPath),header = FALSE)
verticesR = read.csv(sprintf("%s/AAL_atlas_right.txt", dataPath),header = FALSE)
reducedVertices = c(verticesL[0:40961,1],verticesR[0:40961,1])

atlasIndex = pmatch(names(dataFile[1,1]),AALAtlas[,3])
reducedVerticesIndices = which(reducedVertices == AALAtlas[atlasIndex,1],arr.ind=FALSE)
meanThicknessFromVertexFile = mean(gf$nativeRMS_RSLtlink[1,reducedVerticesIndices])

test_that("Mean Thickness from Vertex File is the same as thickness from Anat File",{
	expect_that(meanThicknessFromVertexFile,equals(gf$lobeThickness[[1,1]],tolerance = 0.01))
})

