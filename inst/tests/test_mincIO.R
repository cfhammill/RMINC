context("MINC I/O reading")

if(!exists("dataPath")) dataPath <- "/tmp/rminctestdata/"

# silence the output of mincLm, in order to make the test output information is more clear to read

testCommand <- sprintf("mincGetVolume(\"%s/brain_cut_out.mnc\")", dataPath)
print(testCommand)
testfile <- verboseRun(testCommand, getOption("verbose"))

testCommand <- sprintf("mincextract  -start 0,0,0 -count 1,1,1 %s/brain_cut_out.mnc", dataPath)

mincextract_output_voxel_0_0_0 <- 
  as.numeric(system(testCommand, intern=TRUE))


mask_10_10_10 <- verboseRun(sprintf("mincGetVolume(\"%s/mask_at_voxel_10_10_10.mnc\")", dataPath),getOption("verbose"))
mask_10_31_33 <- verboseRun(sprintf("mincGetVolume(\"%s/mask_at_voxel_10_31_33.mnc\")", dataPath),getOption("verbose"))
mask_37_40_28 <- verboseRun(sprintf("mincGetVolume(\"%s/mask_at_voxel_37_40_28.mnc\")", dataPath),getOption("verbose"))

mincextract_output_voxel_10_10_10 <- as.numeric(system("mincextract  -start 10,10,10 -count 1,1,1 %s/brain_cut_out.mnc", intern=TRUE))
mincextract_output_voxel_10_31_33 <- as.numeric(system("mincextract  -start 10,31,33 -count 1,1,1 %s/brain_cut_out.mnc", intern=TRUE))
mincextract_output_voxel_37_40_28 <- as.numeric(system("mincextract  -start 37,40,28 -count 1,1,1 %s/brain_cut_out.mnc", intern=TRUE))

test_that("mincGetVolume extracts the same value as mincextract", {
    expect_that(testfile[1], equals(mincextract_output_voxel_0_0_0))
    expect_that(testfile[mask_10_10_10 > 0.5], equals(mincextract_output_voxel_10_10_10))
    expect_that(testfile[mask_10_31_33 > 0.5], equals(mincextract_output_voxel_10_31_33))
    expect_that(testfile[mask_37_40_28 > 0.5], equals(mincextract_output_voxel_37_40_28))
})

#
#
#
context("MINC I/O writing")
verboseRun("mincWriteVolume(testfile, \"/tmp/write_out_of_RMINC_test_bed_MINC_IO.mnc\", clobber=TRUE)",getOption("verbose"))

reread_mincextract_output_voxel_0_0_0 <- as.numeric(system("mincextract  -start 0,0,0 -count 1,1,1 /tmp/write_out_of_RMINC_test_bed_MINC_IO.mnc", intern=TRUE))
reread_mincextract_output_voxel_10_10_10 <- as.numeric(system("mincextract  -start 10,10,10 -count 1,1,1 /tmp/write_out_of_RMINC_test_bed_MINC_IO.mnc", intern=TRUE))
reread_mincextract_output_voxel_10_31_33 <- as.numeric(system("mincextract  -start 10,31,33 -count 1,1,1 /tmp/write_out_of_RMINC_test_bed_MINC_IO.mnc", intern=TRUE))
reread_mincextract_output_voxel_37_40_28 <- as.numeric(system("mincextract  -start 37,40,28 -count 1,1,1 /tmp/write_out_of_RMINC_test_bed_MINC_IO.mnc", intern=TRUE))

test_that("mincGetVolume extracts the same value as mincextract", {
    expect_that(object = reread_mincextract_output_voxel_0_0_0, equals(mincextract_output_voxel_0_0_0, tolerance = 0.0001))
    expect_that(reread_mincextract_output_voxel_10_10_10, equals(mincextract_output_voxel_10_10_10, tolerance = 0.0001))
    expect_that(reread_mincextract_output_voxel_10_31_33, equals(mincextract_output_voxel_10_31_33, tolerance = 0.0001))
    expect_that(reread_mincextract_output_voxel_37_40_28, equals(mincextract_output_voxel_37_40_28, tolerance = 0.0001))
})

#
#
#
context("MINC I/O read from write")
reread_testfile <- verboseRun("mincGetVolume(\"/tmp/write_out_of_RMINC_test_bed_MINC_IO.mnc\")",getOption("verbose"))

test_that("mincGetVolume extracts the same value as mincextract", {
    expect_that(reread_testfile[1], equals(reread_mincextract_output_voxel_0_0_0))
    expect_that(reread_testfile[mask_10_10_10 > 0.5], equals(reread_mincextract_output_voxel_10_10_10))
    expect_that(reread_testfile[mask_10_31_33 > 0.5], equals(reread_mincextract_output_voxel_10_31_33))
    expect_that(reread_testfile[mask_37_40_28 > 0.5], equals(reread_mincextract_output_voxel_37_40_28))
})

testfile[1] = NA
test_that("mincWriteVolume stops when na/nans/infs are present", {
    expect_error(mincWriteVolume(testfile, "/tmp/write_out_of_RMINC_test_bed_MINC_IO.mnc", clobber=TRUE))
})



# remove temp file
system("rm -f /tmp/write_out_of_RMINC_test_bed_MINC_IO.mnc")

#
#
#
context("MINC I/O determine volume of segmentation - anatGetAll using jacobians")

filenames <- read.csv(sprintf("%s/filenames.csv", dataPath))
volumes <- anatGetAll(filenames=filenames$absolute_jacobian, atlas=sprintf("%s/test_segmentation.mnc", dataPath), method="jacobians",defs=sprintf("%s/test_defs.csv", dataPath))

# calculate the volume of a structure using minc tools:
# 1) get the exponent of the absolute jacobian file (these are log values)
# 2) sum over a certain area
# 3) multiply that by the voxel size

system("mincmath -clobber -quiet -exp %s/absolute_jacobian_file_1.mnc %s/RMINC_test_bed_MINC_IO_absolute_jacobian_file_1_exponent.mnc")
system("mincmath -clobber -quiet -exp %s/absolute_jacobian_file_2.mnc %s/RMINC_test_bed_MINC_IO_absolute_jacobian_file_2_exponent.mnc") 
system("mincmath -clobber -quiet -exp %s/absolute_jacobian_file_3.mnc %s/RMINC_test_bed_MINC_IO_absolute_jacobian_file_3_exponent.mnc") 

# left (180) and right (181) parieto-temporal lobe
left_parieto_sum_file_1 <- as.numeric(system("mincstats -quiet -sum -mask %s/test_segmentation.mnc  -mask_bin 187 %s/RMINC_test_bed_MINC_IO_absolute_jacobian_file_1_exponent.mnc", intern=TRUE))
left_parieto_sum_file_2 <- as.numeric(system("mincstats -quiet -sum -mask %s/test_segmentation.mnc  -mask_bin 187 %s/RMINC_test_bed_MINC_IO_absolute_jacobian_file_2_exponent.mnc", intern=TRUE))
left_parieto_sum_file_3 <- as.numeric(system("mincstats -quiet -sum -mask %s/test_segmentation.mnc  -mask_bin 187 %s/RMINC_test_bed_MINC_IO_absolute_jacobian_file_3_exponent.mnc", intern=TRUE))
right_parieto_sum_file_1 <- as.numeric(system("mincstats -quiet -sum -mask %s/test_segmentation.mnc  -mask_bin 181 %s/RMINC_test_bed_MINC_IO_absolute_jacobian_file_1_exponent.mnc", intern=TRUE))
right_parieto_sum_file_2 <- as.numeric(system("mincstats -quiet -sum -mask %s/test_segmentation.mnc  -mask_bin 181 %s/RMINC_test_bed_MINC_IO_absolute_jacobian_file_2_exponent.mnc", intern=TRUE))
right_parieto_sum_file_3 <- as.numeric(system("mincstats -quiet -sum -mask %s/test_segmentation.mnc  -mask_bin 181 %s/RMINC_test_bed_MINC_IO_absolute_jacobian_file_3_exponent.mnc", intern=TRUE))

# pons (187) has no separate label for left and right 
pons_sum_file_1 <- as.numeric(system("mincstats -quiet -sum -mask %s/test_segmentation.mnc  -mask_bin 250 %s/RMINC_test_bed_MINC_IO_absolute_jacobian_file_1_exponent.mnc", intern=TRUE))
pons_sum_file_2 <- as.numeric(system("mincstats -quiet -sum -mask %s/test_segmentation.mnc  -mask_bin 250 %s/RMINC_test_bed_MINC_IO_absolute_jacobian_file_2_exponent.mnc", intern=TRUE))
pons_sum_file_3 <- as.numeric(system("mincstats -quiet -sum -mask %s/test_segmentation.mnc  -mask_bin 250 %s/RMINC_test_bed_MINC_IO_absolute_jacobian_file_3_exponent.mnc", intern=TRUE))

voxel_volume <- 1

left_parieto_volume_file_1 <- left_parieto_sum_file_1 * voxel_volume
left_parieto_volume_file_2 <- left_parieto_sum_file_2 * voxel_volume
left_parieto_volume_file_3 <- left_parieto_sum_file_3 * voxel_volume
right_parieto_volume_file_1 <- right_parieto_sum_file_1 * voxel_volume
right_parieto_volume_file_2 <- right_parieto_sum_file_2 * voxel_volume
right_parieto_volume_file_3 <- right_parieto_sum_file_3 * voxel_volume
pons_volume_file_1 <- pons_sum_file_1 * voxel_volume
pons_volume_file_2 <- pons_sum_file_2 * voxel_volume
pons_volume_file_3 <- pons_sum_file_3 * voxel_volume


test_that("anatGetAll extracts the same value as a combination of mincmath and mincstats", {
    expect_that(volumes[, "left Label27"][1], equals(left_parieto_volume_file_1, tolerance = 0.0001))
    expect_that(volumes[, "left Label27"][2], equals(left_parieto_volume_file_2, tolerance = 0.0001))
    expect_that(volumes[, "left Label27"][3], equals(left_parieto_volume_file_3, tolerance = 0.0001))

    expect_that(volumes[, "right Label27"][1], equals(right_parieto_volume_file_1, tolerance = 0.0001))
    expect_that(volumes[, "right Label27"][2], equals(right_parieto_volume_file_2, tolerance = 0.0001))
    expect_that(volumes[, "right Label27"][3], equals(right_parieto_volume_file_3, tolerance = 0.0001))

    expect_that(volumes[, "Label34"][1], equals(pons_volume_file_1, tolerance = 0.0001))
    expect_that(volumes[, "Label34"][2], equals(pons_volume_file_2, tolerance = 0.0001))
    expect_that(volumes[, "Label34"][3], equals(pons_volume_file_3, tolerance = 0.0001))
})


#
#
#
context("MINC I/O determine volume of segmentation - anatCombineStructures using jacobians")

volumes_combined <- anatCombineStructures(vols=volumes, method="jacobians",defs=sprintf("%s/test_defs.csv", dataPath))

test_that("anatGetAll extracts the same value as a combination of mincmath and mincstats", {
    expect_that(volumes_combined[, "Label27"][1], equals(left_parieto_volume_file_1 + right_parieto_volume_file_1, tolerance = 0.0001))
    expect_that(volumes_combined[, "Label27"][2], equals(left_parieto_volume_file_2 + right_parieto_volume_file_2, tolerance = 0.0001))
    expect_that(volumes_combined[, "Label27"][3], equals(left_parieto_volume_file_3 + right_parieto_volume_file_3, tolerance = 0.0001))

    expect_that(volumes_combined[, "Label34"][1], equals(pons_volume_file_1, tolerance = 0.0001))
    expect_that(volumes_combined[, "Label34"][2], equals(pons_volume_file_2, tolerance = 0.0001))
    expect_that(volumes_combined[, "Label34"][3], equals(pons_volume_file_3, tolerance = 0.0001))
})


