suppressPackageStartupMessages({
  library(testthat)
  library(dplyr)
})

context("vertex prediction errors")

getRMINCTestData()
dataPath <- file.path(tempdir(), "rminctestdata/")

gftest <- read.csv(file.path(dataPath, "subject.csv"))
gftest$testfiles <- file.path(dataPath
                              , paste0("vertex", 1:5, ".txt")) 

vertexTable <- vertexTable(gftest$testfiles)

test_env <- new.env()

test_that("vertex_mlm works", {
  evalq({
    mlm <- RMINC:::vertex_mlm(testfiles ~ Age, gftest)
    slm <- apply(vertexTable, 1, function(col) lm(col ~ Age, data = gftest))
    
    expect_equal(mlm$coefficients, sapply(slm, coef))
  }, test_env)
})

test_that("prediction_error_mlm works", {
  evalq({
    
    ## Check that predictions are identical
    mlm_pt <- 
      lapply(seq_len(ncol(vertexTable))
             , function(i) RMINC:::prediction_error_mlm(mlm, gftest[i,], vertexTable[,i]))
    
    preds <- sapply(mlm_pt, function(tbl) tbl$pred)
    preds_ref <- sapply(seq_len(ncol(vertexTable)), function(i) predict(mlm, gftest[i,]))
    
    expect_equal(preds, preds_ref)
    
    ## Check that prediction errors are identical
    get_pred_err <- # Extract half the prediction interval divided by 2.5% t-crit (pred err)
      function(mod, newdata){
        predict(mod, newdata, se.fit = TRUE, interval = "prediction") %>%
        { (.$fit[1] - .$fit[2]) / qt(.975, .$df) }
      } 
    
    # Get the prediction error for each indiv from each model
    pred_err_ref <-
      sapply(slm, function(mod)
        sapply(seq_len(ncol(vertexTable)), function(i)
          get_pred_err(mod, gftest[i,])))
    
    pred_err <- sapply(mlm_pt, function(tbl) tbl$pred_err)
    
    expect_equal(t(pred_err), pred_err_ref)
    
    
    ## Check standardized pred error is (prediction - real) / pred_err
    pred_tbl <- mlm_pt %>% bind_rows
    expect_equal(with(pred_tbl, (real - pred) / pred_err), pred_tbl$std_pred)
    
  }, test_env)
})