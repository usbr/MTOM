context("compare files created from Excel to those created from this package")

library(CRSSIO)

tmpDir <- tempdir()
dir.create(tmpDir)
m1Folder <- file.path(tmpDir, "method1")
m2Folder <- file.path(tmpDir, "method2")
testthat::setup(dir.create(m1Folder))
testthat::setup(dir.create(m2Folder))
teardown(unlink(tmpDir, recursive = TRUE))

test_that("created files from package and Excel match", {
  skip_on_cran()
  skip_on_travis()
  skip_on_appveyor()
  
  xlIn <- "../../data-raw/NaturalFlows1906-2015_withExtensions_8.14.2017.xlsx"
  if (!file.exists(xlIn))
    stop("You need to update the xlIn variable in test_data.R.")
  
  crssi_create_dnf_files(
    "CoRiverNF",
    oFolder = file.path(tmpDir, "method1"),
    startYear = 2018, 
    endYear = 2060
  )
  
  expect_warning(crssi_create_dnf_files(
    xlIn,
    oFolder = file.path(tmpDir, "method2"),
    startYear = 2018, 
    endYear = 2060
  ))
  
  dirs <- list.dirs(m1Folder, recursive = FALSE, full.names = FALSE)
  
  for(curDir in dirs){
    allFiles <- list.files(file.path(m1Folder, curDir))
    for(ff in allFiles){
      test_that("all files are the same", {
        expect_identical(
          scan(file.path(m1Folder, curDir, ff), what = "character", quiet = TRUE),
          scan(file.path(m2Folder, curDir, ff), what = "character", quiet = TRUE),
          info = paste(curDir, ff)
        )
      })
    }
  }
  
})





