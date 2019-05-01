#library(testthat)
#library(shinytest)

####### FORTHCOMING ADDITION OF SHINYTEST TESTS.
####### Below are various copy/pastes from shinytest documentation for quick reference 

#test_that("Application works", {
  # Use compareImages=FALSE because the expected image screenshots were created
  # on a Mac, and they will differ from screenshots taken on the CI platform,
  # which runs on Linux.
#  expect_pass(testApp("../../inst/dragon", compareImages = FALSE))
#})

# 
# If you develop on Linux and create the expected test results on Linux, then you should be able to leave out the compareImages argument; it defaults to TRUE.
# 

#############################################

# context("app-file")
# # This file is for testing the applications in the inst/ directory.
# 
# library(shinytest)
# 
# test_that("sampleapp works", {
#   # Don't run these tests on the CRAN build servers
#   # WHY: If you will submit the package to CRAN, it is best to configure the application tests to not run on the CRAN build servers. 
#   # If you are using testthat, use skip_on_cran() in the test block, as in the example.
#   skip_on_cran()
# 
#   # Use compareImages=FALSE because the expected image screenshots were created
#   # on a Mac, and they will differ from screenshots taken on the CI platform,
#   # which runs on Linux.
#   appdir <- system.file(package = "shinytestPackageExample", "sampleapp")
#   expect_pass(testApp(appdir, compareImages = FALSE))
# })