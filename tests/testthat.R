#Sys.setenv("R_TESTS" = "") ## Workaround for file issues from https://github.com/r-lib/testthat/issues/86

library(testthat)
library(tidyverse)
library(dragon)
source(system.file("dragon/build_network.R", package = "dragon"))

test_check("dragon")
