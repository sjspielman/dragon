Sys.setenv("R_TESTS" = "") ## Workaround for file issues from https://github.com/r-lib/testthat/issues/86

library(testthat)
library(tidyverse)
library(dragon)

build_net <- system.file("inst/dragon/", "build_network.R", package = "dragon")
print(build_net)
source(build_net)
test_check("dragon")
