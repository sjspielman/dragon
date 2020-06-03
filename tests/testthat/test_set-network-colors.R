test_that("fct_set_network_colors::set_cluster_colors() assigns appropriately", {


  cluster_colors2 <- set_cluster_colors("Set2", 2)
  expect_true(length(cluster_colors2) == 2)

  cluster_colors6 <- set_cluster_colors("Set2", 6)
  expect_true(length(cluster_colors6) == 6)

  cluster_colors12 <- set_cluster_colors("Set2", 12)
  expect_true(length(cluster_colors12) == 12)
  

})

    
