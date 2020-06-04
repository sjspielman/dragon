test_that("fct_build-legend::build_legend() returns a legend", {
  
  fake_plot <- ggplot2::ggplot(iris) + ggplot2::aes(x = Sepal.Length) + ggplot2::geom_histogram(bins=3)
  edge_styler_na <- list("edge_legend" = FALSE)
  edge_styler_with <- list("edge_legend" = fake_plot)
  node_styler_both <- list("both_legend" = fake_plot)
  node_styler_separate <- list("both_legend" = FALSE, "mineral_legend" = fake_plot, "element_legend" = fake_plot)
  
  ## Only both
  test_legend <- build_legend(edge_styler_na, node_styler_both)
  expect_true(all(class(test_legend) ==  c("gg", "ggplot")))
  
  ## Only separate
  test_legend <- build_legend(edge_styler_na, node_styler_separate)
  expect_true(all(class(test_legend) ==  c("gg", "ggplot")))
  
  ## Edge and both
  test_legend <- build_legend(edge_styler_with, node_styler_both)
  expect_true(all(class(test_legend) ==  c("gg", "ggplot")))
  
  ## Edge and separate
  test_legend <- build_legend(edge_styler_with, node_styler_separate)
  expect_true(all(class(test_legend) ==  c("gg", "ggplot")))

})

