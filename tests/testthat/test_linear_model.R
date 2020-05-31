initialized <- initialize_network("B", age_range = c(0,5))
clustered   <- specify_community_detect_network(initialized$graph, initialized$nodes)
clustered$nodes %>%
  dplyr::filter(group == "mineral") %>%
  dplyr::select(cluster_ID, network_degree_norm, closeness, num_localities, max_age, mean_pauling, cov_pauling) %>% #sd_pauling
  dplyr::mutate(cluster_ID = factor(cluster_ID)) %>%
  dplyr::rename(!! variable_to_title[["network_degree_norm"]]  := network_degree_norm,
                !! variable_to_title[["closeness"]] := closeness,
                !! variable_to_title[["mean_pauling"]] := mean_pauling,
                !! variable_to_title[["cov_pauling"]] := cov_pauling,
                !! variable_to_title[["num_localities"]] := num_localities,
                !! variable_to_title[["max_age"]] := max_age) -> mineral_nodes  
point_color <- "red" 
point_size <- 2.33 
bestfit_color <- "orange"
n_clusters <- length(unique(clustered$nodes$cluster_ID))


test_that("fct_run_linear_models::fit_linear_model() with numeric predictor", {
  predictor <- variable_to_title[["mean_pauling"]]
  response <- variable_to_title[["num_localities"]]
  test_fitted <- fit_linear_model(response, predictor, mineral_nodes)

  expect_equal(sort(names(test_fitted)), sort(c("model_fit", "tukey_fit", "tukey_ok_variance"))  )
  expect_true(test_fitted$tukey_ok_variance)
  expect_true(is.null(test_fitted$tukey_table))
  
  model_fit_table <- test_fitted$model_fit
  expect_equal(sort(names(model_fit_table)), sort(c("Coefficient", "Coefficient estimate", "Standard error", "t-statistic", "P-value")))
  expect_true(nrow(model_fit_table) == 2)
  
})


test_that("fct_run_linear_models::fit_linear_model() with cluster predictor", {
  predictor <- variable_to_title[["cluster_ID"]]
  response <- variable_to_title[["num_localities"]]
  test_fitted <- fit_linear_model(response, predictor, mineral_nodes)

  expect_equal(sort(names(test_fitted)), sort(c("model_fit", "tukey_fit", "tukey_ok_variance"))  )
  expect_true(is.logical(test_fitted$tukey_ok_variance))
  expect_true(is.null(test_fitted$tukey_table))
  
  model_fit_table <- test_fitted$model_fit
  expect_equal(sort(names(model_fit_table)), sort(c("Coefficient", "Coefficient estimate", "Standard error", "t-statistic", "P-value")))
  expect_true(nrow(model_fit_table) == n_clusters)
  
  tukey_table <- test_fitted$tukey_fit
  expect_equal(sort(names(tukey_table)), sort(c("Cluster Comparison", "Estimated effect size difference", "95% CI Lower bound", "95% CI Upper bound", "Adjusted P-value")))
  mineral_nodes %>% dplyr::count(cluster_ID) %>% nrow() -> n_clusters
  expected_tukey_rows <- (n_clusters * (n_clusters - 1)) / 2
  expect_true(nrow(tukey_table) == expected_tukey_rows)

})


test_that("fct_run_linear_models::plot_linear_model() with numeric predictor", {
  predictor <- variable_to_title[["mean_pauling"]]
  response <- variable_to_title[["num_localities"]]

  test_plotted <- plot_linear_model(response, predictor, mineral_nodes, FALSE, FALSE, point_color, point_size, TRUE, bestfit_color, NA)
  
  plot_data_geom_point <- ggplot2::ggplot_build(test_plotted)$data[[1]]
  plot_data_geom_smooth <- ggplot2::ggplot_build(test_plotted)$data[[2]]
  expect_true(all(plot_data_geom_point$colour == point_color))
  expect_true(all(plot_data_geom_point$size == point_size))
  expect_true(all(plot_data_geom_smooth$colour == bestfit_color))

})


test_that("fct_run_linear_models::plot_linear_model() with cluster predictor", {
  predictor <- variable_to_title[["cluster_ID"]]
  response <- variable_to_title[["num_localities"]]
  cluster_colors <- clustered$cluster_colors
  test_plotted <- plot_linear_model(response, predictor, mineral_nodes, FALSE, FALSE, point_color, point_size, TRUE, bestfit_color, cluster_colors)
  
  plot_data_geom_point <- ggplot2::ggplot_build(test_plotted)$data[[1]]
  plot_data_geom_smooth <- ggplot2::ggplot_build(test_plotted)$data[[2]]
  expect_true(all(plot_data_geom_point$colour %in% cluster_colors))
  expect_true(all(plot_data_geom_point$size == point_size))

})





