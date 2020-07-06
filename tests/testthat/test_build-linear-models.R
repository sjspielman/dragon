test_that("fct_run_linear_models::fit_linear_model() with numeric predictor", {
  predictor <- variable_to_title[["mean_pauling"]]
  response <- variable_to_title[["num_localities"]]
  test_fitted <- fit_linear_model(response, predictor, true_mineral_nodes)

  expect_equal(sort(names(test_fitted)), sort(c("keep_clusters", "model_fit", "tukey_fit", "rsquared", "tukey_ok_variance"))  )
  expect_true(test_fitted$tukey_ok_variance)
  expect_true(is.null(test_fitted$tukey_fit))
  expect_true(is.numeric(test_fitted$rsquared[[1]]))
  expect_true(is.numeric(test_fitted$rsquared[[2]]))
  
  
  model_fit_table <- test_fitted$model_fit
  expect_equal(sort(names(model_fit_table)), sort(c("Coefficient", "Coefficient estimate", "Standard error", "t-statistic", "P-value")))
  expect_true(nrow(model_fit_table) == 2)
  
})


test_that("fct_run_linear_models::fit_linear_model() with cluster predictor", {
  predictor <- variable_to_title[["cluster_ID"]]
  response <- variable_to_title[["num_localities"]]
  
  test_fitted <- fit_linear_model(response, predictor, true_mineral_nodes)

  expect_equal(sort(names(test_fitted)), sort(c("keep_clusters", "model_fit", "tukey_fit", "rsquared", "tukey_ok_variance"))  )
  expect_true(is.logical(test_fitted$tukey_ok_variance))
  expect_true(!(is.null(test_fitted$tukey_fit)))
  expect_true(is.na(test_fitted$rsquared[[1]]))
  expect_true(is.na(test_fitted$rsquared[[2]]))
  
  true_mineral_nodes %>% 
    dplyr::count(cluster_ID) %>% 
    dplyr::filter(n>=3) %>% 
    nrow() -> n_clusters_compared
  expect_true(n_clusters_compared == length(test_fitted$keep_clusters))
  expect_equal( sort(test_fitted$keep_clusters), test_fitted$keep_clusters )
  
  model_fit_table <- test_fitted$model_fit
  expect_equal(sort(names(model_fit_table)), sort(c("Coefficient", "Coefficient estimate", "Standard error", "t-statistic", "P-value")))
  expect_true(nrow(model_fit_table) == n_clusters_compared)
  
  tukey_table <- test_fitted$tukey_fit
  expect_equal(sort(names(tukey_table)), sort(c("Cluster Comparison", "Estimated effect size difference", "95% CI Lower bound", "95% CI Upper bound", "Adjusted P-value")))
  true_mineral_nodes %>% dplyr::count(cluster_ID) %>% nrow() -> true_n_clusters
  expected_tukey_rows <- (n_clusters_compared * (n_clusters_compared - 1)) / 2
  expect_true(nrow(tukey_table) == expected_tukey_rows)
  

})

test_that("fct_run_linear_models::plot_linear_model_scatter()", {
  predictor <- variable_to_title[["max_age"]]
  response <- variable_to_title[["num_localities"]]

  test_plotted <- plot_linear_model_scatter(response, 
                                            predictor, 
                                            c(0.5, 0.01),
                                            true_mineral_nodes, 
                                            FALSE, ## logx
                                            FALSE, ## logy
                                            true_point_color, 
                                            true_point_size, 
                                            TRUE,  ## bestfit
                                            true_bestfit_color, 
                                            FALSE) #grid
  
  ## TODO really should compare to an actual plot
  plot_data_geom_point <- ggplot2::ggplot_build(test_plotted)$data[[1]]
  plot_data_geom_smooth <- ggplot2::ggplot_build(test_plotted)$data[[2]]
  expect_true(all(plot_data_geom_point$colour == true_point_color))
  expect_true(all(plot_data_geom_point$size == true_point_size))
  expect_true(all(plot_data_geom_smooth$colour == true_bestfit_color))

})


test_that("fct_run_linear_models::plot_linear_model_cluster()", {
  predictor <- variable_to_title[["cluster_ID"]]
  response <- variable_to_title[["max_age"]]
  cluster_colors <- set_cluster_colors("Set2", true_n_clusters)
  test_fitted <- fit_linear_model(response, predictor, true_mineral_nodes)

  used_colors <- cluster_colors[test_fitted$keep_clusters]
  
  plot_type <- "boxplot"
  test_box <- plot_linear_model_cluster(response, 
                                        test_fitted$keep_clusters, 
                                        true_mineral_nodes,
                                        cluster_colors, 
                                        plot_type, 
                                        FALSE, ## flip_coord
                                        FALSE, ## show_mean_se
                                        FALSE, ## show legend
                                        true_point_size,
                                        FALSE) ## grid
                                        
  ## TODO really should compare to an actual plot
  plot_data_box <- ggplot2::ggplot_build(test_box)$data[[1]]
  expect_true(all(plot_data_box$fill == used_colors))
  box_legend <- cowplot::get_legend(test_box)
  expect_null(box_legend)

  plot_type <- "strip"
  test_strip <- plot_linear_model_cluster(response, 
                                          test_fitted$keep_clusters, 
                                          true_mineral_nodes,
                                          cluster_colors, 
                                          plot_type, 
                                          FALSE, ## flip_coord
                                          FALSE, ## show_mean_se
                                          TRUE, ## show legend
                                          true_point_size, 
                                          FALSE)
  
  ## TODO really should compare to an actual plot
  plot_data_strip <- ggplot2::ggplot_build(test_strip)$data[[1]]
  expect_true(all(plot_data_strip$colour %in% used_colors))
  expect_true(all(plot_data_strip$size == true_point_size))
  strip_legend <- cowplot::get_legend(test_strip)
  expect_true(!is.null(strip_legend))
})





