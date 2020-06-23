test_that("fct_timeline::baseline_timeline() works", {
  test_baseline <- baseline_timeline()
  expect_equal(sort(names(test_baseline)), sort(c("plot", "legend")))
  expect_true(all(class(test_baseline[["legend"]]) == c("gtable", "gTree", "grob","gDesc")))
  expect_true(all(class(test_baseline[["plot"]]) == c("gg", "ggplot")))
})


test_that("fct_timeline::prepare_timeline_data() works", {
  elements_of_interest <- c("Cd")
  age_range <- c(2, 4)
  max_age_type <- "Maximum"
  elements_only <- initialize_data(med_data_cache, element_redox_states_cache, elements_of_interest, FALSE)
  test_prepare <- prepare_timeline_data(elements_only, age_range, max_age_type)
  expect_equal(sort(names(test_prepare)), sort(c("all", "maxage")))
  
  expected_names_each <- sort(c("mineral_name", "x", "selected_age_range"))
  expect_equal(sort(names(test_prepare$all)), expected_names_each)
  expect_equal(sort(names(test_prepare$maxage)), expected_names_each)
  expect_equal(sort(unique(test_prepare$maxage$mineral_name)), sort(unique(elements_only$mineral_name)))
  expect_equal(sort(unique(test_prepare$all$mineral_name)), sort(unique(elements_only$mineral_name)))
  
  test_prepare$all %>%
    dplyr::filter(selected_age_range == selected_age_range_levels[1]) -> within_time
  outside_time <- dplyr::anti_join(test_prepare$all, within_time)
  ## must be WITHIN
  expect_true(all( (within_time$x / 1000) >= age_range[1] & (within_time$x / 1000) <= age_range[2]) )
  # EITHER above or below
  expect_true(all( (outside_time$x / 1000) < age_range[1] | (outside_time$x / 1000) > age_range[2]) )
})

build_current_timeline <- function(timeline_minerals, nodes, mineral_color_by, mineral_color_palette, outside_range_color)
  
test_that("fct_timeline::build_current_timeline() works", {
  elements_of_interest <- c("As")
  age_range <- c(0, 4)
  max_age_type <- "Maximum"
  elements_only <- initialize_data(med_data_cache, element_redox_states_cache, elements_of_interest, FALSE)
  net <- initialize_network(elements_of_interest, age_range = age_range)
  timeline_minerals <- prepare_timeline_data(elements_only, age_range, max_age_type)
  outside_range_color <- "blue"
  within_range_color <- "red"
  within_range_palette <- "Reds"
  
  p <- build_current_timeline(timeline_minerals$all, net$nodes, "singlecolor", within_range_color, outside_range_color)
  expect_true(all(class(p) == c("gg", "ggplot")))
  
  p <- build_current_timeline(timeline_minerals$all, net$nodes, "max_age", within_range_palette, outside_range_color)
  expect_true(all(class(p) == c("gg", "ggplot")))
  
  p <- build_current_timeline(timeline_minerals$maxage, net$nodes, "singlecolor", within_range_color, outside_range_color)
  expect_true(all(class(p) == c("gg", "ggplot")))
  
  p <- build_current_timeline(timeline_minerals$maxage, net$nodes, "max_age", within_range_palette, outside_range_color)
  expect_true(all(class(p) == c("gg", "ggplot")))
  
})

