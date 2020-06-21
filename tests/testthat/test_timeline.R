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
  expected_names <- sort(c("mineral_name", "x", "yend", "selected_time_frame"))
  expect_equal(sort(names(test_prepare)), expected_names)
  expect_equal(sort(unique(test_prepare$mineral_name)), sort(unique(elements_only$mineral_name)))
  
  test_prepare %>%
    dplyr::filter(selected_time_frame == selected_time_frame_levels[1]) -> within_time
  outside_time <- dplyr::anti_join(test_prepare, within_time)
  ## must be WITHIN
  expect_true(all( (within_time$x / 1000) >= age_range[1] & (within_time$x / 1000) <= age_range[2]) )
  # EITHER above or below
  expect_true(all( (outside_time$x / 1000) < age_range[1] | (outside_time$x / 1000) > age_range[2]) )
})


test_that("fct_timeline::build_current_timeline() works", {
  elements_of_interest <- c("Cd")
  age_range <- c(2, 4)
  max_age_type <- "Maximum"
  elements_only <- initialize_data(med_data_cache, element_redox_states_cache, elements_of_interest, FALSE)
  timeline_minerals <- prepare_timeline_data(elements_only, age_range, max_age_type)
  within_range_color <- "red"
  outside_range_color <- "blue"
  
  p <- build_current_timeline(timeline_minerals, within_range_color, outside_range_color)
  expect_true(all(class(p) == c("gg", "ggplot")))
  
  
})

