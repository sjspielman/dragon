# A forthcoming golem feature
# https://github.com/ThinkR-open/golem/pull/435
expect_running <- function(sleep){
  skip_on_cran()
  skip_on_travis()
  skip_on_appveyor()
  x <- processx::process$new(
    "R", 
    c(
      "-e", 
      "pkgload::load_all(here::here());run_app()"
    )
  )
  Sys.sleep(sleep)
  expect_true(x$is_alive())
  x$kill()
} 