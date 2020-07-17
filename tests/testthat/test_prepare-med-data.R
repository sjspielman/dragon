test_that("fct_prepare-rruff-data::find_most_recent_date() returns a date stamp", {
  skip_if_offline()
  skip_on_travis()
  skip_on_cran() 

  test_date <- find_most_recent_date()
  # Skip tests if couldn't ping
  if (test_date != FALSE){ 
    ## Should be a string with two spaces, 1 comma, last bit matches 202\d
    expect_true(class(test_date) == "character")
    expect_true(stringr::str_count(test_date, " ") == 2)
    expect_true(stringr::str_count(test_date, ",") == 1)
    expect_true(stringr::str_detect(test_date, "202\\d$"))
  }

})

test_that("fct_prepare-rruff-data::try_url() works", {
  skip_if_offline()
  skip_on_travis()
  skip_on_cran() 
  
  should_work <- try_url(med_exporting_url, "html")
  expect_true(should_work$success)
  expect_true(typeof(should_work$content) == "list")
  
  should_fail <- try_url("http://rruff.infottt", "html")
  expect_true(!(should_fail$success))
  expect_true(!(should_fail$content))
})
