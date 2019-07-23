context("fx()")

test_that("fx() creates an HTML file", {
  html <- fx()
  expect_true(fs::file_exists(html))
})
