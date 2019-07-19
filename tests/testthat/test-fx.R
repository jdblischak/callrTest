context("fx first time")

test_that("fx creates an HTML file", {
  html <- fx()
  expect_true(file.exists(html))
})
