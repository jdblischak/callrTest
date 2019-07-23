context("fx()")

test_that("fx() creates an HTML file", {
  html <- fx()
  expect_true(fs::file_exists(html))
})

test_that("fx() throws errors if the session temporary directory is deleted", {
  tmp <- fs::path_temp()
  on.exit(fs::dir_create(tmp))
  fs::dir_delete(tmp)
  expect_error(
    expect_warning(fx(), "cannot open the connection"),
    "cannot open the connection"
  )
})

test_that("fx() creates an HTML file after session temporary directory is restored", {
  expect_true(fs::dir_exists(fs::path_temp()))
  html <- fx()
  expect_true(fs::file_exists(html))
})
