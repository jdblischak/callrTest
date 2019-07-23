context("tmp")

test_that("callr behavior when temp directory is deleted and recreated", {
  tmp <- fs::path_temp()

  f <- function(x) summary(rnorm(x))

  # Run callr
  callr::r(f, args = list(10))

  # Delete the temporary directory and then recreate it
  fs::dir_delete(tmp)
  fs::dir_create(tmp)


  if (utils::packageVersion("callr") > package_version("3.2.0")) {
    expect_error(
      callr::r(f, args = list(10))
    )
  } else {
    expect_silent(
      callr::r(f, args = list(10))
    )
  }
})
