context("report")

# Setup ------------------------------------------------------------------------

library(workflowr)
source("setup.R")

# Test get_versions_df ---------------------------------------------------------

test_that("get_versions_df returns data frame of commits for file(s)", {

  path <- fs::file_temp()
  fs::dir_create(path)
  on.exit(test_teardown(path))
  r <- git2r::init(path)
  git2r::config(r, user.name = "Test User", user.email = "testing")

  f1 <- file.path(path, "f1.txt")
  f2 <- file.path(path, "f2.txt")

  cat("line 1\n", file = f1)
  git2r::add(r, f1)
  c1 <- git2r::commit(r, "The first commit to f1")
  Sys.sleep(1)

  cat("line 1\n", file = f2)
  git2r::add(r, f2)
  c2 <- git2r::commit(r, "The first commit to f2")
  Sys.sleep(1)

  cat("line 2\n", file = f1, append = TRUE)
  git2r::add(r, f1)
  c3 <- git2r::commit(r, "The second commit to f1")
  Sys.sleep(1)

  cat("line 2\n", file = f2, append = TRUE)
  git2r::add(r, f2)
  c4 <- git2r::commit(r, "The second commit to f2")
  Sys.sleep(1)

  versions_f1 <- workflowr:::get_versions_df("f1.txt", r)

  expect_true(all(versions_f1$File == "f1.txt"))
  expect_identical(versions_f1$Version, c(c3$sha, c1$sha))
  expect_true(all(versions_f1$Author == "Test User"))
  expect_identical(versions_f1$Date, c(as.Date(as.POSIXct(c3$author$when)),
                                       as.Date(as.POSIXct(c1$author$when))))
  expect_identical(versions_f1$Message, c(c3$message, c1$message))

  versions_f2 <- workflowr:::get_versions_df("f2.txt", r)

  expect_true(all(versions_f2$File == "f2.txt"))
  expect_identical(versions_f2$Version, c(c4$sha, c2$sha))
  expect_true(all(versions_f2$Author == "Test User"))
  expect_identical(versions_f2$Date, c(as.Date(as.POSIXct(c4$author$when)),
                                       as.Date(as.POSIXct(c2$author$when))))
  expect_identical(versions_f2$Message, c(c4$message, c2$message))

  versions_f1_f2 <- workflowr:::get_versions_df(c("f1.txt", "f2.txt"), r)

  expect_true(all(versions_f1_f2$File == c("f2.txt", "f1.txt", "f2.txt", "f1.txt")))
  expect_identical(versions_f1_f2$Version, c(c4$sha, c3$sha, c2$sha, c1$sha))
  expect_true(all(versions_f1_f2$Author == "Test User"))
  expect_identical(versions_f1_f2$Date, c(as.Date(as.POSIXct(c4$author$when)),
                                          as.Date(as.POSIXct(c3$author$when)),
                                          as.Date(as.POSIXct(c2$author$when)),
                                          as.Date(as.POSIXct(c1$author$when))))
  expect_identical(versions_f1_f2$Message, c(c4$message, c3$message,
                                             c2$message, c1$message))

  # Reversing the input file order should have no effect
  versions_f2_f1 <- workflowr:::get_versions_df(c("f2.txt", "f1.txt"), r)
  expect_identical(versions_f2_f1, versions_f1_f2)

  expect_identical(workflowr:::get_versions_df("non-existent", r), data.frame())

  expect_error(workflowr:::get_versions_df(f1, r),
               "File paths must be relative")
})

# Test get_versions and get_versions_fig ---------------------------------------

test_that("get_versions and get_versions_fig insert GitHub URL if available", {

  skip_on_cran()

  # Setup functions from setup.R
  path <- test_setup()
  on.exit(test_teardown(path))

  rmd <- file.path(path, "analysis", "file.Rmd")
  lines <- c("---",
             "output: workflowr::wflow_html",
             "---",
             "",
             "```{r chunkname}",
             "plot(1:10)",
             "```")
  writeLines(lines, rmd)

  # Add remote
  tmp_remote <- wflow_git_remote("origin", "testuser", "testrepo",
                                 verbose = FALSE, project = path)

  # Go through a few commit cycles
  for (i in 1:3) {
    cat("edit", file = rmd, append = TRUE)
    tmp_publish <- wflow_publish(rmd, view = FALSE, project = path)
  }

  r <- git2r::repository(path)
  output_dir <- workflowr:::get_output_dir(file.path(path, "analysis/"))
  github <- workflowr:::get_host_from_remote(path)
  versions <- workflowr:::get_versions(input = rmd, output_dir, r, github)
  expect_true(any(stringr::str_detect(versions, github)))
  fig <- file.path(output_dir, "figure", basename(rmd), "chunkname-1.png")
  versions_fig <- workflowr:::get_versions_fig(fig, r, github)
  expect_true(any(stringr::str_detect(versions_fig, github)))
})

test_that("get_versions_fig converts spaces to dashes for HTML ID", {

  skip_on_cran()

  # Setup functions from setup.R
  path <- test_setup()
  on.exit(test_teardown(path))

  rmd <- file.path(path, "analysis", "file.Rmd")
  lines <- c("---",
             "output: workflowr::wflow_html",
             "---",
             "",
             "```{r chunk-name}",
             "plot(1:10)",
             "```",
             "",
             "```{r chunk name}",
             "plot(11:20)",
             "```")
  writeLines(lines, rmd)

  # Add remote
  tmp_remote <- wflow_git_remote("origin", "testuser", "testrepo",
                                 verbose = FALSE, project = path)

  # Commit twice so that past versions table is generated
  tmp_publish <- wflow_publish(rmd, view = FALSE, project = path)
  tmp_publish <- wflow_publish(rmd, view = FALSE, project = path)

  r <- git2r::repository(path)
  output_dir <- workflowr:::get_output_dir(file.path(path, "analysis/"))
  github <- workflowr:::get_host_from_remote(path)

  # The figure file without spaces should be displayed as normal
  fig <- file.path(output_dir, "figure", basename(rmd), "chunk-name-1.png")
  versions_fig <- workflowr:::get_versions_fig(fig, r, github)
  versions_fig_lines <- stringr::str_split(versions_fig, "\\n")[[1]]
  data_target <- stringr::str_subset(versions_fig_lines,
                                     'data-target=\"#fig-chunk-name-1\"')
  expect_true(length(data_target) == 1)
  div_id <- stringr::str_subset(versions_fig_lines,
                                'id=\"fig-chunk-name-1\"')
  expect_true(length(div_id) == 1)
  fig_display_name <- stringr::str_subset(versions_fig_lines,
                                          ' chunk-name-1.png')
  expect_true(length(fig_display_name) == 1)

  # The figure file with spaces should be quoted and have spaces replaced with
  # dashes for data-target and id.
  fig <- file.path(output_dir, "figure", basename(rmd), "chunk name-1.png")
  versions_fig <- workflowr:::get_versions_fig(fig, r, github)
  versions_fig_lines <- stringr::str_split(versions_fig, "\\n")[[1]]
  data_target <- stringr::str_subset(versions_fig_lines,
                                     'data-target=\"#fig-no-spaces-chunk-name-1\"')
  expect_true(length(data_target) == 1)
  div_id <- stringr::str_subset(versions_fig_lines,
                                'id=\"fig-no-spaces-chunk-name-1\"')
  expect_true(length(div_id) == 1)
  fig_display_name <- stringr::str_subset(versions_fig_lines,
                                          ' &quot;chunk name-1.png&quot;')
  expect_true(length(fig_display_name) == 1)
})
