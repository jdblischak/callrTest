#' Example call to callr
#'
#' Attempts to render a file
#'
#' @export
fx <- function() {
  rmd <- tempfile()
  lines <- c("---",
             "output: workflowr::wflow_html",
             "---",
             "",
             "```{r chunkname}",
             "plot(1:10)",
             "```")
  writeLines(lines, rmd)

  html <- callr::r_safe(function(x) rmarkdown::render(x), args = list(rmd))

  return(html)
}
