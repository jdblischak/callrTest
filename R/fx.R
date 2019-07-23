#' Example call to callr
#'
#' Attempts to render a file
#'
#' @export
fx <- function() {
  rmd <- fs::file_temp()
  lines <- c("---",
             "output: html_document",
             "---",
             "",
             "```{r chunkname}",
             "plot(1:10)",
             "```")
  writeLines(lines, rmd)

  html <- callr::r_safe(function(x) rmarkdown::render(x), args = list(rmd))

  return(html)
}
