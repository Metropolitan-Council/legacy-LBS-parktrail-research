st_erase <- function(x, y) {
  #' Erase an sf object
  #'
  #' @description erases one sf object from another
  #'
  #' @param x: sf object
  #' @param y: sf object to be erased from x
  st_difference(x, st_union(st_combine(y)))
}

source_rmd <- function(file, ...) {
  #' Source Rmd
  #'
  #' @description sources an .Rmd as an R script
  #'
  #' @param file: file path to the .Rmd to be sourced (string)
  tmp_file <- tempfile(fileext = ".R")
  on.exit(unlink(tmp_file), add = TRUE)
  knitr::purl(file, output = tmp_file)
  source(file = tmp_file, ...)
}
