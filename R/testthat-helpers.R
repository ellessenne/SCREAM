#' @keywords internal
.sample_name <- function(n = 10) {
  name <- sample(x = c(LETTERS, letters, seq(9)), size = n, replace = TRUE)
  name <- paste(name, collapse = "")
  name <- make.names(name)
  return(name)
}
