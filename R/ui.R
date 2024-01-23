# Inspired by the usethis package

ui_done <- function(x, .envir = parent.frame()) {
  x <- glue::glue_collapse(x, "\n")
  x <- glue::glue(x, .envir = .envir)
  cat_bullet(x, crayon::green(clisymbols::symbol$tick))
}


cat_bullet <- function(x, bullet) {
  bullet <- paste0(bullet, " ")
  x <- indent(x, bullet, "  ")
  cat(paste0(x, "\n"), sep = "")
}


indent <- function(x, first = "  ", indent = first) {
  x <- gsub("\n", paste0("\n", indent), x)
  paste0(first, x)
}
