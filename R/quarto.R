# assumes that the template repos all are in the `numbats` org
# with prefix `monash-quarto-`
base_quarto <- "anuopensci/quarto-anu-"

#' List of Quarto templates available
#'
#' This function lists the ANU Quarto templates available.
#'
#' @param type One of either "report", "thesis", or
#'  "letter".
#' @param dir The name of the directory to put the template in. The directory
#'  should not exist.
#'
#' @name quarto_template
#' @examples
#' \dontrun{
#' quarto_template_use("report", dir = "myreport")
#' quarto_template_install("report")
#' quarto_template_add("thesis")
#' }
#' @export
quarto_template_use <- function(type = c("report",
                                         "thesis",
                                         "letter"),
                                dir = type) {
  type <- match.arg(type)
  dir <- dir[1]
  wd_current <- getwd()
  wd_new <- dir
  if(dir.exists(wd_new)) stop(paste0("The directory ", wd_new, " already exists. Please delete this first to proceed."))
  dir.create(wd_new)
  setwd(wd_new)
  system(paste0("quarto use template ", base_quarto, type, " --no-prompt"))
  # back to original working directory
  setwd(wd_current)
  fn <- basename(dir)
  # TODO: add base option
  rstudioapi::navigateToFile(paste0(dir, "/", fn, ".qmd"))
}


#' @rdname quarto_template
#' @export
quarto_template_install <- function(type = c("report",
                                             "thesis",
                                             "letter")) {
  type <- match.arg(type)

  system(paste0("quarto install extension ", base_quarto, type, " --no-prompt"))
  ui_done("Template installed.")
}


#' @rdname quarto_template
#' @export
quarto_template_add <- function(type = c("report",
                                         "thesis",
                                         "letter")) {

  type <- match.arg(type)

  system(paste0("quarto add ", base_quarto, type, " --no-prompt"))
  ui_done("Template added.")
}
