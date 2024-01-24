#' Get the ANU logo
#'
#' A quick way of getting the logo. Not all combinations of category, type, and color (see table below).
#' |     | primary | secondary | crest | favicon |
#' | ---: | :---: | :---: | :---: | :---: |
#' | vertical |  Y |  Y |  |    |
#' | horizontal | Y | Y  |  |    |
#' | inversed |  |   | Y  | Y   |
#' | outline |  |   |  Y |    |
#' | gold_black |  Y |  Y |   |    |
#' | gold_white | Y | Y  |   |    |
#' | black | Y |  Y |  Y |  Y  |
#' | white | Y  | Y  |  Y |  Y  |
#' | gold |  |   |  Y |   Y |
#'
#' @param path The path to save the logo to.
#' @param overwrite Overwrite the logo if it already exists.
#' @param color_mode Use "RGB" for digital communication (e.g. website) and "CMYK" for printing.
#' @param category Either "primary", "secondary", "crest", or "favicon".
#' @param type If category is primary or secondary, then "vertical" or "horizontal", otherwise "inversed" or "outline".
#' @param color The color of the logo ("gold_black", "gold_white", "black", "white", or "gold"). Note that some colors do not exist based on category and type chosen.
#' @param ext The file extension of the logo. Note that some extensions do not exist based on category, type and color chosen.
#' @param filename The output filename of the logo (defaults to the original filename).
#'
#' @source https://anu365.sharepoint.com/sites/ANUidentityHUB/SitePages/ANU-logo.aspx
#' @export
logo_get <- function(path = ".",
                     overwrite = FALSE,
                     color_mode = c("RGB", "CMYK"),
                     category = c("primary", "secondary", "crest", "favicon"),
                     type = c("vertical", "horizontal", "inversed", "outline"),
                     color  = c("gold_black", "gold_white", "black", "white", "gold"),
                     ext = c("png", "jpg", "eps"),
                     filename = NULL) {

  color_mode <- match.arg(color_mode)
  category <- stringr::str_to_title(match.arg(category))
  category_folder <- switch(category,
                             Primary = "01. Primary",
                             Secondary = "02. Secondary",
                             Crest = "03. Crest",
                             Favicon = "04. Favicon")
  type_allowed <- switch(category,
                 Primary = c("vertical", "horizontal"),
                 Secondary = c("vertical", "horizontal"),
                 Crest = c("inversed", "outline"),
                 Favicon = c("inversed"))
  type <- match.arg(intersect(type, type_allowed)[1], type)
  type_folder <- stringr::str_to_title(match.arg(type))
  color_allowed <- switch(category,
                  Primary = c("gold_black", "gold_white", "black", "white"),
                  Secondary = c("gold_black", "gold_white", "black", "white"),
                  Crest = c("black", "white", "gold"),
                  Favicon = c("gold", "white", "black"))
  color <- match.arg(intersect(color, color_allowed)[1], color)
  color_text <- color |>
    stringr::str_replace_all("_", " ") |>
    stringr::str_to_title() |>
    stringr::str_replace_all(" ", "")
  ext <- match.arg(ext)
  ext_folder <- toupper(ext)

  fn <- glue::glue("ANU_{category}_{type_folder}_{color_text}.{ext}")

  logo_path <- fs::path_package(package = "anu",
                                "logos",
                                color_mode,
                                category_folder,
                                type_folder,
                                ext_folder,
                                fn)
  if(!fs::file_exists(logo_path)) cli::cli_alert_danger("Logo {cli::col_red(logo_path)} does not exist.")
  output_fn <- filename %||% fs::path_file(logo_path)
  new_path <- fs::path(path, output_fn)
  fs::file_copy(logo_path, new_path = new_path, overwrite = overwrite)
  ui_done(glue::glue("Logo saved to {new_path}"))
}



