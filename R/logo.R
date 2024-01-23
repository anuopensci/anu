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
#' @param color_mode Use RGB for digital communication (e.g. website) and CMYK for printing.
#' @param category The category of the logo.
#' @param type The type of the logo. Note that some type do not exist based on category chosen.
#' @param color The color of the logo. Note that some colors do not exist based on category and type chosen.
#' @param ext The file extension of the logo. Note that some extensions do not exist based on category, type and color chosen.
#' @param filename The output filename of the logo (defaults to the original filename).
#'
#' @source https://anu365.sharepoint.com/sites/ANUidentityHUB/SitePages/ANU-logo.aspx
#' @export
logo_get <- function(path = ".",
                     overwrite = FALSE,
                     color_mode = c("RGB", "CMYK"),
                     category = c("primary", "secondary", "crest", "favicon"),
                     type = if(category[1] %in% c("primary", "secondary")) c("vertical", "horizontal")
                            else c("inversed", "outline"),
                     color  = if(category[1] %in% c("primary", "secondary")) c("gold_black", "gold_white", "black", "white")
                              else c("black", "white", "gold"),
                     ext = c("png", "jpg", "eps"),
                     filename = NULL) {

  color_mode <- match.arg(color_mode)
  category <- stringr::str_to_title(match.arg(category))
  category_folder <- switch(category,
                             Primary = "01. Primary",
                             Secondary = "02. Secondary",
                             Crest = "03. Crest",
                             Favicon = "04. Favicon")
  type_folder <- stringr::str_to_title(match.arg(type))
  color_text <- match.arg(color) |>
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



