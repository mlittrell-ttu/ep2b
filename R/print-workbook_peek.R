#' Print method for workbook_peek objects
#'
#' Prints a formatted summary of a workbook peek: source file, number of
#' sheets, and per-sheet dimensions plus a column-level summary table.
#'
#' @param x A `workbook_peek` object (returned by [ep2b()]).
#' @param ... Additional arguments (ignored).
#'
#' @return Invisibly returns `x`.
#'
#' @export
print.workbook_peek <- function(x, ...) {

  # Header: source file and sheet count.
  # attr(x, "source") pulls the file path we stashed as an attribute
  # inside ep2b(). Attributes are metadata that tag along with an object.
  cli::cli_h1("Workbook peek")
  cli::cli_text("Source: {.file {attr(x, 'source')}}")
  cli::cli_text("Sheets: {length(x)}")
  cli::cli_text("")

  # purrr::iwalk() is the tidyverse alternative to a for loop when you want
  # to iterate for SIDE EFFECTS (printing) rather than to build a result.
  #   - "walk"  = like map, but throws away return values
  #   - "i" prefix = passes the name/index as a second argument to the function
  # So iwalk(x, \(df, nm) ...) gives us both the sheet's tibble AND its name
  # on each iteration. Cleaner than a for loop + subsetting.
  purrr::iwalk(x, \(df, nm) {
    cli::cli_h2("Sheet: {nm}")
    cli::cli_text("Dimensions: {nrow(df)} rows x {ncol(df)} columns")

    # Delegate column-level details to a helper (next file).
    df |>
      summarize_columns() |>
      print()

    cli::cli_text("")
  })

  invisible(x)
}
