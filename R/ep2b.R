#' Read and peek at all sheets in an Excel workbook
#'
#' Reads every sheet from an `.xlsx` or `.xls` file into individual tibbles
#' in the calling environment, prints a summary of each sheet's shape and
#' column-level information, and invisibly returns the sheets as a named
#' list.
#'
#' @param path Path to an Excel file.
#' @param sheets Optional character vector of sheet names to read. If `NULL`
#'   (the default), all sheets are read.
#' @param skip_empty Logical. If `TRUE` (default), sheets with zero rows are
#'   dropped from the result.
#' @param show Logical. If `TRUE` (default), print a summary report when the
#'   function returns.
#' @param envir Environment into which the individual sheet tibbles are
#'   assigned. Defaults to the caller's environment (i.e., the global
#'   environment when called interactively).
#'
#' @return Invisibly returns an object of class `workbook_peek`: a named list
#'   of tibbles (one per sheet) with a custom print method. Also assigns
#'   each sheet as an individual tibble in `envir` (as a side effect).
#'
#' @examples
#' example_file <- system.file("extdata", "example.xlsx", package = "ep2b")
#' ep2b(example_file)
#'
#' @export
ep2b <- function(path,
                 sheets = NULL,
                 skip_empty = TRUE,
                 show = TRUE,
                 envir = parent.frame()) {

  # --- 1. Validate the file exists ----------------------------------------
  if (!file.exists(path)) {
    cli::cli_abort("File not found: {.file {path}}")
  }

  # --- 2. Figure out which sheets to read ---------------------------------
  all_sheets <- readxl::excel_sheets(path)
  sheets <- sheets %||% all_sheets

  missing_sheets <- setdiff(sheets, all_sheets)
  if (length(missing_sheets) > 0) {
    cli::cli_abort(
      "Sheet(s) not found in workbook: {.val {missing_sheets}}"
    )
  }

  # --- 3. Read each sheet into a tibble -----------------------------------
  data <- sheets |>
    purrr::set_names() |>
    purrr::map(\(s) readxl::read_excel(path, sheet = s))

  # --- 4. Optionally drop empty sheets ------------------------------------
  if (skip_empty) {
    data <- purrr::keep(data, \(d) nrow(d) > 0)
  }

  # --- 5. Assign each sheet into the caller's environment -----------------
  # make.names() sanitizes sheet names into valid R variable names.
  # E.g., "2024 Enrollments" -> "X2024.Enrollments"
  # If the sanitized name already exists in envir, warn before overwriting.
  clean_names <- make.names(names(data))

  purrr::iwalk(data, \(df, nm) {
    var_name <- make.names(nm)

    if (exists(var_name, envir = envir, inherits = FALSE)) {
      cli::cli_alert_warning(
        "Overwriting existing object {.var {var_name}} in environment."
      )
    }

    assign(var_name, df, envir = envir)
  })

  # --- 6. Wrap the list in an S3 class ------------------------------------
  # Rename the list elements with the sanitized names too, so wb$Enrollment
  # and the environment variable match.
  names(data) <- clean_names

  result <- structure(
    data,
    class   = c("workbook_peek", "list"),
    source  = path,
    created = Sys.time()
  )

  # --- 7. Print (if requested) and return invisibly -----------------------
  if (show) print(result)
  invisible(result)
}
