#' Summarize the columns of a data frame
#'
#' Internal helper. Returns a tibble with one row per column of the input,
#' giving name, type, missing count, number of unique values, and a
#' type-appropriate one-line summary.
#'
#' @param df A data frame or tibble.
#'
#' @return A tibble with columns `column`, `type`, `missing`, `n_unique`,
#'   and `summary`.
#'
#' @keywords internal
#' @noRd
summarize_columns <- function(df) {

  # purrr::imap_dfr() iterates over the columns of df, calls the anonymous
  # function on each (col + its name), and row-binds the results into one
  # tibble. It's the tidyverse equivalent of building a list of one-row
  # tibbles and rbind()ing them.
  #
  #   - "imap" = indexed map (get the name/index as a second argument)
  #   - "_dfr" = "data frame, row-bind" (row-bind the results into a tibble)
  #
  # Note: newer purrr recommends map() + list_rbind() instead of _dfr,
  # but _dfr still works and reads cleanly for a small helper.
  purrr::imap_dfr(df, \(col, nm) {
    tibble::tibble(
      column   = nm,
      type     = paste(class(col), collapse = "/"),
      missing  = sum(is.na(col)),
      n_unique = dplyr::n_distinct(col),
      summary  = one_line_summary(col)
    )
  })
}

# --- Internal helper: type-aware one-line summary of a single column -------
# Not exported, not documented in user-facing help. Lives in the same file
# because it's only used by summarize_columns() and is short.

one_line_summary <- function(col) {
  # Dispatch on column type. Order matters: check specific types before
  # more general ones (e.g. Date before numeric would matter if dates
  # ever registered as numeric, though they don't in modern R).
  if (is.numeric(col)) {
    rng <- suppressWarnings(range(col, na.rm = TRUE))
    if (any(is.infinite(rng))) return("all missing")
    sprintf(
      "min %.2f / median %.2f / max %.2f",
      rng[1], stats::median(col, na.rm = TRUE), rng[2]
    )
  } else if (is.logical(col)) {
    sprintf(
      "TRUE: %d / FALSE: %d",
      sum(col, na.rm = TRUE),
      sum(!col, na.rm = TRUE)
    )
  } else if (inherits(col, c("Date", "POSIXt"))) {
    rng <- suppressWarnings(range(col, na.rm = TRUE))
    sprintf("from %s to %s", rng[1], rng[2])
  } else {
    # Character/factor/other: show top 3 most common values with counts.
    # forcats::fct_lump_n() is the tidyverse-idiomatic way to keep the
    # top-N levels and lump the rest, but for a one-line summary a
    # simple table() + head() is clearer.
    top <- col |>
      table() |>
      sort(decreasing = TRUE) |>
      utils::head(3)

    paste(
      sprintf("%s (%d)", names(top), as.integer(top)),
      collapse = ", "
    )
  }
}
