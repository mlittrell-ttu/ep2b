# Tests for ep2b()
#

test_that("ep2b reads all sheets by default", {
  # system.file() finds the example file shipped in inst/extdata/
  path <- system.file("extdata", "example.xlsx", package = "ep2b")

  # We use show = FALSE to suppress the printed summary during tests —
  # tests should be quiet unless they fail.
  # We use envir = new.env() so the assignment side effect doesn't
  # pollute the test environment with variables.
  wb <- ep2b(path, show = FALSE, envir = new.env())

  # Expectations: each expect_* function throws if the condition fails.
  expect_s3_class(wb, "workbook_peek")           # right class
  expect_named(wb, c("Enrollment", "Evaluations")) # right sheet names
  expect_s3_class(wb$Enrollment, "tbl_df")       # sheets are tibbles
  expect_equal(nrow(wb$Enrollment), 5)           # right number of rows
})

test_that("ep2b errors on missing file", {
  # expect_error checks that the code throws an error, and optionally
  # that the message matches a pattern.
  expect_error(
    ep2b("nonexistent_file.xlsx", show = FALSE, envir = new.env()),
    "File not found"
  )
})

test_that("ep2b honors the sheets argument", {
  path <- system.file("extdata", "example.xlsx", package = "ep2b")

  wb <- ep2b(
    path,
    sheets = "Enrollment",
    show   = FALSE,
    envir  = new.env()
  )

  expect_named(wb, "Enrollment")
})

test_that("ep2b assigns sheets into the specified environment", {
  path <- system.file("extdata", "example.xlsx", package = "ep2b")

  # Create a fresh environment to receive the tibbles.
  test_env <- new.env()

  ep2b(path, show = FALSE, envir = test_env)

  # Now the tibbles should exist inside test_env.
  expect_true(exists("Enrollment", envir = test_env))
  expect_s3_class(get("Enrollment", envir = test_env), "tbl_df")
})

test_that("ep2b errors on nonexistent sheet name", {
  path <- system.file("extdata", "example.xlsx", package = "ep2b")

  expect_error(
    ep2b(path, sheets = "NoSuchSheet", show = FALSE, envir = new.env()),
    "not found in workbook"
  )
})
