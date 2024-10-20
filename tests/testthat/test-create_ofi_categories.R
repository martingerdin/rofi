test_that("create_detailed_ofi_categories works correctly", {
  input.data <- data.frame(Problemomrade_.FMP = c(
    "ok", "dokumetation", "handläggning", "annat"
  ))

  result <- create_detailed_ofi_categories(input.data)

  expect_equal(
    result,
    c(NA, "Documentation", "Patient management", "Other")
  )
})

test_that("create_detailed_ofi_categories throws error for unexpected values", {
  unexpected.data <- data.frame(Problemomrade_.FMP = c("ok", "unexpected value"))

  expect_error(
    create_detailed_ofi_categories(unexpected.data),
    "Values found in 'Problemomrade_.FMP' that are not in expected values: unexpected value"
  )
})

test_that("create_broad_ofi_categories works correctly", {
  input.data <- data.frame(Problemomrade_.FMP = c(
    "missad skada", "lång tid till op", "triage på akm"
  ))

  result <- create_broad_ofi_categories(input.data)

  expect_equal(
    result,
    c("Missed diagnosis", "Delay in treatment", "Clinical judgement error")
  )
})

test_that("add_ofi_categories adds correct columns", {
  input_data <- data.frame(Problemomrade_.FMP = c(
    "missad skada", "lång tid till op", "triage på akm"
  ))

  result <- add_ofi_categories(input_data)

  expect_equal(colnames(result), c("Problemomrade_.FMP", "ofi.categories.detailed", "ofi.categories.broad"))
  expect_equal(
    result$ofi.categories.detailed,
    c("Missed injury", "Delay to surgery", "Triage in the ED")
  )
  expect_equal(
    result$ofi.categories.broad,
    c("Missed diagnosis", "Delay in treatment", "Clinical judgement error")
  )
})
