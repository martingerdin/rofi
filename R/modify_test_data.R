#' Modify Test Data
#'
#' Internal functions used to modify test data
add_ofi_test_case <- function() {
    ofi.test.data <- rbind(ofi.test.data,
                           c("Nej", NA, 2, NA, "No"))
    usethis::use_data(ofi.test.data, internal = TRUE, overwrite = TRUE)
}
