test_that("merge_data returns the expected data.frame", {
    ENV <- Sys.getenv("ENV")
    test <- if (ENV == "test") TRUE else FALSE
    datasets <- import_data(silent = TRUE, test = test)
    merged.data <- merge_data(datasets, test = test)
    expect_s3_class(merged.data, "data.frame")
    expect_equal(names(merged.data), get_data_names(test = test))
    expect_equal(nrow(merged.data), 11864)
})

