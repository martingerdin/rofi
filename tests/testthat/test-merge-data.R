test_that("merge_data returns the expected data.frame", {
    datasets <- import_data(silent = TRUE)
    merged.data <- merge_data(datasets)
    expect_s3_class(merged.data, "data.frame")
    expect_equal(names(merged.data), get_data_names())
    expect_equal(nrow(merged.data), 11864)
})

