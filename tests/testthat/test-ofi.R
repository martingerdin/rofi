test_that("create_ofi assigns ofi correctly", {
    expect_equal(create_ofi(ofi.test.data), ofi.test.data$ofi.correct)
})
