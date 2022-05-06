test_that("check_data_shift detects data shift", {
    data.shift <- ofi.test.data
    data.shift <- rbind(data.shift, c("fdjodnaodan", NA, NA, NA, NA))
    names(data.shift) <- c("quality.review.done", "problem.area", "mortality.review.done", "preventable.death")
    expect_error(check_data_shift(data.shift), regexp = "Levels in the")
})

test_that("create_ofi returns a vector", {
    expect_true(is.vector(create_ofi(ofi.test.data)))
})

test_that("create_ofi assigns ofi correctly", {
    expect_equal(create_ofi(ofi.test.data), ofi.test.data$ofi.correct)
})
