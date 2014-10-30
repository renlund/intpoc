test_that("basic functionality is ok", {
    expect_equal(linRepNA(1:3, 1:3, T), 1:3)
    expect_equal(linRepNA(1:3, c(1,NA,3), T), 1:3)
    expect_equal(linRepNA(1:3, c(NA,2,3), T), c(2,2,3))
    expect_equal(linRepNA(1:3, c(NA,2,NA), T), c(2,2,2))
    expect_equal(linRepNA(1:3, c(NA,NA,3), T), c(3,3,3))

    expect_equal(linRepNA(1:3, c(0,NA,4), T), c(0,2,4))
    expect_equal(linRepNA(1:3, c(0,NA,8), T), c(0,4,8))
})
