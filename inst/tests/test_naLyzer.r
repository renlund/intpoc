context("all test of silly function naLyzer")

test_that("naLyzer works", {
    x <- c(NA, 0, Inf, -Inf, NA, 0, 1)
    df <- data.frame(
        Length = 7,
        n.NA = 2,
        n.posInf = 1,
        n.negInf = 1,
        n.character = 0,
        unique = 4
    )
    expect_equal(naLyzer(x), df)
})
