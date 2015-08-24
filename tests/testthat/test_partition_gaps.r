context("all tests of 'partition_gaps'")

test_that("the basics of 'partition_gaps' works", {
    x <- c(1:2,7:8)
    y <- c(1:4)
    expect_equal(
        partition_gaps(x = x, y = y, gap = 5, check = TRUE),
        list(data_frame(x=1:2,y=1:2), data_frame(x=7:8,y=3:4))
    )
    expect_equal(
        partition_gaps(x = x, y = y, gap = Inf, check = TRUE),
        list(data_frame(x=x,y=y))
    )
    x <- c(0,6,7,15,19,30)
    y <- letters[1:6]
    expect_equal(
        partition_gaps(x = x, y = y, gap = 5, check = FALSE),
        list(
            data_frame(x = 0, y = "a"),
            data_frame(x = c(6,7), y = c("b","c")),
            data_frame(x = c(15,19), y = c("d","e")),
            data_frame(x = 30, y = "f")
        )
    )
})
