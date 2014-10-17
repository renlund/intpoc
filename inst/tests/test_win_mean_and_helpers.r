context("'win_mean' and helper functions")

test_that("'check_num_pair' works", {
    expect_null(check_num_pair(1:2, 2:1))
    expect_null(check_num_pair(0:1, c(-1, 1)))
})
test_that("'line' works", {
    expect_equal(line(c(1,2), c(1, 5)), c("intersect"=-3, "slope"=4))
})
test_that("'h_cut' works", {
    expect_equal(h_cut(1:2, 1:2, 1.5), 1.5)
})
test_that("'pairea' works", {
    expect_equal(pairea(1:2, 0:1), 0.5)
    expect_equal(pairea(0:1, c(-1,1)), 0)
    expect_equal(pairea(0:1, c(1,-1)), 0)
    expect_equal(pairea(0:1, c(0,20)), 10)
    expect_equal(pairea(c(0,2), c(-20,10)), -10)
})
