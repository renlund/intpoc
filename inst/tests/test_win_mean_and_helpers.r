context("'win_mean' and helper functions are ok")

test_that("'check_num_pair' works", {
    expect_null(check_num_pair(1:2, 2:1))
    expect_null(check_num_pair(0:1, c(-1, 1)))
})
test_that("'check_num_vec' works", {
    expect_warning(check_num_vec(1, 1))
    expect_warning(check_num_vec(1, 1:2))
    expect_warning(check_num_vec(c(1, NA), 1:2))
    expect_warning(check_num_vec(1:2, c(NA, NA)))
    expect_warning(check_num_vec(1:1, c(NA, NA)))
    expect_message(expect_equal(
        check_num_vec(c(2,3,1), c(1,2,3)),
        data_frame(x=c(1,2,3), y=c(3,1,2))
        ))
    expect_message(expect_equal(
        check_num_vec(c(2,3,1), c(1,2,NA)),
        data_frame(x=c(1,2,3), y=c(1,1,2))
    ))
    expect_message(expect_equal(
        check_num_vec(x = c(2,3,2), y = c(1,2,NA)),
        data_frame(x=c(2,3), y=c(1,2))
    ))
    expect_message(expect_equal(
        check_num_vec(c(1,1,2,4), c(1,3,NA,5)),
        data_frame(x=c(1,2,4), y=c(2,3,5))
    ))
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
test_that("'vecrea' works", {
    expect_equal(vecrea(c(1,3,5,7), c(0,-1,1,3)), c(-1, 0, 4))
    expect_equal(vecrea(c(1,3,5,7), c(0,-1,1,3)), c(-1, 0, 4))
})
test_that("'paire' works", {
    expect_equal(paire(0:1, 0:1),1/3)
    expect_equal(paire(0:1, c(1,1)),1)
    expect_equal(paire(0:1, c(2,2)),4)
    expect_equal(paire(x = 0:1, y = c(-1,1)),1/3)
    expect_equal(paire(x = c(-2,3), y = c(-7,2)), 65)
    expect_equal(paire(x = c(-2,3), y = c(-7,-5)), 181+2/3)
})
test_that("'vecre' works", {
    expect_equal(vecre(x=c(4,5,5.5),y=c(0,1,1.5)), c(1/3, 19/24))
})
test_that("'win_mean' basic functionality works", {
    expect_equal( win_mean( x = c(0, 1), y = c(1, 1))$int, 1)
    expect_equal( win_mean(c(1,3,5,7), c(0,-1,1,3))$int, 3)
    expect_equal( win_mean(c(1,3,5,7), c(NA,-1,1,3))$int, 2)
    expect_equal( win_mean(x = c(1,3,5,7), y = c(0,-1,NA,3))$int, 3)
    expect_equal( win_mean(c(1,3,5,7), c(0,-1,1,NA))$int, 1)
})
test_that("'win_mean' argument 'x.'  works", {
    expect_equal( win_mean( x = c(0, 1), y = c(1, 1), x.=0.85)$int, 0.85)
    expect_equal( win_mean(c(0, 1), c(1, 1), x.=0.5)$int, 0.5)
    expect_equal( win_mean(c(0, 1), c(1, 1), x.=0.05)$int, 0.05)
    expect_equal( win_mean(c(1,3,5,7), c(0,-1,1,3), x.=7)$int, 3)
    expect_equal( win_mean(c(1,3,5,7), c(0,-1,1,3), x.=6)$int, 0.5)
    expect_equal( win_mean(c(1,3,5,7), c(NA,-1,1,3), x.=6)$int, -0.5)
    expect_error( win_mean(x = c(1,3,5,7), y = c(0,-1,1,3)$int, x.=8))
    expect_error( win_mean(c(1,3,5,7), c(0,-1,1,3), x.=0)$int)
})
test_that("'win_mean' argument 'win'  works", {
    expect_equal( win_mean(c(0, 1), c(1, 1), win=0.81)$int, 0.81)
    expect_equal( win_mean(c(0, 1), c(1, 1), win=0.5)$int, 0.5)
    expect_equal( win_mean(c(0, 1), c(1, 1), win=0.123)$int, 0.123)
    expect_equal(win_mean(x=c(1,3,5,7), y=c(0,-1,1,3), win=1)$int, 2.5)
    expect_equal(win_mean(x=c(1,3,5,7), y=c(0,-1,1,3), win=4)$int, 4)
    expect_equal(win_mean(x=c(1,3,5,7), y=c(0,-1,1,3), win=3)$int, 4.5)
    expect_equal(win_mean(x=c(1,3,5,7), y=c(0,-1,1,3), win=5)$int, 3.25)
    expect_equal(win_mean(x=c(1,3,5,7), y=c(0,-1,1,3), win=6)$int, 3)
    expect_equal(win_mean(x=c(1,3,5,7), y=c(0,-1,1,3), win=16)$int, 3)
})
test_that("'win_mean' argument 'win' works with 'x.' works", {
    expect_equal( win_mean(c(0, 1), c(1, 1), win=0.81, x.=0.9)$int, 0.81)
    expect_equal( win_mean(c(0, 1), c(1, 1), win=0.81, x.=0.6)$int, 0.6)
    expect_equal(win_mean(x=c(1,3,5,7), y=c(0,-1,1,3), win=1, x.=6)$int, 1.5)
    expect_equal(win_mean(x=c(1,3,5,7), y=c(0,-1,1,3), win=2, x.=5)$int, 0)
    expect_equal(win_mean(x=c(1,3,5,7), y=c(0,-1,1,3), win=1.5, x.=5.5)$int, 1.125)
    expect_equal(win_mean(x=c(1,3,5,7), y=c(0,-1,1,3), win=0.5, x.=3.5)$int, -0.375)
    expect_equal(win_mean(x=c(1,3,5,7), y=c(0,-1,1,3), win=2.5, x.=3.5)$int, -1.375)
    expect_equal(win_mean(x=c(1,3,5,7), y=c(0,-1,1,3), win=2.51, x.=3.5)$int, -1.375)
    expect_equal(win_mean(x=c(1,3,5,7), y=c(0,-1,1,3), win=Inf, x.=3.5)$int, -1.375)
})
# test_that("'win_mean' works for some test cases", {
#     expect_equal(
#         win_mean(x=c(1,3,5,7), y=c(0,-1,1,3), win=1.5, x.=5.5),
#         data_frame(int=9/8, mean=3/4, sq_int=9/8, sd=3/16)
#     )
# })

