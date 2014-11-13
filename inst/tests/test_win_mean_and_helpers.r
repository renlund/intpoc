context("all tests of 'win_mean' and helper functions")

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
        check_num_vec(c(2,3,1), c(1,2,NA), y.NA = "i"),
        data_frame(x=c(1,2,3), y=c(1,1,2))
    ))
    expect_message(expect_equal(
        check_num_vec(x = c(2,3,2), y = c(1,2,NA)),
        data_frame(x=c(2,3), y=c(1,2))
    ))
    expect_message(expect_equal(
        check_num_vec(c(1,1,2,4), c(1,3,NA,5), y.NA = "i"),
        data_frame(x=c(1,2,4), y=c(2,3,5))
    ))
    expect_message(expect_equal(
        check_num_vec(c(1,1,2,4), c(1,3,NA,5), y.NA = "r"),
        data_frame(x=c(1,4), y=c(2,5))
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
test_that("'windowize' basic functionality works", {
    expect_null(windowize(x = c(1,2), y=c(1,1), x. = 2, win = 1, check = TRUE, mess = TRUE))
    expect_null(windowize(x = c(1,2), y=c(1,1)))
    expect_null(windowize(x = c(1,2), y=c(1,1), x.=3, win=Inf))
    expect_true(is.na(windowize(x=c(0,1),y=c(1,1),x.=7,win=1)))
    expect_equal(windowize(1:5,1:5,x.=3),data_frame(x=1:3,y=1:3))
    expect_equal(windowize(1:5,1:5,x.=4,win=2),data_frame(x=2:4,y=2:4))
    expect_equal(windowize(1:5,1:5,x.=8,win=5),data_frame(x=3:5,y=3:5))
    expect_equal(windowize(1:5,1:5,x.=4.5,win=1.5),data_frame(x=c(3,4,4.5),y=c(3,4,4.5)))
    expect_equal(windowize(1:5,1:5,x.=4.5,win=2),data_frame(x=c(2.5,3,4,4.5),y=c(2.5,3,4,4.5)))
    expect_equal(windowize(1:5,1:5,x.=5.5,win=0.7),data_frame(x=c(4.8,5),y=c(4.8,5)))
    expect_null(windowize(1:5,1:5,x.=8))
})
test_that("'win_mean' basic functionality works", {
    expect_equal( win_mean( x = c(0, 1), y = c(1, 1))$int, 1)
    expect_equal( win_mean(c(1,3,5,7), c(0,-1,1,3))$int, 3)
    expect_equal( win_mean( x = c(1,3,5,7), y = c(NA,-1,1,3))$int, 2)
    expect_equal( win_mean(x = c(1,3,5,7), y = c(0,-1,NA,3))$int, 3)
    expect_equal( win_mean(c(1,3,5,7), c(0,-1,1,NA))$int, 1)
})
test_that("'win_mean' argument 'x.'  works", {
    expect_equal( win_mean( x = c(0, 1), y = c(1, 1), x.=0.85)$int, 0.85)
    expect_equal( win_mean(c(0, 1), c(1, 1), x.=0.5)$int, 0.5)
    expect_equal( win_mean(c(0, 1), c(1, 1), x.=0.05)$int, 0.05)
    expect_equal( win_mean(c(1,3,5,7), c(0,-1,1,3), x.=7)$int, 3)
    expect_equal( win_mean(c(1,3,5,7), c(0,-1,1,3), x.=7, win=3)$int, 4.5)
    expect_equal( win_mean(c(1,3,5,7), c(0,-1,1,3), x.=6)$int, 0.5)
    expect_equal( win_mean(c(1,3,5,7), c(NA,-1,1,3), x.=6)$int, -0.5)
    expect_equal( win_mean(x = c(1,3,5,7), y = c(0,-1,1,3), x.=8)$int, 3)
    expect_equal( win_mean(x = c(1,3,5,7), y = c(0,-1,1,3), x.=8, win=4)$int, 4.5)
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
test_that("'win_mean' works recursively", {
    x1 <- c(1.1,2)
    y1 <- c(-0.3,1)
    x2 <- c(22,23.2)
    y2 <- c(-0.1,0.9)
    i1 <- win_mean(x=x1, y=y1)$int
    i2 <- win_mean(x=x2, y=y2)$int
    isq1 <- win_mean(x=x1, y=y1)$sq_int
    isq2 <- win_mean(x=x2, y=y2)$sq_int
    r  <- win_mean(x=c(x1,x2), y=c(y1,y2), gap=10)
    r. <- win_mean(x=c(x1,x2,35), y=c(y1,y2,0), gap=10)
    expect_equivalent(r$int, i1+i2)
    expect_equivalent(r$sq_int, isq1+isq2)
    expect_equivalent(r$parts, 2)
    expect_equivalent(r$n_y, 4)
    expect_equivalent(r$parts, 2)
    expect_equivalent(r$parts_info, 2)
    expect_equivalent(r.$int, i1+i2)
    expect_equivalent(r.$sq_int, isq1+isq2)
    expect_equivalent(r.$parts, 3)
    expect_equivalent(r.$n_y, 5)
    expect_equivalent(r.$parts, 3)
    expect_equivalent(r.$parts_info, 2)
    y_ <- linRepNA(x = c(x1,3), y=c(y1,NA))
    r1 <- win_mean(x = c(x1, 3), y=c(y1, NA))
    r2 <- win_mean(x = c(x1, 3), y=y_)
    expect_equal(r1$int, r2$int)
    expect_equal(r1$sq_int, r2$sq_int)
    expect_equal(r1$span, r2$span)
    expect_equivalent(r1$sum, sum(y1))
    expect_equivalent(r1$sq_sum, sum(y1^2))
    expect_equivalent(r1$raw_sd, sd(y1))
    expect_equivalent(r1$n_y, 2)
    expect_equivalent(r2$sum, sum(y_))
    expect_equivalent(r2$sq_sum, sum(y_^2))
    expect_equivalent(r2$raw_sd, sd(y_))
    expect_equivalent(r2$n_y, 3)
    r3 <- win_mean(c(1,3,4,6,8,9,11), c(rep(1.0,7)), gap=1.99)
    expect_equivalent(r3$parts, 5)
    expect_equivalent(r3$parts_info, 2)
})
test_that("'partition_gaps' works (more tests needed)", {
    L2 <- partition_gaps(x=c(1,3,5,7,21,23,25,27), y=c(0,-1,1,3,0,-1,1,3), gap=10)
    expect_true(is.list(L2))
    expect_equal(length(L2), 2)
})
test_that("'win_mean' is somewhat sane", {
    for(i in 1:10){
        n <- sample(2:1000, 1)
        x <- 1:n
        y <- round(runif(n = n, min=-1, max = 1), 1)
        expect_less_than(abs(win_mean(x, y)$mean-mean(y)), 1/n)
    }
})
