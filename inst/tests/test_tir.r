context("all tests of 'tir'")

test_that("'tir' works [MORE TESTS NEEDED]", {
    x <- 1:3
    y <- 1:3
    ret <- tir(x, y, y.int = c(-Inf, 1, 2, Inf))
    # dput2(ret)
    comp <- structure(list(R.l=c(-Inf,1,2),R.h=c(1,2,Inf),Time.l=c(1L,1L,1L),Time.h=c(3L,3L,3L),Time=c(2L,2L,2L),TiR=c(0,1,1),percent=c(0,0.5,0.5)),.Names=c("R.l","R.h","Time.l","Time.h","Time","TiR","percent"),row.names=c(NA,-3L),class="data.frame")
    expect_equal(ret, comp)

    ret  <- tir(x, y, y.int = c(-Inf, 1, 2, Inf), x.int = c(-Inf, Inf))
    # dput2(ret)
    comp <- structure(list(R.l=c(-Inf,1,2),R.h=c(1,2,Inf),Time.l=c(-Inf,-Inf,-Inf),Time.h=c(Inf,Inf,Inf),Time=c(2L,2L,2L),TiR=c(0,1,1),percent=c(0,0.5,0.5)),.Names=c("R.l","R.h","Time.l","Time.h","Time","TiR","percent"),row.names=c(NA,-3L),class="data.frame")
    expect_equal(ret, comp)
})
