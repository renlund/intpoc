context("'tir.for.pair' tests")

test_that("the basics work",{
    x = c(0,1)
    y = c(0,1)
    # ---
    expect_equal(tir.for.pair(x,y,c(0,1),c(-1,0)),0)
    expect_equal(tir.for.pair(x,y,c(0,1),c(1,2)),0)
    expect_equal(tir.for.pair(x,y,c(0,1),c(0,1)),1)
    expect_equal(tir.for.pair(x,y,c(0,1),c(0,0.5)),0.5)
    expect_equal(tir.for.pair(x,y,c(0,1),c(-1,0.5)),0.5)
    expect_equal(tir.for.pair(x,y,c(0,1),c(0.2,0.4)),0.2)
    # ---
    expect_equal(tir.for.pair(x,y,c(-1,0),c(0,1)),0)
    expect_equal(tir.for.pair(x,y,c(1,2),c(0,1)),0)
    expect_equal(tir.for.pair(x,y,c(0,1),c(0,1)),1)
    expect_equal(tir.for.pair(x,y,c(0,0.5),c(0,1)),0.5)
    expect_equal(tir.for.pair(x,y,c(-1,0.5),c(0,1)),0.5)
    expect_equal(tir.for.pair(x,y,c(0.2,0.4),c(0,1)),0.2)
    # ---
    expect_equal(tir.for.pair(x,y,c(0,.5),c(.25,1)),.25)
    expect_equal(tir.for.pair(x,y,c(-1,0.5),c(0.5,2)),0)
    expect_equal(tir.for.pair(x,y,c(.25,1),c(0,.5)),.25)
})

test_that("'include.lowest' work",{
    x = c(0,1)
    y = c(0,0)
    expect_equal(tir.for.pair(x, y, incl.low = FALSE, y.int = c(0,1)), 0)
    expect_equal(tir.for.pair(x, y, incl.low = TRUE , y.int = c(0,1)), 1)
})



