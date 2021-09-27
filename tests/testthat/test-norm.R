test_that("d_ of transformed normal distribution", {
    x <- lsf("norm", .location = 1, .scale = 2)
    d <- d_(x)
    expect_equal(d(0), stats::dnorm(-0.5)/2)
    expect_equal(d(1), stats::dnorm(0)/2)
    expect_equal(d(2), stats::dnorm(0.5)/2)
})

test_that("p_ of transformed normal distribution", {
    x <- lsf("norm", .location = 1, .scale = 2)
    p <- p_(x)
    expect_equal(p(-1), stats::pnorm(-1))
    expect_equal(p(1), 0.5)
    expect_equal(p(3), stats::pnorm(1))
})

test_that("q_ of transformed normal distribution", {
    x <- lsf("norm", .location = 1, .scale = 2)
    q <- q_(x)
    expect(is.infinite(q(0)), "q(0) must be -inf")
    expect_equal(q(0.25), 2 * stats::qnorm(0.25) + 1)
    expect_equal(q(0.5), 1)
    expect_equal(q(0.75), 2 * stats::qnorm(0.75) + 1)
    expect(is.infinite(q(1)), "q(1) must be +inf")
})

test_that("r_ of transformed normal distribution", {
    x <- lsf("norm", .location = 1, .scale = 2)
    r <- r_(x)
    data <- r(10000)
    expect_equal(mean(data), 1, tolerance = 0.1)
    expect_equal(sd(data), 2, tolerance = 0.1)
})
