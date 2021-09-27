test_that("d__ of transformed chi-squared distribution", {
    x <- lsf("chisq", .location = 1, .scale = 2)
    d <- d_(x)
    expect_equal(d(0, df = 3), 0)
    expect_equal(d(1, df = 3), 0)
    expect_equal(d(2, df = 3), stats::dchisq(0.5, df = 3)/2)
})

test_that("p__ of transformed chi-squared distribution", {
    x <- lsf("chisq", .location = 1, .scale = 2)
    p <- p_(x)
    expect_equal(p(0, df = 3), 0)
    expect_equal(p(1, df = 3), 0)
    expect_equal(p(3, df = 3), stats::pchisq(1, df = 3))
})

test_that("q__ of transformed chi-squared distribution", {
    x <- lsf("chisq", .location = 1, .scale = 2)
    q <- q_(x)
    expect_equal(q(0.25, df = 3), 2 * stats::qchisq(0.25, df = 3) + 1)
    expect_equal(q(0.5, df = 3), 2 * stats::qchisq(0.5, df = 3) + 1)
    expect_equal(q(0.75, df = 3), 2 * stats::qchisq(0.75, df = 3) + 1)
})

test_that("r_ of transformed chi-squared distribution", {
    x <- lsf("chisq", .location = 1, .scale = 2)
    r <- r_(x)
    data <- r(10000, df = 3)
    expect_equal(mean(data), 7, tolerance = 0.1)
    expect_equal(sd(data), 2 * sqrt(6), tolerance = 0.1)
})
