x0 <- runif(20)
x <- lapply(
    c(1, -1),
    function(i) sapply(seq(10), function(j) x0 * i + runif(10, max = 1))
) %>%
    Reduce(cbind, .) %>%
    set_colnames(paste("Variable", seq(20)))
y <- lapply(
    c(1, -1),
    function(i) sapply(seq(10), function(j) x0 * i + runif(10, max = 1))
) %>%
    Reduce(cbind, .) %>%
    set_colnames(paste("Variable", seq(20))) %>%
    .[, seq(5)]

test_that("mcor default works", {
    res <- mcor_test(x)
    expect_equal(dim(res), c(20, 20))
})

test_that("mcor advanced works", {
    res <- mcor_test(
        x,
        y,
        p.value = TRUE,
        method = "pearson",
        method_adjust = "none"
    )
    expect_length(res, 2)
    expect_equal(
        unname(cor.test(x[, 1], y[, 1])$estimate),
        res[[1]][1, 1]
    )
    expect_equal(
        unname(cor.test(x[, 1], y[, 1])$p.value),
        res[[2]][1, 1]
    )
    expect_equal(rownames(res[[1]]), colnames(y))
    expect_equal(colnames(res[[1]]), colnames(x))
    expect_equal(rownames(res[[2]]), colnames(y))
    expect_equal(colnames(res[[2]]), colnames(x))
})
