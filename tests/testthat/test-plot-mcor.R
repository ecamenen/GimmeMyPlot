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

test_that("mcor_test default works", {
    expect_s3_class(plot_mcor(x), "ggplot")
})

test_that("mcor_test advanced works", {
    p <- plot_mcor(
        x,
        y,
        colour = c("black", brewer.pal(n = 6, name = "RdBu"), 1),
        method = "spearman",
        method_adjust = "none",
        cex = 0.8
    )
    expect_s3_class(p, "ggplot")
    expect_equal(as.character(unique(p$data$Var2)), colnames(x))
    expect_equal(as.character(unique(p$data$Var1)), colnames(y))
})
