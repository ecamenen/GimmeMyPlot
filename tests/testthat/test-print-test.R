data("ToothGrowth")
df <- ToothGrowth

test_that("mean_test test works", {
    res <- anova_test(df, len ~ dose) %>% print_test()
    expect_type(res, "character")
    expect_equal(res, "Anova, F(1, 58) = 105, p < 0.001***")
    res <- kruskal_test(df, len ~ dose) %>% print_test()
    expect_type(res, "character")
    expect_equal(res, "Kruskal-Wallis, K(2) = 41, p < 0.001***")
    res <- wilcox_test(df, len ~ supp) %>% print_test()
    expect_type(res, "character")
    expect_equal(res, "W = 576, p = 0.06")
})

test_that("median test works", {
    res <- print_dispersion(c(0, 0, 0, 1, 1, 1))
    expect_type(res, "character")
    expect_equal(res, "0.5\u00b11")
})

test_that("add_significance0 test works", {
    tmp <- t_test(df, len ~ dose) %>% adjust_pvalue()
    res <- add_significance0(tmp, "p.adj")
    res0 <- add_significance(tmp, "p.adj")
    expect_identical(res0[, -10], res[, -10])
    expect_setequal(pull(res, "p.adj.signif"), "***")
})
