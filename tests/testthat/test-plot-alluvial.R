x <- lapply(seq(3), function(x) {
    runif(100, 1, 3) %>%
        round() %>%
        letters[.]
}) %>%
    as.data.frame() %>%
    set_colnames(paste("variable", LETTERS[seq(3)]))
x[x == "a"] <- NA
x[, 3][is.na(x[, 3])] <- "a"

test_that("alluvial default works", {
    expect_s3_class(plot_alluvial(x), "ggplot")
})

test_that("alluvial advanced works", {
    p <- plot_alluvial(
        x,
        width_label = 5,
        colour = rev(brewer.pal(3, "Reds")),
        cex = 1.2
    )
    expect_s3_class(p, "ggplot")
})
