#' Violin plot
#'
#' Visualize the distribution of single or multiple variables using violin
#' plots, boxplots, and sina plots
#'
#' @param x Vector or data.frame of numerical values visualized on the plot.
#' @param method Character for the test method ('anova', 'kruskal', or
#' 'wilcox').
#' @param method_adjust Character for the multiple correction test among
#' `r paste0("'", paste0(sort(p.adjust.methods), collapse = "', '"), "'")`
#' @param title Character for the title.
#' @param width_text Integer for the maximum length of the subtitle(s).
#' @param width_title Integer for the maximum length of the title.
#' @param color_title Color for the title.
#' @param colour Color or vector of colors for the violin and boxplot.
#' @param alpha Double for the transparency of the violin plot (ranging from 0
#' to 1 for maximum opacity).
#' @param pch_alpha Double for the transparency of the points (ranging from 0
#' to 1 for maximum opacity).
#' @param pch_colour Color for the sina points.
#' @param pch_size Double for the magnification factor for the points relative
#' to the default.
#' @param cex Double for the magnification factor for the text relative to the
#' default.
#' @param cex_axis Double for the magnification factor for the axis labels
#' relative to the default.
#' @param cex_main Double for the magnification factor for the subtitles
#' relative to the default.
#' @param cex_sub Double for the magnification factor for the main title
#' relative to the default.
#' @param test Boolean to display the results of statistical tests.
#' @param digits Integer for the number of decimals.
#' @param coef Double to multiply the quantiles by.
#' @param hjust Double for the horizontal justification (in \[0, 1\]).
#' @param lwd Double for the line width.
#' @param probs Double vector for the probabilities (in \[0, 1\]).
#' @param subtitle Boolean to display the subtitle.
#' @param ylab Character for the title of the Y-axis.
#'
#' @examples
#' library(RColorBrewer)
#'
#' # Default parameters
#' x <- runif(10)
#' plot_violin(x)
#'
#' # Advanced parameters
#' df <- lapply(seq(2), function(x) runif(10))
#' df <- as.data.frame(df)
#' df[, 3] <- runif(10, 1, 2)
#' colnames(df) <- paste0("X", seq(3))
#' plot_violin(
#'     df,
#'     title = "Some numerical variables",
#'     color_title = brewer.pal(9, "Set1")[5],
#'     ylab = "Y-values",
#'     colour = brewer.pal(9, "Set1")[seq(3)],
#'     method = "kruskal",
#'     method_adjust = "none",
#'     cex = 1.2,
#'     pch_size = 3,
#'     width_text = 5,
#'     pch_colour = "gray30",
#'     pch_alpha = 0.5,
#'     width_title = 30,
#'     lwd = 1.25,
#'     digits = 2
#' )
#'
#' @return A ggplot object.
#' @export
plot_violin <- function(
        x,
        method = "kruskal",
        method_adjust = "BH",
        title = NULL,
        width_text = 20,
        width_title = 20,
        width_label = 10,
        colour = "red",
        color_title = colour,
        color_subtitle = "black",
        pch_alpha = 0.5,
        # pch_colour = "black",
        pch_size = cex,
        cex = 1,
        cex_axis = 17 * cex,
        cex_main = 21 * cex,
        cex_sub = 15 * cex,
        test = TRUE,
        digits = 0,
        alpha = 0.3,
        coef = 1.5,
        hjust = 1,
        lwd = 1,
        probs = c(.25, .75),
        subtitle = FALSE,
        ylab = NULL,
        ratio = 7,
        ratio_labs = 7,
        display_log = FALSE,
        log_power = 10,
        label = FALSE,
        breaks = NULL,
        group_ref = NULL,
        label_post_hoc = "p.adj.signif",
        violin = TRUE,
        alpha_error = alpha,
        # group_ref = "none",
        stats = TRUE,
        max.overlaps = 10,
        force = 1,
        func_label = format_labels,
        paired = FALSE,
        ...) {
    if (is.null(title)) {
        if (is.data.frame(x) && length(colnames(x)) == 1) {
            title <- colnames(x)
        } else {
            title <- paste0(deparse(substitute(x)))
        }
    }
    value <- 1
    if (isFALSE(subtitle)) {
        if (!(class(x) %in% c("data.frame", "tibble")) || ncol(x) == 1) {
            subtitle <- paste0(
                print_dispersion(x, digits = digits, width = width_text),
                ", N=",
                length(na.omit(unlist(x)))
            )
        } else {
            if (test) {
                tmp <- as.data.frame(x) %>%
                    pivot_longer(everything()) %>%
                    filter(!is.na(value)) %>%
                    arrange(name)


                if (!paired) {
                    subtitle <- get(paste0(method, "_test"))(tmp, value ~ name)
                } else {
                    if (method %in% c("wilcox", "t")) {
                        subtitle <- get(paste0(method, "_test"))(tmp, value ~ name, paired = TRUE)
                    } else {
                        tmp2 <- tmp %>%
                            group_by(name) %>%
                            mutate(id = row_number()) %>%
                            ungroup() %>%
                            select(id, name, value)
                        if (method == "kruskal") {
                            subtitle <- friedman_test(tmp2, value ~ name | id)
                            effsize <- friedman_effsize(tmp2, value ~ name | id)
                        } else if (method == "anova") {
                            subtitle <- anova_test(tmp2, value ~ name + Error(id / name))$ANOVA
                            effsize <- subtitle$ges
                        } else {
                            subtitle <- lmer(value ~ name + (1 | id), data = tmp2)
                        }
                    }
                }

                if (method %in% c("wilcox", "kruskal")) {
                    effsize <- get(paste0(method, "_effsize"))(tmp, value ~ name) %>%
                        pull("effsize")
                }
                index <- switch(
                    method,
                    "wilcox" = "r",
                    "kruskal" = "H",
                    "anova" = "eta2"
                )
                if (method != "lmer")
                    effsize <- paste0(", ", index, " = ", round(effsize, 2))
                else
                    effsize <- NULL


                subtitle <- print_test(subtitle, digits_p = 3, digits = digits) # %>%
                # paste0(effsize)
            } else {
                subtitle <- NULL
            }
        }
    }
    if (!(class(x) %in% c("data.frame", "tibble")) || ncol(x) == 1) {
        df <- data.frame(value = x, name = 1)
        colnames(df)[1] <- "value"
        colour_fill <- colour
        sub_labs <- ""
        guide <- FALSE
    } else {
        x <- as.data.frame(x) %>%
            set_colnames(colnames(.) %>% paste0(" "))
        df <- pivot_longer(x, everything()) %>%
            mutate(name = factor(name, levels = colnames(x)))
        if (length(colour) == 1)
            colour <- rep(colour, ncol(x))
        colour_fill <- colour
        colour <- factor(df$name, labels = colour)
        if (stats) {
            sub_labs <- group_by(df, name) %>%
                reframe(
                    label = ifelse(
                        stats,
                        paste0(
                            "\n",
                            print_dispersion(value, digits = digits, width = width_label),
                            ",\nN=",
                            length(na.omit(value))
                        ),
                        ""
                    )
                ) %>%
                mutate(label = paste0(name, label)) %>% #str_wrap(width_label)) %>%
                pull(label)
        } else {
            sub_labs <- pull(df, name)
        }
        guide <- TRUE
    }
    colour_fill1 <- factor(df$name, labels = colour_fill)
    colour_fill0 <- factor(df$name, labels = colour_fill)
    quant <- group_by(df, name) %>%
        reframe(quantile(value, probs, na.rm = TRUE)) %>%
        pull(2)
    med <- group_by(df, name) %>%
        reframe(median(value, na.rm = TRUE)) %>%
        pull(2)
    for (i in seq_along(med)) {
        tmp <- (quant[i * 2] - quant[i * 2 - 1]) * coef
        quant[i * 2] <- med[i] + tmp
        quant[i * 2 - 1] <- med[i] - tmp
    }
    iqr <- quant
    quant0 <- group_by(df, name) %>%
        reframe(quantile(value, c(0, 1), na.rm = TRUE)) %>%
        pull(2)
    iqr_even <- function(x) {
        which(seq(length(iqr)) %% 2 == x)
    }
    for (i in iqr_even(1)) {
        if (iqr[i] < quant0[i]) {
            iqr[i] <- quant0[i]
        }
    }
    for (i in iqr_even(0)) {
        if (iqr[i] > quant0[i]) {
            iqr[i] <- quant0[i]
        }
    }
    get_iqr <- function(x) {
        factor(df$name, labels = iqr[iqr_even(x)]) %>%
            as.character() %>%
            as.numeric()
    }

    p <- ggplot(df, aes(x = name, y = as.numeric(value))) +
        # geom_violin(
        #     alpha = 0,
        #     aes(fill = colour_fill0),
        #     # colour = NA,
        #     draw_quantiles = c(0.5),
        #     linewidth = 1
        # ) +
        geom_boxplot(
            coef = 0,
            outlier.shape = NA,
            colour = NA,
            aes(fill = colour_fill0),
            linewidth = 1,
            alpha = alpha,
            na.rm = TRUE,
            # width = 0.25,
        ) +
        stat_summary(
            fun = median,
            geom = "crossbar",
            width = 0.75,
            aes(ymin = ..y.., ymax = ..y.., colour = colour_fill0),
            linewidth = 0.5
        ) +
        geom_errorbar(
            width = .5,
            lwd = 1,
            na.rm = TRUE,
            colour = colour_fill0,
            alpha = alpha_error / 10,
            aes(
                ymin = get_iqr(1),
                ymax = get_iqr(0)
            )
        )
    if (violin)
        p <- p + geom_violin(
            aes(colour = colour_fill0),
            fill = NA,
            # draw_quantiles = c(0.25, 0.75),
            linewidth = 1,
            na.rm = TRUE
        )
    p <- p +
        theme_minimal() +
        labs(
            title = str_wrap(title, width_title),
            subtitle = subtitle,
            y = ylab
        ) +
        scale_fill_manual(values = unique(colour_fill)) +
        scale_color_manual(values = unique(colour_fill)) +
        scale_x_discrete(limits = colnames(x), labels = sub_labs) +
        scale_y_continuous(breaks = if(is.null(breaks)) pretty_breaks(n = 3) else breaks, labels = func_label)
    if (display_log) {
        min_x <- min(unlist(x), na.rm = TRUE)
        if (min_x == 0) {
            add <- 0.1
        } else {
            add <- 0
        }
        if (log_power > 1) {
            log_func <- function(x) log(x + add, base = log_power)
            comp_func <- function(x) `^`(log_power, x) - add
        } else {
            log_func <- function(x) log(x + add)
            comp_func <- function(x) exp(x) - add
        }
        # if (log_power == 10 && min_x < 1e-1) {
        #     label_func <- scientific_format
        # } else {
        #     label_func <- number_format
        # }
        if (is.null(breaks)) {
            breaks <- c(min_x, max(unlist(x), na.rm = TRUE)) %>%
                log_func() %>%
                ceiling()
            breaks <- seq(breaks[1], breaks[2]) %>%
                comp_func() %>%
                round_multiple_digits()
        }
        # breaks <- c(min(unlist(x), na.rm = TRUE), max(unlist(x), na.rm = TRUE)) %>%
        #     log_func() %>%
        #     ceiling()
        # comp_func() %>%
        #     as.numeric() %>%
        #     c(0, .)
        p <- p + scale_y_continuous(
            trans = trans_new(
                "logxn",
                function(x) log_func(x),
                function(x) comp_func(x)
            ),
            breaks = breaks,
            labels = func_label(breaks)
        )
    }
    if ((class(x) %in% c("data.frame", "tibble")) && ncol(x) > 2 && test) {
        if (!paired) {
            func_posthoc <- ifelse(method == "kruskal", dunn_test, tukey_hsd)
            post_hoc0 <- func_posthoc(
                df,
                value ~ name,
                p.adjust.method = method_adjust
            )
        } else {
            if (method != "lmer") {
                func_posthoc <- ifelse(method == "kruskal", wilcox_test, pairwise_t_test)
                func_posthoc(
                    tmp,
                    value ~ name,
                    paired = TRUE,
                    p.adjust.method = method_adjust
                )
            }
            post_hoc0 <- emmeans(subtitle, ~ name) %>%
                pairs(adjust = method_adjust) %>%
                as_tibble() %>%
                separate(contrast, into = c("group1", "group2"), sep = " - ") %>%
                rename(
                    statistic = "t.ratio",
                    p.adj = "p.value"
                ) %>%
                mutate(
                    .y. = "value"
                ) %>%
                add_significance("p.adj")
        }
        if(!is.null(group_ref)) {
            post_hoc0 <- post_hoc0 %>%
                filter(str_detect(group1, group_ref) | str_detect(group2, group_ref))
        }
        if (method == "kruskal") {
            post_hoc0 <- add_significance(post_hoc0, "p")
        }
        post_hoc <- post_hoc0 %>%
            filter(p.adj <= 0.05) %>%
            mutate(
                p.adj.signif = str_replace_all(
                    p.adj.signif,
                    "\\*\\*\\*\\*",
                    "***"
                ),
                y.position = max(df$value, na.rm = TRUE) +
                    as.numeric(rownames(.)) *
                    max(df$value, na.rm = TRUE) / ratio,
                none = ""
            )
        if (display_log) {
            post_hoc <- mutate(
                post_hoc,
                y.position = log_func(y.position) + 0.1
            )
        }
        # col_post <- (str_detect(post_hoc$group1, paste0(group_ref, ": ")) |
        #                  str_detect(post_hoc$group2, paste0(group_ref, ": "))) %>%
        #     ifelse("red", "gray50") %>%
        #     c(., rep("gray50", nrow(post_hoc0) - length(.))) %>%
        #     rep(each = 2)
        p <- p +
            stat_pvalue_manual(
                post_hoc,
                label = label_post_hoc,
                color = "black", # col_post,
                bracket.size = 1,
                size = cex * 6,
                hide.ns = TRUE,
                tip.length = 0
            )
    }
    p <- p +
        geom_sina(
            size = pch_size,
            aes(colour = colour_fill0),
            alpha = pch_alpha,
            seed = 1,
            # ...
        )
    if ((any(class(x) %in% c("data.frame", "tibble"))) && label && !is.null(rownames(x))) {
        tmp <- list.map(
            x,
            f(i, j) ~{
                identify_outliers(i, replace = FALSE, ...) %>%
                    names() %>%
                    as.numeric() %>%
                    `+`((j - 1) * nrow(x))
            }
        ) %>% unlist()
        if (display_log) {
            unlist(x) %>% as.numeric() %>% log_func()
        } else {
            unlist(x) %>% as.numeric()
        }
        p <- p + geom_text_repel(
            aes(
                label = ifelse(
                    seq(ncol(x) * nrow(x)) %in% tmp,
                    rep(rownames(x), ncol(x)),
                    ""
                ),
                x = unique(name) %>% rep(each = nrow(x)),
                y = unlist(x) %>% as.numeric()
            ),
            max.overlaps = max.overlaps,
            size = cex * 5,
            colour = "gray40",
            force = force
        )
    }
    theme_violin(
        p,
        cex = cex,
        cex_main = cex_main,
        cex_sub = cex_sub,
        cex_axis = cex_axis,
        guide = guide,
        color_title = color_title,
        hjust = hjust,
        color_subtitle = color_subtitle,
        lwd = lwd,
        grid = TRUE
    ) %>% suppressWarnings() +
        theme(
            # plot.margin = margin(l = 0 + margin_spacer(sub_labs, ratio_labs)),
            plot.subtitle = element_text(hjust = 0.5)
        )
}

log10x_trans <- trans_new(
    "log10x",
    function(x) ifelse(x != 0, log10(x), 0),
    function(x) ifelse(x != 0, 10^(x), 0),
    breaks = breaks_extended(7),
    format = label_number_auto()
)
log2x_trans <- trans_new(
    "log2x",
    function(x) ifelse(x != 0, log2(x), 0),
    function(x) ifelse(x != 0, 2^(x), 0),
    breaks = breaks_extended(6),
    format = label_number_auto()
)
log1x_trans <- trans_new(
    "log1x",
    function(x) ifelse(x != 0, log(x), 0),
    function(x) ifelse(x != 0, exp(x), 0),
    breaks = breaks_extended(6),
    format = label_number_auto()
)
