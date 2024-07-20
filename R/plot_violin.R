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
#' @param stats Boolean to display the results of statistical tests.
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
        pch_alpha = 1,
        pch_colour = "gray50",
        pch_size = cex,
        cex = 1,
        cex_axis = 17 * cex,
        cex_main = 21 * cex,
        cex_sub = 15 * cex,
        stats = TRUE,
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
        breaks = c(0, 5, 15, 30, 60, 100),
        group_ref = NULL,
        label_post_hoc = "p.adj.signif",
        # group_ref = "none",
        ...) {
    if (is.null(title)) {
        title <- paste0(deparse(substitute(x)))
    }
    value <- 1
    if (isFALSE(subtitle)) {
        if (!(class(x) %in% c("data.frame", "tibble")) || ncol(x) == 1) {
            subtitle <- paste0(
                print_median(x, digits = digits, width = width_text),
                ", N=",
                length(na.omit(unlist(x)))
            )
        } else {
            if (stats) {
                tmp <- as.data.frame(x) %>%
                    pivot_longer(everything()) %>%
                    filter(!is.na(value))
                if (method %in% c("wilcox", "kruskal")) {
                    effsize <- get(paste0(method, "_effsize"))(tmp, value ~ name) %>%
                        pull("effsize") %>%
                        round(2)
                    if (method == "wilcox") {
                        effsize <- paste0(", r = ", effsize)
                    } else {
                        effsize <- paste0(", H = ", effsize)
                    }
                } else {
                    effsize <- aov(value ~ name, data = tmp) %>%
                        eta_squared()  %>%
                        round(2) %>%
                        paste0(", \u03B7² = ", .)
                }
                subtitle <- get(paste0(method, "_test"))(tmp, value ~ name) %>%
                    print_mean_test(digits_p = 3, digits = digits) # %>%
                # paste0(effsize)
            } else {
                subtitle <- NULL
            }
        }
    }
    color_subtitle <- colour
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
        sub_labs <- group_by(df, name) %>%
            reframe(
                label = paste0(
                    # "\n",
                    print_median(value, digits = digits, width_label)#,
                    # ",\nN=",
                    # length(na.omit(value))
                )
            ) %>%
            mutate(label = paste0(name, label)) %>% #str_wrap(width_label)) %>%
            pull(label)
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
        geom_errorbar(
            width = .1,
            lwd = lwd,
            colour = colour_fill1,
            aes(
                ymin = get_iqr(1),
                ymax = get_iqr(0)
            )
        ) +
        geom_boxplot(
            coef = 0,
            outlier.shape = NA,
            colour = "white",
            aes(fill = colour_fill0),
            lwd = lwd * 0.25
        ) +
        geom_violin(alpha = alpha, aes(fill = colour_fill0), colour = NA) +
        theme_minimal() +
        labs(
            title = str_wrap(title, width_title),
            subtitle = subtitle,
            y = ylab
        ) +
        scale_fill_manual(values = unique(colour_fill)) +
        scale_x_discrete(limits = colnames(x), labels = sub_labs) +
        scale_y_continuous(breaks = pretty_breaks(n = 3))
    if (display_log) {
        log_func <- get(paste0("log", log_power))
        if(as.numeric(log_power) == 10) {
            breaks = max(unlist(x), na.rm = TRUE) %>%
                log_func() %>%
                ceiling() %>%
                seq(0, .) %>%
                `^`(log_power, .) %>%
                # paste0("1e", .) %>%
                as.numeric() %>%
                c(0, .)
        }
        p <- p + scale_y_continuous(
            trans = get(paste0("log", log_power, "x_trans")),
            breaks = breaks
        )
    }
    if ((class(x) %in% c("data.frame", "tibble")) && ncol(x) > 2 && stats) {
        post_hoc0 <- dunn_test(
            df,
            value ~ name,
            p.adjust.method = method_adjust
        )
        if(!is.null(group_ref)) {
            post_hoc0 <- post_hoc0 %>%
                filter(str_detect(group1, group_ref) | str_detect(group2, group_ref))
        }
        post_hoc <- add_significance(post_hoc0, "p") %>%
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
                y.position = log_func(y.position)
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
                color = "gray50", # col_post,
                bracket.size = lwd,
                size = cex * 6,
                hide.ns = TRUE,
                tip.length = 0
            )
    }
    p <- p +
        geom_sina(
            size = pch_size,
            colour = pch_colour,
            alpha = pch_alpha,
            seed = 1
        )
    if ((class(x) %in% c("data.frame", "tibble")) && label && !is.null(rownames(x))) {
        tmp <- list.map(
            x,
            f(i, j) ~{
                get_outliers(i, replace = FALSE, ...) %>%
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
            size = cex * 5,
            colour = "gray40"
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
        color_subtitle = color_subtitle
    ) %>% suppressWarnings() +
        theme(
            plot.margin = margin(l = 0 + margin_spacer(sub_labs, ratio_labs)),
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
