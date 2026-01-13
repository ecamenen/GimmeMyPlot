#' Node (for a network)
#'
#' Counts the number of connections to a node.
#'
#' @inheritParams plot_network
#' @param cex Double for the magnification factor for the node width relative
#' to the default.
#' @param scale Boolean to scale the size of the nodes (to divide by the size of
#' the biggest node).
#'
#' @return A node object in the form of a data.frame with two columns: id and
#' size. The first column contains the node's name, the second its size.
#' @export
#'
#' @examples
#' x <- sapply(seq(5), function(j) runif(5))
#' x[x < 0.5] <- diag(x) <- 0
#' colnames(x) <- rownames(x) <- paste("Variable", seq(5))
#' x[lower.tri(x)] <- t(x)[lower.tri(x)]
#' e <- Edge(x)
#' Node(e)
Node <- function(x, cex = 12, scale = TRUE) {
    stopifnot(is(x, "Edge"))
    res <- unlist(x[, seq(2)]) %>%
        data.frame(id = .) %>%
        group_by(id) %>%
        summarise(size = n()) %>%
        as.data.frame()
    if (scale) {
        n <- pull(res, size) %>% max()
        res$size <- res$size / n * cex
    }
    class(res) <- c(class(res), "Node")
    return(res)
}

#' Edge (for a network)
#'
#' Extracts variable pairs with non-zero values as edges.
#'
#' @inheritParams plot_network
#' @param x,y Symmetrical data.frame with column and row names.
#'
#' @return An object edge in the form of a data.frame with three columns: from,
#'  to and weight. The first two columns contain the names of the variable
#'  pairs, and the last, the corresponding values.
#' @export
#'
#' @examples
#' x <- sapply(seq(5), function(j) runif(5))
#' x[x < 0.5] <- diag(x) <- 0
#' colnames(x) <- rownames(x) <- paste("Variable", seq(5))
#' x[lower.tri(x)] <- t(x)[lower.tri(x)]
#' Edge(x)
#' Edge(x, x)
Edge <- function(x, y = NULL, digits = 2) {
    stopifnot(isSymmetric(x))
    if (!is.null(y)) {
        stopifnot(isSymmetric(y))
    }
    n <- NCOL(x)
    res <- list()

    k <- 0
    for (j in seq(n)) {
        for (i in seq(n)) {
            if (i > k && abs(x[i, j]) > 0) {
                if (is.null(y)) {
                    d <- NULL
                } else {
                    d <- y[i, j]
                }
                res[[length(res) + 1]] <- c(
                    colnames(x)[j],
                    colnames(x)[i],
                    x[i, j],
                    d
                )
            }
        }
        k <- k + 1
    }

    n <- length(res[[1]])
    res <- as.data.frame(t(matrix(unlist(res), n, length(res))))
    colnames(res) <- c("from", "to", "weight", "p")[seq(n)]
    res[, 3] <- as.numeric(res[, 3])
    if (!is.null(y)) {
        res[, 4] <- as.numeric(res[, 4])
    }
    res$title <- res$label <- round(res[, 3], digits)
    class(res) <- c(class(res), "Edge")
    return(res)
}

#' Network plot
#'
#' Plot the connection between variables. The higher the number of connections,
#' the larger the node.
#'
#' @inheritParams plot_violin
#' @param x Symmetrical data.frame with column and row names.
#' @param cex_node Double for the magnification factor for the node width
#' relative to the default.
#' @param cex_edge Double for the magnification factor for the edge width
#' relative to the default.
#' @param color Color vector of length 2 corresponding respectively to
#' background and node label.
#' @param shape Character for node shape (among: 'circle', 'square' or 'none').
#' @param dashed Boolean for dashed edges.
#' @param node Node object.
#' @param edge Edge object.
#' @param dist Integer for text distance from node.
#' @param label Boolean for edge text display.
#'
#' @return NULL (launch a basic plot)
#' @export
#'
#' @examples
#' x <- sapply(seq(5), function(j) runif(5))
#' x[x < 0.5] <- diag(x) <- 0
#' colnames(x) <- rownames(x) <- paste("Variable", seq(5))
#' x[lower.tri(x)] <- t(x)[lower.tri(x)]
#' plot_network(x)
#' e <- Edge(x)
#' plot_network(
#'     title = "Gimme a network",
#'     cex = 1.5,
#'     color = c("white", "black"),
#'     shape = "square",
#'     dashed = FALSE,
#'     edge = e,
#'     node = Node(e),
#'     dist = 6,
#'     label = TRUE
#' )
plot_network <- function(
    x = NULL,
    title = NULL,
    color = c("#eee685", "#686868"),
    cex = 1,
    cex_main = 14 * cex,
    cex_node = 3 * cex,
    cex_edge = 2 * cex,
    shape = "dot",
    dashed = TRUE,
    node = NULL,
    edge = NULL,
    dist = 1,
    label = FALSE,
    digits = 2) {
    title <- paste0(title, collapse = " ")
    if (is.null(edge)) {
        edge <- Edge(x, digits = digits)
    } else {
        stopifnot(is(edge, "Edge"))
    }
    if (is.null(node)) {
        node <- Node(edge, cex * 12)
    } else {
        stopifnot(is(node, "Node"))
    }
    if (!label) {
        edge$label <- ""
    }
    edge$weight <- abs(edge$weight)
    net <- graph_from_data_frame(
        d = edge,
        vertices = node,
        directed = FALSE
    )
    V(net)$color <- as.vector(color[1])
    V(net)$label <- node$id
    if (shape == "dot") {
        shape <- "circle"
    }
    V(net)$shape <- shape
    if (is.null(edge$color)) {
        edge_color <- "gray80"
    } else {
        edge_color <- edge$color
    }
    E(net)$width <- E(net)$weight * cex_edge

    plot(
        net,
        edge.color = edge_color,
        edge.lty = ifelse(dashed, 2, 1),
        edge.label.color = color[2],
        edge.label.family = "sans",
        edge.label.cex = cex * 0.95,
        vertex.frame.color = "gray80",
        vertex.label.cex = cex,
        vertex.label.color = color[2],
        vertex.label.dist = dist,
        vertex.label.degree = 1.5,
        vertex.label.family = "sans",
        vertex.size = cex_node * node$size * 0.5,
        vertex.frame.width = cex_node * 0.9,
        margin = c(0.1, 0, 0, 0)
    )
    title(title, cex.main = cex_main * 0.1)
}

#' Network plot (dynamic)
#'
#' Plot the connection between variables (dynamic plot). The higher the number
#' of connections, the larger the node.
#'
#' @inheritParams plot_violin
#' @inheritParams plot_network
#' @inheritParams plot_cor_network
#'
#' @return visNetwork object
#' @export
#'
#' @examples
#' x <- sapply(seq(5), function(j) runif(5))
#' x[x < 0.5] <- diag(x) <- 0
#' colnames(x) <- rownames(x) <- paste("Variable", seq(5))
#' x[lower.tri(x)] <- t(x)[lower.tri(x)]
#' plot_network(x)
#' e <- Edge(x)
#' plot_network_dyn(
#'     title = "Gimme a network",
#'     cex = 1.5,
#'     color = c("white", "gray"),
#'     shape = "square",
#'     dashed = FALSE,
#'     edge = e,
#'     node = Node(e)
#' )
plot_network_dyn <- function(
    x = NULL,
    title = NULL,
    color = c("#eee685", "#686868"),
    cex = 1,
    cex_main = 14 * cex,
    cex_node = 3 * cex,
    cex_edge = 2 * cex,
    shape = "dot",
    dashed = TRUE,
    node = NULL,
    edge = NULL,
    label = FALSE,
    digits = 2,
    ...) {
    title <- paste0(title, collapse = " ")
    if (length(color) < 2) {
        color <- c(color, "gray")
    }
    if (is.null(edge)) {
        edge <- Edge(x, digits = digits)
    } else {
        stopifnot(is(edge, "Edge"))
    }
    edge$width <- edge$weight * cex_edge
    if (is.null(node)) {
        node <- Node(edge, cex * 12)
    } else {
        stopifnot(is(node, "Node"))
    }
    node$label <- node$id
    node$title <- node$label <- node$id
    node$color.background <- rep(as.vector(color[1]), nrow(node))
    if (label) {
      edge$label <- as.character(edge$label)
    }

    visNetwork(
        node,
        edge,
        main = list(
            text = title,
            style = paste0(
                "font-family:sans;font-weight:bold;font-size:",
                cex_main * 1.4,
                "px;text-align:center;"
            )
        )
    ) %>%
        visNodes(
            borderWidth = 2,
            shape = shape,
            shadow = TRUE,
            size = cex_node * 11,
            font = list(size = cex * 21, color = color[2]),
            color = list(
                border = "#686868",
                highlight = list(background = "black", border = "darkred")
            ),
            ...
        ) %>%
        visEdges(
            smooth = FALSE,
            shadow = TRUE,
            dashes = dashed,
            font = list(size = cex * 18, color = color[2], strokeWidth = -1),
            color = list(color = color[2], highlight = "darkred")
        )
}

#' Correlation tables
#'
#' Calculates the correlation between variables in a data.frame and filters
#' according to a threshold.
#'
#' @inheritParams GimmeMyStats::mcor_test
#' @param cutoff Double for the correlation threshold.
#'
#' @return List of data.frames containing correlation and p-value matrices.
#' @export
#'
#' @examples
#' library(magrittr)
#' x <- runif(20)
#' x <- lapply(
#'     c(1, -1),
#'     function(i) sapply(seq(10), function(j) x * i + runif(10, max = 1))
#' ) %>%
#'     Reduce(cbind, .) %>%
#'     set_colnames(paste("Variable", seq(20)))
#' correlate(x)
#' correlate(
#'     x,
#'     method = "pearson",
#'     method_adjust = "none",
#'     cutoff = 0.7
#' )
correlate <- function(
    x,
    y = NULL,
    method = "spearman",
    method_adjust = "BH",
    cutoff = 0.75) {
    res <- mcor_test(
        x,
        y,
        estimate = TRUE,
        p.value = TRUE,
        method = method,
        method_adjust = method_adjust
    )
    r <- res$estimate
    if (is.null(y)) {
        diag(r) <- 0
    }
    r[abs(r) < cutoff] <- 0
    r[is.na(r)] <- 0
    p <- res$p.value
    r[p >= 0.05] <- 0
    return(list(r = r, p = p))
}


#' Correlation network
#'
#' Plot a correlation network. The higher the number of connections, the larger
#' the node. By default, negative correlations are shown in red, positive
#' correlations in green.
#'
#' @inheritParams plot_pie
#' @inheritParams plot_violin
#' @inheritParams correlate
#' @inheritParams plot_network
#' @param x Data.frame with column and row names.
#' @param colour_edge Color vector of length 2 corresponding respectively to
#' a positive or negative correlation.
#' @param colour_node Color vector of length 2 corresponding respectively to
#'  background and node label.
#' @param method Character for the test method ('pearson' or 'spearman').
#' @param is_cor Boolean to determine if x is a already a correlation object
#' or not.
#' @param ... Additional parameters in [visNetwork::visNodes].
#'
#' @return visNetwork object
#' @export
#'
#' @examples
#' library(magrittr)
#' library(RColorBrewer)
#' x <- runif(20)
#' x <- lapply(
#'     c(1, -1),
#'     function(i) sapply(seq(10), function(j) x * i + runif(10, max = 1))
#' ) %>%
#'     Reduce(cbind, .) %>%
#'     set_colnames(paste("Variable", seq(20)))
#' plot_cor_network(x)
#' plot_cor_network(
#'     x,
#'     colour_edge = c(
#'         brewer.pal(3, "Pastel1")[3],
#'         brewer.pal(3, "Pastel1")[1]
#'     ),
#'     colour_node = c("white", "black"),
#'     cex = 1.5,
#'     method = "pearson",
#'     method_adjust = "none",
#'     cutoff = 0.7,
#'     digits = 1
#' )
#' cor_obj <- correlate(x)
#' plot_cor_network(
#'     cor_obj,
#'     colour_edge = c(
#'         brewer.pal(3, "Pastel1")[3],
#'         brewer.pal(3, "Pastel1")[1]
#'     ),
#'     colour_node = c("white", "black"),
#'     cex = 1.5,
#'     is_cor = TRUE,
#'     digits = 1
#' )
plot_cor_network <- function(
    x = NULL,
    width_text = 30,
    colour_edge = c("#4DAF4A", "#EE6363"),
    colour_node = c("white", "#3D3D3D"),
    cex = 1,
    cex_node = cex * 12,
    method = "spearman",
    method_adjust = "BH",
    is_cor = FALSE,
    cutoff = 0.75,
    digits = 2,
    dyn = TRUE,
    ...) {
    if (!is_cor) {
        x <- x %>% set_colnames(colnames(.) %>% str_wrap(width_text))
        x <- correlate(x, method = method, method_adjust = method_adjust, cutoff = cutoff)
    }
    edge <- Edge(x$r, x$p, digits = digits)
    font <- "14px arial black"
    edge$font.bold.mod <- ifelse(edge$p < 0.05, paste(font, "bold"), font)
    node <- Node(edge, cex = cex_node)
    edge$color <- ifelse(
        edge$weight > 0,
        colour_edge[1],
        colour_edge[2]
    )

    if(dyn)
    plot_network_dyn(
        dashed = FALSE,
        node = node,
        edge = edge,
        cex = cex,
        cex_edge = edge$weight * 20 * cex,
        color = c(colour_node[1], colour_node[2]),
        ...
    )
    else
    plot_network(
        dashed = FALSE,
        node = node,
        edge = edge,
        cex = cex,
        cex_edge = edge$weight * 20 * cex,
        color = c(colour_node[1], colour_node[2]),
        ...
    )
}
