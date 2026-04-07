#' @export
save_plot <- function(
        f,
        filename = "plot.tiff",
        width = 2000,
        height = 2000,
        res = 300,
        type = "tiff"
    ) {
    get(type)(
        filename,
        units = "px",
        width = width,
        height = height,
        res = res
    )
    print(f)
    dev.off()
}
