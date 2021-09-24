#' Plot a heatmap of the stability results
#'
#' @param data input data frame resulting from [microSTASIS::st_previz()].
#' @param order character: none, mean or median; if the heatmap should be sorted by any of those statistics of the stability values by individuals.
#' @param label logical; FALSE to avoid printing the value or TRUE to print it.
#' @param low color for the lowest value.
#' @param mid color for the middle value.
#' @param high color for the highest values.
#' @param midpoint value to situate the middle.
#'
#' @return A heatmap of the stability values in the form of a "ggplot" object
#' @export
#'
#' @examples
#' t1_t2 <- paired_times(data = clr[,1:25], first = "_1",
#'                       second = "_25", common = "_0_")
#' t2_t3 <- paired_times(data = clr[,1:25], first = "_25",
#'                       second = "_26", common = "_0_")
#' klist_t1_t2 <- iterative_clustering(data = t1_t2, parallel = FALSE)
#' klist_t2_t3 <- iterative_clustering(data = t2_t3, parallel = FALSE)
#' result_t1_t2 <- stabilitas(klist_t1_t2, common = "_0_")
#' result_t2_t3 <- stabilitas(klist_t2_t3, common = "_0_")
#' results <- st_previz(results = list(result_t1_t2, result_t2_t3),
#'                      times = c("t1_t2", "t2_t3"))
#' st_heatmap(data = results, order = "mean", label = TRUE)
st_heatmap <- function(data, order = "none", label = FALSE, low = 'red2', mid = 'yellow',
                       high = 'forestgreen', midpoint = 0.5) {
  data[,1] <- factor(data[,1])
  melt.data <- reshape2::melt(data, id = c("individual"))
  if (order == "median"){
    melt.data$individual <- factor(x = melt.data$individual,
                                   levels = data$individual[as.integer(data[order(apply(data[,-1], 1,
                                                                                        function(x) {stats::median(x, na.rm = TRUE)}), decreasing = FALSE),1])], ordered = TRUE)
  } else if (order == "mean") {
    melt.data$individual <- factor(x = melt.data$individual,
                                   levels = data$individual[as.integer(data[order(apply(data[,-1], 1,
                                                                                        function(x) {mean(x, na.rm = TRUE)}), decreasing = FALSE),1])], ordered = TRUE)
  }
  individual <- variable <- value <- NULL
  plot <- ggplot2::ggplot(melt.data, ggplot2::aes(y = individual, x = variable)) +
    ggplot2::geom_tile(ggplot2::aes(fill = value)) +
    ggplot2::scale_fill_gradient2(high = high, mid = mid, midpoint = midpoint,
                         low = low, na.value = 'white') +
    ggplot2::theme_void() + ggplot2::guides(fill = ggplot2::guide_colorbar(title.position = 'top',
                                                title.hjust = .5,
                                                barwidth = grid::unit(10, 'lines'),
                                                barheight = grid::unit(.5, 'lines'))) +
    ggplot2::theme(legend.position = 'top', legend.title = ggplot2::element_blank(),
          axis.text.x = ggplot2::element_text(margin = ggplot2::margin(t = 2.5, r = 0, b = 0, l = 0)),
          axis.text.y = ggplot2::element_text(margin = ggplot2::margin(t = 0, r = 5, b = 0, l = 0)),
          axis.ticks = ggplot2::element_line(size = 0.5),
          axis.ticks.length.y = grid::unit(.25, "cm"),
          axis.ticks.length.x = grid::unit(.25, "cm"))
  if (label) {
    plot + ggplot2::geom_text(ggplot2::aes(label = value))
  } else {
    plot
  }
}
