#' Plot a scatter and side boxplot of the stability results
#'
#' @param data input data frame resulting from [microSTASIS::st_previz()].
#' @param order character: none, mean or median; if the scatter plot should be sorted by any of those statistics of the stability values by individuals.
#' @param times a vector with the names of each paired time, e.g. "t1_t2".
#' @param grid_lines logical; FALSE to print a blank background or TRUE to include a gray grid
#' @param side_scale numeric; scale of the side boxplot
#'
#' @return A scatter plot and a side boxplot of the stability values in the form of a "ggplot" object.
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
#' st_scatter(data = results, order = "median",
#'             times = c("t1_t2", "t2_t3"), grid_lines = TRUE,
#'             side_scale = 0.2)
st_scatter <- function(data, order = "none", times, grid_lines = FALSE, side_scale = 0.3){
  data <- data[,c(1, which(colnames(data) %in% times))]
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
  p <- ggplot2::ggplot(melt.data, ggplot2::aes(y = individual, x = value)) +
    ggplot2::geom_point(ggplot2::aes(color = variable), size = 3) + ggplot2::theme_bw() +
    ggplot2::xlim(0,1) + ggplot2::labs(y = "Individual", x = "Stability") +
    ggside::geom_xsideboxplot(ggplot2::aes(y = variable, fill = variable),
                              orientation = "y")
  if (grid_lines){
    p + ggplot2::theme(legend.position = 'none', legend.title = ggplot2::element_blank(),
                     panel.grid.major.x = ggplot2::element_line(colour = "gray95"),
                     panel.grid.major.y = ggplot2::element_line(colour = "gray90"),
                     axis.ticks = ggplot2::element_blank(),
                     ggside.panel.scale = side_scale)
  } else{
    p + ggplot2::theme(legend.position = 'none', legend.title = ggplot2::element_blank(),
                       panel.grid.major.x = ggplot2::element_blank(),
                       panel.grid.major.y = ggplot2::element_blank(),
                       axis.ticks = ggplot2::element_blank(),
                       ggside.panel.scale = side_scale)
  }
}
