#' Generate boxplots of the stability evolution throughout sampling times by groups
#'
#' @param data input data frame resulting from [microSTASIS::st_previz()].
#' @param groups vector with the same length as the number of rows in the [microSTASIS::st_previz()] output.
#' @param points logical; FALSE to only visualize boxplots or TRUE to also add individual points.
#' @param linetype numeric; type of line to connect the median value of paired times; 0 to avoid the line.
#'
#' @return A plot with as many boxes as paired times by group in the form of a "ggplot" object.
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
#' metadata <- data.frame(Sample = rownames(clr),
#'                        age = c(rep("youth", 65), rep("old", 131-65)))
#' group <- metadata_groups(metadata = metadata, samples = metadata$Sample,
#'                          individuals = results$individual, col_number = 2)
#' st_evolution(results, groups = group, points = TRUE, linetype = 0)
st_evolution <- function(data, groups = NULL, points = TRUE, linetype = 2){
  if (is.null(groups)){
    message("There are not groups;
this plot is the same as the one on top of st_scatter() output ")
    evo <- reshape2::melt(data)
    variable <- value <- NULL
    ggplot2::ggplot(evo, ggplot2::aes(x = variable, y = value, fill = variable)) +
      ggplot2::geom_jitter(size = 1.4) +
      ggplot2::geom_boxplot(outlier.shape = NA, alpha = 0.7,
                   width = 0.2, colour = "black") +
      ggplot2::theme_classic() + ggplot2::labs(x = NULL, y = "Stability") +
      ggplot2::guides(fill = ggplot2::guide_colorbar(title.position = 'top',
                                   title.hjust = .5,
                                   barwidth = grid::unit(10, 'lines'),
                                   barheight = grid::unit(.5, 'lines'))) +
      ggplot2::theme(legend.position = 'top', legend.title = ggplot2::element_blank(),
            axis.text.x = ggplot2::element_text(margin = ggplot2::margin(t = 2.5, r = 0, b = 0, l = 0)),
            axis.text.y = ggplot2::element_text(margin = ggplot2::margin(t = 0, r = 5, b = 0, l = 0)),
            axis.ticks = ggplot2::element_line(size = 0.5),
            axis.ticks.length.y = grid::unit(.25, "cm"),
            axis.ticks.length.x = grid::unit(.25, "cm"))
  } else {
    data$group <- groups
    evo <- reshape2::melt(data)
    group <- variable <- value <- NULL
    plot <- ggplot2::ggplot(evo, ggplot2::aes(x = variable, y = value)) +
      ggplot2::stat_summary(fun = stats::median, geom = "line", ggplot2::aes(group = group, color = group),
                   linetype = linetype, size = 1.3) +
      ggplot2::geom_boxplot(ggplot2::aes(fill = group), outlier.shape = NA,
                   width = 0.2, colour = "black", alpha = 0.6) +
      ggplot2::labs(x = NULL, y = "Stability") + ggplot2::theme_classic() +
      ggplot2::theme(legend.position = 'top', legend.title = ggplot2::element_blank(),
            axis.text.x = ggplot2::element_text(margin = ggplot2::margin(t = 2.5, r = 0, b = 0, l = 0)),
            axis.text.y = ggplot2::element_text(margin = ggplot2::margin(t = 0, r = 5, b = 0, l = 0)),
            axis.ticks = ggplot2::element_line(size = 0.5),
            axis.ticks.length.y = grid::unit(.25, "cm"),
            axis.ticks.length.x = grid::unit(.25, "cm"))
    if (points) {
      plot + ggplot2::geom_jitter(ggplot2::aes(fill = group),
                         position = ggplot2::position_jitterdodge(jitter.width = 0.15),
                         size = 2, pch = 21, alpha = 0.75)
    } else {plot}}
}
