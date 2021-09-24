#' Plot a radar chart of paired samples from one individual or from different groups.
#'
#' @param data input matrix with paired times.
#' @param samples position of the samples from an individual to be plotted.
#' @param pre_radar list; the output from [microSTASIS::pre_radarPC()].
#' @param legend text to plot.
#' @param colors specify desired colors.
#' @param groups vector with the same length as the number of rows in the [microSTASIS::st_previz()] output.
#' @param fun character: mean or median; if the lines should correspond to any of those statistics when adding a groups vector.
#'
#' @return A radar chart (or spider plot) of the principal components for two samples from the same individual or for two groups.
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
#' pre_radar <- pre_radarPC(data = t1_t2, exp.var = 0.85)
#' radarPC(data = t1_t2, samples = c(3,4), pre_radar = pre_radar, colors = c(1,2))
#' metadata <- data.frame(Sample = rownames(clr),
#'                        age = c(rep("youth", 65), rep("old", 131-65)))
#' group <- metadata_groups(metadata = metadata, samples = metadata$Sample,
#'                          individuals = results$individual, col_number = 2)
#' radarPC(data = t1_t2, pre_radar = pre_radar, groups = rep(group, each = 2),
#'         colors = c("cyan1", "cyan3", "brown1", "brown3"))
radarPC <- function(data, samples = c(1,2), pre_radar = pre_radar,
                    legend = expression("t"[1], "t"[2]),
                    colors = c("orange", "royalblue"),
                    groups = NULL, fun = "mean") {
  if (is.null(groups)) {
    ind <- data.frame(rbind(pre_radar$out.max, pre_radar$in.min, pre_radar$PCs[samples,]))
    exp.var <- round(sum(pre_radar$explained_variance[1:pre_radar$neededPCs])*100, 1)
    dist.clr <- as.matrix(stats::dist(data))
    colors_border <- c(grDevices::adjustcolor(colors[1], alpha.f = 1),
                       grDevices::adjustcolor(colors[2], alpha.f = 1))
    colors_in <- c(grDevices::adjustcolor(colors[1], alpha.f = 0.3),
                   grDevices::adjustcolor(colors[2], alpha.f = 0.3))
    fmsb::radarchart(ind, cglcol = 1, cglty = 3, plwd = 3, plty = 1,
               title = paste("Individual:", stringr::str_split(rownames(pre_radar$PCs[samples,])[1], "_")[[1]][1],
                             "\nExplained variance:", exp.var, "%",
                             "\nAitchinson distance:", round(dist.clr[samples[1],samples[2]], 2)),
               pcol = colors_border, pfcol = colors_in, vlabels = 1:pre_radar$neededPCs)
    graphics::legend("topleft", legend = legend, col = colors_border,
           seg.len = 2, border = "transparent", pch = 16, lty = 1)
  } else {
    if (length(groups) == dim(data)[1]){
      names_groups <- unique(groups)
      number_groups <- length(names_groups)
      sub_list <- lapply(1:number_groups, function(x){
        subset(pre_radar$PCs, groups == names_groups[x])
      })
      if (fun == "median"){
        reslt <- lapply(sub_list, function(x){
          list(
            t1 = apply(x[seq(1, dim(x)[1], by = 2),], 2, stats::median),
            t2 = apply(x[seq(2, dim(x)[1], by = 2),], 2, stats::median)
          )
        })
      } else if (fun == "mean"){
        reslt <- lapply(sub_list, function(x){
          list(
            t1 = apply(x[seq(1, dim(x)[1], by = 2),], 2, mean),
            t2 = apply(x[seq(2, dim(x)[1], by = 2),], 2, mean)
          )
        })
      }
      ind <- data.frame(rbind(pre_radar$out.max, pre_radar$in.min,
                              do.call(rbind, lapply(reslt, function(x){
                                t(as.data.frame(x))
                              }))))
      exp.var <- round(sum(pre_radar$explained_variance[1:pre_radar$neededPCs])*100, 1)
      if (all(colors == c("orange", "royalblue"))) {
        colors <- colors()[stringr::str_detect(colors(), "1") |
                             stringr::str_detect(colors(), "3")]
        colors <- colors[!(stringr::str_detect(colors, "gray") |
                             stringr::str_detect(colors, "grey"))]
        random_col <- sample(1:(length(colors) - number_groups*2), 1)
        if(!((random_col %% 2) == 0)) {
          random_col <- random_col - 1
        }
        colors_border <- unlist(lapply((1:number_groups)*2, function(x) {
          c(colors[x-1+random_col], colors[x+random_col])
        }))
      } else if (length(colors) == number_groups*2){
        colors_border <- colors
      } else {
        message("The length of the colors argument do not match with two times the number of different groups.
The tool will automatically set the colors.")
        colors <- colors()[stringr::str_detect(colors(), "1") |
                             stringr::str_detect(colors(), "3")]
        colors <- colors[!(stringr::str_detect(colors, "gray") |
                             stringr::str_detect(colors, "grey"))]
        random_col <- sample(1:(length(colors) - number_groups*2), 1)
        if(!((random_col %% 2) == 0)) {
          random_col <- random_col - 1
        }
        colors_border <- unlist(lapply((1:number_groups)*2, function(x) {
          c(colors[x-1+random_col], colors[x+random_col])
        }))
      }
      legend_g <- unlist(lapply(names_groups, function(x, time = legend){
        c(substitute(paste(t[time1], " ", var), list(var = x, time1 = time[1])),
          substitute(paste(t[time2], " ", var), list(var = x, time2 = time[2])))
      }))
      fmsb::radarchart(ind, cglcol = 1, cglty = 3, plwd = 3, plty = 1,
                 title = paste("\nExplained variance:", exp.var, "%"),
                 pcol = colors_border, vlabels = 1:pre_radar$neededPCs)
      graphics::legend("topleft", legend = as.expression(legend_g), col = colors_border,
             seg.len = 2, border = "transparent", pch = 16, lty = 1)
    } else {
      message("The length of groups vector and the number of rows in the data matrix are not the same")
    }
  }
}
