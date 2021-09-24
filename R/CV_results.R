#' Compute the error or plot the stability values after [microSTASIS::CV_iterative_clustering()].
#'
#' @description Compute the mean absolute error after the cross validation or plot lines connecting the stability values for each subset of the original matrix of paired times.
#'
#' @param data input matrix with paired times, i.e. samples to be stressed to multiple iterations.
#' @param cv_klist list resulting from [microSTASIS::CV_iterative_clustering()].
#' @param output character: MAE or viz; to return a data frame with the MAE or to visualize a line plot.
#' @param points logical; if plotting, FALSE to only plot lines and TRUE to add points on the original stability value, i.e. result from [microSTASIS::stabilitas()].
#' @param k integer; number of individuals to subset from the data. The same as used in [microSTASIS::CV_iterative_clustering()].
#' @param size_line numeric; if plotting, size of the multiple lines.
#'
#' @return A vector with MAE values or a line plot  in the form of a "ggplot" object with the values of stability for the multiple subsets and the original matrix of paired samples.
#' @export
#'
#' @examples
#' \donttest{
#' t1_t2 <- paired_times(data = clr, first = "_1",
#'                       second = "_25", common = "_0_")
#' klist_t1_t2 <- iterative_clustering(data = t1_t2, parallel = FALSE)
#' result_t1_t2 <- stabilitas(klist_t1_t2, common = "_0_")
#' cv_klist_t1_t2_k2 <- CV_iterative_clustering(data = t1_t2, results = result_t1_t2,
#'                                              common = "_0_", k = 2L, parallel = FALSE)
#' MAE_t1_t2 <- CV_results(data = t1_t2, cv_klist = cv_klist_t1_t2_k2,
#'                        output = "MAE", k = 2L)
#' MAE <- st_previz(results = list(MAE_t1_t2),
#'                  times = "MAE_t1_t2")
#' st_heatmap(data = MAE, label = TRUE,
#'            high = 'red2', midpoint = 0.075, low = 'forestgreen')
#' CV_results(data = t1_t2, cv_klist = cv_klist_t1_t2_k2,
#'            output = "viz", k = 2L)
#' }
CV_results <- function(data, cv_klist, output = "MAE",
                       points = TRUE, k = 1L, size_line = 0.5){
  individuals <- unique(stringr::str_split(rownames(data), "_0_", simplify = TRUE)[,1])
  if (output == "viz"){
    if (k == 1L){
      object <- data.frame(x = stats::reorder(individuals, sort(as.character(individuals))),
                           y = unlist(cv_klist),
                           individual = sort(rep(individuals, length(cv_klist))))
    } else {
      samples <- seq(1, dim(data)[1], by = 2)
      removed_ind <- sapply(1:length(cv_klist[[1]]), function(r){
        stringr::str_c(individuals[which(samples %in%
                                  as.vector(stringr::str_split(names(cv_klist[[1]][r]),
                                                      ", ", simplify = TRUE)))],
              collapse = ", ")
      })
      object <- data.frame(x = stats::reorder(removed_ind, sort(as.character(removed_ind))),
                           y = unlist(cv_klist),
                           individual = sort(rep(individuals, length(cv_klist[[1]]))))
    }
    individual <- x <- y <- NULL
    p <- ggplot2::ggplot(object, ggplot2::aes(x = x, y = y, group = individual, colour = individual)) +
      ggplot2::geom_line(size = size_line) + ggplot2::theme_bw() + ggplot2::labs(x = NULL, y = "Stability") +
      ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))
    if (points){
      if (k == 1L){
        p + ggplot2::geom_point(data = subset(object, x == individual),
                       mapping = ggplot2::aes(x = x, y = y, group = individual, colour = individual))
      } else {
        p + ggplot2::geom_point(data = subset(object, stringr::str_detect(x, individual)),
                       mapping = ggplot2::aes(x = x, y = y, group = individual, colour = individual))
      }
    } else {
      p
    }
  } else if (output == "MAE"){
    if (k == 1L){
      CV_matrix <- matrix(unlist(cv_klist), nrow = length(cv_klist)[[1]], byrow = TRUE)
    } else {
      samples <- seq(1, dim(data)[1], by = 2)
      matrix <- matrix(nrow = length(samples), ncol = length(samples))
      CV_matrix <- t(sapply(1:length(cv_klist), function(l){
        sapply(1:length(samples), function(i){
          site <- which(stringr::str_split(names(cv_klist[[l]]),
                                  ", ", simplify = TRUE) == as.character(samples[i]))
          while (site > length(cv_klist[[l]])){
            site <- site - length(cv_klist[[l]])
          }
          matrix[l,i] <- cv_klist[[l]][site]
        })
      }))
    }
    MAE <- round(sapply(1:dim(CV_matrix)[2], function(i){
      (sum(abs(CV_matrix[i,i] - CV_matrix[i,]))) / (length(CV_matrix[i,]) - k)
    }), 2)
    names(MAE) <- individuals
    message("Summary of MAE for the whole data set:")
    print(summary(MAE))
    message("\n")
    return(MAE)
  } else {
    message("Enter output = \"viz\" for visualization or
output = \"MAE\" for mean absolute error by individual")
  }
}
