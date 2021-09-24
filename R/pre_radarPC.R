#' Compute the PCA to the [microSTASIS::paired_times()] output previously to visualize radar charts of principal components.
#'
#' @param data input matrix with paired times.
#' @param exp.var desired explained variance, i.e. a value between 0 and 1.
#' @param limit times to distance an interquartile range from the first and third quartile; default is 1.5, i.e. the whiskers of a boxplot.
#'
#' @return The needed objects to later plot a radar chart (or spider plot) of principal components.
#' @export
#'
#' @examples
#' t1_t2 <- paired_times(data = clr, first = "_1",
#'                       second = "_25", common = "_0_")
#' pre_radarPC(data = t1_t2, exp.var = 0.85)
pre_radarPC <- function(data, exp.var = 0.75, limit = 1.5) {
  data <- stats::prcomp(data)
  explained_variance <- data$sdev^2/sum(data$sdev^2)
  for (i in 1:length(explained_variance)) {
    if (sum(explained_variance[1:i]) > exp.var) {
      break
    }
  }
  message(paste("For explaining at least", exp.var*100,
                "% of the variance,", i, "principal components are required"))
  neededPCs <- i
  PCs <- data$x[,1:i]
  out.max <- apply(PCs, 2, function(x){
    if(
      max(x) < (stats::quantile(x)[4] + limit*(stats::quantile(x)[4] - stats::quantile(x)[2]))
    ) {return(max(x))}
    else {
      return(stats::quantile(x)[4] + limit*(stats::quantile(x)[4] - stats::quantile(x)[2]))
    }
  })
  in.min <- apply(PCs, 2, function(x){
    if(
      min(x) > (stats::quantile(x)[2] - limit*(stats::quantile(x)[4] - stats::quantile(x)[2]))
    ) {return(min(x))}
    else {
      return(stats::quantile(x)[2] - limit*(stats::quantile(x)[4] - stats::quantile(x)[2]))
    }
  })
  return(list(explained_variance = explained_variance, neededPCs = neededPCs, PCs = PCs,
              out.max = out.max, in.min = in.min))
}
