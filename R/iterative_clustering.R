#' Iterative Hartigan-Wong k-means clustering.
#'
#' @description Perform Hartigan-Wong [stats::kmeans()] algorithm as many times as possible. The values of k are from 2 to the number of rows minus 1.
#'
#' @param data input matrix with paired times, i.e. samples to be stressed to multiple iterations.
#' @param parallel logical; FALSE to sequentially run the internal loop or TRUE to do it by parallel computing (number of cores = 4).
#'
#' @return A list with multiple objects of class "kmeans".
#' @export
#'
#' @examples
#' t1_t2 <- paired_times(data = clr[,1:50], first = "_1",
#'                       second = "_25", common = "_0_")
#' klist_t1_t2 <- iterative_clustering(data = t1_t2, parallel = FALSE)
iterative_clustering <- function(data, parallel = TRUE) {
  if (parallel) {
    future::plan(future::multisession(workers = 4L))
    progressr::handlers(global = TRUE)
    progressr::handlers(progressr::handler_pbcol(complete = function(s) crayon::bgGreen(crayon::white(s)),
                           incomplete = function(s) crayon::bgRed(crayon::white(s))))
    par_wk <- function(f, ...) {
      p <- progressr::progressor(along = 2:(dim(f)[1]-1))
      y <- future.apply::future_lapply(2:(dim(f)[1]-1), function(x, ...) {
        p()
        stats::kmeans(f, x, iter.max = 20, nstart = 50)
      }, future.seed = NULL)
    }
    return(par_wk(data))
  } else {
    return(lapply(2:(dim(data)[1]-1), function(s){
      stats::kmeans(data, s, iter.max = 20, nstart = 50)
    }))
  }
}
