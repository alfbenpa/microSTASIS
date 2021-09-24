#' Cross validation of the iterative Hartigan-Wong k-means clustering.
#'
#' @description Perform cross validation in the way of leave-one-out (LOO) or k-fold of the stability results from [microSTASIS::iterative_clustering()].
#'
#' @param data input matrix with paired times, i.e. samples to be stressed to multiple iterations.
#' @param results the [microSTASIS::stabilitas()] output for the concrete paired times used for validation.
#' @param common pattern that separates the ID and the sampling time.
#' @param k integer; number of individuals to subset from the data for each time running [microSTASIS::iterative_clustering()].
#' @param parallel logical; FALSE to sequentially run the internal loop or TRUE to do it by parallel computing (number of cores = 4).
#'
#' @return Multiple lists with multiple objects of class "kmeans".
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
#' }
CV_iterative_clustering <- function(data, results, common,
                                    k = 1L, parallel = TRUE) {
  samples <- seq(1, dim(data)[1], by = 2)
  kfold <- list()
  if (((dim(data)[1]/2)%%k) == 0) {
    for (i in 1:trunc((dim(data)[1]/2)/k)){
      if (length(samples) != k) {
        kfold[[i]] <- sample(samples, k, replace = FALSE)
        samples <- samples[!(samples %in% kfold[[i]])]
      } else {kfold[[i]] <- samples}
    }
    if (parallel) {
      future::plan(future::multisession(workers = 4L))
      progressr::handlers(global = TRUE)
      progressr::handlers(progressr::handler_pbcol(complete = function(s) crayon::bgGreen(crayon::white(s)),
                           incomplete = function(s) crayon::bgRed(crayon::white(s))))

      cv_par_wk <- function(xs, kfold, ...) {
        p <- progressr::progressor(along = kfold)
        y <- future.apply::future_lapply(kfold, function(x){
          p()
          lapply(2:(dim(xs)[1]-(1+length(x)*2)), function(j) {
            stats::kmeans(xs[-c(x,x+1),], j,
                             iter.max = 20, nstart = 50)
        })
        }, future.seed = NULL)
      }
    CVklist <- cv_par_wk(data, kfold)
    } else {
      CVklist <- lapply(kfold, function(x) {
        lapply(2:(dim(data)[1]-(1+length(x)*2)), function(s){
        stats::kmeans(data[-c(x,x+1),], s, iter.max = 20, nstart = 50)
      })})
    }
    CVresult <- lapply(CVklist, microSTASIS::stabilitas, common = common)
    individuals <- unique(stringr::str_split(rownames(data), common, simplify = TRUE)[,1])

    LOO <- lapply(individuals, function(ind) {
      LOOCV <- unlist(lapply(1:length(CVresult), function(i) {
        if (any(ind == names(CVresult[[i]]))) {
          CVresult[[i]][(ind == names(CVresult[[i]]))]
        } else {
          results[names(results) == ind]
        }
      }), use.names = FALSE)
      if (k == 1) {
        names(LOOCV) <- unlist(kfold)
        return(LOOCV[order(as.integer(names(LOOCV)))])
      } else {
        names(LOOCV) <- unlist(lapply(kfold, paste, collapse = ", "))
        return(LOOCV[order(sort(names(LOOCV)))])
      }
    })
    return(LOO)
  } else {message("This k number do not allow for exact sampling")}
}
