#' Extract the stability results of individuals from the output of [microSTASIS::iterative_clustering()].
#'
#' @description Those individuals whose samples are clustered under the same label in list`[[x]]`$cluster sum 1. If samples are in different clusters sum 0. Then, this is done for all possible values of k and, finally, divided the sum by k, so obtaining a value between 0 and 1.
#'
#' @param klist input list corresponding to the output of [microSTASIS::iterative_clustering()].
#' @param common pattern that separates the ID and the sampling time.
#'
#' @return µSTASIS stability score (mS) for the individuals from the two selected sampling times.
#' @export
#'
#' @examples
#' t1_t2 <- paired_times(data = clr[,1:50], first = "_1",
#'                       second = "_25", common = "_0_")
#' klist_t1_t2 <- iterative_clustering(data = t1_t2, parallel = FALSE)
#' result_t1_t2 <- stabilitas(klist_t1_t2, common = "_0_")
stabilitas <- function(klist, common){
  samples <- stringr::str_split(names(klist[[1]]$cluster), common, simplify = TRUE)
  ind <- unique(samples[,1])
  stability <- data.frame(matrix(0, nrow = length(ind),
                                 ncol = length(klist)))
  rownames(stability) <- ind
  colnames(stability) <- 1+(1:length(klist))
  for (i in 1:length(klist)) {
    for (j in 1:length(ind)){
      ind_samples <- which(stringr::str_detect(names(klist[[i]]$cluster), ind[j]))
      if (klist[[i]]$cluster[ind_samples][1] ==
          klist[[i]]$cluster[ind_samples][2]){
        stability[j,i] <- 1
      }
    }
  }
  return(round(apply(stability, 1, sum) /
                 length(stability), 3))
}
