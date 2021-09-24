#' Process the [microSTASIS::stabilitas()] output to a new format ready for the visualization functions.
#'
#' @param results a list with the [microSTASIS::stabilitas()] outputs.
#' @param times a vector with the names of each paired time, e.g. "t1_t2".
#'
#' @return A data frame ready for its use under the visualization functions.
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
st_previz <- function(results, times){
  if (is.list(results)) {
    if (length(results) == length(times)) {
      individual <- sort(unique(unlist(lapply(results, names))))
      sTable <- data.frame(individual)
      by_times <- lapply(1:length(results), function(x) {
        d <- data.frame(names(results[[x]]), results[[x]])
        colnames(d) <- c("ind", times[x])
        d
      })
      for (i in 1:length(by_times)){
        sTable <- merge(sTable, by_times[[i]], by.x = "individual",
                        by.y = "ind", all.x = TRUE, all.y = FALSE, sort = TRUE)
      }
      sTable } else {
        message("Both arguments must have the same length")
      }
  } else {
    message("The argument results is not a list")
  }
}
