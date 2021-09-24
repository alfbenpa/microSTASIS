#' Generate a subset matrix with paired times.
#'
#' @param data input matrix with each rowname including an ID, a common pattern and a sampling time.
#' @param first pattern associated with the first of the two sampling times.
#' @param second pattern associated with the second of the two sampling times.
#' @param common pattern that separates the ID and the sampling time.
#'
#' @return A matrix with the same number of columns as input and the samples from both samples times.
#' @export
#'
#' @examples
#' t1_t2 <- paired_times(data = clr, first = "_1",
#'                       second = "_25", common = "_0_")
paired_times <- function(data, first, second, common){
  a <- subset(data, stringr::str_detect(rownames(data), first) |
                stringr::str_detect(rownames(data), second))
  individuals <- c()
  for (i in 1:dim(a)[1]) {
    individuals[i] <- stringr::str_split(rownames(a), common)[[i]][1]
  }
  individuals <- unique(individuals)
  remove <- c()
  for (j in 1:length(individuals)){
    ind <- which(stringr::str_detect(rownames(a), individuals[j]))
    if (length(ind) == 1){
      remove <- c(remove, ind)
    }
  }
  if (is.null(remove)){
    (a)
  } else {
    (result <- a[-remove,])
  }
}
