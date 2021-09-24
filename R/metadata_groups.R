#' Easily extract groups of individuals from metadata variables.
#'
#' @param metadata data frame with one column of samples matching with the rownames of the original input matrix to [microSTASIS::paired_times()].
#' @param samples vector from metadata corresponding to the samples ID.
#' @param individuals vector of individuals located in the first column of the [microSTASIS::st_previz()] output.
#' @param col_number number of the column with the variable used for grouping individuals.
#'
#' @return A vector with the same length as the number of rows in the [microSTASIS::st_previz()] output.
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
metadata_groups <- function(metadata, samples, individuals, col_number){
  variable <- rep("", length(individuals))
  for (i in 1:length(individuals)){
    samples_ind <- which(stringr::str_detect(samples, paste(individuals[i], "_", sep = "")))
    for (j in 1:length(samples_ind)){
      if (metadata[samples_ind[1], col_number] == metadata[samples_ind[j], col_number]){
        variable[i] <- as.character(metadata[samples_ind[1], col_number])
      } else {
        print(metadata[samples_ind[c(1,j)], col_number])
        message(paste("Individual", individuals[i], "presents multiple values for variable", colnames(metadata)[col_number]))
        variable[i] <- as.character(readline("Enter the desired value:"))
        break
      }
    }
  }
  return(variable)
}
