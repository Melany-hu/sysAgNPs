#' Calculate transition probability matrix
#'
#' @param binary_dataset A binary dataframe of datasets used to establish Transition Scores criteria.
#' @importFrom purrr map
#' @return A transfer probability matrix.
#' @export
#'
#' @examples tran_matrix <- sys_tran(binary_dataset)

sys_tran <- function(binary_dataset){

  # Initiate transition matrix.
  tran_matrix = matrix(0, ncol = ncol(binary_dataset), nrow = ncol(binary_dataset))

  # i represents the current column, j represents the column i transferred, sij represents the number of columns i transferred to column j, si0 represents the number of columns i transferred to itself
  for (i in 1:ncol(binary_dataset)) {
    sij <- colSums(binary_dataset == 1 & binary_dataset[, i] == 1)  # Count the number of remaining columns and current column i that are 1 at the same time, i.e., the number representing the transfer of column i to column j
    sii <- sum(binary_dataset[, i] == 1 & !apply(binary_dataset[, -i] == 1, 1, any))  # The number of rows with 1 in the current column i alone is si0
    sij[i] <- sii
    si <- sum(sij)  # st represents the total number of transfers
    tran_matrix[i, ] <- sij/si  # transition matrix
  }

   return(tran_matrix)
}

