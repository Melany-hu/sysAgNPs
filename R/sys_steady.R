#' @title Iterate to obtain the steady state probability
#' @description Change the values of the constraints step by step and record the number of iterations to reach the steady state.
#'
#' @param binary_dataset A binary dataframe of datasets used to establish evaluation criteria.
#' @param tran_matrix A transfer probability matrix.
#' @param tol_vec A smaller constants used as constraints.
#' @importFrom expm %^%
#'
#' @return A data frame containing the constraints and the number of iterations to reach the steady state.
#' @export
#'
#' @examples tol_iter <- sys_steady(binary_dataset, tran_matrix, 1e-5)

sys_steady <- function(binary_dataset, tran_matrix, tol_vec = c(1e-2, 1e-3, 1e-4, 1e-5, 1e-6, 1e-7)){

  # Initial state probability vector.
  M_0 <- rep(1/ncol(binary_dataset), ncol(binary_dataset))

  # Store the results of the iteration count
  iter_result <- c()

  # Define the initial probability vector
  prob_vec_old <- M_0 %*% tran_matrix
  prob_vec_new <- M_0

  # Initialize the number of iterations, iter_count=1 represents the initial probability probability vector
  iter_count <- 1

  # Initialize list to store the number of iterations and the result of the probability vector
  iter_list <- list()

  # Run the iterative computation inside the loop. At the end of each iteration, use the if statement to determine if the current iteration has reached convergence, and if so, store the corresponding iter_count value in a list of results, otherwise continue with the next iteration.
  for (i in 1:length(tol_vec)) {
    tolerance <- tol_vec[i]
    while (!isTRUE(all.equal(prob_vec_old, prob_vec_new, tolerance))) {
      prob_vec_old <- prob_vec_new
      prob_vec_new <- M_0 %*% (tran_matrix %^% iter_count)
      iter_list[[iter_count]] <- prob_vec_new
      iter_count <- iter_count + 1
      if (isTRUE(all.equal(prob_vec_old, prob_vec_new, tolerance))) {
        iter_result[i] <- iter_count
      }
    }
  }

  # Obtain the relationship between the number of iterations and the constraints
  iter_result <- data.frame(tolerance = tol_vec, iteration = iter_result)

  return(iter_result)
}
