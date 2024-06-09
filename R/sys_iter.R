#' @title Obtain the transition probability of each iteration
#' @description Loop "n_iter" times to obtain the transition probability of each iteration.
#'
#' @param binary_dataset A binary dataframe of datasets used to establish evaluation criteria.
#' @param n_iter The number of iterations to reach the steady state.
#' @param vars_to_discretize Variables or columns to be discretized. Default id NULL.
#' @importFrom expm %^%
#'
#' @return A dataframe containing the number of iterations and the transition probability of each iteration.
#' @export
#'
#' @examples iter_prob <- sys_iter(binary_dataset, 6, c("Shape", "pH"))

sys_iter <- function(binary_dataset, n_iter, vars_to_discretize = NULL) {

  if(is.null(vars_to_discretize)) {

    # NULL
    dis_data <- sys_discretize(binary_dataset, vars_to_discretize)

    # Calculate transfer probability matrix.
    tran_matrix <- sys_tran(dis_data)

    # Initial state probability vector.
    M_0 <- rep(1/ncol(dis_data), ncol(dis_data))

    # Iterate "n_iter" times to obtain the transfer probability vector.
    prob_vec_list <- list()

    # Loop "n_iter" times to count the results of each iteration.
    for (i in 1:n_iter) {
      prob_vec <- M_0 %*% (tran_matrix %^% i)
      prob_vec_list[[1]] <- M_0  # Starting from M_0*tran_matrix for the first iteration.
      prob_vec_list[[i + 1]] <- prob_vec  # [[n+1]] is the result of the n-th iteration.
    }

    # Transform transfer probability vectors into dataframe.
    iter_prob <- data.frame(feature = rep(colnames(dis_data), times = n_iter+1),
                            iteration = rep(c(0:n_iter), each = ncol(tran_matrix)),
                            probability = unlist(prob_vec_list))
    return(iter_prob)

  } else {

    # Convert categorical variables into discrete variables.
    dis_data <- sys_discretize(binary_dataset, vars_to_discretize)

    # Calculate transfer probability matrix.
    tran_matrix <- sys_tran(dis_data)

    # Initial state probability vector.
    M_0 <- rep(1/ncol(dis_data), ncol(dis_data))

    # Iterate "n_iter" times to obtain the transfer probability vector
    prob_vec_list <- list()

    # Loop "n_iter" times to count the results of each iteration
    for (i in 1:n_iter) {
      prob_vec <- M_0 %*% (tran_matrix %^% i)
      prob_vec_list[[1]] <- M_0  # Starting from the initial M_0M
      prob_vec_list[[i + 1]] <- prob_vec  # [[7]] is the result of the 6th iteration
    }

    # Transformation of transfer probability vectors into dataframe
    iter_prob <- data.frame(feature = rep(colnames(dis_data), times = n_iter+1),
                            iteration = rep(c(0:n_iter), each = ncol(tran_matrix)),
                            probability = unlist(prob_vec_list))

    # Group
    group_list <- sapply(binary_dataset[, vars_to_discretize], unique)

    # Initialize group column
    iter_prob$group <- iter_prob$feature

    for (i in 1:length(group_list)) {
      iter_prob$group <- ifelse(iter_prob$feature %in% group_list[[i]],
                                names(group_list)[i],
                                iter_prob$group)
    }

    iter_prob %<>%
      select(group, everything())

    return(iter_prob)
  }
}

