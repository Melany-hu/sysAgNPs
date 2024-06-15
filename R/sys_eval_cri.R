#' @title Build Transition Scores criteria
#' @description This function evaluates the criteria for a binary dataset by calculating the transfer probability matrix and iterating to obtain the transfer probability vector.
#'
#' @param binary_dataset A binary dataframe of datasets used to establish evaluation criteria.
#' @param n_iter The number of iterations to reach the steady state.
#' @param vars_to_discretize Variables or columns to be discretized. Default is NULL.
#' @importFrom rio import export
#' @importFrom tibble tibble
#' @importFrom dplyr select mutate pull arrange
#' @importFrom tidyr unnest
#' @importFrom purrr map_dfr accumulate
#' @importFrom expm %^%
#'
#' @return A dataframe containing the scores of nanomaterial features.
#' @export
#'
#' @examples criteria <- sys_eval_cri(binary_dataset, 6, var_dis)

sys_eval_cri <- function(binary_dataset, n_iter, vars_to_discretize = NULL) {

  if(is.null(vars_to_discretize)) {

    # Convert categorical variables into discrete variables.
    dis_data <- sys_discretize(binary_dataset, vars_to_discretize)

    # Calculate transfer probability matrix.
    tran_matrix <- sys_tran(dis_data)

    # Initial state probability vector.
    M_0 <- rep(1/ncol(dis_data), ncol(dis_data))

    # Iterate "n_iter" times to obtain the transfer probability vector.
    M_n <- M_0 %*% (tran_matrix %^% n_iter)
    M_n <- data.frame(100*M_n)
    colnames(M_n) <- colnames(dis_data)

    # Three decimal places are reserved.
    M_n[,1:ncol(M_n)] %>%
      round(3)

    # Export the evaluation criteria.
    eval <- M_n %>%
      as.data.frame()
    rownames(eval) <- "Score"

    return(eval)

} else {

    # Convert categorical variables into discrete variables.
    dis_data <- sys_discretize(binary_dataset, vars_to_discretize)

    # Calculate transfer probability matrix.
    tran_matrix <- sys_tran(dis_data)

    # Loop "n_iter" times to count the results of each iteration.
    iter_prob <- sys_iter(binary_dataset, n_iter, vars_to_discretize)

    # Initial state probability vector.
    M_0 <- rep(1/ncol(dis_data), ncol(dis_data))

    # Iterate "n_iter" times to obtain the transfer probability vector.
    M_n <- M_0 %*% (tran_matrix %^% n_iter)
    M_n <- data.frame(M_n)
    colnames(M_n) <- colnames(dis_data)

    # Group and feature
    group_fea <-
      iter_prob %>%
      select(group, feature) %>%
      unique()

    # Results
    result_mc_score = tibble(group_fea = group_fea, score = M_n %>%
                               as.numeric()) %>%
      unnest(cols = c(group_fea))

    # Add up the scores of each category.
    # Split into multiple tibbles by group.
    grouped <- split(result_mc_score, result_mc_score$group)
    needed_groups <- subset(grouped, names(grouped) %in% vars_to_discretize)

    # Sort the scores within eight groups, and the score of each level is the sum of the scores of the previous levels.
    result_add_score <- needed_groups %>%

      map_dfr(function(splitted_df) {

        # Sort each tibble in ascending order by score.
        splitted_df = splitted_df %>%
          arrange(score)

        # Pull the score for totalization.
        sum_score = splitted_df %>%
          pull(score) %>%
          accumulate(.f = sum)

        # Add the 'sum_score' column, which is the result of the cumulative score.
        splitted_df %>%
          mutate(sum_score = sum_score)
      })

    # 'Experimental reagents' and 'Characterization' remains the original scores.
    result_add_score %<>%
      rbind(
        subset(result_mc_score, group %in% setdiff(unique(result_mc_score$group), vars_to_discretize)) %>%
          mutate(sum_score = score)
      )

    # Three decimal places are reserved.
    result_add_score$sum_score %<>%
      round(4)

    # Export the evaluation criteria.
    eval <- result_add_score %>%
      select(feature, sum_score) %>%
      t() %>%
      as.data.frame()

    # Transform to numeric
    colnames(eval) <- eval[1,]
    eval <- eval[-1,]
    rownames(eval) <- "Score"

    return(eval)
  }
}

