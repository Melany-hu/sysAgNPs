#' @title Calculate the Transition Scores
#'
#' @param data A dataframe that contains experimental data.
#' @param dataset A binary dataframe. Datasets used to establish evaluation criteria.
#' @param n_iter The number of iterations to reach the steady state.
#' @param vars_to_discretize Variables or columns to be discretized. Default is NULL.
#' @importFrom rio import export
#' @importFrom dplyr arrange
#' @return A dataframe that contains sysAgNPs scores.
#' @export
#'
#' @examples score <- sys_score(users_data, criteria)


sys_TS <- function(data, dataset, n_iter, vars_to_discretize){

  # Convert categorical variables into discrete variables.
  dis_data <- sys_discretize(dataset, vars_to_discretize)

  # Calculate transfer probability matrix.
  tran_matrix <- sys_tran(dis_data)

  # Loop "n_iter" times to count the results of each iteration.
  iter_prob <- sys_iter(dataset, n_iter, vars_to_discretize)

  # Import evaluation criteria of AgNPs.
  criteria <- sys_eval_cri(dataset, n_iter, vars_to_discretize)

  # Data processing
  # Loop through each cell of the data frame.
  for (i in 1:nrow(data)) {
    for (j in 1:ncol(data)) {

      value <- data[i, j]

      if (is.na(value)) {
        # If the element is NA, replace it with 0.
        data[i, j] <- 0

      } else {
        # Check if the value in data matches a column name in criteria.
        if (value %in% colnames(criteria)) {
          data[i, j] <- criteria[value]

        # The features that are not discretized:
        } else {
          data[i, j] <- colnames(data)[j]
        }
        if (data[i, j] %in% colnames(criteria)) {
          data[i, j] <- criteria[data[i, j]]
        }
      }
    }

    # Sum of score value
    row_data <- as.numeric(as.character(data[i, ]))
    data$`TS`[i] <- sum(row_data, na.rm = TRUE)

  }

  data$`TS` <- as.numeric(data$`TS`)
  data$`TS` <- round(data$`TS`, 3)
  # Return dataframe that contains score value of AgNPs
  return(data)
}

