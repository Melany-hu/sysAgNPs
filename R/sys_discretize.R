#' Convert categorical variables into discrete variables
#'
#' @param dataset A dataframe of dataset. Datasets used to establish evaluation criteria.
#' @param vars_to_discretize Variables or columns to be discretized. Default is NULL.
#' @importFrom magrittr %<>%
#'
#' @return A binary dataframe.
#' @export
#'
#' @examples dis_data <- sys_discretize(dataset, c("Shape", "pH"))

sys_discretize <- function(dataset, vars_to_discretize = NULL) {

  if(is.null(vars_to_discretize)) {

    # When no columns are specified, no discretization is performed.
    dataset[, 1:ncol(dataset)] <- ifelse(is.na(dataset[, 1:ncol(dataset)]), 0, 1)
    dataset %<>%
      sapply(as.numeric) %>%
      as.data.frame()

    return(dataset)
  }

  # Get original column name
  orig_cols <- colnames(dataset)

  # Get the columns to be discretized, set 'drop = FALSE' so that 'disc_vars' is a two-dimensional array, which also successfully uses '[, i]' to index specific columns when extracting a column of dataset
  disc_vars <- dataset[, vars_to_discretize, drop = FALSE]

  # Iterate through each column for discretization
  for (i in 1:length(disc_vars)){

    # Get the current column's categorical variable
    levels <- unique(disc_vars[, i])

    # Generate the corresponding dichotomous variables, named by "vars_to_discretize:levels", then add them to the dataframe
    for (level in levels) {
      dataset[, paste0(level)] <-
        ifelse(is.na(disc_vars[, i]), 0,
               ifelse(disc_vars[, i] == level, 1, 0))
    }

  # Because the discretization generates a column with the column name "NA", delete the column and delete the original column
  dataset <- dataset[, !(colnames(dataset) %in% c(paste0("NA"), vars_to_discretize[i]))]
  }

  # Get the remaining columns except vars_to_discretize
  other_vars <- setdiff(orig_cols, vars_to_discretize)

  # Binarize the other undiscretized columns
  for (var in other_vars) {
    dataset[, var] <- ifelse(is.na(dataset[, var]), 0, 1)
  }

  dataset %<>%
    sapply(as.numeric) %>%
    as.data.frame()

  return(dataset)
}

