#' @title Calculate the Combination Entropy
#' @description Calculate the average value of the probability of surprising level of the  presence and absence of a particular category within the specific category to measure the average uncertainty of feature categories.
#'
#' @param data A dataframe that contains experimental data.
#' @param dataset The dataset used to to calculate the ratio of the number of reporting a certain feature in the AgNPs dataset to the total number of samples.
#' @importFrom dplyr %>% rowwise mutate mutate_all c_across
#' @importFrom purrr map
#' @importFrom tibble tibble
#' @return A dataframe including: 1. the ratio of the number of reporting a certain feature in the AgNPs dataset to the total number of samples; 2. pc:the probability of the feature combination occurring; 3. Hi:the probability of surprising level of the presence and absence of feature combinations to measure the uncertainty of feature combination.
#' @export
#'
#' @examples CE <- sys_CE(users_data, dataset)

sys_CE <- function(data, dataset){

  # Convert data to binary dataframe
  binary_df <- dataset %>%
    mutate_all(~ ifelse(is.na(.), 0, 1))

  # Sum the number of reporting a certain feature in the AgNPs dataset
  ti <- binary_df %>%
    colSums() %>%
    t() %>%
    as.data.frame()
  rownames(ti) <- "ti"

  # Calculate the ratio of the number of reporting a certain feature in the AgNPs dataset to the total number of samples.
  pi <- data.frame(ti/nrow(binary_df))
  pi[2,] <- 1 - pi
  rownames(pi)[1:2] <- c("pi", "1-pi")
  colnames(pi) <- colnames(binary_df)


  # pi_combine: In a total of samples in the dataset, how many times each feature was reported in the total number of samples.
  pi_combine <- data %>%
    apply(MARGIN = 1, function(row) ifelse(is.na(row), 1, pi[1, ])) %>%
    unlist() %>%
    matrix(nrow = nrow(data), byrow = TRUE) %>%
    as.data.frame()
  colnames(pi_combine) <- colnames(data)


  # pE_combine: The probability of the feature combination occurring.
  pE_combine <- pi_combine %>%
    rowwise() %>%
    mutate(pE = prod(c_across(everything()), na.rm = TRUE))

  # H_pE_combine: The probability of surprising level of the presence and absence of feature combinations to measure the uncertainty of feature combination.
  H_pE_combine <- pE_combine %>%
    select(pE) %>%
    mutate(H_pE = ifelse(pE == 1, 0, -pE * log2(pE) - (1 - pE) * log2(1 - pE))) %>%
    round(3)

  return(H_pE_combine)
}

