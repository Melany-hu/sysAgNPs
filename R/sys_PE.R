#' @title Calculate the Proclivity Entropy
#' @description Measure the preference of feature categories.
#'
#' @param data A dataframe that contains experimental data.
#' @importFrom dplyr %>% bind_rows mutate mutate_all
#' @importFrom tibble tibble
#' @importFrom purrr map
#' @importFrom tibble tibble
#' @return A dataframe including 1. the number of feature in a certain category; 2. the total number of features in the sample; 3. the expected value to measure the average description level across different feature categories.
#' @export
#'
#' @examples PE = sys_PE(users_data)


sys_PE <- function(data){

  # Convert data to binary dataframe
  binary_df <- data %>%
    mutate_all(~ ifelse(is.na(.), 0, 1))

  # For each row:
  fea_category <- map(seq_len(nrow(binary_df)), function(i){

    # Get the current row
    row <- binary_df[i, ]

    # Classify and count for the row, return as tibble
    tibble(
      K1 = sum(row[,1]),
      K2 = sum(row[,c(2:7)]),
      K3 = sum(row[,c(8:14)]),
      K4 = sum(row[,15])
    )
  })

  # The number of feature in a certain category:
  fea_category <- bind_rows(fea_category)

  # Calculate the ratio of the number of features in a certain category to the total number of features in a AgNPs sample.
  # Calculate the shannon entropy
  entropy_fea_category <- fea_category %>%
    as.data.frame() %>%
    mutate(m = rowSums(.)) %>% # Get sums m, m is the total number of features in the sample
    mutate(H_pK1 = ifelse(K1 == 0, 0, -(K1/m) * log2(K1/m)),
           H_pK2 = ifelse(K2 == 0, 0, -(K2/m) * log2(K2/m)),
           H_pK3 = ifelse(K3 == 0, 0, -(K3/m) * log2(K3/m)),
           H_pK4 = ifelse(K4 == 0, 0, -(K4/m) * log2(K4/m)),
           H_pK = H_pK1 + H_pK2 + H_pK3 + H_pK4) %>% # Calculate Shannon entropy
    round(3) # Round to 3 decimal places

  return(entropy_fea_category)
}

