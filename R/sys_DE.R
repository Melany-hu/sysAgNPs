#' @title Calculate the Distribution Entropy
#' @description Measure the distribution variability of the presence and absence of feature categories.
#' @param data A dataframe that contains experimental data.
#' @importFrom dplyr %>% bind_rows mutate mutate_all
#' @importFrom tibble tibble
#' @importFrom purrr map
#' @importFrom tibble tibble
#' @return A dataframe including 1. the number of feature in a certain category; 2. the total number of features in the sample; 3.  the average value to measure the average uncertainty of feature categories
#' @export
#'
#' @examples DE = sys_DE(users_data)
#'

sys_DE <- function(data){

  # Convert data to binary dataframe
  binary_df <- data %>%
    mutate_all(~ ifelse(is.na(.), 0, 1))

  # For each row:
  fea_category <- map(seq_len(nrow(binary_df)), function(i){

    # Get the current row
    row <- binary_df[i, ]

    # Classify and count for the row, return as tibble
    tibble(
      B1 = sum(row[,1]),
      B2 = sum(row[,c(2:7)]),
      B3 = sum(row[,c(8:14)]),
      B4 = sum(row[,15])
    )
  })

  # The number of feature in a certain category:
  fea_category <- bind_rows(fea_category)

  # Calculate the ratio of the number of features in a certain category to the total number of features in a AgNPs sample.
  # Calculate the Shannon entropy
  ave_fea_category_entropy <- fea_category %>%
    mutate(m = rowSums(.)) %>% # Get sums m, m is the total number of features in the sample
    mutate(H_pB2 = ifelse(B2 %in% c(0,6), 0, -(B2/6) * log2(B2/6) - (1 - B2/6) * log2(1 - B2/6)),
           H_pB3 = ifelse(B3 %in% c(0,7), 0, -(B3/7) * log2(B3/7) - (1 - B3/7) * log2(1 - B3/7)),
           H_pB = (H_pB2 + H_pB3)/2) %>% # Calculate Shannon entropy
    round(3) # Round to 3 decimal places

  return(ave_fea_category_entropy)
}
