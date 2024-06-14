#' Line and Radar Plot of sysAgNPs score
#'
#' @param sysAgNPs_score A dataframe containing four columns of numeric data.
#' @param num_plots The range of the graph to be output and saved can be a vector or a single value.
#' @param output_directory Directory where the plots will be saved.
#' @return A `ggplot` object.
#' @importFrom ggplot2 ggplot aes geom_line geom_point geom_text position_nudge scale_color_manual labs scale_y_continuous theme_bw theme element_text ggsave
#' @importFrom dplyr mutate select
#' @importFrom tibble as_tibble
#' @importFrom ggradar ggradar
#' @importFrom ggpubr ggarrange
#' @importFrom rlang sym
#' @importFrom patchwork wrap_plots
#' @export
#' @examples sysAgNPs_line_radar <- sys_line_radar(sysAgNPs_score, 10, "sysAgNPs_line_radar/")
sys_line_radar <- function(sysAgNPs_score, num_plots, output_directory) {

  # Transpose the data frame for drawing line plots
  sysAgNPs_score_line <- as.data.frame(t(sysAgNPs_score))
  # Add the 'sysAgNPs score' column
  sysAgNPs_score_line$`sysAgNPs score` <- rownames(sysAgNPs_score_line)
  # Define the factor order
  level = c("DE", "PE", "CE", "TS")
  sysAgNPs_score_line$`sysAgNPs score` <- factor(sysAgNPs_score_line$`sysAgNPs score`, levels = level)

  # Create an empty list to store multiple line plots
  lollipop_plots <- list()
  # Calculate the number of samples and exclude the last column (character type)
  num_columns <- ncol(sysAgNPs_score_line) - 1

  # Batch draw line plots
  for (i in 1:num_columns) {
    # Get the column name for the current iteration
    current_column <- colnames(sysAgNPs_score_line)[i]
    # Set the title of the line plot
    title_text <- sprintf("sysAgNP-%d", i)

    lollipop_plot <- ggplot(sysAgNPs_score_line,
                            aes(x = `sysAgNPs score`,
                                y = !!sym(current_column), # Use sym function to get the column name as a variable
                                group = 1)) +
      # Add lines
      geom_line(color = "#868e96", # Line color
                size = 1) + # Line thickness
      # Add points
      geom_point(aes(color = `sysAgNPs score`), # Point color
                 size = 4) + # Point size
      # Add data labels
      geom_text(aes(label = !!sym(current_column)),
                position = position_nudge(y = 0.2), # Move data labels up by 0.2 units to avoid overlap with the lines
                size = 3) + # Size of data labels
      # Customize the colors of the line plot
      scale_color_manual(values = c("#51cf66", "#ff922b", "#cc5de8", "#ff6b6b")) +
      labs(title = title_text, # Set the title of the line plot to the previously defined title_text
           x = "sysAgNPs score", # Set the title of the x-axis
           y = "") + # Do not display the title of the y-axis
      # Set the y-axis tick values
      scale_y_continuous(breaks = seq(0, 2, 0.2), # Range from 0 to 2, with a step size of 0.2
                         expand = c(0, 0), # Remove the space between the plot area and the axes
                         limits = c(-0.1, 2.1)) + # Set the display range of the Y-axis to -0.1 to 2.1
      theme_bw() + # Remove the gray background from the plot
      theme(legend.position = "none", # Do not display the legend
            # Set the title to be horizontally centered and with a font size of 12
            plot.title = element_text(hjust = 0.5, size = 12))
    # Store the plot objects in the lollipop_plots list
    lollipop_plots[[current_column]] <- lollipop_plot
  }

  # Calculate the area of radar charts and name the column as 'area'
  sysAgNPs_score <- sysAgNPs_score %>%
    mutate(area = (1/2) * (DE + CE) * (TS + PE))
  # Round the data in the 'area' column to three decimal places
  sysAgNPs_score$area <- round(sysAgNPs_score$area, 3)

  breaks <- seq(0.0, 3.0, 0.75)
  # Generate labels using sprintf() function
  labels <- sprintf("%.1f-%.1f", head(breaks, -1), tail(breaks, -1))
  # Divide the results into sections
  sysAgNPs_score$results <- cut(sysAgNPs_score$area, #
                                breaks = breaks,  # Specify breakpoints
                                labels = labels,  # Assign labels
                                right = TRUE) # Specify that the interval includes the right endpoint
  # Add grouping and name it 'group'
  sysAgNPs_score_radar <- sysAgNPs_score %>%
    as_tibble(rownames = "group") %>%
    select(1:5) # Select the first five columns for radar chart drawing
  # Define four colors for radar charts
  results_colours <- c("#ffd43b", "#94d82d", "#339af0", "#22b8cf")
  # Create an empty list to store multiple radar charts
  radar_plots <- list()

  # Draw radar charts in batches
  for (i in 1:nrow(sysAgNPs_score_radar)) {
    # Extract data for the current row from sysAgNPs_score_radar
    current_data <- sysAgNPs_score_radar[i, ]
    # Get the 'area' value corresponding to the current row from sysAgNPs_score_line
    current_area <- sysAgNPs_score[i, "area"]
    # Get the 'results' value corresponding to the current row from sysAgNPs_score
    current_result <- sysAgNPs_score[i, "results"]
    # Build a title including the row number
    title_text <- sprintf("sysAgNP-%d area: %.3f", i, current_area)
    # Choose color based on the category of 'results'
    point_colour <- results_colours[as.numeric(current_result)]

    # Create a radar chart using ggradar function
    radar <- ggradar(current_data,
                     values.radar = c("0", "1", "2"), # Set the minimum, average, and maximum values displayed in the radar chart
                     grid.min = 0, # Value for drawing the minimum grid line
                     grid.mid = 1, # Value for drawing the average grid line
                     grid.max = 2, # Value for drawing the maximum grid line
                     background.circle.colour = "white", # Background color
                     group.colours = point_colour, # Line color
                     fill = TRUE, # Enable fill color
                     fill.alpha = 0.4, # Transparency of fill color
                     gridline.mid.colour = "grey", # Color of middle grid lines
                     axis.line.colour = "grey") + # Crossline color
      labs(title = title_text) + # Set the title of the radar chart
      # Set the style of the title
      theme(plot.title = element_text(hjust = 0.5, # Center the title
                                      size = 12)) # Title font size
    # Add the drawn radar chart to the radar_plots list
    radar_plots[[i]] <- radar
  }

  # Combine line plots and radar charts
  # Initialize a new list to store the combined graphical objects
  combined_plots <- list()
  # Calculate the length of the list, which is the number of graphical objects
  n <- length(lollipop_plots)

  # Interleave line plots and radar charts in the combined_plots list
  for (i in 1:n) {
    combined_plots[[1 + 2 * (i - 1)]] <- lollipop_plots[[i]]
    combined_plots[[2 + 2 * (i - 1)]] <- radar_plots[[i]]
  }

  # Group plots
  # Initialize a new list to store grouped graphical objects
  extracted_plots <- list()

  # Divide the plots in the combined_plots list into groups of 2, with a total of 600 groups
  for (i in 1:nrow(sysAgNPs_score)) {
    start_index <- (i - 1) * 2 + 1 # Starting index
    end_index <- start_index + 1 # Ending index
    extracted_plots[[i]] <- combined_plots[start_index:end_index]
  }

  # Stitch plots, combining line plots and radar charts for the same sample into one
  wrapped_plots <- lapply(extracted_plots, function(x) {
    # Set the layout of the stitched plot to 2 columns and 1 row
    wrap_plots(x, ncol = 2, nrow = 1)
  })

  # Checks whether the specified output directory exists
  if (!dir.exists(output_directory)) {
    # If the output directory does not exist, create it
    dir.create(output_directory, recursive = TRUE)
  }

  # Check if the num_plots parameter is a vector with multiple elements
  if (length(num_plots) > 1) {
    # If num_plots are a vector, then selected_indices is directly equal to num_plots
    selected_indices <- num_plots
  } else {
    # If num_plots are a separate number, a single element vector is constructed
    selected_indices <- num_plots:num_plots
  }

  # Corresponding wrapped_plots were extracted according to selected_indices
  selected_plots <- wrapped_plots[selected_indices]

  # Save images
  for (i in seq_along(selected_plots)) {
    # Image name to be saved
    file_name <- paste0("sysAgNP", selected_indices[i], ".png")
    # Save path
    file_path <- file.path(output_directory, file_name)
    ggsave(file = file_path,
           plot = selected_plots[[i]],
           width = 10, # Image width for saving
           height = 6, # Image height for saving
           dpi = 600) # Image resolution for saving
  }
  # Output the images
  return(selected_plots)
}

