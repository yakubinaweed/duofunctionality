# R/serversecond/plotting.R (Complete Script)

library(ggplot2) # Ensure ggplot2 is loaded

# Function to plot HGB vs. Age with GMM clusters
# data_df: A data frame containing HGB, Age, Gender, and cluster columns.
# male_hgb_transformed: Logical, TRUE if male HGB was transformed for GMM.
# female_hgb_transformed: Logical, TRUE if female HGB was transformed for GMM.
plot_age_hgb <- function(data_df, male_hgb_transformed = FALSE, female_hgb_transformed = FALSE) {
  if (is.null(data_df) || nrow(data_df) == 0) {
    return(ggplot() + annotate("text", x = 0.5, y = 0.5, label = "No data to plot.", size = 6, color = "grey50"))
  }

  # Ensure cluster is a factor for discrete colors
  data_df$cluster <- as.factor(data_df$cluster)

  # Construct a dynamic plot title based on transformation status
  plot_title <- "HGB vs. Age with Detected Subpopulations"
  transformation_notes <- c()
  if (male_hgb_transformed) {
    transformation_notes <- c(transformation_notes, "Male HGB was log-transformed for GMM")
  }
  if (female_hgb_transformed) {
    transformation_notes <- c(transformation_notes, "Female HGB was log-transformed for GMM")
  }

  if (length(transformation_notes) > 0) {
    plot_title <- paste0(plot_title, "\n(Note: ", paste(transformation_notes, collapse = "; "), ")")
  }


  p <- ggplot(data_df, aes(x = Age, y = HGB, color = cluster, shape = Gender)) +
    geom_point(alpha = 0.7) +
    labs(
      title = plot_title,
      x = "Age",
      y = "HGB (g/dL)"
    ) +
    theme_minimal() +
    scale_color_brewer(palette = "Set1") + # Use a color palette for clusters
    theme(legend.position = "bottom",
          plot.title = element_text(hjust = 0.5)) # Center plot title

  p
}


# Function to plot refineR output (from Main Analysis tab)
plot_refiner_output <- function(data_df, lower_limit, upper_limit, user_lower = NULL, user_upper = NULL, unit = "") {
  if (is.null(data_df) || nrow(data_df) == 0) {
    return(ggplot() + annotate("text", x = 0.5, y = 0.5, label = "No data available for plot.", size = 6, color = "grey50"))
  }

  p <- ggplot(data_df, aes(x = Value)) +
    geom_density(fill = "lightblue", alpha = 0.5) +
    geom_vline(xintercept = lower_limit, linetype = "dashed", color = "blue", size = 1,
               aes(text = paste("RefineR Lower Limit:", round(lower_limit, 2), unit))) +
    geom_vline(xintercept = upper_limit, linetype = "dashed", color = "blue", size = 1,
               aes(text = paste("RefineR Upper Limit:", round(upper_limit, 2), unit))) +
    labs(title = "RefineR Estimated Reference Interval",
         x = paste0("Value (", unit, ")"),
         y = "Density") +
    theme_minimal()

  if (!is.null(user_lower) && !is.na(user_lower)) {
    p <- p + geom_vline(xintercept = user_lower, linetype = "dotted", color = "red", size = 1,
                        aes(text = paste("User Lower Limit:", user_lower, unit)))
  }
  if (!is.null(user_upper) && !is.na(user_upper)) {
    p <- p + geom_vline(xintercept = user_upper, linetype = "dotted", color = "red", size = 1,
                        aes(text = paste("User Upper Limit:", user_upper, unit)))
  }
  p
}

# Helper function to generate safe filenames
generate_safe_filename <- function(prefix, extension = "png") {
  timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
  # Sanitize prefix to be filesystem friendly
  safe_prefix <- gsub("[^a-zA-Z0-9_.-]", "_", prefix)
  paste0(safe_prefix, "_", timestamp, ".", extension)
}