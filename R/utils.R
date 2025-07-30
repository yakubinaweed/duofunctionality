# R/utils.R

# Function to generate a safe filename for plots
generate_safe_filename <- function(prefix, extension = "png") {
  # Replace non-alphanumeric characters with underscores
  safe_title <- gsub("[^a-zA-Z0-9_.-]", "_", prefix)
  # Add date and time for uniqueness
  timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S") # Added seconds for more uniqueness
  paste0(safe_title, "_", timestamp, ".", extension) # Adjusted to just return filename
}

# The server.R is already using file.path(output_dir_rv(), filename)
# So this version is compatible.