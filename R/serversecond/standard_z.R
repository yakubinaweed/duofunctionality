# R/serversecond/standard_z.R (Complete Script)

# Function to perform Z-transformation (Standardization)
z_transform <- function(x) {
  (x - mean(x, na.rm = TRUE)) / sd(x, na.rm = TRUE)
}