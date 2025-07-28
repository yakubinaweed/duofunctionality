library(ggplot2)

plot_ri_bars <- function(df_summary) {
  ggplot(df_summary, aes(x = subgroup, ymin = lower, ymax = upper)) +
    geom_errorbar(width = 0.2) +
    geom_point(aes(y = mean), size = 3) +
    theme_minimal() +
    labs(title = "Estimated Reference Intervals by Subgroup",
         y = "HGB (g/dL)", x = "Subgroup")
}

plot_age_hgb <- function(df) {
  ggplot(df, aes(x = age, y = hgb, color = factor(cluster))) +
    geom_point(alpha = 0.7) +
    theme_minimal() +
    labs(title = "HGB vs Age by Cluster")
}
