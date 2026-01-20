# Generate Experimental Strategies Comparison Figure
# This script creates fig-experimental-strategies.png for the how-it-works vignette

library(ggplot2)

# Data from benchmark results (determination-rates-benchmark.Rmd)
data <- data.frame(
  strategy = factor(c("Strict", "+Probabilistic", "+UPA Aggregation", "+Both",
                      "Strict", "+Probabilistic", "+UPA Aggregation", "+Both"),
                   levels = c("Strict", "+Probabilistic", "+UPA Aggregation", "+Both")),
  period = factor(c(rep("Fortnight", 4), rep("Week", 4)),
                 levels = c("Fortnight", "Week")),
  rate = c(7.64, 13.61, 57.69, 60.33,
           1.54, 1.82, 14.36, 14.60)
)

# Create the plot
p <- ggplot(data, aes(x = strategy, y = rate, fill = period)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8), width = 0.7) +
  geom_text(aes(label = paste0(rate, "%")),
            position = position_dodge(width = 0.8),
            vjust = -0.5, size = 3.2) +
  scale_fill_manual(values = c("Fortnight" = "#3498db", "Week" = "#e67e22"),
                   name = "Period Type") +
  scale_y_continuous(limits = c(0, 70), breaks = seq(0, 70, 10),
                    labels = function(x) paste0(x, "%")) +
  labs(
    title = "Experimental Strategies Improve Determination Rates",
    subtitle = "Fortnight and week identification rates using different strategies (PNADC 2012-2025)",
    x = "Strategy",
    y = "Determination Rate"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    plot.title = element_text(face = "bold", size = 13),
    plot.subtitle = element_text(color = "gray40", size = 10),
    axis.title = element_text(size = 10),
    axis.text.x = element_text(angle = 0, hjust = 0.5, size = 9),
    legend.position = "bottom",
    legend.title = element_text(size = 9),
    legend.text = element_text(size = 9),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank()
  )

# Create output directory if it doesn't exist
output_dir <- "vignettes/figures/how-it-works"
if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
}

# Save the figure
ggsave(
  filename = file.path(output_dir, "fig-experimental-strategies.png"),
  plot = p,
  width = 8,
  height = 5,
  dpi = 150,
  bg = "white"
)

cat("Figure saved to:", file.path(output_dir, "fig-experimental-strategies.png"), "\n")
