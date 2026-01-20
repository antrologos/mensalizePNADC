# Verify vignette structure
# This script checks that the how-it-works vignette parses correctly

library(knitr)

cat("Checking vignette structure...\n\n")

# Read the vignette
vignette_text <- readLines("vignettes/how-it-works.Rmd")
cat("Total lines:", length(vignette_text), "\n")

# Check for section headers
headers <- grep("^##? ", vignette_text, value = TRUE)
cat("\nSection headers found:\n")
cat(paste(headers, collapse = "\n"), "\n")

# Check for code chunks
chunks <- grep("```\\{r", vignette_text, value = TRUE)
cat("\nCode chunks found:", length(chunks), "\n")

# Check for figure references
figures <- grep("\\!\\[.*\\]\\(figures/", vignette_text, value = TRUE)
cat("\nFigure references found:", length(figures), "\n")
for (fig in figures) {
  cat("  -", trimws(fig), "\n")
}

# Try to parse YAML header
yaml_start <- grep("^---$", vignette_text)[1]
yaml_end <- grep("^---$", vignette_text)[2]
if (!is.na(yaml_start) && !is.na(yaml_end)) {
  cat("\nYAML header found (lines", yaml_start, "to", yaml_end, ")\n")
} else {
  cat("\nWARNING: YAML header not found!\n")
}

# Check for the new experimental section
exp_section <- grep("## Experimental Strategies", vignette_text)
if (length(exp_section) > 0) {
  cat("\nExperimental Strategies section found at line", exp_section[1], "\n")
} else {
  cat("\nWARNING: Experimental Strategies section not found!\n")
}

# Check if figure file exists
fig_path <- "vignettes/figures/how-it-works/fig-experimental-strategies.png"
if (file.exists(fig_path)) {
  cat("\nFigure file exists:", fig_path, "\n")
  cat("  Size:", file.info(fig_path)$size, "bytes\n")
} else {
  cat("\nWARNING: Figure file not found:", fig_path, "\n")
}

cat("\n\nVignette structure check complete!\n")
