# Fix YAML encoding in determination-rates-benchmark.Rmd
file_path <- "vignettes/determination-rates-benchmark.Rmd"

# Read with explicit encoding
content <- readLines(file_path, encoding = "UTF-8", warn = FALSE)

# Show first 20 lines for debugging
cat("First 20 lines:\n")
for (i in 1:min(20, length(content))) {
  cat(sprintf("Line %d: %s\n", i, content[i]))
}

# Check for invisible characters in first 5 lines
cat("\n\nHex dump of first 5 lines:\n")
for (i in 1:min(5, length(content))) {
  bytes <- charToRaw(content[i])
  cat(sprintf("Line %d: %s\n", i, paste(as.character(bytes), collapse = " ")))
}
