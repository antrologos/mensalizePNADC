# Build pkgdown site
library(pkgdown)

cat("Building pkgdown site...\n\n")

# Build the site
build_site(pkg = ".", preview = FALSE)

cat("\n\nPkgdown site built successfully!\n")
