#!/usr/bin/env Rscript

# Start the API
library(plumber)

cat("Starting API on http://localhost:8000\n")
cat("Press Ctrl+C to stop\n\n")

pr <- plumb("simple_api.R")
pr$run(host = "0.0.0.0", port = 8000)
