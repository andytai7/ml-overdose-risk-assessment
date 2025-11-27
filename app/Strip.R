# strip_models.R
# Run this locally where your RF_OD and RF_FOD models exist
# This extracts just what's needed for prediction, dramatically reducing size

library(caret)

# Load your original models
rf_od_full  <- readRDS("RF_OD.rds")
rf_fod_full <- readRDS("RF_FOD.rds")

# Check original sizes
cat("Original RF_OD size:", format(object.size(rf_od_full), units = "MB"), "\n")
cat("Original RF_FOD size:", format(object.size(rf_fod_full), units = "MB"), "\n")

# Function to strip a caret model down to essentials
strip_caret_model <- function(model) {
  # Keep only what's needed for prediction
  # modelInfo and method are required for caret to translate type="prob" correctly
  stripped <- list(
    finalModel = model$finalModel,
    preProcess = model$preProcess,
    xNames = model$xNames,
    levels = model$levels,
    terms = model$terms,
    modelInfo = model$modelInfo,
    modelType = model$modelType,
    method = model$method
  )
  
  # Copy over the class so predict() still works
  class(stripped) <- class(model)
  
  return(stripped)
}

# Strip the models
rf_od_slim  <- strip_caret_model(rf_od_full)
rf_fod_slim <- strip_caret_model(rf_fod_full)

# Check new sizes
cat("Stripped RF_OD size:", format(object.size(rf_od_slim), units = "MB"), "\n")
cat("Stripped RF_FOD size:", format(object.size(rf_fod_slim), units = "MB"), "\n")

# Verify predictions still work
# Create a test row using the required feature names
required_features <- rf_od_full$coefnames
if (is.null(required_features)) {
  required_features <- attr(rf_od_full$terms, "term.labels")
}

# Build a test data frame with all zeros
test_row <- as.data.frame(
  setNames(as.list(rep(0, length(required_features))), required_features),
  check.names = FALSE
)

cat("\nVerifying predictions work...\n")
pred_full <- predict(rf_od_full, newdata = test_row, type = "prob")
pred_slim <- predict(rf_od_slim, newdata = test_row, type = "prob")
cat("Full model prediction:", pred_full[1, "Yes"], "\n")
cat("Slim model prediction:", pred_slim[1, "Yes"], "\n")
cat("Predictions match:", all.equal(pred_full, pred_slim), "\n")

# Save the slim versions
saveRDS(rf_od_slim, "RF_OD_slim.rds")
saveRDS(rf_fod_slim, "RF_FOD_slim.rds")

cat("\nSlim models saved! Use these for deployment.\n")