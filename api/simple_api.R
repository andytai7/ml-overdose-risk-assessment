
# simple_api.R
# Simple Overdose Prediction API (numeric-aligned + templates)
library(plumber)
library(caret)
library(ranger)
library(jsonlite)

# -----------------------------
# Load models (adjust paths)
# -----------------------------

rf_od  <- readRDS(file.path("..","models","RF_OD.rds"))
rf_fod <- readRDS(file.path("..","models","RF_FOD.rds"))


# -----------------------------
# CORS filter
# -----------------------------
#* @filter cors
function(req, res) {
  res$setHeader("Access-Control-Allow-Origin", "*")
  res$setHeader("Access-Control-Allow-Methods", "GET,POST,OPTIONS")
  res$setHeader("Access-Control-Allow-Headers", "Content-Type")
  if (req$REQUEST_METHOD == "OPTIONS") {
    res$status <- 200
    return(list())
  } else {
    plumber::forward()
  }
}

# -----------------------------
# Health check
# -----------------------------
#* Health check
#* @get /health
function() {
  list(status = "healthy", timestamp = as.character(Sys.time()))
}

# -----------------------------
# Feature listing
# -----------------------------
#* Get required features for each model
#* @get /features
function() {
  list(
    rf_od  = list(
      model_name   = "Overdose Prediction",
      num_features = length(attr(rf_od$terms, "term.labels")),
      features     = attr(rf_od$terms, "term.labels")
    ),
    rf_fod = list(
      model_name   = "Fatal Overdose Prediction",
      num_features = length(attr(rf_fod$terms, "term.labels")),
      features     = attr(rf_fod$terms, "term.labels")
    )
  )
}

# -----------------------------
# Helpers: robust 0/1 coercion
# -----------------------------
coerce01 <- function(x) {
  if (is.null(x)) return(NA_real_)
  if (is.numeric(x)) return(as.numeric(x))
  if (is.logical(x)) return(as.numeric(x))
  s <- tolower(as.character(x))
  if (s %in% c("1","yes","y","true","t")) return(1)
  if (s %in% c("0","no","n","false","f")) return(0)
  suppressWarnings(as.numeric(s))  # last resort for "0"/"1" strings
}

build_numeric_df <- function(input_list, required_features) {
  missing <- setdiff(required_features, names(input_list))
  if (length(missing) > 0) {
    return(list(
      error = "Missing required features",
      required_features = required_features,
      provided_features = names(input_list),
      missing_features  = missing
    ))
  }
  vals <- lapply(required_features, function(nm) {
    if (nm == "total_od_n") {
      as.numeric(input_list[[nm]])
    } else {
      coerce01(input_list[[nm]])
    }
  })
  nd <- as.data.frame(setNames(vals, required_features), check.names = FALSE)
  
  # Ensure all non-total_od_n columns are numeric (0/1)
  for (nm in names(nd)) {
    if (nm != "total_od_n") nd[[nm]] <- suppressWarnings(as.numeric(nd[[nm]]))
  }
  nd
}

# -----------------------------
# Templates (examples clients can copy)
# -----------------------------

#* Template payload for OD model (string-based example, the API will coerce to 0/1)
#* @get /template/overdose
function() {
  req_features <- attr(rf_od$terms, "term.labels")
  tpl <- as.list(setNames(rep("0", length(req_features)), req_features))
  # Put a few "Yes" examples to illustrate accepted strings
  yes_keys <- c("ost","diabetes_date_b","Alcoholuse","Opioiduse","Tobaccouse","polysubstance_dummy")
  yes_keys <- intersect(yes_keys, req_features)
  for (k in yes_keys) tpl[[k]] <- "1"  # clients may also send "yes"/"true"
  
  list(
    description = "Template for RF_OD endpoint. Values may be '0'/'1', 'yes'/'no', 'true'/'false'. The API coerces them to numeric 0/1.",
    endpoint    = "/predict/overdose",
    contentType = "application/json",
    payload     = tpl,
    note        = "OD model expects 'substancerelated.disorders' (with a dot) as the substance-related feature."
  )
}

#* Template payload for FOD model (string-based example + total_od_n)
#* @get /template/fatal_overdose
function() {
  req_features <- attr(rf_fod$terms, "term.labels")
  tpl <- as.list(setNames(rep("0", length(req_features)), req_features))
  # Include a realistic number for overdose history
  if ("total_od_n" %in% names(tpl)) tpl[["total_od_n"]] <- "2"  # string allowed; coerced to numeric
  # Show several "1" examples
  yes_keys <- c("Opioiduse","Alcoholuse","Cocaineuse","Stimulantuse","Sedativeandhypnoticuse")
  yes_keys <- intersect(yes_keys, names(tpl))
  for (k in yes_keys) tpl[[k]] <- "1"
  
  list(
    description = "Template for RF_FOD endpoint. Values may be '0'/'1', 'yes'/'no', 'true'/'false'. 'total_od_n' may be a number or numeric string. The API coerces appropriately.",
    endpoint    = "/predict/fatal_overdose",
    contentType = "application/json",
    payload     = tpl,
    note        = "FOD model expects 'substancerelateddisorders.x' (no dot, with .x) and includes 'total_od_n'."
  )
}

#* JSON Schema-like description (field names and expected value types)
#* @get /schema
function() {
  list(
    rf_od = list(
      model   = "RF_OD",
      type    = "classification",
      fields  = lapply(attr(rf_od$terms, "term.labels"), function(nm) {
        list(
          name = nm,
          expected = "string or number",
          accepted_values = c("0","1","yes","no","true","false",0,1),
          coerced_to = "numeric (0/1)"
        )
      })
    ),
    rf_fod = list(
      model   = "RF_FOD",
      type    = "classification",
      fields  = lapply(attr(rf_fod$terms, "term.labels"), function(nm) {
        list(
          name = nm,
          expected = if (nm == "total_od_n") "string or number",
          accepted_values = if (nm == "total_od_n") "any non-negative integer"
          else c("0","1","yes","no","true","false",0,1),
          coerced_to = if (nm == "total_od_n") "numeric (integer)" else "numeric (0/1)"
        )
      })
    )
  )
}

# -----------------------------
# Predict: OD
# -----------------------------
#* Predict overdose probability
#* @post /predict/overdose
#* @param req The request object
function(req) {
  required_features <- attr(rf_od$terms, "term.labels")
  input_data <- tryCatch(jsonlite::fromJSON(req$postBody), error = function(e) NULL)
  if (is.null(input_data)) {
    return(list(error = "Invalid JSON payload"))
  }
  
  nd <- build_numeric_df(input_data, required_features)
  if (is.list(nd) && !is.null(nd$error)) return(nd)
  
  pred_prob  <- predict(rf_od, nd, type = "prob")
  pred_class <- predict(rf_od, nd, type = "raw")
  
  class_levels <- colnames(pred_prob)
  prob_no  <- if ("No"  %in% class_levels) pred_prob[1, "No"]  else pred_prob[1, class_levels[1]]
  prob_yes <- if ("Yes" %in% class_levels) pred_prob[1, "Yes"] else pred_prob[1, class_levels[min(2, length(class_levels))]]
  
  list(
    model          = "RF_OD",
    prediction     = as.character(pred_class[1]),
    probability_no = round(as.numeric(prob_no), 4),
    probability_yes= round(as.numeric(prob_yes), 4),
    confidence     = round(max(pred_prob[1, ]), 4)
  )
}

# -----------------------------
# Predict: FOD
# -----------------------------
#* Predict fatal overdose probability
#* @post /predict/fatal_overdose
#* @param req The request object
function(req) {
  required_features <- attr(rf_fod$terms, "term.labels")
  input_data <- tryCatch(jsonlite::fromJSON(req$postBody), error = function(e) NULL)
  if (is.null(input_data)) {
    return(list(error = "Invalid JSON payload"))
  }
  
  nd <- build_numeric_df(input_data, required_features)
  if (is.list(nd) && !is.null(nd$error)) return(nd)
  
  pred_prob  <- predict(rf_fod, nd, type = "prob")
  pred_class <- predict(rf_fod, nd, type = "raw")
  
  class_levels <- colnames(pred_prob)
  prob_no  <- if ("No"  %in% class_levels) pred_prob[1, "No"]  else pred_prob[1, class_levels[1]]
  prob_yes <- if ("Yes" %in% class_levels) pred_prob[1, "Yes"] else pred_prob[1, class_levels[min(2, length(class_levels))]]
  
  list(
    model          = "RF_FOD",
    prediction     = as.character(pred_class[1]),
    probability_no = round(as.numeric(prob_no), 4),
    probability_yes= round(as.numeric(prob_yes), 4),
    confidence     = round(max(pred_prob[1, ]), 4)
  )
}
