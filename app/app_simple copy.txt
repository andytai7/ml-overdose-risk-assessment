
# Simple Overdose Risk Assessment Shiny App
# No API needed - everything runs in R

library(shiny)
library(caret)
library(ranger)

# -----------------------------
# Load models (ensure paths OK)
# -----------------------------

rf_od  <- readRDS(file.path("..","models","RF_OD.rds"))
rf_fod <- readRDS(file.path("..","models","RF_FOD.rds"))


# -----------------------------
# UI
# -----------------------------
ui <- fluidPage(
  tags$head(
    tags$style(HTML("
      body { background: linear-gradient(135deg, #667eea 0%, #764ba2 100%); padding: 20px; }
      .main-container { background: white; border-radius: 15px; padding: 30px; max-width: 900px; margin: 0 auto; box-shadow: 0 10px 40px rgba(0,0,0,0.2); }
      .header { background: linear-gradient(135deg, #667eea 0%, #764ba2 100%); color: white; padding: 30px; border-radius: 10px; text-align: center; margin-bottom: 30px; }
      .section-title { color: #667eea; font-size: 20px; font-weight: 600; margin-top: 30px; margin-bottom: 15px; border-bottom: 2px solid #667eea; padding-bottom: 10px; }
      .question { background: #f8f9fa; padding: 15px; border-radius: 8px; margin-bottom: 15px; }
      .question label { font-weight: 500; }
      .btn-calculate { background: linear-gradient(135deg, #667eea 0%, #764ba2 100%); color: white; padding: 15px 40px; font-size: 16px; font-weight: 600; border: none; border-radius: 8px; margin-top: 20px; }
      .btn-calculate:hover { transform: translateY(-2px); box-shadow: 0 5px 15px rgba(102,126,234,0.4); }
      .results-high { background: #f8d7da; border-left: 4px solid #dc3545; padding: 20px; border-radius: 8px; margin-top: 20px; }
      .results-low { background: #d4edda; border-left: 4px solid #28a745; padding: 20px; border-radius: 8px; margin-top: 20px; }
      .result-item { margin: 10px 0; font-size: 15px; }
      .result-label { font-weight: 600; display: inline-block; width: 160px; }
    "))
  ),
  div(class = "main-container",
      div(class = "header",
          h1("🏥 Overdose Risk Assessment"),
          p("Machine Learning-Based Clinical Decision Support")
      ),
      
      # Model selection
      radioButtons("model_type", "Select Assessment Type:",
                   choices = c("General Overdose Risk (RF_OD)" = "od",
                               "Fatal Overdose Risk (RF_FOD)"    = "fod"),
                   selected = "od"),
      hr(),
      
      # Medical History
      h3(class = "section-title", "Medical History"),
      div(class = "question",
          radioButtons("ost", "Opioid Substitution Treatment (OST)", choices = c("No" = "0", "Yes" = "1"), inline = TRUE)
      ),
      div(class = "question",
          radioButtons("diabetes_date_b", "Diabetes", choices = c("No" = "0", "Yes" = "1"), inline = TRUE)
      ),
      div(class = "question",
          radioButtons("hypertension_date_b", "Hypertension", choices = c("No" = "0", "Yes" = "1"), inline = TRUE)
      ),
      div(class = "question",
          radioButtons("asthma_date_b", "Asthma", choices = c("No" = "0", "Yes" = "1"), inline = TRUE)
      ),
      div(class = "question",
          radioButtons("copd_date_b", "COPD", choices = c("No" = "0", "Yes" = "1"), inline = TRUE)
      ),
      div(class = "question",
          radioButtons("ckd_date_b", "Chronic Kidney Disease", choices = c("No" = "0", "Yes" = "1"), inline = TRUE)
      ),
      div(class = "question",
          radioButtons("depression_date_b", "Depression", choices = c("No" = "0", "Yes" = "1"), inline = TRUE)
      ),
      div(class = "question",
          radioButtons("epilepsy_date_b", "Epilepsy", choices = c("No" = "0", "Yes" = "1"), inline = TRUE)
      ),
      
      # Cardiovascular
      h3(class = "section-title", "Cardiovascular Conditions"),
      div(class = "question",
          radioButtons("heart_failure_date_b", "Heart Failure", choices = c("No" = "0", "Yes" = "1"), inline = TRUE)
      ),
      div(class = "question",
          radioButtons("ihd_date_b", "Ischemic Heart Disease", choices = c("No" = "0", "Yes" = "1"), inline = TRUE)
      ),
      div(class = "question",
          radioButtons("angina_date_b", "Angina", choices = c("No" = "0", "Yes" = "1"), inline = TRUE)
      ),
      div(class = "question",
          radioButtons("ami_date_b", "Heart Attack (AMI)", choices = c("No" = "0", "Yes" = "1"), inline = TRUE)
      ),
      div(class = "question",
          radioButtons("isch_stroke_date_b", "Ischemic Stroke", choices = c("No" = "0", "Yes" = "1"), inline = TRUE)
      ),
      div(class = "question",
          radioButtons("haemorr_stroke_date_b", "Hemorrhagic Stroke", choices = c("No" = "0", "Yes" = "1"), inline = TRUE)
      ),
      div(class = "question",
          radioButtons("hosp_stroke_date_b", "Hospitalization for Stroke", choices = c("No" = "0", "Yes" = "1"), inline = TRUE)
      ),
      div(class = "question",
          radioButtons("hosp_tia_date_b", "Hospitalization for TIA", choices = c("No" = "0", "Yes" = "1"), inline = TRUE)
      ),
      
      # Neurological
      h3(class = "section-title", "Neurological Conditions"),
      div(class = "question",
          radioButtons("alzheimer_dementia_date_b", "Alzheimer's/Dementia", choices = c("No" = "0", "Yes" = "1"), inline = TRUE)
      ),
      div(class = "question",
          radioButtons("parkinsonism_date_b", "Parkinson's Disease", choices = c("No" = "0", "Yes" = "1"), inline = TRUE)
      ),
      div(class = "question",
          radioButtons("ms_date_b", "Multiple Sclerosis", choices = c("No" = "0", "Yes" = "1"), inline = TRUE)
      ),
      
      # Musculoskeletal
      h3(class = "section-title", "Musculoskeletal Conditions"),
      div(class = "question",
          radioButtons("rheumatoid_arthritis_date_b", "Rheumatoid Arthritis", choices = c("No" = "0", "Yes" = "1"), inline = TRUE)
      ),
      div(class = "question",
          radioButtons("osteo_arthritis_date_b", "Osteoarthritis", choices = c("No" = "0", "Yes" = "1"), inline = TRUE)
      ),
      div(class = "question",
          radioButtons("osteo_porosis_date_b", "Osteoporosis", choices = c("No" = "0", "Yes" = "1"), inline = TRUE)
      ),
      
      # Mental Health
      h3(class = "section-title", "Mental Health Conditions"),
      div(class = "question",
          radioButtons("mood_anx_date_b", "Mood or Anxiety Disorder", choices = c("No" = "0", "Yes" = "1"), inline = TRUE)
      ),
      div(class = "question",
          radioButtons("Mooddisorders", "Mood Disorders (Bipolar, Depression)", choices = c("No" = "0", "Yes" = "1"), inline = TRUE)
      ),
      div(class = "question",
          radioButtons("psychoticdisorders", "Psychotic Disorders", choices = c("No" = "0", "Yes" = "1"), inline = TRUE)
      ),
      div(class = "question",
          radioButtons("Personalitydisorders", "Personality Disorders", choices = c("No" = "0", "Yes" = "1"), inline = TRUE)
      ),
      div(class = "question",
          radioButtons("Neuroticrelateddisorders", "Neurotic/Stress Disorders", choices = c("No" = "0", "Yes" = "1"), inline = TRUE)
      ),
      div(class = "question",
          radioButtons("Neurocognitivedisorders", "Neurocognitive Disorders", choices = c("No" = "0", "Yes" = "1"), inline = TRUE)
      ),
      div(class = "question",
          radioButtons("Intellectualdisability", "Intellectual Disability", choices = c("No" = "0", "Yes" = "1"), inline = TRUE)
      ),
      div(class = "question",
          radioButtons("Developmentdisorders", "Developmental Disorders", choices = c("No" = "0", "Yes" = "1"), inline = TRUE)
      ),
      div(class = "question",
          radioButtons("earlyonsetdisorders", "Early Onset Disorders", choices = c("No" = "0", "Yes" = "1"), inline = TRUE)
      ),
      div(class = "question",
          radioButtons("Multiplementalillness", "Multiple Mental Illness", choices = c("No" = "0", "Yes" = "1"), inline = TRUE)
      ),
      div(class = "question",
          radioButtons("Behaviouralpsychologicaldisturbances", "Behavioral Disturbances", choices = c("No" = "0", "Yes" = "1"), inline = TRUE)
      ),
      
      # Substance Use
      h3(class = "section-title", "Substance Use History"),
      div(class = "question",
          radioButtons("substancerelated.disorders", "Substance-Related Disorders", choices = c("No" = "0", "Yes" = "1"), inline = TRUE)
      ),
      div(class = "question",
          radioButtons("Opioiduse", "Opioid Use Disorder", choices = c("No" = "0", "Yes" = "1"), inline = TRUE)
      ),
      div(class = "question",
          radioButtons("Alcoholuse", "Alcohol Use Disorder", choices = c("No" = "0", "Yes" = "1"), inline = TRUE)
      ),
      div(class = "question",
          radioButtons("Tobaccouse", "Tobacco Use Disorder", choices = c("No" = "0", "Yes" = "1"), inline = TRUE)
      ),
      div(class = "question",
          radioButtons("Cannabinoiduse", "Cannabis Use Disorder", choices = c("No" = "0", "Yes" = "1"), inline = TRUE)
      ),
      div(class = "question",
          radioButtons("Cocaineuse", "Cocaine Use Disorder", choices = c("No" = "0", "Yes" = "1"), inline = TRUE)
      ),
      div(class = "question",
          radioButtons("Stimulantuse", "Stimulant Use Disorder", choices = c("No" = "0", "Yes" = "1"), inline = TRUE)
      ),
      div(class = "question",
          radioButtons("Sedativeandhypnoticuse", "Sedative/Hypnotic Use", choices = c("No" = "0", "Yes" = "1"), inline = TRUE)
      ),
      div(class = "question",
          radioButtons("Hallucinogensuse", "Hallucinogen Use Disorder", choices = c("No" = "0", "Yes" = "1"), inline = TRUE)
      ),
      div(class = "question",
          radioButtons("Polysubstance", "Polysubstance Use", choices = c("No" = "0", "Yes" = "1"), inline = TRUE)
      ),
      div(class = "question",
          radioButtons("Otherpsychoactivedruguse", "Other Psychoactive Drug Use", choices = c("No" = "0", "Yes" = "1"), inline = TRUE)
      ),
      
      # Infections
      h3(class = "section-title", "Infectious Complications"),
      div(class = "question",
          radioButtons("Endocarditis", "Endocarditis", choices = c("No" = "0", "Yes" = "1"), inline = TRUE)
      ),
      div(class = "question",
          radioButtons("Osteomyelitis", "Osteomyelitis", choices = c("No" = "0", "Yes" = "1"), inline = TRUE)
      ),
      div(class = "question",
          radioButtons("Tissueinfection", "Tissue Infection", choices = c("No" = "0", "Yes" = "1"), inline = TRUE)
      ),
      div(class = "question",
          radioButtons("Sepsis", "Sepsis", choices = c("No" = "0", "Yes" = "1"), inline = TRUE)
      ),
      
      # Overdose history (for FOD only)
      conditionalPanel(
        condition = "input.model_type == 'fod'",
        h3(class = "section-title", "Overdose History"),
        div(class = "question",
            numericInput("total_od_n", "Total Previous Overdoses:", value = 0, min = 0)
        )
      ),
      
      # Calculate button
      actionButton("calculate", "Calculate Risk", class = "btn-calculate"),
      
      # Results
      uiOutput("results")
  )
)

# -----------------------------
# Server
# -----------------------------
server <- function(input, output, session) {
  
  # Helper to build a numeric input row aligned with training schema
  build_newdata_numeric <- function(input, model_type = c("od","fod")) {
    model_type <- match.arg(model_type)
    
    # Common 0/1 predictors (order intentional to match training formula)
    common_vars <- c(
      "ost",
      "rheumatoid_arthritis_date_b","parkinsonism_date_b",
      "osteo_arthritis_date_b","osteo_porosis_date_b","ms_date_b",
      "mood_anx_date_b","isch_stroke_date_b","ihd_date_b","hypertension_date_b",
      "hosp_tia_date_b","hosp_stroke_date_b","heart_failure_date_b",
      "haemorr_stroke_date_b","epilepsy_date_b","diabetes_date_b","depression_date_b",
      "copd_date_b","ckd_date_b","asthma_date_b","angina_date_b","ami_date_b",
      "alzheimer_dementia_date_b",
      "Tobaccouse","Tissueinfection","Stimulantuse","Sepsis",
      "Sedativeandhypnoticuse","psychoticdisorders","Polysubstance",
      "Personalitydisorders","Otherpsychoactivedruguse","Osteomyelitis","Opioiduse",
      "Neuroticrelateddisorders","Neurocognitivedisorders","Multiplementalillness",
      "Mooddisorders","Intellectualdisability","Hallucinogensuse","Endocarditis",
      "earlyonsetdisorders","Developmentdisorders","Cocaineuse","Cannabinoiduse",
      "Behaviouralpsychologicaldisturbances","Alcoholuse"
    )
    
    # Collect numeric values for all common vars
    data_list <- lapply(common_vars, function(nm) as.numeric(input[[nm]]))
    names(data_list) <- common_vars
    
    # Model-specific substance-related column + total_od_n for FOD
    if (model_type == "od") {
      data_list[["substancerelated.disorders"]] <- as.numeric(input$substancerelated.disorders)
    } else {
      data_list[["substancerelateddisorders.x"]] <- as.numeric(input$substancerelated.disorders)
      data_list[["total_od_n"]] <- as.numeric(input$total_od_n)
    }
    
    # One-row data.frame with exact names
    as.data.frame(data_list, check.names = FALSE)
  }
  
  result <- eventReactive(input$calculate, {
    # Select model
    if (input$model_type == "od") {
      model <- rf_od
      model_name <- "General Overdose Risk"
      model_type <- "od"
    } else {
      model <- rf_fod
      model_name <- "Fatal Overdose Risk"
      model_type <- "fod"
    }
    
    # Build numeric newdata aligned to training
    newdata <- build_newdata_numeric(input, model_type)
    
    # Optional: apply caret preProcess if present in the model
    if (!is.null(model$preProcess)) {
      newdata <- predict(model$preProcess, newdata)
    }
    
    # Debug (uncomment if needed)
    # cat("Newdata classes:\n"); print(sapply(newdata, class))
    
    # Predict
    pred_prob  <- predict(model, newdata, type = "prob")
    pred_class <- predict(model, newdata, type = "raw")
    
    # Safely extract "Yes"/"No" probabilities if those levels exist
    class_levels <- colnames(pred_prob)
    prob_no  <- if ("No"  %in% class_levels) pred_prob[1, "No"]  else NA_real_
    prob_yes <- if ("Yes" %in% class_levels) pred_prob[1, "Yes"] else NA_real_
    
    list(
      model_name = model_name,
      prediction = as.character(pred_class[1]),
      prob_no    = prob_no,
      prob_yes   = prob_yes,
      confidence = max(pred_prob[1, ])
    )
  })
  
  output$results <- renderUI({
    req(result())
    res <- result()
    is_high_risk <- identical(res$prediction, "Yes")
    result_class <- ifelse(is_high_risk, "results-high", "results-low")
    risk_level   <- ifelse(is_high_risk, "HIGH RISK", "LOW RISK")
    
    div(class = result_class,
        h3(paste0("🎯 Risk Assessment Results: ", risk_level)),
        div(class = "result-item",
            span(class = "result-label", "Model Used:"), res$model_name
        ),
        div(class = "result-item",
            span(class = "result-label", "Risk Classification:"), strong(res$prediction)
        ),
        div(class = "result-item",
            span(class = "result-label", "Confidence Level:"),
            sprintf("%.1f%%", res$confidence * 100)
        ),
        div(class = "result-item",
            span(class = "result-label", "Probability (No):"),
            if (is.na(res$prob_no)) "N/A" else sprintf("%.1f%%", res$prob_no * 100)
        ),
        div(class = "result-item",
            span(class = "result-label", "Probability (Yes):"),
            if (is.na(res$prob_yes)) "N/A" else sprintf("%.1f%%", res$prob_yes * 100)
        ),
        hr(),
        p(style = "font-size: 13px; color: #6c757d; margin-top: 15px;",
          strong("Note:"), " This is a clinical decision support tool. Results should be interpreted by qualified healthcare professionals.")
    )
  })
}

shinyApp(ui = ui, server = server)
