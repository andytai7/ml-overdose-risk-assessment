
# Advanced Green Shiny App (final fixed):
# - Multi-page wizard (5 questions/page)
# - Progress bar + animated transitions
# - Category headings + tooltips
# - Rich visualizations
# - Safe input coercion to avoid 'sum: invalid type (list)'
# - Simple PDF report
# - "Substance-Related Disorders (General)" moved into a page
# - "Total Previous Overdoses (FOD only)" shown on a page

library(shiny)
library(shinyWidgets)
library(plotly)
library(caret)
library(ranger)
library(dplyr)
library(glue)
library(rmarkdown)

# ---- Load models ----
rf_od  <- readRDS("RF_OD_slim.rds")
rf_fod <- readRDS("RF_FOD_slim.rds")

# ---- Feature groups ----
medical <- c("ost","diabetes_date_b","hypertension_date_b","asthma_date_b","copd_date_b",
             "ckd_date_b","depression_date_b","epilepsy_date_b")
cardio  <- c("heart_failure_date_b","ihd_date_b","angina_date_b","ami_date_b",
             "isch_stroke_date_b","haemorr_stroke_date_b","hosp_stroke_date_b","hosp_tia_date_b")
neuro   <- c("alzheimer_dementia_date_b","parkinsonism_date_b","ms_date_b")
musculo <- c("rheumatoid_arthritis_date_b","osteo_arthritis_date_b","osteo_porosis_date_b")
mental  <- c("mood_anx_date_b","Mooddisorders","psychoticdisorders","Personalitydisorders",
             "Neuroticrelateddisorders","Neurocognitivedisorders","Intellectualdisability",
             "Developmentdisorders","earlyonsetdisorders","Multiplementalillness",
             "Behaviouralpsychologicaldisturbances")
substance <- c("Opioiduse","Alcoholuse","Tobaccouse","Cannabinoiduse","Cocaineuse",
               "Stimulantuse","Sedativeandhypnoticuse","Hallucinogensuse","Polysubstance",
               "Otherpsychoactivedruguse")
infect <- c("Endocarditis","Osteomyelitis","Tissueinfection","Sepsis")

common_vars <- c(medical, cardio, neuro, musculo, mental, substance, infect)

# Category helper
var_category <- function(v) {
  if (v %in% medical)   return("Medical")
  if (v %in% cardio)    return("Cardiovascular")
  if (v %in% neuro)     return("Neurological")
  if (v %in% musculo)   return("Musculoskeletal")
  if (v %in% mental)    return("Mental Health")
  if (v %in% substance) return("Substance Use")
  if (v %in% infect)    return("Infections")
  if (v == sr_ui_id)    return("Substance-Related (General)")
  if (v == "total_od_n")return("Overdose History")
  return("Other")
}

# Tooltip helper
var_tip <- function(v) {
  if (v == sr_ui_id) {
    return("General substance-related disorders indicator (0 = No, 1 = Yes).")
  }
  if (v == "total_od_n") {
    return("Enter total number of previous overdoses (non-negative integer).")
  }
  paste0("Select \"Yes\" if ", var_category(v), " condition ", v,
         " is present before baseline; otherwise \"No\".")
}

# Model-specific field mapping ids (UI-only ids that we include on pages)
sr_ui_id <- "substancerelated_disorders_ui"

# ---- Wizard paging: put EVERYTHING into pages ----
# We include the UI-only ids in the wizard flow but keep model data-building logic separate.
wizard_vars <- c(common_vars, sr_ui_id, "total_od_n")

chunk <- function(x, n=5) split(x, ceiling(seq_along(x)/n))
pages_list <- chunk(wizard_vars, 5)
TOTAL_PAGES <- length(pages_list) + 1  # +1 for Review page

# ---- Safe input coercion (prevents list/NULL issues) ----
safe01 <- function(x) {
  # Treat unset/null as 0
  if (is.null(x) || length(x) == 0) return(0)
  val <- suppressWarnings(as.numeric(x))
  if (length(val) == 0 || is.na(val)) return(0)
  if (val >= 1) return(1)
  return(0)
}

# ---- UI ----
ui <- fluidPage(
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "assets/styles.css"),
    tags$script(src = "assets/app.js")
  ),
  div(class = "main-container",
      div(class = "header", h2("Overdose Risk Assessment"),
          span(class = "badge", "v3 • Advanced")),
      
      radioButtons("model_type", "Select Assessment Type:",
                   choices = c("General Overdose Risk (RF_OD)" = "od",
                               "Fatal Overdose Risk (RF_FOD)"   = "fod"),
                   selected = "od", inline = TRUE
      ),
      
      # Page container
      div(style = "display:none;", numericInput("page", NULL, value = 1)),
      div(id = "page_container", class = "fade-in", uiOutput("page_ui")),
      
      div(class = "progress-wrapper",
          progressBar(id = "progress", value = 0, total = 100, display_pct = TRUE, status = "success")
      ),
      
      fluidRow(
        column(6, actionButton("btn_prev", "\u25C0 Previous", class = "btn-secondary")),
        column(6, div(style="text-align:right;",
                      actionButton("btn_next", "Next \u25B6", class = "btn-primary"),
                      actionButton("calculate", "Calculate Risk", class = "btn-primary")
        ))
      ),
      
      br(),
      uiOutput("results_panel"),
      br(),
      
      fluidRow(
        column(6, plotlyOutput("prob_bar", height = "280px")),
        column(6, plotlyOutput("confidence_gauge", height = "280px"))
      ),
      fluidRow(
        column(6, plotlyOutput("yes_donut", height = "320px")),
        column(6, plotlyOutput("category_bars", height = "320px"))
      ),
      
      br(),
      uiOutput("yes_table"),
      br(),
      
      downloadButton("download_report", "Download PDF Report", class = "btn-primary")
  )
)

# ---- Server ----
server <- function(input, output, session) {
  # Navigation
  current_page <- reactiveVal(1)
  observeEvent(input$btn_prev, { if (current_page() > 1) current_page(current_page() - 1) })
  observeEvent(input$btn_next, { if (current_page() < TOTAL_PAGES) current_page(current_page() + 1) })
  
  observe({
    updateNumericInput(session, "page", value = current_page())
    pct <- round(100 * current_page() / TOTAL_PAGES)
    updateProgressBar(session, "progress", value = pct)
    session$sendCustomMessage(type = "addFadeIn", message = list(id = "page_container"))
  })
  
  # Render page content (5 items/page) with category heading + tooltips
  output$page_ui <- renderUI({
    pg <- current_page()
    if (pg <= length(pages_list)) {
      vars <- pages_list[[pg]]
      # Determine title based on the first var
      title_var <- vars[1]
      cat_title <- var_category(title_var)
      
      tagList(
        div(class = "section-title", glue("{cat_title} — Page {pg} of {TOTAL_PAGES-1}")),
        lapply(vars, function(q) {
          # Special controls for sr_ui_id and total_od_n
          if (q == sr_ui_id) {
            div(class = "question-card", title = var_tip(q),
                radioButtons(sr_ui_id, "Substance-Related Disorders (General)",
                             choices = c("No" = "0", "Yes" = "1"), inline = TRUE),
                tags$small(class = "tooltip-note", var_tip(q))
            )
          } else if (q == "total_od_n") {
            div(class = "question-card", title = var_tip(q),
                numericInput("total_od_n", "Total Previous Overdoses (FOD only)", value = 0, min = 0),
                tags$small(class = "tooltip-note", var_tip(q))
            )
          } else {
            # All standard radio questions
            div(class = "question-card", title = var_tip(q),
                radioButtons(q, label = q, choices = c("No" = "0", "Yes" = "1"), inline = TRUE),
                tags$small(class = "tooltip-note", var_tip(q))
            )
          }
        })
      )
    } else {
      tagList(
        div(class = "section-title", "Review & Submit"),
        div(class = "card",
            p('You\'ve reached the last page. Click \"Calculate Risk\" to run the model.'),
            p("Tip: You can still navigate back to change answers."))
      )
    }
  })
  
  # Build numeric newdata row for prediction (only model-required features)
  build_newdata <- reactive({
    # Use safe01 for ALL variables to handle NULL/uninitialized inputs
    vals <- lapply(common_vars, function(nm) safe01(input[[nm]]))
    names(vals) <- common_vars
    
    # Map SR to model-specific name + add total_od_n for FOD
    sr_val <- safe01(input[[sr_ui_id]])
    if (identical(input$model_type, "od")) {
      vals[["substancerelated.disorders"]] <- sr_val
    } else {
      vals[["substancerelateddisorders.x"]] <- sr_val
      # For total_od_n, handle as numeric count
      total_od <- input$total_od_n
      vals[["total_od_n"]] <- if (is.null(total_od)) 0 else as.numeric(total_od)
    }
    
    as.data.frame(vals, check.names = FALSE)
  })
  
  # Prediction
  result <- eventReactive(input$calculate, {
    req(input$model_type)
    
    model <- if (identical(input$model_type, "od")) rf_od else rf_fod
    nd <- build_newdata()
    if (!is.null(model$preProcess)) nd <- predict(model$preProcess, nd)
    
    pred_prob  <- predict(model, nd, type = "prob")
    pred_class <- predict(model, nd, type = "raw")
    
    class_levels <- colnames(pred_prob)
    prob_no  <- if ("No"  %in% class_levels) pred_prob[1, "No"]  else pred_prob[1, class_levels[1]]
    prob_yes <- if ("Yes" %in% class_levels) pred_prob[1, "Yes"] else pred_prob[1, class_levels[min(2, length(class_levels))]]
    
    list(
      prediction = as.character(pred_class[1]),
      prob_no    = as.numeric(prob_no),
      prob_yes   = as.numeric(prob_yes),
      confidence = max(pred_prob[1, ]),
      probs      = pred_prob[1, , drop = TRUE]
    )
  })
  
  # Results panel
  output$results_panel <- renderUI({
    req(result())
    res <- result()
    is_high <- identical(tolower(res$prediction), "yes")
    div(class = if (is_high) "results-high" else "results-low",
        h3(glue("\U0001F3AF Risk Assessment Results: {if (is_high) 'HIGH RISK' else 'LOW RISK'}")),
        p(tagList(
          strong("Risk Classification: "), res$prediction, tags$br(),
          strong("Probability (No): "), sprintf("%.1f%%", res$prob_no * 100), tags$br(),
          strong("Probability (Yes): "), sprintf("%.1f%%", res$prob_yes * 100)
        ))
    )
  })
  
  # Visualization: probabilities bar
  output$prob_bar <- renderPlotly({
    req(result())
    res <- result()
    df <- data.frame(class = names(res$probs), prob = as.numeric(res$probs))
    plot_ly(df, x = ~class, y = ~prob, type = 'bar',
            marker = list(color = c('#81c784','#388e3c')))
  })
  
  # Visualization: confidence gauge
  output$confidence_gauge <- renderPlotly({
    req(result())
    res <- result()
    plot_ly(type = "indicator", mode = "gauge+number", value = res$confidence,
            gauge = list(axis = list(range = list(NULL, 1)),
                         bar = list(color = '#388e3c'),
                         steps = list(
                           list(range = c(0, 0.5), color = '#e8f5e9'),
                           list(range = c(0.5, 0.8), color = '#a5d6a7'),
                           list(range = c(0.8, 1.0), color = '#81c784')
                         )),
            number = list(suffix = "", valueformat = ".2f"))
  })
  
  # Visualization: donut of Yes counts by category (SAFE)
  output$yes_donut <- renderPlotly({
    groups <- list(
      Medical = medical, Cardiovascular = cardio, Neurological = neuro,
      Musculoskeletal = musculo, Mental = mental, Substance = substance, Infections = infect
    )
    
    counts <- sapply(names(groups), function(cat) {
      vars <- groups[[cat]]
      sum(vapply(vars, function(v) safe01(input[[v]]) == 1, logical(1)))
    })
    
    df <- data.frame(cat = names(counts), yes = as.numeric(counts), row.names = NULL)
    
    plot_ly(
      df, labels = ~cat, values = ~yes,
      type = 'pie', hole = 0.5,
      textinfo = 'label+value',
      marker = list(colors = c('#66bb6a','#81c784','#a5d6a7','#43a047','#388e3c','#76d275','#c8e6c9'))
    )
  })
  
  # Visualization: stacked bars Yes/No by category (SAFE)
  output$category_bars <- renderPlotly({
    groups <- list(
      Medical = medical, Cardiovascular = cardio, Neurological = neuro,
      Musculoskeletal = musculo, Mental = mental, Substance = substance, Infections = infect
    )
    
    df <- do.call(rbind, lapply(names(groups), function(cat) {
      vars <- groups[[cat]]
      yes_cnt <- sum(vapply(vars, function(v) safe01(input[[v]]) == 1, logical(1)))
      no_cnt  <- sum(vapply(vars, function(v) safe01(input[[v]]) == 0, logical(1)))
      data.frame(category = cat, yes = yes_cnt, no = no_cnt)
    }))
    
    plot_ly(df, x = ~category, y = ~yes, type = 'bar', name = 'Yes',
            marker = list(color = '#66bb6a')) %>%
      add_trace(y = ~no, name = 'No', marker = list(color = '#c8e6c9')) %>%
      layout(barmode = 'stack')
  })
  
  # Table of selected Yes features (SAFE)
  output$yes_table <- renderUI({
    yes_vars <- common_vars[sapply(common_vars, function(v) safe01(input[[v]]) == 1)]
    if (length(yes_vars) == 0) {
      return(div(class = "card", p("No features marked Yes.")))
    }
    tags$table(class = "card", style = "width:100%;",
               tags$thead(tags$tr(tags$th("Selected Yes Features"))),
               tags$tbody(lapply(yes_vars, function(v) tags$tr(tags$td(v))))
    )
  })
  
  # Simple PDF report (text summary; charts remain in app)
  output$download_report <- downloadHandler(
    filename = function() { "risk_report.pdf" },
    content = function(file) {
      res <- result()
      yes_rate <- sprintf("%.1f%%", 100 * res$prob_yes)
      no_rate  <- sprintf("%.1f%%", 100 * res$prob_no)
      conf     <- sprintf("%.1f%%", 100 * res$confidence)
      
      rmd <- paste0(
        "---\n",
        "title: 'Overdose Risk Report'\n",
        "output: pdf_document\n",
        "---\n\n",
        "## Summary\n\n",
        "**Prediction:** ", res$prediction, "\n\n",
        "**Confidence:** ", conf, "\n\n",
        "**Probability (Yes):** ", yes_rate, "\n\n",
        "**Probability (No):** ", no_rate, "\n"
      )
      tf <- tempfile(fileext = ".Rmd")
      writeLines(rmd, tf)
      rmarkdown::render(tf, output_file = file)
    }
  )
}

shinyApp(ui, server)
