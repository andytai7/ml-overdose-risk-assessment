
# Overdose Risk Assessment System (Full Documentation)

## Project Overview
This project addresses the drug toxicity crisis in British Columbia through machine learning models that predict overdose risk. Built on data from 2015-2019, these Random Forest models achieve over 90% accuracy in classifying general overdoses and provide crucial risk assessment for healthcare interventions.
Key Innovation: The Risk Assessment and Management Platform (RAMP) approach uses eHealth solutions to personalize addiction psychiatry treatment and prevention strategies.

## Overview
This project offers two ways to run overdose risk prediction:
1. **Shiny App** — local interactive UI for clinicians and researchers.
2. **Plumber API** — REST API, plus a browser questionnaire client.

Models:
- **RF_OD**: General overdose risk (classification).
- **RF_FOD**: Fatal overdose risk (classification; includes `total_od_n`).

All predictors are **numeric (0/1)**. The outcome in each saved model (`.rds`) is **factor** internally.

---

## Repository Structure

```
overdose-risk/
├── app/                      # Shiny app
│   ├── app_simple.R
│   └── assets/               # optional
├── api/                      # Plumber API
│   ├── simple_api.R
│   ├── start_api.R
│   └── example_requests/
│       ├── od_example.json
│       └── fod_example.json
├── models/                   # Saved models
│   ├── RF_OD.rds
│   └── RF_FOD.rds
├── web/                      # Browser questionnaire front-end
│   └── questionnaire.html
├── docs/                     # Documentation
│   └── README.md
├── .gitignore
└── LICENSE
```

> **Important:** The code in `app_simple.R` and `simple_api.R` reads models from `../models/RF_*.rds`. Keep your folder layout as shown or adjust those paths.

---

## Setup

### Requirements
- R ≥ 4.2 (recommended 4.3+)
- Packages: `plumber`, `caret`, `ranger`, `jsonlite`, `shiny`

Install packages:
```r
install.packages(c("plumber","caret","ranger","jsonlite","shiny"))
```

Place model files:
- `models/RF_OD.rds`
- `models/RF_FOD.rds`

---

## Run the API

```bash
cd api
Rscript start_api.R            # default port 8000
# or specify a port:
Rscript start_api.R 8080
```

Health & docs endpoints:
- Health: `GET http://localhost:8000/health`
- Feature list: `GET http://localhost:8000/features`
- Templates:  
  - OD: `GET http://localhost:8000/template/overdose`  
  - FOD: `GET http://localhost:8000/template/fatal_overdose`
- Schema: `GET http://localhost:8000/schema`

### Predict with `curl` (examples)
```bash
# OD (general overdose)
curl -X POST http://localhost:8000/predict/overdose   -H "Content-Type: application/json"   -d @api/example_requests/od_example.json

# FOD (fatal overdose)
curl -X POST http://localhost:8000/predict/fatal_overdose   -H "Content-Type: application/json"   -d @api/example_requests/fod_example.json
```

---

## Questionnaire (Web Client)

1. Open `web/questionnaire.html` in a browser.
2. Set `API_BASE` at the top of the script to match your API (e.g., `http://127.0.0.1:8000`).
3. Select model type (OD/FOD), fill the Yes/No items, submit.

The client sends **numeric 0/1** values, ensures the correct feature key for the substance‑related variable:
- OD → `substancerelated.disorders`
- FOD → `substancerelateddisorders.x`
…and includes `total_od_n` for FOD.

---

## Shiny App

Run locally:
```r
setwd("app")
shiny::runApp("app_simple.R")
```

The app:
- Converts all radio inputs (`"0"/"1"` strings) to **numeric** before prediction.
- Aligns feature names and classes to match model training.
- Shows the predicted class, probabilities, and confidence.

---

## API Payload Rules

- **Binary predictors** accept any of:
  - `"0"`, `"1"`, `"yes"`, `"no"`, `"true"`, `"false"`, `0`, `1`
  - The API coerces to **numeric 0/1** server-side.
- **FOD only**: `total_od_n` must be a non-negative integer (string or number).
- **Feature name difference**:
  - OD: `substancerelated.disorders`
  - FOD: `substancerelateddisorders.x`

Use `GET /features` to obtain the exact required field names for each model.

---

## Example Payloads

**OD** → `api/example_requests/od_example.json`
```json
{
  "ost": "0",
  "rheumatoid_arthritis_date_b": "0",
  "parkinsonism_date_b": "0",
  "osteo_arthritis_date_b": "0",
  "osteo_porosis_date_b": "0",
  "ms_date_b": "0",
  "mood_anx_date_b": "1",
  "isch_stroke_date_b": "0",
  "ihd_date_b": "0",
  "hypertension_date_b": "1",
  "hosp_tia_date_b": "0",
  "hosp_stroke_date_b": "0",
  "heart_failure_date_b": "0",
  "haemorr_stroke_date_b": "0",
  "epilepsy_date_b": "0",
  "diabetes_date_b": "1",
  "depression_date_b": "1",
  "copd_date_b": "0",
  "ckd_date_b": "0",
  "asthma_date_b": "0",
  "angina_date_b": "0",
  "ami_date_b": "0",
  "alzheimer_dementia_date_b": "0",
  "Tobaccouse": "1",
  "Tissueinfection": "0",
  "Stimulantuse": "0",
  "Sepsis": "0",
  "Sedativeandhypnoticuse": "0",
  "psychoticdisorders": "0",
  "Polysubstance": "0",
  "Personalitydisorders": "0",
  "Otherpsychoactivedruguse": "0",
  "Osteomyelitis": "0",
  "Opioiduse": "1",
  "Neuroticrelateddisorders": "0",
  "Neurocognitivedisorders": "0",
  "Multiplementalillness": "0",
  "Mooddisorders": "1",
  "Intellectualdisability": "0",
  "Hallucinogensuse": "0",
  "Endocarditis": "0",
  "earlyonsetdisorders": "0",
  "Developmentdisorders": "0",
  "Cocaineuse": "0",
  "Cannabinoiduse": "0",
  "Behaviouralpsychologicaldisturbances": "0",
  "Alcoholuse": "1",
  "substancerelated.disorders": "1"
}
```

**FOD** → `api/example_requests/fod_example.json`
```json
{
  "ost": "0",
  "rheumatoid_arthritis_date_b": "0",
  "parkinsonism_date_b": "0",
  "osteo_arthritis_date_b": "0",
  "osteo_porosis_date_b": "0",
  "ms_date_b": "0",
  "mood_anx_date_b": "1",
  "isch_stroke_date_b": "0",
  "ihd_date_b": "0",
  "hypertension_date_b": "1",
  "hosp_tia_date_b": "0",
  "hosp_stroke_date_b": "0",
  "heart_failure_date_b": "0",
  "haemorr_stroke_date_b": "0",
  "epilepsy_date_b": "0",
  "diabetes_date_b": "1",
  "depression_date_b": "1",
  "copd_date_b": "0",
  "ckd_date_b": "0",
  "asthma_date_b": "0",
  "angina_date_b": "0",
  "ami_date_b": "0",
  "alzheimer_dementia_date_b": "0",
  "Tobaccouse": "1",
  "Tissueinfection": "0",
  "Stimulantuse": "0",
  "Sepsis": "0",
  "Sedativeandhypnoticuse": "0",
  "psychoticdisorders": "0",
  "Polysubstance": "0",
  "Personalitydisorders": "0",
  "Otherpsychoactivedruguse": "0",
  "Osteomyelitis": "0",
  "Opioiduse": "1",
  "Neuroticrelateddisorders": "0",
  "Neurocognitivedisorders": "0",
  "Multiplementalillness": "0",
  "Mooddisorders": "1",
  "Intellectualdisability": "0",
  "Hallucinogensuse": "0",
  "Endocarditis": "0",
  "earlyonsetdisorders": "0",
  "Developmentdisorders": "0",
  "Cocaineuse": "0",
  "Cannabinoiduse": "0",
  "Behaviouralpsychologicaldisturbances": "0",
  "Alcoholuse": "1",
  "substancerelateddisorders.x": "1",
  "total_od_n": "2"
}
```

---

## Troubleshooting

- **Missing features** → Call `/features` and ensure your JSON includes all required keys.
- **Type mismatch** → The API coerces strings/booleans/numbers into numeric 0/1; avoid arrays/nested structures.
- **Client connection** → Ensure `API_BASE` in `web/questionnaire.html` matches your API host/port.

---

## Security (Production)

- Use HTTPS/TLS.
- Add authentication (API keys/OAuth).
- Log access & avoid storing PHI.
- Consider RBAC and audit trails.

---

## License
This project is licensed under the **MIT License** — see [`LICENSE`](../LICENSE).

---

## Citation
If you use this system in research, please cite:

**Tai, Andy Man Yeung** (Year). *Machine Learning for Overdose Risk Assessment in British Columbia*. University of British Columbia.  
Link: https://open.library.ubc.ca/soa/cIRcle/collections/ubctheses/24/items/1.0441300

## Disclaimer: This tool is designed for research and clinical decision support. It should be used in conjunction with comprehensive clinical assessment and professional judgment. Always follow established medical protocols and guidelines for substance use disorder treatment and overdose prevention.
