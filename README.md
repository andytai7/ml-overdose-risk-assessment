
# Overdose Risk Assessment System

This repository delivers two interfaces for ML-based overdose risk prediction:

- **Shiny App** — local interactive interface.
- **Plumber API** — REST service with a browser questionnaire client.

Both models (**RF_OD** and **RF_FOD**) expect **numeric predictors (0/1)** and output a **Yes/No classification** with probabilities.

## Quick Links
- 📘 Full docs: [`docs/README.md`](docs/README.md)
- 🧪 API code: [`api/simple_api.R`](api/simple_api.R)
- 🖥️ Shiny app: [`app/app_simple.R`](app/app_simple.R)
- 🌐 Questionnaire: [`web/questionnaire.html`](web/questionnaire.html)
- 📦 Models: [`models/RF_OD.rds`](models/RF_OD.rds), [`models/RF_FOD.rds`](models/RF_FOD.rds)

## Quick Start

```bash
# 1) Start the API (default port 8000)
cd api
Rscript start_api.R

# 2) Run the Shiny App
cd app
R -e "shiny::runApp('app_simple.R')"
```

> If your API runs on a different host/port, update `API_BASE` in `web/questionnaire.html`.

## License
This project is licensed under the **MIT License** — see [`LICENSE`](LICENSE).

## Citation
If you use this system in research, please cite:
- **Tai, Andy Man Yeung** (Year). *Machine Learning for Overdose Risk Assessment in British Columbia*. University of British Columbia.  
  Link: https://open.library.ubc.ca/soa/cIRcle/collections/ubctheses/24/items/1.0441300
  
## Disclaimer: 
This tool is designed for research and clinical decision support. It should be used in conjunction with comprehensive clinical assessment and professional judgment. Always follow established medical protocols and guidelines for substance use disorder treatment and overdose prevention.

