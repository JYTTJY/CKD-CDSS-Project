# ![]()Clinical Decision Support System (CDSS) for CKD Exercise Rehabilitation

An interactive **R Shiny** Clinical Decision Support System (CDSS) designed for physical therapists and renal care teams to safely evaluate, prescribe, and monitor exercise rehabilitation for Chronic Kidney Disease (CKD) patients.

![CDSS Interface Screenshot](screenshot.png)

------------------------------------------------------------------------

## 🌟 Key Features

- **Red-Flag Safety Intercept**: Evaluates real-time clinical parameters (e.g., SBP ≥ 180 mmHg, K+ ≥ 6.0 mEq/L) to instantly flag exercise contraindications.
- **Dynamic FITT Prescription**: Generates tailored Frequency, Intensity, Time, and Type (FITT) exercise recommendations based on CKD stage and dialysis schedule.
- **Medication & Dietary Safety**: Provides eGFR-adjusted dosage alerts for nephrotoxic drugs (e.g., NSAIDs, Metformin) and individualized electrolyte/fluid restriction guidance.
- **Longitudinal Trajectory**: Tracks sCr/eGFR trends to identify AKI progression risks using Plotly.
- **Clinical Report Export**: Exports standardized evaluation summaries for multidisciplinary medical teams.

------------------------------------------------------------------------

## 🚀 How to Run Locally

### Prerequisites

Ensure you have `R` or `RStudio` installed along with the required packages:

``` R
install.packages(c("shiny", "shinydashboard", "plotly", "DT", "dplyr"))
```

### Running the Application

1.  Clone or download this repository to your local machine.

2.  Open `app.R` in RStudio.

3.  Execute the following command in your R console:

    ``` R
    shiny::runApp()
    ```

🔒 **Data Privacy & Compliance**

This decision support tool operates strictly on real-time, local clinical inputs. No real Patient Identifiable Information (PII) is uploaded, stored, or transmitted. All patient names, medical record numbers (MRNs), and lab values shown in screenshots or test cases are entirely synthetic and used solely for prototype demonstration.
