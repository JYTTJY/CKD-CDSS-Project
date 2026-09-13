# ==============================================================================
# Shiny Application Entry Point (app.R) - FIXED
# ==============================================================================

library(shiny)
library(shinydashboard)
library(plotly)
library(DT)

# Load decision engine
source("logic.R")

# ------------------------------------------------------------------------------
# 1. UI Design
# ------------------------------------------------------------------------------
ui <- dashboardPage(
  skin = "blue",
  dashboardHeader(title = "Renal PT CDSS Platform"),
  
  dashboardSidebar(
    # --- Global Patient Context ---
    div(class = "patient-card",
        h5(icon("id-card"), strong(" Patient Demographics"), style = "margin-top:0; margin-bottom:10px; color: #5dade2; font-size: 13px;"),
        textInput("pt_name", "Patient Name:", value = "Tan Ah Kow"),
        textInput("pt_mrn", "MRN:", value = "MRN-8839201"),
        textInput("pt_ic", "IC / Passport:", value = "850712-01-5432"),
        hr(style = "border-top: 1px solid #37474f; margin-top: 5px; margin-bottom: 10px;")
    ),
    
    # --- Sidebar Navigation Menu ---
    sidebarMenu(
      id = "tabs",
      menuItem("1. Pre-Test Assessment & FITT", tabName = "pre_eval", icon = icon("user-nurse")),
      menuItem("2. Post-Test Monitoring & Discharge", tabName = "post_eval", icon = icon("heartbeat")),
      menuItem("3. Longitudinal Trends & Cohort", tabName = "longitudinal", icon = icon("chart-line")),
      menuItem("4. Clinical Report & Export", tabName = "print_report", icon = icon("print"))
    )
  ),
  
  dashboardBody(
    tags$head(
      tags$style(HTML("
        /* ---------- Global background ---------- */
        .content-wrapper { background-color: #f4f6f9; }

        /* ---------- Header bar ---------- */
        .main-header .logo {
          font-weight: 700; letter-spacing: 0.3px;
          background: linear-gradient(135deg, #367fa9 0%, #3c8dbc 100%) !important;
        }
        .main-header .navbar {
          background: linear-gradient(135deg, #3c8dbc 0%, #4aa3d5 100%) !important;
        }

        /* ---------- Cards / boxes ---------- */
        .box {
          border-radius: 8px;
          box-shadow: 0 1px 3px rgba(0,0,0,0.08);
          border-top-width: 3px !important;
          transition: box-shadow 0.15s ease-in-out;
        }
        .box:hover { box-shadow: 0 3px 10px rgba(0,0,0,0.10); }
        .box-header { border-radius: 8px 8px 0 0; }
        .box.box-solid.box-primary > .box-header { background: #3c8dbc; }
        .box.box-solid.box-info > .box-header { background: #17a2b8; }
        .box.box-solid.box-warning > .box-header { background: #f0ad4e; }
        .box.box-solid.box-success > .box-header { background: #00a65a; }
        .box.box-solid.box-danger > .box-header { background: #dd4b39; }
        .box-title { font-weight: 600; letter-spacing: 0.2px; }

        /* ---------- Section sub-headers (h4/h5 inside boxes) ---------- */
        .box-body h4 {
          font-size: 15px; font-weight: 600; color: #2c3e50;
          margin-top: 18px; margin-bottom: 8px;
          padding-bottom: 6px; border-bottom: 1px solid #eef0f2;
        }
        .box-body h4:first-of-type { margin-top: 0; }
        .box-body h5 { font-weight: 600; color: #34495e; }
        .box-body hr { border-top: 1px solid #ecf0f1; margin: 14px 0; }

        /* ---------- Sidebar ---------- */
        .main-sidebar { box-shadow: 2px 0 4px rgba(0,0,0,0.06); }
        .sidebar-menu > li > a { font-size: 13.5px; padding: 13px 15px; }
        .sidebar-menu > li.active > a { border-left-color: #4aa3d5; background: #1e282c; }
        .sidebar-menu > li > a > .fa, .sidebar-menu > li > a > i { width: 22px; }

        /* ---------- Sidebar patient card ---------- */
        .patient-card { padding: 14px 12px; color: #ecf0f1; background-color: #1a2226; }
        .patient-card .form-group { margin-bottom: 8px; }
        .patient-card label { color: #95a5a6; font-size: 11px; text-transform: uppercase; letter-spacing: 0.4px; margin-bottom: 2px; }
        .patient-card input.form-control {
          background-color: #263238; border: 1px solid #3c4a50; color: #ecf0f1; font-size: 13px;
        }
        .patient-card input.form-control:focus { border-color: #3c8dbc; box-shadow: none; }

        /* ---------- Status / clinical alert banners ---------- */
        .clinical-alert { padding: 12px 14px; border-radius: 6px; font-weight: 600; font-size: 14px; border-left: 5px solid transparent; }
        .clinical-alert-success { background-color: #e8f8ee; color: #14532d; border-left-color: #28a745; }
        .clinical-alert-warning { background-color: #fff8e6; color: #7a5c00; border-left-color: #ffc107; }
        .clinical-alert-danger  { background-color: #fdecea; color: #7f1d1d; border-left-color: #dc3545; }
        .clinical-alert-info    { background-color: #e7f5fa; color: #0c5460; border-left-color: #17a2b8; }
        .clinical-alert ul { margin: 6px 0 0 0; padding-left: 20px; }

        /* ---------- Idle / not-yet-run placeholder ---------- */
        .cdss-placeholder {
          padding: 18px 14px; text-align: center; color: #8492a6;
          background: repeating-linear-gradient(45deg, #fafbfc, #fafbfc 10px, #f4f6f9 10px, #f4f6f9 20px);
          border: 1px dashed #d5dbe0; border-radius: 6px; font-size: 13px;
        }
        .cdss-placeholder i { display: block; font-size: 20px; margin-bottom: 6px; color: #b7c0c9; }

        /* ---------- Value / status badges ---------- */
        .value-badge {
          display: inline-block; color: #fff; padding: 4px 9px; border-radius: 4px;
          font-weight: 600; font-size: 12px; letter-spacing: 0.2px;
        }

        /* ---------- CDSS compact card grid (medication / diet) ---------- */
        .cdss-grid-2 {
          display: grid; grid-template-columns: repeat(auto-fit, minmax(255px, 1fr));
          gap: 10px; align-items: start;
        }

        /* --- Medication alert card: severity-first, rationale on demand --- */
        .med-card {
          background: #ffffff; border-left: 4px solid #ccc; border-radius: 6px;
          padding: 10px 12px; box-shadow: 0 1px 2px rgba(0,0,0,0.07);
        }
        .med-card-head { display: flex; justify-content: space-between; align-items: flex-start; gap: 8px; }
        .med-card-title { font-weight: 600; font-size: 13px; color: #2c3e50; line-height: 1.35; }
        .med-card-examples { font-weight: 400; color: #8492a6; font-size: 11px; display: block; }
        .med-card-badge { flex-shrink: 0; padding: 2px 7px !important; font-size: 10.5px !important; }
        .med-card-action { font-size: 12.5px; color: #333; margin: 6px 0 0 0; line-height: 1.4; }
        .med-card details { margin-top: 6px; }
        .med-card summary {
          cursor: pointer; font-size: 11px; font-weight: 600; color: #3c8dbc;
          list-style: none; user-select: none;
        }
        .med-card summary::-webkit-details-marker { display: none; }
        .med-card summary::before { content: '▸ Rationale'; }
        .med-card details[open] summary::before { content: '▾ Rationale'; }
        .med-card .rationale-text { font-size: 11.5px; color: #6b7280; font-style: italic; margin: 4px 0 0 0; line-height: 1.4; }

        /* --- Diet guidance card --- */
        .diet-card {
          background: #ffffff; border: 1px solid #e2e8f0; border-radius: 6px;
          padding: 10px 12px; height: 100%; box-sizing: border-box;
        }
        .diet-card h5 { margin: 0 0 5px 0; font-size: 13px; }
        .diet-card .diet-action-line { font-size: 12.5px; font-weight: 700; margin: 0 0 6px 0; }
        .diet-card p { margin: 0 0 4px 0; }
        .diet-food-cols { display: flex; gap: 12px; flex-wrap: wrap; margin-top: 4px; }
        .diet-food-cols > div { flex: 1 1 110px; min-width: 110px; }
        .diet-food-cols .col-label { font-size: 10.5px; font-weight: 700; letter-spacing: 0.2px; text-transform: uppercase; }
        .diet-food-cols ul { padding-left: 16px; margin: 3px 0 0 0; }
        .diet-food-cols li { font-size: 11.5px; color: #2d3748; line-height: 1.5; }
        .diet-card details { margin-top: 5px; }
        .diet-card summary {
          cursor: pointer; font-size: 11px; font-weight: 600; color: #3c8dbc;
          list-style: none; user-select: none;
        }
        .diet-card summary::-webkit-details-marker { display: none; }
        .diet-card summary::before { content: '▸ Clinical note'; }
        .diet-card details[open] summary::before { content: '▾ Clinical note'; }
        .diet-card .note-text { font-size: 11.5px; color: #4a5568; font-style: italic; margin: 4px 0 0 0; line-height: 1.4; }

        /* ---------- Buttons ---------- */
        .btn { border-radius: 5px; font-weight: 600; letter-spacing: 0.2px; transition: transform 0.05s ease-in-out; }
        .btn:active { transform: scale(0.98); }
        .btn-block { padding-top: 10px; padding-bottom: 10px; }

        /* ---------- Tables ---------- */
        table.dataTable { font-size: 13px; }
        .table > thead > tr > th { border-bottom: 2px solid #dee2e6; color: #2c3e50; }

        /* ---------- Print layout ---------- */
        @media print {
          .main-sidebar, .main-header, .btn, .no-print {
            display: none !important;
          }
          .content-wrapper, .right-side, .main-footer {
            margin-left: 0 !important;
            background-color: white !important;
            padding: 0 !important;
          }
          .box {
            border: none !important;
            box-shadow: none !important;
          }
          .box-body h4 { break-after: avoid; }
          .clinical-alert, .value-badge { -webkit-print-color-adjust: exact; print-color-adjust: exact; }
        }
      "))
    ),
    
    tabItems(
      # ------------------------------------------------------------------------
      # TAB 1: PRE-TEST Assessment & Prescription
      # ------------------------------------------------------------------------
      tabItem(
        tabName = "pre_eval",
        fluidRow(
          box(
            title = "1. Pre-Treatment Baseline Inputs", width = 5, status = "primary", solidHeader = TRUE,
            
            h5(strong("Demographics & Renal Function Baseline:")),
            fluidRow(
              column(6, numericInput("age", "Age (years):", 65, 18, 100)),
              column(6, selectInput("gender", "Gender:", choices = c("Male" = "male", "Female" = "female")))
            ),
            fluidRow(
              column(12, numericInput("scr", "Serum Creatinine (mg/dL):", 3.5, 0.2, 20.0, step = 0.1))
            ),
            uiOutput("egfr_display_ui"),
            hr(),
            
            selectInput("dialysis_status", "Dialysis Timing:", 
                        choices = c("Intradialytic (During Dialysis)" = "intradialytic", 
                                    "Post-Dialysis (Same Day)" = "post_dialysis", 
                                    "Non-Dialysis Day / CKD Non-Dialysis" = "non_dialysis")),
            
            conditionalPanel(
              condition = "input.dialysis_status == 'intradialytic'",
              numericInput("dial_time", "Dialysis Duration Elapsed (Hours):", 1.0, 0.1, 4.0, step = 0.5)
            ),
            
            selectInput("fistula_arm", "AV Fistula / Graft Location:", choices = c("None", "Left Arm", "Right Arm")),
            hr(),
            h5(strong("Vital Signs & Labs:")),
            fluidRow(
              column(6, numericInput("sbp", "SBP (mmHg):", 130, 70, 220)),
              column(6, numericInput("dbp", "DBP (mmHg):", 80, 40, 130))
            ),
            uiOutput("bp_validation_ui"),
            
            fluidRow(
              column(6, numericInput("hr", "HR (bpm):", 75, 40, 160)),
              column(6, numericInput("spo2", "SpO2 (%):", 98, 70, 100))
            ),
            fluidRow(
              column(6, numericInput("temp", "Temp (°C):", 36.8, 35.0, 41.0, step = 0.1)),
              column(6, numericInput("glucose", "Glucose (mmol/L):", 6.0, 2.0, 25.0, step = 0.5))
            ),
            fluidRow(
              column(4, numericInput("k_level", "Potassium K+ (mEq/L):", 4.5, 2.0, 8.0, step = 0.1)),
              column(4, numericInput("phos_level", "Phosphate (mg/dL):", 4.0, 1.0, 15.0, step = 0.1)),
              column(4, numericInput("hb_level", "Hemoglobin Hb (g/dL):", 10.5, 4.0, 18.0, step = 0.1))
            ),
            hr(),
            h5(strong("Fluid Balance & Dry Weight:")),
            fluidRow(
              column(6, numericInput("dry_weight", "Dry Weight (kg):", 60.0, 30.0, 150.0)),
              column(6, numericInput("pre_weight", "Pre-Weight (kg):", 62.0, 30.0, 150.0))
            ),
            uiOutput("idwg_display_ui"),
            
            hr(),
            h5(strong("Red Flag Symptoms Checklist:")),
            checkboxGroupInput("symptoms", NULL,
                               choices = c("Chest Pain / Angina" = "chest_pain",
                                           "Severe Dyspnea / Orthopnea" = "severe_sob",
                                           "Significant Dizziness / Lightheaded" = "dizziness",
                                           "Fistula Bleeding / Redness / Pain" = "fistula_bleeding")),
            br(),
            actionButton("run_cdss", "Evaluate PT Safety & Generate FITT", class = "btn-primary btn-block", icon = icon("stethoscope"))
          ),
          
          box(
            title = "2. CDSS Pre-Tx Guidance & FITT", width = 7, status = "info", solidHeader = TRUE,
            uiOutput("status_alert"),
            hr(),
            h4(icon("notes-medical"), "Action Plan & Emergency SOP:"),
            uiOutput("action_plan_output"),
            hr(),
            h4(icon("calendar-alt"), "Rescheduling & Follow-up Guidance:"),
            uiOutput("next_appointment_output"),
            hr(),
            h4(icon("shield-alt"), "Clinical Precautions:"),
            uiOutput("precautions_output"),
            hr(),
            h4(icon("clipboard-list"), "Tailored FITT Exercise Protocol:"),
            uiOutput("fitt_output")
          )
        )
      ),
      
      # ------------------------------------------------------------------------
      # TAB 2: POST-TEST Monitoring & Discharge
      # ------------------------------------------------------------------------
      tabItem(
        tabName = "post_eval",
        fluidRow(
          box(
            title = "1. Post-Exercise Vitals & Recovery Symptoms", width = 5, status = "warning", solidHeader = TRUE,
            h5(strong("Post-Exercise Vital Signs:")),
            fluidRow(
              column(6, numericInput("post_sbp", "Post SBP (mmHg):", 120, 60, 220)),
              column(6, numericInput("post_dbp", "Post DBP (mmHg):", 75, 40, 130))
            ),
            fluidRow(
              column(6, numericInput("post_hr", "Post HR (bpm):", 85, 40, 180)),
              column(6, numericInput("post_spo2", "Post SpO2 (%):", 97, 70, 100))
            ),
            hr(),
            sliderInput("post_rpe", "Post-Session Perceived Exertion (Borg RPE 6-20):", min = 6, max = 20, value = 12),
            hr(),
            h5(strong("Post-Exercise Adverse Symptoms:")),
            checkboxGroupInput("post_symptoms", NULL,
                               choices = c("Post-Exercise Dizziness / Lightheadedness" = "post_dizziness",
                                           "Severe Exhaustion / Heavy Fatigue" = "post_fatigue",
                                           "Muscle Cramps (Calf / Hamstring)" = "post_cramps",
                                           "Persistent Dyspnea / Shortness of Breath" = "post_sob")),
            br(),
            actionButton("run_post_cdss", "Evaluate Recovery & Generate Discharge Guidance", class = "btn-warning btn-block", icon = icon("file-medical-alt"))
          ),
          
          box(
            title = "2. Post-Session Discharge Guidance & Adaptive Prescription", width = 7, status = "success", solidHeader = TRUE,
            uiOutput("discharge_alert"),
            hr(),
            h4(icon("user-check"), "Post-Session Clinical & Discharge Instructions:"),
            uiOutput("discharge_instructions_output"),
            hr(),
            h4(icon("sliders-h"), "Adaptive Next-Session FITT Adjustment:"),
            uiOutput("next_adjustment_output"),
            hr(),
            h4(icon("pills"), "Medication Safety & Dose Adjustment Warnings (Based on eGFR):"),
            uiOutput("medication_card_output"),
            hr(),
            h4(icon("utensils"), "Dietary Restrictions & Daily Protein Prescriptions:"),
            uiOutput("diet_card_output")
          )
        )
      ),
      
      # ------------------------------------------------------------------------
      # TAB 3: Longitudinal Trends & Cohort
      # ------------------------------------------------------------------------
      tabItem(
        tabName = "longitudinal",
        fluidRow(
          box(
            title = "Patient Trend Filters & Clinical Context", width = 12, status = "primary", solidHeader = TRUE,
            fluidRow(
              column(6, dateRangeInput("date_range", "Date Range:", start = Sys.Date() - 30, end = Sys.Date())),
              column(6, selectInput("metric_focus", "Key Metric Focus:", choices = c("Renal Function Trend (eGFR & Creatinine)", "Blood Pressure (BP)", "Interdialytic Weight Gain (IDWG %)", "Potassium & Hemoglobin (K+ / Hb)")))
            )
          )
        ),
        
        fluidRow(
          box(
            title = "Pt Monitoring: Longitudinal Renal Trajectory Evaluation", width = 12, status = "danger", solidHeader = TRUE,
            uiOutput("longitudinal_renal_alert_ui")
          )
        ),
        
        fluidRow(
          box(
            title = "Pt Monitoring: eGFR & Serum Creatinine Follow-up Trend", width = 6, status = "primary", solidHeader = TRUE,
            plotlyOutput("egfr_scr_trend_plot", height = "350px")
          ),
          box(
            title = "Pre vs Post BP & HR Longitudinal Trend", width = 6, status = "info", solidHeader = TRUE,
            plotlyOutput("bp_trend_plot", height = "350px")
          )
        ),
        fluidRow(
          box(
            title = "RPE & Safety Outcome Track", width = 12, status = "warning", solidHeader = TRUE,
            plotlyOutput("rpe_trend_plot", height = "250px")
          )
        ),
        fluidRow(
          box(
            title = "Cohort Historic Session Database", width = 12, status = "success", solidHeader = TRUE,
            DT::dataTableOutput("history_table")
          )
        )
      ),
      
      # ------------------------------------------------------------------------
      # TAB 4: Dynamic Clinical Report & Print Export
      # ------------------------------------------------------------------------
      tabItem(
        tabName = "print_report",
        
        div(
          class = "no-print",
          style = "background: #f8f9fa; padding: 15px; border-radius: 5px; margin-bottom: 20px; border: 1px solid #e9ecef;",
          fluidRow(
            column(4, 
                   radioButtons("report_type", "Select Report Scope:",
                                choices = c("Today's Session Report" = "today",
                                            "Specific Historical Date" = "specific_date",
                                            "Longitudinal Trend Report" = "longitudinal"),
                                selected = "today")
            ),
            column(4,
                   conditionalPanel(
                     condition = "input.report_type == 'specific_date'",
                     dateInput("rpt_select_date", "Select Date:", value = Sys.Date() - 3, max = Sys.Date())
                   ),
                   conditionalPanel(
                     condition = "input.report_type == 'longitudinal'",
                     dateRangeInput("rpt_date_range", "Select Analysis Period:", start = Sys.Date() - 30, end = Sys.Date())
                   )
            ),
            column(4,
                   style = "text-align: right; margin-top: 15px;",
                   tags$button("🖨️ Print / Export PDF", onclick = "window.print()", class = "btn btn-primary btn-lg")
            )
          )
        ),
        
        fluidRow(
          box(
            width = 12, status = "primary", solidHeader = FALSE,
            div(
              style = "padding: 20px; background: white;",
              
              fluidRow(
                column(8, h2("Renal Physical Therapy Clinical Report", style = "margin-top:0; font-weight:bold; color:#2c3e50;")),
                column(4, div(style = "text-align:right;", h4("Renal Rehab CDSS"), p(style = "color:#6c757d; font-size:12px;", paste("Generated At:", format(Sys.time(), "%d %b %Y, %H:%M")))))
              ),
              hr(style = "border-top: 2px solid #2c3e50;"),
              
              uiOutput("dynamic_report_body")
            )
          )
        )
      )
    )
  )
)

# ------------------------------------------------------------------------------
# 2. SERVER Logic
# ------------------------------------------------------------------------------
server <- function(input, output, session) {
  
  # --- Dynamic UI Calculations ---
  output$egfr_display_ui <- renderUI({
    req(input$scr, input$age, input$gender)
    egfr_res <- calculate_egfr(input$scr, input$age, input$gender)
    
    if (!is.null(egfr_res)) {
      badge_color <- if (egfr_res$egfr < 15) "#dc3545" else if (egfr_res$egfr < 60) "#ffc107" else "#28a745"
      div(style = "margin-top: -5px; margin-bottom: 10px;",
          span(class = "value-badge", style = sprintf("background-color: %s;", badge_color),
               sprintf("eGFR: %.1f mL/min/1.73m² | %s", egfr_res$egfr, egfr_res$stage))
      )
    }
  })
  
  output$idwg_display_ui <- renderUI({
    req(input$dry_weight, input$pre_weight)
    if (input$dry_weight > 0) {
      idwg_pct <- ((input$pre_weight - input$dry_weight) / input$dry_weight) * 100
      badge_color <- if (idwg_pct > 5.0) "#dc3545" else if (idwg_pct > 3.0) "#ffc107" else "#28a745"
      div(style = "margin-top: -5px; margin-bottom: 10px;",
          span(class = "value-badge", style = sprintf("background-color: %s;", badge_color),
               sprintf("Calculated IDWG: %.2f%% (%+.1f kg)", idwg_pct, input$pre_weight - input$dry_weight))
      )
    }
  })
  
  output$bp_validation_ui <- renderUI({
    if (!is.null(input$sbp) && !is.null(input$dbp) && input$sbp <= input$dbp) {
      p(style = "color: #dc3545; font-size: 12px; font-weight: bold; margin-top: -5px;",
        "⚠️ Warning: SBP should be higher than DBP.")
    }
  })
  
  # --- Pre-Test Evaluation ---
  pre_res <- eventReactive(input$run_cdss, {
    dial_time_val <- if (input$dialysis_status == "intradialytic") input$dial_time else 0.0
    generate_cdss_recommendation(
      sbp = input$sbp, dbp = input$dbp, hr = input$hr, spo2 = input$spo2, temp = input$temp, glucose = input$glucose,
      scr = input$scr, age = input$age, gender = input$gender,
      dry_weight = input$dry_weight, pre_weight = input$pre_weight,
      k_level = input$k_level, hb_level = input$hb_level,
      dialysis_status = input$dialysis_status, dial_time = dial_time_val,
      fistula_arm = input$fistula_arm, symptoms = input$symptoms
    )
  })
  
  pre_res_safe <- reactive({ tryCatch(pre_res(), error = function(e) NULL) })
  
  idle_placeholder <- function(msg = "Enter patient data and click \"Evaluate PT Safety & Generate FITT\" to view results.") {
    div(class = "cdss-placeholder", icon("hourglass-half"), msg)
  }
  
  output$status_alert <- renderUI({
    res <- pre_res_safe()
    if (is.null(res)) return(idle_placeholder())
    if (res$status == "CONTRAINDICATED") {
      div(class = "clinical-alert clinical-alert-danger",
          p(strong("🚨 EXERCISE CONTRAINDICATED (SAFETY INTERCEPT TRIGGERED):"), style = "margin-bottom: 5px;"),
          tags$ul(lapply(res$alerts, function(alt) tags$li(style = "font-weight: bold;", alt)))
      )
    } else {
      div(class = "clinical-alert clinical-alert-success",
          "✅ SAFE FOR PT TREATMENT: Patient passed pre-treatment safety filters.")
    }
  })
  
  output$action_plan_output <- renderUI({
    res <- pre_res_safe()
    if (is.null(res)) return(idle_placeholder("Action plan will appear here after evaluation."))
    if (length(res$action_plan_groups) > 0) {
      tagList(
        lapply(names(res$action_plan_groups), function(group_name) {
          steps <- res$action_plan_groups[[group_name]]
          div(style = "background-color: #fff3cd; border-left: 4px solid #ffc107; padding: 8px 12px; margin-bottom: 10px; border-radius: 4px;",
              h5(strong(group_name), style = "color: #856404; margin-top: 0; margin-bottom: 5px;"),
              tags$ol(lapply(steps, function(step) tags$li(style = "color: #856404;", step)))
          )
        })
      )
    } else {
      p("No emergency actions required.", style = "color: #666;")
    }
  })
  
  output$next_appointment_output <- renderUI({
    res <- pre_res_safe()
    if (is.null(res)) return(idle_placeholder("Follow-up guidance will appear here after evaluation."))
    div(class = "clinical-alert clinical-alert-info", res$next_appointment)
  })
  
  output$precautions_output <- renderUI({
    res <- pre_res_safe()
    if (is.null(res)) return(idle_placeholder("Precautions will appear here after evaluation."))
    if (length(res$precautions) > 0) {
      tags$ul(lapply(res$precautions, function(p) tags$li(p)))
    } else {
      p("No additional specific precautions needed.", style = "color: #666;")
    }
  })
  
  # --- FITT Protocol UI Visual Redesign ---
  output$fitt_output <- renderUI({
    res <- pre_res_safe()
    if (is.null(res)) return(idle_placeholder("The tailored FITT protocol will appear here after evaluation."))
    
    if (res$status != "SAFE") {
      return(
        div(
          style = "background-color: #f8d7da; color: #721c24; padding: 12px; border-radius: 6px; font-weight: bold; border-left: 4px solid #dc3545;",
          "🔒 [LOCKED] Exercise prescription withheld due to absolute contraindications."
        )
      )
    }
    
    f <- res$fitt
    
    items <- list(
      list(title = "Frequency",  val = f$Frequency,  icon = "calendar-alt",  color = "#17a2b8"),
      list(title = "Intensity",  val = f$Intensity,  icon = "tachometer-alt", color = "#ffc107"),
      list(title = "Time",       val = f$Time,       icon = "clock",          color = "#28a745"),
      list(title = "Type",       val = f$Type,       icon = "running",        color = "#007bff"),
      list(title = "Monitoring", val = f$Monitoring, icon = "heartbeat",      color = "#dc3545")
    )
    
    tagList(
      div(
        style = "background: #ffffff; border: 1px solid #e0e0e0; border-radius: 8px; padding: 12px; box-shadow: 0 2px 4px rgba(0,0,0,0.05);",
        lapply(items, function(item) {
          div(
            style = "display: flex; align-items: flex-start; margin-bottom: 10px; padding-bottom: 8px; border-bottom: 1px solid #f0f0f0;",
            div(
              style = sprintf("min-width: 120px; font-weight: bold; color: %s; display: flex; align-items: center;", item$color),
              icon(item$icon, style = "margin-right: 8px;"),
              item$title
            ),
            div(
              style = "flex-grow: 1; color: #333333; font-size: 14px; line-height: 1.4; word-break: break-word;",
              item$val
            )
          )
        })
      )
    )
  })
  
  # --- Post-Test Evaluation ---
  post_res <- eventReactive(input$run_post_cdss, {
    generate_post_exercise_guidance(
      post_sbp = input$post_sbp, post_dbp = input$post_dbp, post_hr = input$post_hr,
      post_spo2 = input$post_spo2, post_rpe = input$post_rpe, pre_sbp = input$sbp,
      post_symptoms = input$post_symptoms,
      scr = input$scr, age = input$age, gender = input$gender,
      k_level = input$k_level, phos_level = input$phos_level,
      dialysis_status = input$dialysis_status,
      weight_kg = input$dry_weight, pre_weight_kg = input$pre_weight, dry_weight_kg = input$dry_weight
    )
  })
  
  post_res_safe <- reactive({ tryCatch(post_res(), error = function(e) NULL) })
  
  post_idle_placeholder <- function(msg = "Enter post-exercise data and click \"Evaluate Recovery & Generate Discharge Guidance\" to view results.") {
    div(class = "cdss-placeholder", icon("hourglass-half"), msg)
  }
  
  output$discharge_alert <- renderUI({
    post_res_val <- post_res_safe()
    if (is.null(post_res_val)) return(post_idle_placeholder())
    if (post_res_val$discharge_status == "OBSERVATION_REQUIRED") {
      div(class = "clinical-alert clinical-alert-warning",
          "⚠️ HOLD DISCHARGE: Patient requires post-exercise monitoring & observation.")
    } else if (post_res_val$discharge_status == "MEDICAL_REVIEW_REQUIRED") {
      div(class = "clinical-alert clinical-alert-danger",
          "🚨 URGENT MEDICAL REVIEW REQUIRED: Do NOT discharge patient until physician clears severe symptom.")
    } else {
      div(class = "clinical-alert clinical-alert-success",
          "✅ CLEARED FOR DISCHARGE: Patient meets all post-exercise safety criteria.")
    }
  })
  
  output$discharge_instructions_output <- renderUI({
    post_res_val <- post_res_safe()
    if (is.null(post_res_val)) return(post_idle_placeholder("Discharge instructions will appear here after evaluation."))
    tags$ul(lapply(post_res_val$instructions, function(inst) tags$li(style = "font-size: 14px; margin-bottom: 5px;", inst)))
  })
  
  output$next_adjustment_output <- renderUI({
    post_res_val <- post_res_safe()
    if (is.null(post_res_val)) return(post_idle_placeholder("Next-session FITT adjustment will appear here after evaluation."))
    div(style = "color: #155724; background-color: #e2e3e5; padding: 12px; border-radius: 5px; font-weight: bold; font-size: 14px;",
        post_res_val$next_adjustment)
  })
  
  # --- Medication Safety Card ---
  output$medication_card_output <- renderUI({
    post_res_val <- post_res_safe()
    if (is.null(post_res_val)) return(post_idle_placeholder("Medication safety warnings will appear here after evaluation."))
    med_eval <- post_res_val$medication_alerts
    items <- med_eval$structured_items
    
    if (is.null(items) || length(items) == 0) {
      return(div(style = "color: #666;", "No medication alerts available."))
    }
    
    div(
      class = "cdss-grid-2",
      lapply(items, function(item) {
        div(
          class = "med-card", style = sprintf("border-left-color: %s;", item$color_code),
          div(
            class = "med-card-head",
            div(
              span(class = "med-card-title", item$drug_class),
              span(class = "med-card-examples", sprintf("(%s)", item$examples))
            ),
            span(class = "value-badge med-card-badge", style = sprintf("background-color: %s;", item$color_code), item$severity)
          ),
          div(class = "med-card-action", strong("Action: "), item$action),
          tags$details(tags$summary(), p(class = "rationale-text", item$rationale))
        )
      })
    )
  })
  
  # --- Diet & Electrolyte Card (Syntax Cleaned) ---
  output$diet_card_output <- renderUI({
    post_res_val <- post_res_safe()
    if (is.null(post_res_val)) return(post_idle_placeholder("Dietary & protein guidance will appear here after evaluation."))
    diet <- post_res_val$dietary_guidelines
    
    div(
      class = "cdss-grid-2",
      
      # 1. Protein Matrix Card
      div(
        class = "diet-card",
        h5(strong("🥩 Protein Intake Target"), style = "color: #2b6cb0;"),
        p(class = "diet-action-line", style = "color: #1a365d;",
          sprintf("%s %s", diet$protein_matrix$target,
                  if (!is.null(diet$protein_matrix$daily_total_grams)) sprintf("(~%.1f g/day)", diet$protein_matrix$daily_total_grams) else "")),
        p(style = "font-size: 11.5px; color: #4a5568;", diet$protein_matrix$clinical_rationale)
      ),
      
      # 2. Potassium Matrix Card
      div(
        class = "diet-card",
        h5(strong("🍌 Potassium (K+) Management"), style = "color: #d69e2e;"),
        p(class = "diet-action-line", style = "color: #744210;", diet$potassium_matrix$action),
        div(
          class = "diet-food-cols",
          if (length(diet$potassium_matrix$foods_to_avoid) > 0) {
            div(
              span(class = "col-label", style = "color: #c53030;", "Restrict"),
              tags$ul(lapply(diet$potassium_matrix$foods_to_avoid, function(f) tags$li(f)))
            )
          },
          if (length(diet$potassium_matrix$recommended_substitutes) > 0) {
            div(
              span(class = "col-label", style = "color: #2f855a;", "Substitute"),
              tags$ul(lapply(diet$potassium_matrix$recommended_substitutes, function(s) tags$li(s)))
            )
          }
        ),
        if (nzchar(diet$potassium_matrix$cooking_technique)) {
          tags$details(tags$summary(), p(class = "note-text", diet$potassium_matrix$cooking_technique))
        }
      ),
      
      # 3. Phosphorus Matrix Card
      div(
        class = "diet-card",
        h5(strong("🦴 Phosphorus Management"), style = "color: #c53030;"),
        p(class = "diet-action-line", style = "color: #742a2a;", diet$phosphorus_matrix$action),
        if (length(diet$phosphorus_matrix$foods_to_avoid) > 0) {
          div(
            span(class = "col-label", style = "color: #c53030;", "High-Inorganic Phosphorus Foods to Avoid"),
            tags$ul(lapply(diet$phosphorus_matrix$foods_to_avoid, function(f) tags$li(style = "font-size: 11.5px; color: #2d3748;", f)))
          )
        },
        if (nzchar(diet$phosphorus_matrix$clinical_tip)) {
          tags$details(tags$summary(), p(class = "note-text", diet$phosphorus_matrix$clinical_tip))
        }
      ),
      
      # 4. Sodium & Fluid Management Card
      div(
        class = "diet-card",
        h5(strong("💧 Sodium & Fluid Regulation"), style = "color: #3182ce;"),
        p(style = "font-size: 12px; color: #2d3748;", strong("Sodium Limit: "), diet$sodium_fluid_matrix$sodium_limit),
        p(style = "font-size: 12px; color: #2d3748;", strong("Fluid Limit: "), diet$sodium_fluid_matrix$fluid_limit),
        if (nzchar(diet$sodium_fluid_matrix$clinical_warning)) {
          p(style = "font-size: 12px; font-weight: bold; color: #c53030;", diet$sodium_fluid_matrix$clinical_warning)
        },
        tags$ul(style = "padding-left: 16px; margin: 4px 0 0 0;",
                lapply(diet$sodium_fluid_matrix$practical_tips, function(tip) tags$li(style = "font-size: 11.5px; color: #4a5568;", tip)))
      )
    )
  })
  
  # --- Historic Dataset ---
  mock_history_data <- reactive({
    dates <- seq.Date(from = Sys.Date() - 27, to = Sys.Date(), by = "3 days")
    scr_vals <- c(2.8, 2.9, 3.0, 3.1, 3.2, 3.1, 3.3, 3.4, 3.5, 3.5)
    egfr_vals <- sapply(scr_vals, function(x) {
      res <- calculate_egfr(x, input$age, input$gender)
      if (!is.null(res)) res$egfr else NA
    })
    
    data.frame(
      Date = dates,
      sCr = scr_vals,
      eGFR = egfr_vals,
      Pre_SBP = c(135, 142, 138, 150, 130, 128, 134, 132, 136, 130),
      Post_SBP = c(125, 130, 126, 138, 122, 120, 128, 124, 128, 122),
      Pre_HR = c(72, 75, 78, 82, 70, 68, 74, 73, 76, 71),
      RPE = c(11, 12, 13, 15, 12, 11, 12, 13, 12, 11),
      Status = c("SAFE", "SAFE", "SAFE", "CONTRAINDICATED", "SAFE", "SAFE", "SAFE", "SAFE", "SAFE", "SAFE")
    )
  })
  
  # --- Trajectory Alert ---
  output$longitudinal_renal_alert_ui <- renderUI({
    df <- mock_history_data()
    eval_res <- evaluate_longitudinal_trends(df)
    
    alert_class <- if (eval_res$trend_status == "HIGH_RISK_AKI") {
      "clinical-alert-danger"
    } else if (eval_res$trend_status == "MODERATE_DECLINE") {
      "clinical-alert-warning"
    } else {
      "clinical-alert-success"
    }
    
    div(class = paste("clinical-alert", alert_class), eval_res$message)
  })
  
  # --- Plots ---
  output$egfr_scr_trend_plot <- renderPlotly({
    df <- mock_history_data()
    plot_ly(df, x = ~Date) %>%
      add_trace(y = ~eGFR, name = 'eGFR (mL/min/1.73m²)', type = 'scatter', mode = 'lines+markers',
                line = list(color = '#00a65a', width = 3), marker = list(size = 8)) %>%
      add_trace(y = ~sCr, name = 'Serum Creatinine (mg/dL)', type = 'scatter', mode = 'lines+markers', yaxis = "y2",
                line = list(color = '#dd4b39', width = 2, dash = 'dash'), marker = list(size = 6)) %>%
      layout(
        yaxis = list(title = 'eGFR (mL/min/1.73m²)', titlefont = list(color = '#00a65a'), tickfont = list(color = '#00a65a')),
        yaxis2 = list(title = 'sCr (mg/dL)', titlefont = list(color = '#dd4b39'), tickfont = list(color = '#dd4b39'), overlaying = "y", side = "right"),
        xaxis = list(title = 'Follow-up Date'),
        hovermode = "x unified",
        legend = list(orientation = "h", x = 0.1, y = 1.15)
      )
  })
  
  output$bp_trend_plot <- renderPlotly({
    df <- mock_history_data()
    plot_ly(df, x = ~Date) %>%
      add_trace(y = ~Pre_SBP, name = 'Pre-Exercise SBP', type = 'scatter', mode = 'lines+markers', line = list(color = '#0073b7')) %>%
      add_trace(y = ~Post_SBP, name = 'Post-Exercise SBP', type = 'scatter', mode = 'lines+markers', line = list(color = '#3c8dbc')) %>%
      layout(yaxis = list(title = 'mmHg'), xaxis = list(title = 'Session Date'), hovermode = "x unified",
             legend = list(orientation = "h", x = 0.1, y = 1.15))
  })
  
  output$rpe_trend_plot <- renderPlotly({
    df <- mock_history_data()
    plot_ly(df, x = ~Date, y = ~RPE, type = 'bar', marker = list(color = '#f39c12')) %>%
      layout(yaxis = list(title = 'Borg RPE (6-20)', range = c(6, 20)), xaxis = list(title = 'Session Date'))
  })
  
  output$history_table <- DT::renderDataTable({
    DT::datatable(mock_history_data(), options = list(pageLength = 5, dom = 'tip'))
  })
  
  # --- Dynamic Report UI ---
  output$dynamic_report_body <- renderUI({
    
    patient_header <- div(
      style = "background-color: #f8f9fa; padding: 12px; border-radius: 5px; margin-bottom: 20px; border: 1px solid #dee2e6;",
      fluidRow(
        column(3, p(strong("Patient Name: "), input$pt_name)),
        column(3, p(strong("MRN: "), input$pt_mrn)),
        column(3, p(strong("Age / Gender: "), paste(input$age, "/", toupper(input$gender)))),
        column(3, p(strong("IC / Passport: "), input$pt_ic))
      )
    )
    
    if (input$report_type %in% c("today", "specific_date")) {
      selected_date_str <- if (input$report_type == "today") as.character(Sys.Date()) else as.character(input$rpt_select_date)
      
      tagList(
        patient_header,
        h4(strong(paste("Session Report Date: ", selected_date_str))),
        br(),
        h4(strong("1. Renal Function & Dialysis Context")),
        uiOutput("rpt_egfr_summary"),
        fluidRow(
          column(4, p(strong("Dialysis Status: "), input$dialysis_status)),
          column(4, p(strong("Access Site: "), input$fistula_arm)),
          column(4, p(strong("Dry Weight: "), paste(input$dry_weight, "kg")))
        ),
        br(),
        h4(strong("2. Pre-Test Safety Evaluation Results")),
        uiOutput("rpt_pre_status"),
        br(),
        tableOutput("rpt_vitals_table"),
        br(),
        h4(strong("3. Prescribed FITT Protocol")),
        uiOutput("rpt_fitt_display"),
        br(),
        h4(strong("4. Post-Test Recovery, Medication & Dietary Guidance")),
        uiOutput("rpt_post_status"),
        br(),
        hr(),
        fluidRow(
          column(6, p(strong("Attending Physician / Nephrologist Signature: "), "______________________")),
          column(6, p(strong("Physiotherapist (PT) Signature: "), "______________________"))
        )
      )
      
    } else if (input$report_type == "longitudinal") {
      tagList(
        patient_header,
        h4(strong(sprintf("Longitudinal Rehabilitation Progress & Pt Monitoring Report (%s to %s)", 
                          input$rpt_date_range[1], input$rpt_date_range[2]))),
        br(),
        h4(strong("1. Renal Trajectory & Function Trend (Pt Monitoring)")),
        plotlyOutput("egfr_scr_trend_plot", height = "300px"),
        br(),
        h4(strong("2. Hemodynamic Response & Exertion Trends")),
        fluidRow(
          column(8, plotlyOutput("bp_trend_plot", height = "300px")),
          column(4, plotlyOutput("rpe_trend_plot", height = "300px"))
        ),
        br(),
        h4(strong("3. Treatment History Audit Trail")),
        tableOutput("rpt_history_summary_table"),
        br(),
        hr(),
        fluidRow(
          column(6, p(strong("Lead Physical Therapist: "), "______________________")),
          column(6, p(strong("Date of Review: "), as.character(Sys.Date())))
        )
      )
    }
  })
  
  output$rpt_egfr_summary <- renderUI({
    egfr_res <- calculate_egfr(input$scr, input$age, input$gender)
    if (!is.null(egfr_res)) {
      p(strong("Renal Stage: "), sprintf("%.1f mL/min/1.73m² (%s)", egfr_res$egfr, egfr_res$stage))
    }
  })
  
  output$rpt_vitals_table <- renderTable({
    data.frame(
      Timing = c("Pre-Exercise", "Post-Exercise"),
      SBP_DBP = c(paste0(input$sbp, "/", input$dbp, " mmHg"), paste0(input$post_sbp, "/", input$post_dbp, " mmHg")),
      Heart_Rate = c(paste(input$hr, "bpm"), paste(input$post_hr, "bpm")),
      SpO2 = c(paste0(input$spo2, "%"), paste0(input$post_spo2, "%")),
      Blood_Glucose = c(paste(input$glucose, "mmol/L"), "N/A")
    )
  }, striped = TRUE, bordered = TRUE)
  
  output$rpt_pre_status <- renderUI({
    res <- tryCatch(pre_res(), error = function(e) NULL)
    if (is.null(res)) {
      p("Please run calculation in the Pre-Test tab first.")
    } else if (res$status == "SAFE") {
      span(style = "color: green; font-weight: bold; font-size: 16px;", "✅ SAFE: Passed Pre-Test safety screening")
    } else {
      span(style = "color: red; font-weight: bold; font-size: 16px;", "🚨 CONTRAINDICATED: Prescription execution intercepted due to red flags")
    }
  })
  
  output$rpt_fitt_display <- renderUI({
    res <- tryCatch(pre_res(), error = function(e) NULL)
    if (is.null(res) || res$status != "SAFE") return(p("No active exercise prescription available."))
    f <- res$fitt
    tags$ul(
      tags$li(strong("Frequency: "), f$Frequency),
      tags$li(strong("Intensity: "), f$Intensity),
      tags$li(strong("Time: "), f$Time),
      tags$li(strong("Type: "), f$Type),
      tags$li(strong("Monitoring: "), f$Monitoring)
    )
  })
  
  output$rpt_post_status <- renderUI({
    post_res_val <- tryCatch(post_res(), error = function(e) NULL)
    if (is.null(post_res_val)) return(p("Please run evaluation in the Post-Test tab first."))
    
    diet <- post_res_val$dietary_guidelines
    med_eval <- post_res_val$medication_alerts
    
    tagList(
      p(strong("Discharge Status: "), post_res_val$discharge_status),
      p(strong("Next Session Strategy: "), post_res_val$next_adjustment),
      hr(),
      p(strong("Medication Safety Guidance:")),
      tags$ul(
        lapply(med_eval$structured_items, function(m) {
          tags$li(sprintf("[%s] %s (%s) - %s", m$severity, m$drug_class, m$examples, m$action))
        })
      ),
      p(strong("Dietary & Protein Guidance:")),
      p(paste("Protein Target:", diet$protein_matrix$target)),
      p(paste("Potassium Action:", diet$potassium_matrix$action)),
      p(paste("Phosphorus Action:", diet$phosphorus_matrix$action)),
      p(paste("Fluid Limit:", diet$sodium_fluid_matrix$fluid_limit))
    )
  })
  
  output$rpt_history_summary_table <- renderTable({
    mock_history_data()
  }, striped = TRUE, bordered = TRUE)
}

# ------------------------------------------------------------------------------
# 3. App Launch Trigger
# ------------------------------------------------------------------------------
shinyApp(ui = ui, server = server)