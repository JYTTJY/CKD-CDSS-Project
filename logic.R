# ==============================================================================
# Clinical Decision Support System (CDSS) Logic Module (logic.R)
# Medical-Grade Renal Decision Engine with Structured Severity Matrix,
# Dual-Layer Medication Safety Guidance, & Dietary/Fluid Management Matrix
# ==============================================================================

# --- 1. eGFR Calculation (CKD-EPI 2021 Equation - Race Free) ---
calculate_egfr <- function(scr, age, gender) {
  if (is.null(scr) || is.null(age) || is.null(gender) || 
      is.na(scr) || is.na(age) || is.na(gender) || 
      scr <= 0 || age <= 0) {
    return(NULL)
  }
  
  gender_clean <- tolower(trimws(as.character(gender)))
  kappa <- if (gender_clean == "female") 0.7 else 0.9
  alpha <- if (gender_clean == "female") -0.241 else -0.302
  gender_factor <- if (gender_clean == "female") 1.012 else 1.0
  
  scr_kappa <- scr / kappa
  min_val <- min(scr_kappa, 1)
  max_val <- max(scr_kappa, 1)
  
  egfr <- 142 * (min_val^alpha) * (max_val^-1.200) * (0.9938^age) * gender_factor
  
  # Stage Determination
  stage <- if (egfr >= 90) "Stage 1 (Normal / High GFR)"
  else if (egfr >= 60) "Stage 2 (Mildly Decreased)"
  else if (egfr >= 45) "Stage 3a (Mild-Moderate Decrease)"
  else if (egfr >= 30) "Stage 3b (Moderate-Severe Decrease)"
  else if (egfr >= 15) "Stage 4 (Severely Decreased)"
  else "Stage 5 (Kidney Failure)"
  
  return(list(egfr = round(egfr, 1), stage = stage))
}

# --- 2. Medication Alert Engine (Structured Severity Matrix & Dual-Layer Guidance) ---
evaluate_medication_alerts <- function(egfr) {
  if (is.null(egfr) || is.na(egfr)) {
    return(list(
      summary_alerts = c("eGFR data missing: Unable to calculate medication safety alerts."),
      structured_items = list()
    ))
  }
  
  alerts_list <- list()
  summary_vec <- c()
  
  # Helper function for card construction
  add_alert <- function(drug_class, examples, severity, color_code, action, rationale) {
    item <- list(
      drug_class = drug_class,
      examples = examples,
      severity = severity, # CONTRAINDICATED, DOSE_ADJUSTMENT, MONITOR, INFO, SAFE
      color_code = color_code, # #ef4444 (Red), #f97316 (Orange), #eab308 (Yellow), #3b82f6 (Blue), #22c55e (Green)
      action = action,
      rationale = rationale,
      formatted_text = sprintf("[%s] %s (%s) - Action: %s | Rationale: %s", 
                               severity, drug_class, examples, action, rationale)
    )
    return(item)
  }
  
  # 2.1 NSAIDs Evaluation
  if (egfr < 30) {
    item <- add_alert(
      drug_class = "NSAIDs",
      examples = "Ibuprofen, Naproxen, Diclofenac, Celecoxib",
      severity = "CONTRAINDICATED",
      color_code = "#ef4444",
      action = "Discontinue immediately. Strictly avoid prescribing all non-steroidal anti-inflammatory drugs.",
      rationale = "Inhibits renal prostaglandin synthesis, impairing afferent arteriolar vasodilation; induces severe risk of Acute Kidney Injury (AKI) and hyperkalemia."
    )
    alerts_list[[length(alerts_list) + 1]] <- item
    summary_vec <- c(summary_vec, item$formatted_text)
  } else if (egfr < 60) {
    item <- add_alert(
      drug_class = "NSAIDs",
      examples = "Ibuprofen, Naproxen, Diclofenac",
      severity = "DOSE_ADJUSTMENT",
      color_code = "#f97316",
      action = "Avoid chronic or prolonged use. Consider Paracetamol (Acetaminophen) for analgesia. Monitor renal function closely if short-term therapy is essential.",
      rationale = "Prolonged exposure increases cumulative nephrotoxicity and impairs renal hemodynamics, accelerating CKD progression."
    )
    alerts_list[[length(alerts_list) + 1]] <- item
    summary_vec <- c(summary_vec, item$formatted_text)
  }
  
  # 2.2 Metformin Evaluation
  if (egfr < 30) {
    item <- add_alert(
      drug_class = "Biguanides (Metformin)",
      examples = "Metformin",
      severity = "CONTRAINDICATED",
      color_code = "#ef4444",
      action = "Discontinue immediately. Switch to non-renally cleared antidiabetic agents (e.g., Linagliptin or Insulin).",
      rationale = "Impaired renal drug clearance causes systemic drug accumulation, creating a high risk of life-threatening Lactic Acidosis."
    )
    alerts_list[[length(alerts_list) + 1]] <- item
    summary_vec <- c(summary_vec, item$formatted_text)
  } else if (egfr < 45) {
    item <- add_alert(
      drug_class = "Biguanides (Metformin)",
      examples = "Metformin",
      severity = "DOSE_ADJUSTMENT",
      color_code = "#f97316",
      action = "Reduce maximum daily dosage to 1000 mg/day. Do not initiate new Metformin therapy at this eGFR level. Monitor eGFR every 3 months.",
      rationale = "Decreased renal clearance increases plasma concentration; strict dose caps are mandatory to prevent toxic accumulation."
    )
    alerts_list[[length(alerts_list) + 1]] <- item
    summary_vec <- c(summary_vec, item$formatted_text)
  }
  
  # 2.3 RAAS Blockers (ACEi / ARBs) Evaluation
  if (egfr < 30) {
    item <- add_alert(
      drug_class = "RAAS Blockers (ACEi / ARBs)",
      examples = "Enalapril, Lisinopril, Losartan, Valsartan",
      severity = "MONITOR",
      color_code = "#eab308",
      action = "Monitor serum Potassium (K+) and Serum Creatinine closely (weekly). Temporarily suspend therapy during acute volume depletion, infection, or severe hypotension.",
      rationale = "Efferent arteriolar vasodilation reduces intraglomerular pressure, which may exacerbate acute hemodynamic GFR decline during systemic hypoperfusion."
    )
    alerts_list[[length(alerts_list) + 1]] <- item
    summary_vec <- c(summary_vec, item$formatted_text)
  }
  
  # 2.4 SGLT2 Inhibitors Evaluation
  if (egfr < 20) {
    item <- add_alert(
      drug_class = "SGLT2 Inhibitors",
      examples = "Dapagliflozin, Empagliflozin, Canagliflozin",
      severity = "INFO",
      color_code = "#3b82f6",
      action = "Do not initiate for glycemic control due to diminished efficacy. If already tolerated, continuation may be maintained for cardiorenal protection unless dialysis is initiated.",
      rationale = "Glycemic efficacy declines in advanced CKD, but intraglomerular pressure reduction and reno-protective benefits persist down to dialysis initiation."
    )
    alerts_list[[length(alerts_list) + 1]] <- item
    summary_vec <- c(summary_vec, item$formatted_text)
  }
  
  # Default Safe Status
  if (length(alerts_list) == 0) {
    item <- add_alert(
      drug_class = "General Renal Drug Screening",
      examples = "NSAIDs, Metformin, RAASb, SGLT2i",
      severity = "SAFE",
      color_code = "#22c55e",
      action = "Maintain standard clinical dosing with routine renal function surveillance.",
      rationale = "Current eGFR level does not trigger specific renal dose reductions or contraindication thresholds for key evaluated drug classes."
    )
    alerts_list[[1]] <- item
    summary_vec <- c(item$formatted_text)
  }
  
  return(list(
    summary_alerts = summary_vec,
    structured_items = alerts_list
  ))
}

# --- 3. Dietary, Electrolyte, & Fluid Management Engine ---
evaluate_dietary_guidelines <- function(egfr, k_level = NULL, phos_level = NULL, 
                                        dialysis_status = "non_dialysis", 
                                        weight_kg = 60, pre_weight_kg = NULL, dry_weight_kg = NULL) {
  
  # 3.1 Protein Intake Determination Matrix
  protein_target <- ""
  protein_option <- ""
  calculated_daily_protein_g <- NULL
  
  if (dialysis_status == "intradialytic" || dialysis_status == "hemodialysis" || dialysis_status == "peritoneal") {
    protein_target <- "1.2 - 1.3 g/kg/day"
    protein_option <- "High-protein intake required to compensate for amino acid and peptide losses during dialysis procedure."
    if (!is.null(weight_kg) && weight_kg > 0) {
      calculated_daily_protein_g <- round(weight_kg * 1.2, 1)
    }
  } else {
    if (!is.null(egfr) && !is.na(egfr)) {
      if (egfr < 30) {
        protein_target <- "0.55 - 0.60 g/kg/day (Low Protein Diet - LPD) OR 0.28 - 0.43 g/kg/day (Very Low Protein Diet) + Ketoacid Analogues"
        protein_option <- "Low-protein regimen reduces uremic toxin generation, mitigates intraglomerular hyperfiltration, and delays dialysis initiation."
        if (!is.null(weight_kg) && weight_kg > 0) {
          calculated_daily_protein_g <- round(weight_kg * 0.60, 1)
        }
      } else if (egfr < 60) {
        protein_target <- "0.60 - 0.80 g/kg/day (Moderate Protein Restriction)"
        protein_option <- "Moderate protein restriction to decrease renal metabolic workload."
        if (!is.null(weight_kg) && weight_kg > 0) {
          calculated_daily_protein_g <- round(weight_kg * 0.75, 1)
        }
      } else {
        protein_target <- "0.80 g/kg/day (Standard Recommended Dietary Allowance - RDA)"
        protein_option <- "Standard balanced protein intake."
        if (!is.null(weight_kg) && weight_kg > 0) {
          calculated_daily_protein_g <- round(weight_kg * 0.80, 1)
        }
      }
    } else {
      protein_target <- "0.60 - 0.80 g/kg/day (Default CKD Recommendation)"
      protein_option <- "Standard CKD protein restriction."
    }
  }
  
  # 3.2 Potassium Management Matrix
  k_status <- "NORMAL"
  k_action <- ""
  k_foods_to_avoid <- c()
  k_recommended_substitutes <- c()
  k_cooking_tip <- ""
  
  k_val <- if (!is.null(k_level) && !is.na(k_level)) k_level else 4.2
  
  if (k_val >= 5.5) {
    k_status <- "STRICT_RESTRICTION"
    k_action <- "STRICT POTASSIUM RESTRICTION (< 2000 mg/day or < 50 mmol/day)"
    k_foods_to_avoid <- c("Bananas, Oranges, Avocados, Kiwi", "Potatoes, Tomatoes, Spinach, Bamboo shoots", "Nuts, Dried fruits, Dark chocolate", "Low-sodium salt substitutes (KCl based)", "Concentrated vegetable/meat soups")
    k_recommended_substitutes <- c("Apples, Peaches, Berries, Grapes", "Cabbage, Cucumbers, Zucchini, Cauliflower", "White rice, Refined pasta")
    k_cooking_tip <- "【Double-Boiling / Leaching Method】: Peel and chop vegetables into small cubes, soak in warm water for 2 hours, boil in fresh water for 5 minutes, and discard the water before final cooking. This technique leaches out 50% - 70% of potassium content."
  } else if (k_val >= 5.0 || (!is.null(egfr) && egfr < 30)) {
    k_status <- "MODERATE_RESTRICTION"
    k_action <- "MODERATE POTASSIUM RESTRICTION (2000 - 3000 mg/day)"
    k_foods_to_avoid <- c("High-potassium fruits (Bananas, Dried fruit)", "Fruit juices", "Potassium-enriched salt substitutes")
    k_recommended_substitutes <- c("Low-potassium fruits and vegetables in portion-controlled servings")
    k_cooking_tip <- "Chop and soak vegetables before cooking; avoid drinking vegetable broth or canned syrup."
  } else {
    k_status <- "NORMAL"
    k_action <- "Normal serum potassium: Maintain balanced intake with periodic monitoring."
    k_foods_to_avoid <- c("Avoid excessive use of potassium supplements or salt substitutes")
    k_recommended_substitutes <- c("Standard fresh fruits and vegetables")
    k_cooking_tip <- "Standard healthy culinary preparation."
  }
  
  # 3.3 Phosphorus Management Matrix
  phos_status <- "NORMAL"
  phos_action <- ""
  phos_foods_to_avoid <- c()
  phos_recommended_substitutes <- c()
  phos_clinical_tip <- ""
  
  phos_val <- if (!is.null(phos_level) && !is.na(phos_level)) phos_level else 1.2
  
  if (phos_val > 4.5 || (!is.null(egfr) && egfr < 30)) {
    phos_status <- "STRICT_RESTRICTION"
    phos_action <- "INORGANIC PHOSPHATE RESTRICTION (< 800 - 1000 mg/day)"
    phos_foods_to_avoid <- c("Processed meats (Bacon, Sausages, Hot dogs)", "Dark colas and canned beverages", "Processed cheese spreads", "Organ meats and game", "Foods with inorganic phosphate additives (E-numbers: E338-E343, E450-E452)")
    phos_recommended_substitutes <- c("Fresh unfortified poultry and lean meats", "Egg whites", "Natural unfortified dairy substitutes")
    phos_clinical_tip <- "Inorganic phosphate additives in processed foods have 90%-100% intestinal absorption rates compared to 40%-60% for natural organic phosphates. Boiling raw meat in water and discarding the broth removes up to 50% of phosphorus."
  } else {
    phos_status <- "NORMAL"
    phos_action <- "Normal serum phosphate parameters."
    phos_foods_to_avoid <- c("Excessive intake of heavily processed convenience foods")
    phos_recommended_substitutes <- c("Whole natural foods")
    phos_clinical_tip <- "Maintain natural food diet and re-assess during routine follow-ups."
  }
  
  # 3.4 Sodium & Fluid Management (IDWG & Dialysis Status)
  idwg_pct <- 0
  if (!is.null(pre_weight_kg) && !is.null(dry_weight_kg) && dry_weight_kg > 0) {
    idwg_pct <- round(((pre_weight_kg - dry_weight_kg) / dry_weight_kg) * 100, 1)
  }
  
  sodium_restriction <- "< 2000 mg Sodium/day (equivalent to < 5.0 g Table Salt/day)"
  fluid_restriction <- ""
  fluid_clinical_warning <- ""
  
  if (dialysis_status != "non_dialysis") {
    if (idwg_pct > 5.0) {
      fluid_clinical_warning <- sprintf("CRITICAL FLUID OVERLOAD DETECTED (IDWG %.1f%% > 5.0%%): Severe risk of acute left heart failure, pulmonary congestion, and malignant hypertension.", idwg_pct)
      fluid_restriction <- "Strict Fluid Restriction: Limit total liquid intake to < 800 - 1000 mL/day until dry weight target is re-established."
    } else if (idwg_pct > 3.0) {
      fluid_clinical_warning <- sprintf("MODERATE FLUID GAIN (IDWG %.1f%%): Monitor interdialytic weight gain closely.", idwg_pct)
      fluid_restriction <- "Fluid Restriction: Total daily fluids = Previous 24-hr Urine Output + 500 mL."
    } else {
      fluid_clinical_warning <- sprintf("OPTIMAL FLUID CONTROL (IDWG %.1f%% <= 3.0%%).", idwg_pct)
      fluid_restriction <- "Standard Dialysis Fluid Guideline: Daily fluids = Previous 24-hr Urine Output + 500 - 800 mL."
    }
  } else {
    if (!is.null(egfr) && egfr < 30) {
      fluid_restriction <- "Fluid Guideline: If oliguric or edematous, cap intake at 24-hr Urine Output + 500 mL/day; otherwise drink to thirst."
      fluid_clinical_warning <- "Monitor daily morning body weight and lower extremity edema."
    } else {
      fluid_restriction <- "Ad libitum fluid intake according to physiological thirst mechanism."
      fluid_clinical_warning <- "Maintain normal hydration status."
    }
  }
  
  return(list(
    protein_matrix = list(
      target = protein_target,
      clinical_rationale = protein_option,
      daily_total_grams = calculated_daily_protein_g
    ),
    potassium_matrix = list(
      status = k_status,
      action = k_action,
      foods_to_avoid = k_foods_to_avoid,
      recommended_substitutes = k_recommended_substitutes,
      cooking_technique = k_cooking_tip
    ),
    phosphorus_matrix = list(
      status = phos_status,
      action = phos_action,
      foods_to_avoid = phos_foods_to_avoid,
      recommended_substitutes = phos_recommended_substitutes,
      clinical_tip = phos_clinical_tip
    ),
    sodium_fluid_matrix = list(
      sodium_limit = sodium_restriction,
      fluid_limit = fluid_restriction,
      idwg_percentage = idwg_pct,
      clinical_warning = fluid_clinical_warning,
      practical_tips = c(
        "Use a standard 2g salt spoon to measure daily culinary salt.",
        "Avoid hidden sodium sources such as soy sauce, monosodium glutamate (MSG), canned soups, and pickles.",
        "Manage severe thirst using ice chips or rinsing the mouth with cold water without swallowing."
      )
    )
  ))
}

# --- 4. Pre-Test Evaluation & FITT Generation Engine ---
generate_cdss_recommendation <- function(sbp, dbp, hr, spo2, temp, glucose,
                                         scr, age, gender, dry_weight, pre_weight,
                                         k_level, hb_level, dialysis_status, dial_time,
                                         fistula_arm, symptoms) {
  
  alerts <- c()
  action_plan_groups <- list()
  precautions <- c()
  
  # 1. Vital Signs Safety Screening
  if (!is.null(sbp) && sbp >= 180) {
    alerts <- c(alerts, sprintf("Severe Hypertension (SBP %d mmHg >= 180)", sbp))
    action_plan_groups[["Hypertensive Crisis Protocol"]] <- c(
      "Immediately withhold physical therapy session.",
      "Notify attending nephrologist / medical officer on duty.",
      "Re-assess blood pressure after 15 minutes of quiet rest."
    )
  }
  if (!is.null(sbp) && sbp < 90) {
    alerts <- c(alerts, sprintf("Severe Hypotension (SBP %d mmHg < 90)", sbp))
    action_plan_groups[["Hypotension Protocol"]] <- c(
      "Place patient in Trendelenburg / supine position if tolerable.",
      "Assess for symptoms of cerebral hypoperfusion (dizziness, nausea).",
      "Consult dialysis team for saline bolus evaluation if intradialytic."
    )
  }
  if (!is.null(dbp) && dbp >= 110) {
    alerts <- c(alerts, sprintf("Severe Diastolic Hypertension (DBP %d mmHg >= 110)", dbp))
  }
  if (!is.null(hr) && (hr > 110 || hr < 50)) {
    alerts <- c(alerts, sprintf("Abnormal Resting Heart Rate (%d bpm)", hr))
  }
  if (!is.null(spo2) && spo2 < 90) {
    alerts <- c(alerts, sprintf("Hypoxemia (SpO2 %d%% < 90%%)", spo2))
  }
  if (!is.null(temp) && temp >= 38.0) {
    alerts <- c(alerts, sprintf("Fever / Active Infection (Temp %.1f °C)", temp))
  }
  if (!is.null(glucose) && glucose < 3.9) {
    alerts <- c(alerts, sprintf("Hypoglycemia (Blood Glucose %.1f mmol/L < 3.9)", glucose))
    action_plan_groups[["Hypoglycemia Protocol"]] <- c(
      "Administer 15-20g fast-acting oral carbohydrates (e.g., juice, glucose tabs).",
      "Re-check blood glucose in 15 minutes.",
      "Delay exercise session until glucose recovers above 5.0 mmol/L."
    )
  }
  
  # 2. Lab Thresholds Screening
  if (!is.null(k_level) && k_level >= 6.0) {
    alerts <- c(alerts, sprintf("Severe Hyperkalemia (K+ %.1f mEq/L >= 6.0)", k_level))
    action_plan_groups[["Hyperkalemia Safety Intercept"]] <- c(
      "Withhold exercise due to risk of fatal cardiac dysrhythmias.",
      "Obtain urgent 12-lead ECG and alert nephrology team."
    )
  }
  if (!is.null(hb_level) && hb_level < 8.0) {
    alerts <- c(alerts, sprintf("Severe Anemia (Hb %.1f g/dL < 8.0)", hb_level))
  }
  
  # 3. Fluid Overload (IDWG %) Check
  if (!is.null(dry_weight) && !is.null(pre_weight) && dry_weight > 0) {
    idwg_pct <- ((pre_weight - dry_weight) / dry_weight) * 100
    if (idwg_pct > 5.0) {
      alerts <- c(alerts, sprintf("Excessive Fluid Gain (IDWG %.1f%% > 5.0%%)", idwg_pct))
      precautions <- c(precautions, "High fluid gain detected: monitor closely for dyspnea or acute pulmonary congestion during therapy.")
    }
  }
  
  # 4. Intradialytic Timing Screening
  if (dialysis_status == "intradialytic") {
    if (!is.null(dial_time) && dial_time > 2.0) {
      alerts <- c(alerts, sprintf("Late Intradialytic Window (Elapsed %.1f hrs > 2.0 hrs)", dial_time))
      precautions <- c(precautions, "Exercising in late dialysis hours increases risk of intradialytic hypotension.")
    } else {
      precautions <- c(precautions, "Optimal Intradialytic Exercise Window (First 1-2 hrs). Monitor fluid removal rate.")
    }
  }
  
  # 5. Access Site Precautions
  if (!is.null(fistula_arm) && fistula_arm != "None") {
    precautions <- c(precautions, sprintf("AV Access in %s: Avoid BP cuff placement, weight bearing, or heavy resistance on this extremity.", fistula_arm))
  }
  
  # 6. Red Flag Symptoms Screening
  if (!is.null(symptoms)) {
    if ("chest_pain" %in% symptoms) {
      alerts <- c(alerts, "Red Flag Symptom: Acute Chest Pain / Angina")
      action_plan_groups[["Cardiovascular Emergency"]] <- c(
        "Cease all therapy immediately.",
        "Initiate emergency response (Code Blue / Emergency Medical Call).",
        "Monitor vital signs and prepare AED/ECG."
      )
    }
    if ("severe_sob" %in% symptoms) {
      alerts <- c(alerts, "Red Flag Symptom: Severe Dyspnea / Orthopnea")
    }
    if ("dizziness" %in% symptoms) {
      alerts <- c(alerts, "Red Flag Symptom: Dizziness / Lightheadedness")
    }
    if ("fistula_bleeding" %in% symptoms) {
      alerts <- c(alerts, "Red Flag Symptom: AV Access Bleeding / Pain")
    }
  }
  
  # Decision Logic Execution
  is_safe <- length(alerts) == 0
  
  # Tailored FITT Prescription Generation
  fitt <- list(
    Frequency = ifelse(dialysis_status == "intradialytic", "3x per week (during dialysis sessions)", "3-5 days per week"),
    Intensity = "Moderate Intensity (Borg RPE 11-13 / 40-60% VO2peak)",
    Time = "20-30 minutes per session (10 min warm-up/cool-down included)",
    Type = ifelse(dialysis_status == "intradialytic", 
                  "Intradialytic Recumbent Cycling / Ankle Pumps / Seated Resistance (Elastic Bands)", 
                  "Stationary Cycling / Aerobic Walking / Progressive Lower Extremity Resistance"),
    Monitoring = "Check BP/HR every 10 mins, continuously monitor SpO2 and subjective RPE."
  )
  
  next_appt <- if (is_safe) {
    "Proceed with session today. Next follow-up: Next routine dialysis treatment."
  } else {
    "Session postponed. Re-evaluate clinical stability prior to next scheduled appointment."
  }
  
  # Calculate eGFR for Medication & Dietary Evaluation
  egfr_res <- calculate_egfr(scr, age, gender)
  egfr_val <- if (!is.null(egfr_res)) egfr_res$egfr else NULL
  
  med_eval <- evaluate_medication_alerts(egfr_val)
  diet_eval <- evaluate_dietary_guidelines(
    egfr = egfr_val, 
    k_level = k_level, 
    phos_level = NULL, 
    dialysis_status = dialysis_status,
    weight_kg = dry_weight,
    pre_weight_kg = pre_weight,
    dry_weight_kg = dry_weight
  )
  
  return(list(
    status = if (is_safe) "SAFE" else "CONTRAINDICATED",
    alerts = alerts,
    action_plan_groups = action_plan_groups,
    precautions = precautions,
    fitt = fitt,
    next_appointment = next_appt,
    medication_alerts = med_eval,
    dietary_guidelines = diet_eval
  ))
}

# --- 5. Post-Exercise Guidance Engine ---
generate_post_exercise_guidance <- function(post_sbp, post_dbp, post_hr, post_spo2, post_rpe, pre_sbp, post_symptoms,
                                            scr = NULL, age = NULL, gender = NULL, k_level = NULL, phos_level = NULL, 
                                            dialysis_status = "non_dialysis", weight_kg = 60, pre_weight_kg = NULL, dry_weight_kg = NULL) {
  
  discharge_status <- "CLEARED"
  instructions <- c("Patient tolerating post-exercise recovery well.", "Vital signs within expected baseline limits.")
  next_adjustment <- "Maintain current FITT protocol for next session."
  
  # Hypotensive Drop Check
  if (!is.null(post_sbp) && !is.null(pre_sbp)) {
    if ((pre_sbp - post_sbp) >= 20) {
      discharge_status <- "OBSERVATION_REQUIRED"
      instructions <- c("Significant SBP drop (>20 mmHg post-exercise) detected.", "Keep patient seated/supine with legs elevated for 15 minutes before discharge.")
      next_adjustment <- "Consider reducing exercise intensity or duration by 10-20% for the next session."
    }
  }
  
  # High Exertion
  if (!is.null(post_rpe) && post_rpe >= 15) {
    next_adjustment <- "Patient reported high exertional effort (RPE >= 15). Step down intensity for next treatment."
  }
  
  # Symptom Check
  if (!is.null(post_symptoms) && length(post_symptoms) > 0) {
    discharge_status <- "MEDICAL_REVIEW_REQUIRED"
    instructions <- c("Post-exercise adverse symptoms reported. Do not discharge until cleared by clinical supervisor.")
  }
  
  # Calculate eGFR for Medication & Diet Evaluation
  egfr_val <- NULL
  if (!is.null(scr) && !is.null(age) && !is.null(gender)) {
    egfr_res <- calculate_egfr(scr, age, gender)
    if (!is.null(egfr_res)) {
      egfr_val <- egfr_res$egfr
    }
  }
  
  # Generate Medication Safety & Diet Cards
  medication_alerts <- evaluate_medication_alerts(egfr_val)
  dietary_guidelines <- evaluate_dietary_guidelines(
    egfr = egfr_val, 
    k_level = k_level, 
    phos_level = phos_level, 
    dialysis_status = dialysis_status,
    weight_kg = weight_kg,
    pre_weight_kg = pre_weight_kg,
    dry_weight_kg = dry_weight_kg
  )
  
  return(list(
    discharge_status = discharge_status,
    instructions = instructions,
    next_adjustment = next_adjustment,
    medication_alerts = medication_alerts,
    dietary_guidelines = dietary_guidelines
  ))
}

# --- 6. Patient Monitoring & Longitudinal Renal Trend Engine ---
evaluate_longitudinal_trends <- function(df) {
  if (is.null(df) || nrow(df) < 2) {
    return(list(
      trend_status = "INSUFFICIENT_DATA",
      message = "At least 2 historical follow-up sessions are required for renal trend & AKI analysis.",
      egfr_change = 0,
      scr_change = 0
    ))
  }
  
  df_sorted <- df[order(df$Date), ]
  first_rec <- head(df_sorted, 1)
  last_rec <- tail(df_sorted, 1)
  
  egfr_change <- last_rec$eGFR - first_rec$eGFR
  scr_change <- last_rec$sCr - first_rec$sCr
  
  # AKI / Rapid Decline Criteria: Serum Creatinine increase >= 0.3 mg/dL OR eGFR decline > 25%
  is_aki_risk <- (scr_change >= 0.3) || (first_rec$eGFR > 0 && (egfr_change / first_rec$eGFR) <= -0.25)
  
  if (is_aki_risk) {
    status <- "HIGH_RISK_AKI"
    msg <- sprintf("🚨 HIGH RISK / AKI ALERT: Serum Creatinine increased by %+.2f mg/dL (eGFR change: %+.1f mL/min/1.73m²). Rapid renal function deterioration detected!", scr_change, egfr_change)
  } else if (egfr_change < -5) {
    status <- "MODERATE_DECLINE"
    msg <- sprintf("⚠️ MODERATE DECLINE: eGFR decreased by %.1f mL/min/1.73m² over the observation period. Close monitoring recommended.", abs(egfr_change))
  } else {
    status <- "STABLE"
    msg <- sprintf("✅ RENAL FUNCTION STABLE: eGFR trend is maintained within baseline trajectory (%+.1f mL/min/1.73m²).", egfr_change)
  }
  
  return(list(
    trend_status = status,
    message = msg,
    egfr_change = egfr_change,
    scr_change = scr_change
  ))
}