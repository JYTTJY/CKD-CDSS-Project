library(dplyr)

set.seed(42)
n_patients <- 50

patient_data <- data.frame(
  Patient_ID = sprintf("HD-%03d", 1:n_patients),
  Age = sample(45:80, n_patients, replace = TRUE),
  Gender = sample(c("Male", "Female"), n_patients, replace = TRUE),
  eGFR = round(runif(n_patients, 5, 14), 1), # CKD Stage 5D < 15
  SBP = sample(100:190, n_patients, replace = TRUE),
  DBP = sample(60:115, n_patients, replace = TRUE),
  Dialysis_Hour = round(runif(n_patients, 0.5, 3.5), 1), # 透析进行的时间(小时)
  SixMWT = sample(150:450, n_patients, replace = TRUE),  # 6分钟步行距离(米)
  RPE_baseline = sample(10:15, n_patients, replace = TRUE)
)

write.csv(patient_data, "ckd_patients.csv", row.names = FALSE)
