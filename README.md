# 🦠 COVID-19 Epidemiological Surveillance & SLA Analytics Dashboard

An end-to-end data analytics project processing CDC COVID-19 case surveillance data using **R** for data wrangling and **Power BI** for dynamic, dark-themed epidemiological reporting.

<img width="575" height="323" alt="dashboard_preview" src="https://github.com/user-attachments/assets/0fa6bca2-b489-4428-baad-a0328cd6a0f8" />


---

## 📌 Project Overview
This project transforms large-scale raw CDC surveillance dataset into an actionable public health dashboard. It measures key epidemiological metrics, tracks case reporting delays (SLA adherence), and analyzes demographic and clinical severity distributions.

### **Key Metrics Tracked:**
* **Total Cases:** Overall tracked COVID-19 cases (500K cohort).
* **Reporting Delay SLA:** Tracking cases processed within a 3-day Service Level Agreement (SLA).
* **Case Fatality / Severity Rate (CFR%):** Tracking clinical outcomes (Urgent, Major, Normal).
* **Epidemiological Curve:** Monthly case distribution stratified by reporting delay SLA.

---

## 🛠️ Tech Stack & Methods

* **Data Wrangling & Cleaning:** R (`dplyr`, `tidyverse`, `lubridate`)
* **Visualization & BI:** Power BI Desktop (DAX, Custom Dark Theme UI/UX)
* **Data Source:** CDC COVID-19 Case Surveillance Public Use Data

---

## 🔄 Data Pipeline & Processing (R Script)

Raw data clean-up and feature engineering were executed using R (`dplyr`, `tidyverse`, `lubridate`):

```R
cleaned_data <- data %>%
  select(
    cdc_report_dt, pos_spec_dt, onset_dt, current_status, sex, 
    age_group, Race.and.ethnicity..combined., hosp_yn, icu_yn, death_yn, medcond_yn
  ) %>% 
  mutate(
    cdc_report_dt = as.Date(parse_date_time(cdc_report_dt, orders = c("ymd", "Ymd", "Y/m/d"))),
    pos_spec_dt   = as.Date(parse_date_time(pos_spec_dt, orders = c("ymd", "Ymd", "Y/m/d"))),
    
    year  = year(cdc_report_dt),
    month = month(cdc_report_dt, label = TRUE, abbr = TRUE),
    
    reporting_delay_days = ifelse(!is.na(cdc_report_dt) & !is.na(pos_spec_dt), 
                                  as.numeric(cdc_report_dt - pos_spec_dt), NA_real_),
    within_sla = case_when(
      is.na(reporting_delay_days) ~ "Unknown",
      reporting_delay_days <= 3   ~ "within SLA",
      TRUE                        ~ "Out of SLA"
    ),
    severity = case_when(
      death_yn == "Yes" | icu_yn == "Yes" ~ "Urgent",
      hosp_yn == "Yes"                    ~ "Major",
      hosp_yn == "No"                     ~ "Normal",
      TRUE                                ~ "Low/Unknown"
    ),
    sex       = if_else(sex %in% c("Male", "Female"), sex, "Unknown"),
    age_group = if_else(is.na(age_group) | age_group == "Missing", "Unknown", age_group),
    ethnicity = if_else(is.na(Race.and.ethnicity..combined.), "Unknown", Race.and.ethnicity..combined.)
  ) %>%
  filter(!is.na(cdc_report_dt))

write_csv(cleaned_data, "covid_surveillance_cleaned_for_powerbi.csv")


---

## 📊 Power BI DAX Measures

```DAX
Total Cases = COUNTROWS('covid_surveillance_cleaned_for_powerbi')

Within SLA Cases = CALCULATE([Total Cases], 'covid_surveillance_cleaned_for_powerbi'[within_sla] = "within SLA")

SLA Rate % = DIVIDE([Within SLA Cases], [Total Cases], 0)

CFR % = DIVIDE(CALCULATE([Total Cases], 'covid_surveillance_cleaned_for_powerbi'[severity] = "Urgent"), [Total Cases], 0)

