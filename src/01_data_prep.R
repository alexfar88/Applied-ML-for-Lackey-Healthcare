rm(list=ls())

# ==============================================================================
# LACKEY CLINIC CHURN PREDICTION
# ==============================================================================

library(tidyverse)
library(lubridate)
library(fastDummies)
library(ISLR2)
library(class)
library(zipcodeR)
library(geosphere)
library(pROC)
library(caret)
library(MASS)

# ==============================================================================
# 1. LOAD DATA
# ==============================================================================

people <- read_csv("data/people.csv", show_col_types = FALSE)
encounters <- read_csv("data/encounters.csv", show_col_types = FALSE)
enrollment <- read_csv("data/enrollment.csv", show_col_types = FALSE)
noshow <- read_csv("data/noshow.csv", show_col_types = FALSE)
observations <- read_csv("data/observations.csv", show_col_types = FALSE)
diagnosis <- read_csv("data/diagnosis.csv", show_col_types = FALSE)
orders <- read_csv("data/orders.csv", show_col_types = FALSE)
residence <- read_csv("data/residence.csv", show_col_types = FALSE)

# ==============================================================================
# 1B. SET REFERENCE DATE FOR REPRODUCIBILITY
# ==============================================================================
# CRITICAL: Use fixed date (when data was received), NOT Sys.Date()
# This ensures everyone gets the same results

reference_date <- as.Date("2025-09-30")  # End of September 2025

cat(sprintf("📅 Reference date: %s (data receipt/extraction date)\n", reference_date))
cat("   All 'days since' calculations will use this date.\n\n")

# ==============================================================================
# 2. DEFINE YOUR CHURN VARIABLE
# ==============================================================================
# CRITICAL: Choose ONE approach and document your rationale
# ==============================================================================
# Goal: CHURN = (No Completed Encounter in 365 days) OR (Disenrolled Status)

# 1. Identify the encounter date column dynamically
enc_date_col <- names(encounters)[grepl("date|datetime|time", names(encounters), ignore.case = TRUE)][1]

# --- PART A: ENCOUNTER-BASED CHURN (365-day Gap) ---
churn_encounter_flag <- encounters %>%
  # CRITICAL: Convert to Date format
  mutate(enc_date = as.Date(.data[[enc_date_col]])) %>%
  
  # CRITICAL: Filter for only 'Completed' encounters (Active care)
  filter(enc_status == "Completed") %>%
  # Filter out data that occurred AFTER our analysis date (no look-ahead)
  filter(enc_date <= reference_date) %>%
  
  group_by(mrn_pseudo) %>%
  summarize(
    last_completed_encounter = max(enc_date, na.rm = TRUE),
    days_since_completed_enc = as.numeric(reference_date - last_completed_encounter),
    .groups = "drop"
  ) %>%
  mutate(
    # FLAG 1: Churned if last completed encounter was more than 365 days ago
    churn_enc_flag = if_else(days_since_completed_enc > 365, 1, 0, missing = 1)
  )

# FINAL FIX: Select the columns outside the pipe using Base R indexing
churn_encounter_flag <- churn_encounter_flag[ , c("mrn_pseudo", "churn_enc_flag")]


# --- PART B: ENROLLMENT-BASED CHURN (Disenrolled Status) ---
churn_disenrolled_flag <- enrollment %>%
  # Filter for records that explicitly mark the patient as 'Disenrolled'
  filter(enr_status == "Disenrolled") %>%
  # Only count disenrolled status if the event occurred BEFORE or AT the reference date
  filter(as.Date(enr_start) <= reference_date) %>% 
  
  group_by(mrn_pseudo) %>%
  summarize(
    # FLAG 2: Churned if the patient has *ever* been formally disenrolled
    churn_disenr_flag = 1,
    .groups = "drop"
  )

# Remove the simple churn_encounter object from the old code if it still exists
if(exists("churn_encounter")) rm(churn_encounter)

# ==============================================================================
# 3. CREATE PATIENT-LEVEL SUMMARIES
# ==============================================================================

# Enrollment summary (use if() instead of if_else() to avoid warnings)
if ("enr_start" %in% names(enrollment) && "enr_exp" %in% names(enrollment)) {
  enrollment_summary <- enrollment %>%
    filter(enr_start <= enr_exp | is.na(enr_start) | is.na(enr_exp)) %>%
    group_by(mrn_pseudo) %>%
    summarize(
      total_enrollments = n(),
      first_enrollment = if(all(is.na(enr_start))) as.Date(NA_character_) else min(enr_start, na.rm = TRUE),
      last_enrollment_end = if(all(is.na(enr_exp))) as.Date(NA_character_) else max(enr_exp, na.rm = TRUE),
      total_enrollment_days = sum(as.numeric(enr_exp - enr_start), na.rm = TRUE),
      currently_enrolled = if(all(is.na(enr_exp))) FALSE else max(enr_exp, na.rm = TRUE) >= reference_date,
      .groups = "drop"
    )
}

# Encounters summary (handles different date column names)
enc_date_col <- names(encounters)[grepl("date|datetime|time", names(encounters), ignore.case = TRUE)]

if (length(enc_date_col) > 0 && inherits(encounters[[enc_date_col[1]]], c("Date", "POSIXct", "POSIXt"))) {
  enc_date_col <- enc_date_col[1]
  
  # Check for telehealth column (handle different naming conventions)
  if ("telehealth" %in% names(encounters)) {
    # telehealth = "Yes"/"No"
    encounters_summary <- encounters %>%
      group_by(mrn_pseudo) %>%
      summarize(
        total_encounters = n(),
        telehealth_count = sum(telehealth == "Yes", na.rm = TRUE),
        telehealth_pct = mean(telehealth == "Yes", na.rm = TRUE) * 100,
        first_visit = if(all(is.na(.data[[enc_date_col]]))) as.Date(NA_character_) else min(.data[[enc_date_col]], na.rm = TRUE),
        last_visit = if(all(is.na(.data[[enc_date_col]]))) as.Date(NA_character_) else max(.data[[enc_date_col]], na.rm = TRUE),
        days_since_last_visit = if(all(is.na(.data[[enc_date_col]]))) NA_real_ else as.numeric(reference_date - max(.data[[enc_date_col]], na.rm = TRUE)),
        # NEW: visits in past 365 days
        visits_in_past_year = sum(
          !is.na(.data[[enc_date_col]]) &
            .data[[enc_date_col]] >= (reference_date - 365) &
            .data[[enc_date_col]] <= reference_date
        ),
        #gap_between_last_2_visits = as.numeric(enc_date - lag(enc_date)),
        .groups = "drop"
      )
  } else if ("enc_is_teleh" %in% names(encounters)) {
    # enc_is_teleh = 0/1
    encounters_summary <- encounters %>%
      group_by(mrn_pseudo) %>%
      summarize(
        total_encounters = n(),
        telehealth_count = sum(enc_is_teleh == 1, na.rm = TRUE),
        telehealth_pct = mean(enc_is_teleh == 1, na.rm = TRUE) * 100,
        first_visit = if(all(is.na(.data[[enc_date_col]]))) as.Date(NA_character_) else min(.data[[enc_date_col]], na.rm = TRUE),
        last_visit = if(all(is.na(.data[[enc_date_col]]))) as.Date(NA_character_) else max(.data[[enc_date_col]], na.rm = TRUE),
        days_since_last_visit = if(all(is.na(.data[[enc_date_col]]))) NA_real_ else as.numeric(reference_date - max(.data[[enc_date_col]], na.rm = TRUE)),
        # NEW: visits in past 365 days
        visits_in_past_year = sum(
          !is.na(.data[[enc_date_col]]) &
            .data[[enc_date_col]] >= (reference_date - 365) &
            .data[[enc_date_col]] <= reference_date
        ),
        #gap_between_last_2_visits = as.numeric(enc_date - lag(enc_date)),
        .groups = "drop"
      )
  } else {
    # No telehealth column
    encounters_summary <- encounters %>%
      group_by(mrn_pseudo) %>%
      summarize(
        total_encounters = n(),
        first_visit = if(all(is.na(.data[[enc_date_col]]))) as.Date(NA_character_) else min(.data[[enc_date_col]], na.rm = TRUE),
        last_visit = if(all(is.na(.data[[enc_date_col]]))) as.Date(NA_character_) else max(.data[[enc_date_col]], na.rm = TRUE),
        days_since_last_visit = if(all(is.na(.data[[enc_date_col]]))) NA_real_ else as.numeric(reference_date - max(.data[[enc_date_col]], na.rm = TRUE)),
        # NEW: visits in past 365 days
        visits_in_past_year = sum(
          !is.na(.data[[enc_date_col]]) &
            .data[[enc_date_col]] >= (reference_date - 365) &
            .data[[enc_date_col]] <= reference_date
        ),
        #gap_between_last_2_visits = as.numeric(enc_date - lag(enc_date)),
        .groups = "drop"
      )
  }
}

# No-shows summary
noshow_summary <- noshow %>%
  group_by(mrn_pseudo) %>%
  summarize(
    total_noshows = n(),
    .groups = "drop"
  )

# ==============================================================================
# Add no-show proportion
# ==============================================================================

noshow <- noshow %>%
  mutate(ns_reason = str_to_title(str_to_lower(ns_reason)))

noshow <- noshow %>%
  filter(ns_reason != "Dental")

ggplot(data = noshow, aes(x = ns_reason, y = after_stat(prop), group = 1)) +
  geom_bar(fill = "lightblue") +
  labs(title = "No Shows per Appointment Type", x = "Appointment Type", y = "Proportion") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# ==============================================================================
# 4. BUILD MASTER DATASET (ONE ROW PER PATIENT)
# ==============================================================================

master <- people %>%
  # Join summaries
  left_join(enrollment_summary, by = "mrn_pseudo") %>%
  left_join(encounters_summary, by = "mrn_pseudo") %>%
  left_join(noshow_summary, by = "mrn_pseudo") %>%
  # Create binary flags for sparse data
  mutate(
    has_observations = mrn_pseudo %in% observations$mrn_pseudo,
    has_diagnosis = mrn_pseudo %in% diagnosis$mrn_pseudo,
    has_noshows = !is.na(total_noshows)
  )

# Save raw version for missing data analysis
master_raw <- master

# Add chosen churn definition
master <- master %>% 
  left_join(churn_encounter_flag, by = "mrn_pseudo") %>%
  left_join(churn_disenrolled_flag, by = "mrn_pseudo")

# ==============================================================================
# 5. MISSING DATA ANALYSIS & HANDLING
# ==============================================================================

cat("\n=== MISSING DATA SUMMARY (RAW DATA) ===\n")

# Calculate % missing for key variables (use RAW data)
missing_summary <- master_raw %>%
  summarize(across(everything(), ~sum(is.na(.)))) %>%
  pivot_longer(everything(), names_to = "variable", values_to = "n_missing") %>%
  mutate(pct_missing = (n_missing / nrow(master_raw)) * 100) %>%
  filter(pct_missing > 0) %>%
  arrange(desc(pct_missing))

cat(sprintf("Variables with missing data: %d\n", nrow(missing_summary)))
if (nrow(missing_summary) > 0) {
  cat(sprintf("Most missing: %s (%.1f%%)\n\n", 
              missing_summary$variable[1], 
              missing_summary$pct_missing[1]))
}

# STRATEGY: Domain-specific defaults
# For Lackey data, most NAs are structural (patient never had that event)

master <- master %>%
  mutate(
    # Counts: NA means zero occurrences
    total_encounters = replace_na(total_encounters, 0),
    total_noshows = replace_na(total_noshows, 0),
    total_enrollments = replace_na(total_enrollments, 0),
    
    # Rates/percentages: NA means 0% (or could keep as NA)
    telehealth_pct = replace_na(telehealth_pct, 0),
    
    # Time features: NA means "never happened" - use sentinel value
    days_since_last_visit = replace_na(days_since_last_visit, 9999),
    
    # Create indicators for structural missingness (informative!)
    never_visited = is.na(first_visit),
    never_enrolled = is.na(first_enrollment)
  )


cat("✓ Applied domain-specific missing data handling\n")
cat("  • Counts → 0 (no events recorded)\n")
cat("  • Rates → 0% (no activity)\n")
cat("  • Time features → 9999 days (sentinel value for 'never')\n")
cat("  • Created 'never_*' indicators\n\n")

# Validation: Check remaining NAs
remaining_na <- master %>%
  summarize(across(everything(), ~sum(is.na(.)))) %>%
  pivot_longer(everything()) %>%
  filter(value > 0)

if (nrow(remaining_na) > 0) {
  cat(sprintf("Note: %d variables still have NAs (dates, demographics)\n", 
              nrow(remaining_na)))
  cat("     This is expected - will derive features from dates in Part 6\n")
  cat("     Time features (days_since_last_visit) already handled with sentinel value\n\n")
} else {
  cat("✓ No remaining NAs in dataset\n\n")
}