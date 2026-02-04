# ==============================================================================
# 6. FEATURE ENGINEERING
# ==============================================================================

master <- master %>%
  mutate(
    # Transformations (for skewed distributions)
    log_encounters = log(total_encounters + 1),  # +1 handles zeros
    
    # Ratios & Rates (handle division by zero)
    noshow_rate = if_else(total_encounters > 0,
                          total_noshows / (total_noshows + total_encounters),
                          NA_real_),
    
    # Visit frequency (encounters per year of enrollment)
    visit_frequency = if_else(total_enrollment_days > 0,
                              (total_encounters / total_enrollment_days) * 365,
                              NA_real_),
    
    # Temporal features
    days_since_exp = if_else(!is.na(last_enrollment_end),
                             as.numeric(reference_date - last_enrollment_end),
                             NA_real_),
    
    tenure_days = if_else(!is.na(first_enrollment) & !is.na(last_enrollment_end),
                          as.numeric(last_enrollment_end - first_enrollment),
                          NA_real_)
  )


## drop all the family members (never_enrolled = TRUE)
master_new <- master %>% filter(never_enrolled == FALSE)


# ==============================================================================
# DIAGNOSIS CODE GROUPING EXAMPLES
# ==============================================================================
# all the diagnoses with labels are considered chronic

diagnosis <- diagnosis %>% mutate(chronic =
                                    case_when(dia_dxgroup == "Other" ~ 0,
                                              TRUE ~ 1))

# join into master_new
diagnosis_summary <- diagnosis %>%
  group_by(mrn_pseudo) %>%
  summarize(
    has_chronic_diagnosis = max(chronic, na.rm = TRUE),
    .groups = 'drop'
  )

master_new <- master_new %>%
  left_join(diagnosis_summary, by = "mrn_pseudo") %>%
  mutate(
    has_chronic_diagnosis = replace_na(has_chronic_diagnosis, 0)
  )


## drop all the family members (never_enrolled = TRUE)
master_new <- master_new %>% filter(never_enrolled == FALSE)


# ==============================================================================
# 6B. FINAL CHURN VARIABLE COMBINATION (Logical OR)
# (This REPLACES the old complex case_when logic)
# ==============================================================================

master_new <- master_new %>%
  # Fill NA churn flags with 0. 
  # This correctly handles patients who might not have an encounter/enrollment record, 
  # but were not deemed 'churned' by the initial flag creation (e.g., they exist in 'people' but nowhere else).
  mutate(
    churn_enc_flag = replace_na(churn_enc_flag, 0),
    churn_disenr_flag = replace_na(churn_disenr_flag, 0)
  ) %>%
  
  # FINAL CHURN DEFINITION: CHURN = (Encounter Gap > 365d) OR (Disenrolled Status)
  mutate(
    # The pmax() function performs a logical OR: Churned (1) if EITHER flag is 1.
    churned = pmax(churn_enc_flag, churn_disenr_flag),
    # Convert to a clear factor for modeling
    churned = factor(churned, levels = c(0, 1))#, labels = c("Retained", "Churned"))
  )

master_new <- master_new[ , !names(master_new) %in% c("churn_enc_flag", "churn_disenr_flag")] # Drop the temporary flags

table(master_new$churned)

# ==============================================================================
# 7. DATA QUALITY CHECKS
# ==============================================================================

cat("\n=== FINAL DATA QUALITY CHECKS ===\n")

# Check for missing outcome on the master_new dataset
if ("churned" %in% names(master_new)) { 
  cat(sprintf("Missing churn labels: %d (%.1f%%)\n",
              sum(is.na(master_new$churned)), 
              mean(is.na(master_new$churned)) * 100))
  
  # Check class balance
  master_new %>% 
    filter(!is.na(churned)) %>%
    count(churned) %>%
    mutate(percentage = n / sum(n) * 100) %>%
    print()
} else {
  cat("⚠️ No churn variable found - remember to add it!\n")
}

# Complete cases comparison
cat(sprintf("\nComplete cases (CLEANED): %s / %s (%.1f%%)\n",
            format(sum(complete.cases(master_new)), big.mark = ","),
            format(nrow(master_new), big.mark = ","),
            (sum(complete.cases(master_new)) / nrow(master_new)) * 100))

cat(sprintf("Complete cases (CLEANED): %s / %s (%.1f%%)\n",
            format(sum(complete.cases(master)), big.mark = ","),
            format(nrow(master), big.mark = ","),
            (sum(complete.cases(master)) / nrow(master)) * 100))


cat("\n✨ Script complete! Master dataset ready for modeling.\n")
cat(sprintf("📅 Remember: All time calculations use reference_date = %s\n", reference_date))


age_levels <- c("Under 18", "18-19", "20-29", "30-39",
                "40-49", "50-59", "60-64", "65+")
master_new <- master_new %>%
  mutate(
    age_range = factor(
      age_range,
      levels = age_levels,
      ordered = FALSE
    )
  )


master_new <- master_new %>%
  mutate(
    Employment = str_to_title(Employment),
    Employment = str_replace_all(Employment, "\\]", ")"),
    Employment = if_else(Employment %in% c("Null", "Unknown"), NA_character_, Employment),
    Employment = factor(
      Employment,
      levels = c(
        "Employed", "Self Employed", "Seasonal",
        "Unemployed (Actively Looking For Work)",
        "Not In Workforce (Choosing Not To Work)", "Retired"
      )
    )
  )

master_new <- master_new %>% mutate(Education = factor(
  Education))

master_new <- master_new %>%
  mutate(
    Housing = str_to_title(Housing),  # make capitalization consistent
    Housing = if_else(
      Housing %in% c("Null", "Unknown", ""), 
      NA_character_, 
      Housing
    ),
    Housing = factor(
      Housing,
      levels = c(
        "Own",
        "Rent",
        "Lives With Others",
        "Mobile Home",
        "Homeless"
      )
    )
  )

master_new <- master_new %>%
  mutate(
    Language = str_to_title(Language),                  # normalize capitalization
    Language = str_trim(Language),                      # remove stray spaces
    Language = str_replace_all(Language, "Other:", ""), # drop "Other:" prefix
    Language = str_replace_all(Language, "Other", ""),  # remove plain "Other"
    Language = str_replace_all(Language, ":", ""),      # extra cleanup
    Language = if_else(
      Language %in% c("Null", "Unknown", "", NA_character_), 
      NA_character_, 
      Language
    ),
    
    # Collapse into 4 buckets
    Language_grouped = case_when(
      str_detect(Language, "English") ~ "English",
      str_detect(Language, "Spanish") ~ "Spanish",
      is.na(Language) ~ "Unknown/Missing",
      TRUE ~ "Other"
    ),
    
    # Convert to factor with clear levels
    Language_grouped = factor(
      Language_grouped,
      levels = c("English", "Spanish", "Other", "Unknown/Missing")
    )
  )


master_new <- master_new %>%
  mutate(
    has_chronic_diagnosis = factor(
      has_chronic_diagnosis
    )
  )


############################################
########## FEATURE TYPES ############
############################################

#DAYS SINCE LAST VISIT
#  The longer it's been since someone's last visit, the more likely they are to further disengage and potentially churn
days_since_summary <- master_new %>%
  summarise(
    total = n(),
    missing = sum(is.na(days_since_last_visit)),
    missing_pct = mean(is.na(days_since_last_visit)) * 100,
    mean = mean(days_since_last_visit, na.rm = TRUE),
    median = median(days_since_last_visit, na.rm = TRUE),
    sd = sd(days_since_last_visit, na.rm = TRUE),
    min = min(days_since_last_visit, na.rm = TRUE),
    max = max(days_since_last_visit, na.rm = TRUE)
  )
days_since_summary

# DEMOGRAPHIC FEATURES
#---------------------
# LANGUAGE
# Patients whose language differs from English, what is most likely to be spoken at lackey, may experience more 
#     difficulty with their healthcare than others, increasing chances for churn
# Data Issues: Way to many groups and varying notation of language spoken, so we decided to group them into 4 main categories

language_raw_summary <- master_new %>%
  summarise(
    total = n(),
    missing = sum(is.na(Language)),
    missing_pct = mean(is.na(Language)) * 100
  )

language_raw_counts <- master_new %>%
  count(Language) %>%
  arrange(desc(n)) %>%
  mutate(percent = n / sum(n, na.rm = TRUE) * 100)

language_raw_summary
print(language_raw_counts, n = 39)

lang_summary <- master_new %>%
  summarise(
    total = n(),
    missing = sum(is.na(Language_grouped)),
    missing_pct = mean(is.na(Language_grouped)) * 100
  )

lang_counts <- master_new %>%
  count(Language_grouped) %>%
  mutate(percent = n / sum(n) * 100)

lang_summary
lang_counts

# HOUSING Feature
# Unstable housing conditions could face more issues around mobility and overall stress
# Data issues:
housing_summary <- master_new %>%
  summarise(
    total = n(),
    missing = sum(is.na(Housing)),
    missing_pct = mean(is.na(Housing)) * 100
  )

housing_counts <- master_new %>%
  count(Housing) %>%
  mutate(percent = n / sum(n) * 100)

housing_summary
housing_counts

# EMPLOYMENT Feature
# Those unemployed may be in the process of gaining medicare/medicaid and those employed may be to busy to make it to appointments
# Data Issues: Some small issues with ('s and ['s in notation

employment_summary <- master_new %>%
  summarise(
    total = n(),
    missing = sum(is.na(Employment)),
    missing_pct = mean(is.na(Employment)) * 100
  )

employment_counts <- master_new %>%
  count(Employment) %>%
  mutate(percent = n / sum(n) * 100)

employment_summary
employment_counts

# VISITS IN PAST YEAR feature
# Patients with a small amount or 0 visits in the past year are more likely to have disengaged and thus possibly churn

visits_summary <- master_new %>%
  summarise(
    total_records = n(),
    missing = sum(is.na(visits_in_past_year)),
    missing_pct = mean(is.na(visits_in_past_year)) * 100,
    mean_visits = mean(visits_in_past_year, na.rm = TRUE),
    median_visits = median(visits_in_past_year, na.rm = TRUE),
    max_visits = max(visits_in_past_year, na.rm = TRUE),
    min_visits = min(visits_in_past_year, na.rm = TRUE)
  )

visits_counts <- master_new %>%
  count(visits_in_past_year) %>%
  mutate(percent = n / sum(n) * 100)

visits_summary
visits_counts
print(visits_counts, n = nrow(visits_counts))
sum(!master_new$mrn_pseudo %in% encounters$mrn_pseudo)

# ZIP-CODE-DISTANCE feature
# Patients who live closer to lackey may be more likely to visit and
# less likely to churn. We will calculate distance from the geographical 
# center of each county to lackey's lat and long in meters

ref_lat <- 37.23110133397947
ref_lng <- -76.55492831279969

residence <- residence %>% 
  group_by(mrn_pseudo) %>%
  slice_max(order_by = r_start, n=1, with_ties = FALSE) %>%
  ungroup()

residence$r_zip <- as.character(residence$r_zip)

# Add latitude and longitude for each ZIP
zip_lookup <- zipcodeR::zip_code_db[, c("zipcode", "lat", "lng")]
colnames(zip_lookup)
colnames(zip_lookup)[1] <- "r_zip"

residence <- merge(residence, zip_lookup, by = "r_zip", all.x = TRUE)

# Compute distance for each ZIP
residence$distance_meters <- distHaversine(
  cbind(residence$lng, residence$lat),
  c(ref_lng, ref_lat)
)

# cleaning and joining
residence_clean <- residence %>%
  drop_na(distance_meters)

residence_dist <- residence_clean

residence_dist <- residence_dist[c("mrn_pseudo", "distance_meters")]

master_new <- master_new %>%
  left_join(residence_dist, by = "mrn_pseudo")

# No-Show Rate Feature Summary
#Patients with higher no-show rates may be more likely to churn, since missed 
#appointments often indicate lower engagement or access barriers.

noshow_summary_stats <- master_new %>%
  summarise(
    total = n(),
    missing = sum(is.na(noshow_rate)),
    missing_pct = mean(is.na(noshow_rate)) * 100,
    mean_noshow_rate = mean(noshow_rate, na.rm = TRUE),
    median_noshow_rate = median(noshow_rate, na.rm = TRUE),
    min_noshow_rate = min(noshow_rate, na.rm = TRUE),
    max_noshow_rate = max(noshow_rate, na.rm = TRUE)
  )

noshow_summary_stats

# Summary statistics for has_chronic_condition
chronic_summary <- master_new %>%
  summarise(
    total = n(),
    missing = sum(is.na(has_chronic_diagnosis)),
    missing_pct = mean(is.na(has_chronic_diagnosis)) * 100
  )

# Frequency table
chronic_counts <- master_new %>%
  count(has_chronic_diagnosis) %>%
  mutate(percent = n / sum(n) * 100)

chronic_summary
chronic_counts

############################################
############################################