###################################
########### VISUALS ###############
###################################

master_new <- master_new %>%
  mutate(
    # Create 3-month enrollment bins
    enroll_bin = cut(
      last_enrollment_end,
      breaks = seq(
        from = floor_date(min(last_enrollment_end, na.rm = TRUE), unit = "month"),
        to = ceiling_date(max(last_enrollment_end, na.rm = TRUE), unit = "month") + months(4),
        by = "4 months"
      ),
      include.lowest = TRUE,
      right = FALSE
    ),
    # Create churn label, convert NA to "NA" string
    churned_label = case_when(
      churned == 1 ~ "Churned",
      churned == 0 ~ "Not Churned",
      is.na(churned) ~ "NA"
    ),
    churned_label = factor(churned_label, levels = c("Not Churned", "Churned", "NA"))
  )


enroll_counts <- master_new %>%
  group_by(enroll_bin, churned_label) %>%
  summarise(n = n(), .groups = "drop")

ggplot(enroll_counts, aes(x = enroll_bin, y = n, fill = churned_label)) +
  geom_col(position = "dodge") +
  geom_text(aes(label = n), position = position_dodge(width = 0.9), vjust = -0.5) +
  labs(
    title = "Patients by Last Enrollment End (3-Month Bins) and Churn Status",
    x = "Last Enrollment End (3-Month Bins)",
    y = "Number of Patients",
    fill = "Churn Status"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

table(master_new$churned)


# ==================================================================================
# ==================================================================================
# ==================================================================================


# The model confirms that churn is primarily driven by life stage and socioeconomic instability, 
# not by clinical or appointment-keeping behavior alone.
# 
# The difference between the highest-risk group (Age 65+) and the lowest-risk group (Spanish/Other Language speakers) 
# spans a massive range, from a 74.9% churn likelihood down to a 6.7% likelihood.


#Top Churn Drivers
# -------------
# 1. Extreme Age Risk
# Age is the dominant predictor. The jump from the baseline (18-19) to the 65+ group (risk of 74.9%) is quite severe 
# Could necessitates specialized senior care engagement to address mobility, health literacy, and social isolation barriers.

# 2. Socioeconomic Instability
#Housing and employment uncertainty double the odds of churn. Patients who are Homeless, Lives With Others, Self Employed, or 
# Unemployed are struggling with basic needs, making their clinic appointments or overall a lower priority over other needs.
# ___________________

# Top Retention Factors
# -------------
# 1. Language Services Success
# Patients speaking Spanish or "Other" languages are highly retained. Their churn risk drops significantly (~7% to 8% absolute risk).

# 2. Chronic Care Engagement
# Patients with a Chronic Diagnosis are less likely to churn. Their ongoing health needs may foster stronger clinic 
# relationships and appointment adherence.


library(ggplot2)
library(dplyr)
library(scales)

# Visualization of Churn Breakdown by our Churn Def

# Age Range Plot
train_data %>%
  # Ensure age_range is ordered logically (optional, but good practice)
  mutate(age_range = factor(age_range, levels = c("18-19", "20-29", "30-39", "40-49", "50-59", "60-64", "65+"))) %>%
  
  ggplot(aes(x = age_range, fill = factor(churned))) +
  geom_bar(position = "fill") +
  scale_y_continuous(labels = percent) +
  labs(
    title = "Churn Proportion by Age Range (Highest Risk Factor)",
    y = "Proportion of Patients",
    x = "Age Range",
    fill = "Churned (1=Yes)"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Housing Status Plot
train_data %>%
  # Filter out low-impact or non-significant housing groups if desired, or keep all
  filter(Housing %in% c("Own", "Rent", "Lives With Others", "Homeless")) %>%
  
  ggplot(aes(x = Housing, fill = factor(churned))) +
  geom_bar(position = "fill") +
  scale_y_continuous(labels = percent) +
  labs(
    title = "Churn Proportion by Housing Status",
    y = "Proportion of Patients",
    x = "Housing Status",
    fill = "Churned (1=Yes)"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Employment Status Plot
train_data %>%
  # Filter to focus on the significant groups (Employed is reference)
  filter(Employment %in% c("Employed", "Self Employed", "Unemployed (Actively Looking For Work)")) %>%
  
  ggplot(aes(x = Employment, fill = factor(churned))) +
  geom_bar(position = "fill") +
  scale_y_continuous(labels = percent) +
  labs(
    title = "Churn Proportion by Key Employment Status",
    y = "Proportion of Patients",
    x = "Employment Status",
    fill = "Churned (1=Yes)"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# Language Plot
train_data %>%
  # Filter to focus on the significant groups
  filter(Language_grouped %in% c("English", "Spanish", "Other")) %>%
  
  ggplot(aes(x = Language_grouped, fill = factor(churned))) +
  geom_bar(position = "fill") +
  scale_y_continuous(labels = percent) +
  labs(
    title = "Churn Proportion by Language (Key Retention Factor)",
    y = "Proportion of Patients",
    x = "Language Group",
    fill = "Churned (1=Yes)"
  ) +
  theme_minimal()

# No-Show Rate vs Churn
ggplot(train_data, aes(x = factor(churned), y = noshow_rate, fill = factor(churned))) +
  geom_boxplot(alpha = 0.7, outlier.colour = "red", outlier.shape = 1) +
  labs(
    title = "Distribution of No-Show Rates by Churn Status",
    subtitle = "Do patients who miss appointments churn more often?",
    x = "Churn Status (0 = Retained, 1 = Churned)",
    y = "No-Show Rate (0 to 1)",
    fill = "Churned"
  ) +
  theme_minimal() +
  scale_y_continuous(labels = scales::percent) # Formats Y axis as percentage

# Distance vs Churn
ggplot(train_data, aes(x = factor(churned), y = distance_meters, fill = factor(churned))) +
  geom_boxplot(alpha = 0.7) +
  # scale_y_log10() + # Uncomment this line if outliers make the plot hard to read
  labs(
    title = "Distance from Clinic (Meters) by Churn Status",
    x = "Churn Status",
    y = "Distance (Meters)",
    fill = "Churned"
  ) +
  theme_minimal()

# Chronic Diagnosis vs Churn
train_data %>%
  # Filter out NAs if necessary, though your cleaning steps likely handled this
  filter(!is.na(has_chronic_diagnosis)) %>%
  
  ggplot(aes(x = factor(has_chronic_diagnosis), fill = factor(churned))) +
  geom_bar(position = "fill") +
  scale_y_continuous(labels = scales::percent) +
  scale_x_discrete(labels = c("0" = "No Chronic Condition", "1" = "Has Chronic Condition")) +
  labs(
    title = "Churn Proportion by Chronic Condition Status",
    subtitle = "Patients with chronic conditions are often 'stickier' (lower churn)",
    y = "Proportion of Patients",
    x = "Diagnosis Status",
    fill = "Churned (1=Yes)"
  ) +
  theme_minimal()