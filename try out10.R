
library(tidyverse)
library(readr)
library(ggplot2)
library(janitor)
library(knitr)
library(dplyr)
options(tibble.print_max = Inf, width = 1000) 




df <- read_csv("D:/Downloads/Caregivers data.csv")

colnames(df)


# Clean column names
colnames(df) <- colnames(df) %>%
  str_replace_all("\\s+", "_") %>%            # Replace spaces with underscores
  str_replace_all("\\n", "_") %>%             # Replace newline characters with underscores
  str_replace_all("[^[:alnum:]_]", "") %>%    # Remove non-alphanumeric characters (except _)
  tolower()  


colnames(df)

class(df$age)
class(df$age_in_years)


caregiver_df <- df %>%
  select(
    timestamp,
    name_of_the_warafacility,
    date,
    time,
    age_in_years,
    gender6,
    relationship_to_the_child,
    occupation,
    level_of_education,
    do_you_have_the_social_health_insurancesha,
    do_you_have_any_health_insurance_other_than_the_sha,
    if_the_answer_is_no_to_either_of_the_above_2_questions_what_is_the_reason
  )


hcw_df <- df %>%
  select(
    gender28,
    age,
    healthcare_worker_cadre,
    duration_of_practice,
    is_the_healthcare_worker_imci_trained_and_received_certification_for_the_same,
    how_long_ago_did_you_get_the_imci_training_in_years
  )


hcw_observations_df <- df %>%
  select(
    does_health_worker_ask_whether_the_child_is_able_to_drink_or_breastfeed,
    does_health_worker_ask_whether_the_child_vomits_everything,
    does_health_worker_ask_whether_the_child_has_convulsions,
    does_health_worker_ask_for_cough_or_difficult_breathing,
    does_health_worker_ask_for_diarrhea,
    does_health_worker_askfeel_for_fever_or_refer_to_temperature_if_taken_previously,
    does_the_healthcare_worker_ask_if_the_child_has_pain_or_discharge_in_the_ears,
    does_the_health_worker_or_another_staff_weigh_and_record_the_weight_of_the_child_today,
    does_the_health_worker_or_another_staff_check_the_temperature_of_the_child,
    does_health_worker_look_for_palmar_pallor,
    does_health_worker_look_for_oedema_of_both_feet,
    does_the_health_worker_check_the_skin_pinchskin_turgor,
    does_health_worker_look_in_the_sick_childs_ears,
    does_health_worker_count_the_respiratory_rate,
    does_the_health_worker_undress_the_childlift_the_childs_shirtdress
  )

child_df <- df %>%
  select(
    age_of_child_in_months,
    gender_of_child,
    what_is_the_main_reason_you_brought_the_child_to_the_health_facility_today,
    what_is_the_final_diagnosis_of_the_child
  )


# Age Summary
summary(caregiver_df$age_in_years)

# Gender Distribution
table(caregiver_df$gender6)
prop.table(table(caregiver_df$gender6))

# Relationship to Child
table(caregiver_df$relationship_to_the_child)

# Occupation
sort(table(caregiver_df$occupation), decreasing = TRUE)

# Education Level
table(caregiver_df$level_of_education)

# Health Insurance Access
table(caregiver_df$do_you_have_the_social_health_insurancesha)
table(caregiver_df$do_you_have_any_health_insurance_other_than_the_sha)

# If No Insurance - Reasons
table(caregiver_df$if_the_answer_is_no_to_either_of_the_above_2_questions_what_is_the_reason)

library(dplyr)
library(knitr)

# Create a summary table for caregiver demographics
caregiver_summary <- caregiver_df %>%
  summarise(
    n = n(),
    avg_age = round(mean(age_in_years, na.rm = TRUE), 1),
    min_age = min(age_in_years, na.rm = TRUE),
    max_age = max(age_in_years, na.rm = TRUE),
    
    female_percent = round(mean(gender6 == "Female", na.rm = TRUE) * 100, 1),
    male_percent = round(mean(gender6 == "Male", na.rm = TRUE) * 100, 1),
    
    most_common_relationship = names(sort(table(relationship_to_the_child), decreasing = TRUE))[1],
    most_common_occupation = names(sort(table(occupation), decreasing = TRUE))[1],
    most_common_education = names(sort(table(level_of_education), decreasing = TRUE))[1],
    
    has_sha_percent = round(mean(do_you_have_the_social_health_insurancesha == "yes", na.rm = TRUE) * 100, 1),
    has_other_insurance_percent = round(mean(do_you_have_any_health_insurance_other_than_the_sha == "yes", na.rm = TRUE) * 100, 1),
    
    most_common_reason_no_insurance = names(sort(table(if_the_answer_is_no_to_either_of_the_above_2_questions_what_is_the_reason), decreasing = TRUE))[1]
  )

# Display the table
kable(caregiver_summary, caption = "Summary of Caregiver Demographics")



library(dplyr)
library(janitor)

# Vector of variables to summarize
vars <- c(
  "age_in_years",
  "gender6",
  "relationship_to_the_child",
  "level_of_education",
  "do_you_have_the_social_health_insurancesha",
  "do_you_have_any_health_insurance_other_than_the_sha"
)

# Function to summarize each variable
summarise_var <- function(var, data) {
  data %>%
    filter(!is.na(.data[[var]])) %>%
    count(Category = .data[[var]]) %>%
    mutate(
      Characteristic = var,
      `Percentage (%)` = round(100 * n / sum(n), 1)
    ) %>%
    select(Characteristic, Category, Frequency = n, `Percentage (%)`)
}

# Apply function to each variable and bind rows
demographic_summary <- bind_rows(lapply(vars, summarise_var, data = caregiver_df))

# Clean and arrange the summary
demographic_summary <- demographic_summary %>%
  mutate(Characteristic = recode(Characteristic,
                                 age_in_years = "Age in Years",
                                 gender6 = "Gender",
                                 relationship_to_the_child = "Relationship to Child",
                                 level_of_education = "Level of Education",
                                 do_you_have_the_social_health_insurancesha = "Has SHA",
                                 do_you_have_any_health_insurance_other_than_the_sha = "Has Other Insurance"))

# Print table
print(demographic_summary, n = Inf)



hcw_df_clean <- hcw_df %>%
  mutate(
    practice_duration_group = case_when(
      str_detect(duration_of_practice, "0\\s*-\\s*5") ~ "0–5 years",
      str_detect(duration_of_practice, "6\\s*-\\s*10") ~ "6–10 years",
      str_detect(duration_of_practice, "11\\s*-\\s*15") ~ "11–15 years",
      str_detect(duration_of_practice, "16\\s*-\\s*20") ~ "16–20 years",
      str_detect(duration_of_practice, ">\\s*20") ~ ">20 years",
      TRUE ~ NA_character_
    ),
    
    # Create a helper numeric column for years since IMCI, parsing text carefully
    years_since_imci_numeric = case_when(
      str_detect(how_long_ago_did_you_get_the_imci_training_in_years, "^[0-9]+$") ~
        as.numeric(how_long_ago_did_you_get_the_imci_training_in_years),
      str_detect(how_long_ago_did_you_get_the_imci_training_in_years, regex("month|mo", ignore_case = TRUE)) ~
        0,  # less than 1 year, treat as 0
      str_detect(how_long_ago_did_you_get_the_imci_training_in_years, regex("week|wk", ignore_case = TRUE)) ~
        0,  # less than 1 year, treat as 0
      TRUE ~ NA_real_
    ),
    
    years_since_imci_group = case_when(
      !is.na(years_since_imci_numeric) & years_since_imci_numeric <= 10 ~ "0–10 years",
      !is.na(years_since_imci_numeric) & years_since_imci_numeric <= 20 ~ "11–20 years",
      !is.na(years_since_imci_numeric) & years_since_imci_numeric <= 30 ~ "21–30 years",
      !is.na(years_since_imci_numeric) & years_since_imci_numeric > 30 ~ ">30 years",
      TRUE ~ NA_character_
    )
  ) %>%
  select(-years_since_imci_numeric)  # drop helper column if you want


print(hcw_df_clean)


library(dplyr)

# Your summary function for one variable
summarise_var1 <- function(var, data) {
  data %>%
    filter(!is.na(.data[[var]])) %>%
    count(Category = .data[[var]]) %>%
    mutate(
      Characteristic = var,
      `Percentage (%)` = round(100 * n / sum(n), 1)
    ) %>%
    select(Characteristic, Category, Frequency = n, `Percentage (%)`)
}

# Variables to summarize — use the cleaned grouping columns as well
summary_vars <- c(
  "age",
  "gender28",
  "healthcare_worker_cadre",
  "practice_duration_group",
  "is_the_healthcare_worker_imci_trained_and_received_certification_for_the_same",
  "years_since_imci_group"
)

# Create the demographic summary by applying the function over each variable
demographic_summary <- bind_rows(
  lapply(summary_vars, function(var) summarise_var1(var, hcw_df_clean))
)

# Rename Characteristic values to user-friendly labels
demographic_summary <- demographic_summary %>%
  mutate(
    Characteristic = recode(Characteristic,
                            age = "Age in Years",
                            gender28 = "Gender",
                            healthcare_worker_cadre = "HCW Cadre",
                            practice_duration_group = "Practice Duration",
                            is_the_healthcare_worker_imci_trained_and_received_certification_for_the_same = "IMCI Trained",
                            years_since_imci_group = "Years Since IMCI"
    )
  )

# Print all rows in the summary table
print(demographic_summary, n = Inf)


child_df <- child_df %>%
  mutate(age_group = case_when(
    is.na(age_of_child_in_months) ~ NA_character_,
    age_of_child_in_months < 10 ~ "0–9 months",
    age_of_child_in_months < 20 ~ "10–19 months",
    age_of_child_in_months < 30 ~ "20–29 months",
    age_of_child_in_months < 40 ~ "30–39 months",
    age_of_child_in_months < 50 ~ "40–49 months",
    age_of_child_in_months < 60 ~ "50–59 months",
    TRUE ~ "60+ months"
  ))


summary_vars_child <- c(
  "age_group",  # changed here
  "gender_of_child",
  "what_is_the_main_reason_you_brought_the_child_to_the_health_facility_today",
  "what_is_the_final_diagnosis_of_the_child"
)


child_demographic_summary <- bind_rows(
  lapply(summary_vars_child, function(var) summarise_var1(var, child_df))
)

child_demographic_summary <- child_demographic_summary %>%
  mutate(
    Characteristic = recode(Characteristic,
                            age_group = "Age of Child (months)",
                            gender_of_child = "Gender of Child",
                            what_is_the_main_reason_you_brought_the_child_to_the_health_facility_today = "Reason for Visit",
                            what_is_the_final_diagnosis_of_the_child = "Final Diagnosis"
    )
  )

print(child_demographic_summary, n = Inf)


# Prepare data
sha_data <- caregiver_df %>%
  filter(!is.na(do_you_have_the_social_health_insurancesha)) %>%
  count(Has_SHA = do_you_have_the_social_health_insurancesha) %>%
  mutate(percentage = round(100 * n / sum(n), 1),
         label = paste0(Has_SHA, " (", percentage, "%)"))

# Plot
ggplot(sha_data, aes(x = "", y = n, fill = Has_SHA)) +
  geom_col(width = 1) +
  coord_polar(theta = "y") +
  labs(title = "Has Social Health Insurance (SHA)", fill = "Response") +
  theme_void() +
  geom_text(aes(label = label), position = position_stack(vjust = 0.5))


# Prepare data
other_insurance_data <- caregiver_df %>%
  filter(!is.na(do_you_have_any_health_insurance_other_than_the_sha)) %>%
  count(Other_Insurance = do_you_have_any_health_insurance_other_than_the_sha) %>%
  mutate(percentage = round(100 * n / sum(n), 1),
         label = paste0(Other_Insurance, " (", percentage, "%)"))

# Plot
ggplot(other_insurance_data, aes(x = "", y = n, fill = Other_Insurance)) +
  geom_col(width = 1) +
  coord_polar(theta = "y") +
  labs(title = "Has Other Health Insurance", fill = "Response") +
  theme_void() +
  geom_text(aes(label = label), position = position_stack(vjust = 0.5))

