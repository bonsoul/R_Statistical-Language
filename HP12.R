library(readr)
DF1 <- read_csv("D:/Downloads/Final Household data Edit.csv")

colnames(DF1)


#library(dplyr)
library(tidyr)
library(knitr)


att1 <- DF1 %>%
  summarise(
    Yes = sum(Yes13 == "Yes", na.rm = TRUE),
    No  = sum(No14 == "Yes", na.rm = TRUE),
    DontKnow = sum(DontKnow21 == "Yes", na.rm = TRUE)
  ) %>%
  pivot_longer(cols = everything(), names_to = "Response", values_to = "n") %>%
  mutate(Percent = round(100 * n / sum(n), 1),
         Question = "Do you think you can get Hepatitis B? (n=203)")


att2 <- DF1 %>%
  summarise(
    Fear = sum(Fear == "Yes", na.rm = TRUE),
    Shame = sum(Shame == "Yes", na.rm = TRUE),
    Surprise = sum(Surprise == "Yes", na.rm = TRUE),
    Saddness = sum(Saddness == "Yes", na.rm = TRUE),
    Other = sum(OtherSpecify6 == "Yes", na.rm = TRUE)
  ) %>%
  pivot_longer(cols = everything(), names_to = "Response", values_to = "n") %>%
  mutate(Percent = round(100 * n / sum(n), 1),
         Question = "What would be your reaction if you found that you have Hepatitis B? (n=203)")

att3 <- DF1 %>%
  summarise(
    HealthWorker = sum(Healthworker == "Yes", na.rm = TRUE),
    Spouse = sum(Spouse == "Yes", na.rm = TRUE),
    Parents = sum(`Parent1` == "Yes", na.rm = TRUE),  # use backticks for special chars
    Children = sum(Child == "Yes", na.rm = TRUE),
    OtherRelatives = sum(Otherrelatives == "Yes", na.rm = TRUE),
    ReligiousLeaders = sum(Religiousleaders == "Yes", na.rm = TRUE),
    NoOne = sum(Noone == "Yes", na.rm = TRUE),
    Other = sum(Otherspecify7 == "Yes", na.rm = TRUE)
  ) %>%
  pivot_longer(cols = everything(), names_to = "Response", values_to = "n") %>%
  mutate(Percent = round(100 * n / sum(n), 1),
         Question = "Who would you talk to about your illness? (n=203)")


# 4. What will you do if you think you have symptoms ---------------
att4 <- DF1 %>%
  summarise(
    HealthFacility = sum(Gotohealthfacility == "Yes", na.rm = TRUE),
    TraditionalHealer = sum(Gototraditionalhealer == "Yes", na.rm = TRUE),
    SelfMedication = sum(Selfmedicate == "Yes", na.rm = TRUE)
  ) %>%
  pivot_longer(cols = everything(), names_to = "Response", values_to = "n") %>%
  mutate(Percent = round(100 * n / sum(n), 1),
         Question = "What will you do if you think that you have symptoms of Hepatitis B? (n=203)")

# 5. At what stage will you go to health facility ------------------
att5 <- DF1 %>%
  summarise(
    After34Weeks = sum(After34weeksoftheappearanceofsymptoms == "Yes", na.rm = TRUE),
    Immediately = sum(AssoonasIrealizethesymptomsareofHepatitisB == "Yes", na.rm = TRUE)
  ) %>%
  pivot_longer(cols = everything(), names_to = "Response", values_to = "n") %>%
  mutate(Percent = round(100 * n / sum(n), 1),
         Question = "If you had symptoms of HB, at what stage will you go to the health facility? (n=203)")

# 6. How expensive is diagnosis/treatment --------------------------
att6 <- DF1 %>%
  summarise(
    Free = sum(Free == "Yes", na.rm = TRUE),
    Reasonable = sum(Reasonable == "Yes", na.rm = TRUE),
    SomewhatExp = sum(Somewhatexpensive == "Yes", na.rm = TRUE),
    Expensive = sum(Expensive == "Yes", na.rm = TRUE),
    DontKnow = sum(Dontknow == "Yes", na.rm = TRUE)
  ) %>%
  pivot_longer(cols = everything(), names_to = "Response", values_to = "n") %>%
  mutate(Percent = round(100 * n / sum(n), 1),
         Question = "How expensive do you think is the diagnosis and treatment of Hepatitis B? (n=203)")

# 7. Which source do you trust the most ----------------------------
att7 <- DF1 %>%
  summarise(
    Relatives = sum(Relativeorneighbors == "Yes", na.rm = TRUE),
    ReligiousLeaders = sum(Imamspastorsorreligiousleaders == "Yes", na.rm = TRUE),
    TraditionalLeaders = sum(Communitystraditionalleaderseldersandmobilizers == "Yes", na.rm = TRUE),
    Chiefs = sum(Chiefandadministrationleaders == "Yes", na.rm = TRUE),
    HealthWorkers = sum(Healthworkers == "Yes", na.rm = TRUE),
    CHV = sum(Communityhealthvolunteers == "Yes", na.rm = TRUE),
    Poster = sum(Poster == "Yes", na.rm = TRUE),
    Television = sum(Television1 == "Yes", na.rm = TRUE),
    Radio = sum(Radio1 == "Yes", na.rm = TRUE)
  ) %>%
  pivot_longer(cols = everything(), names_to = "Response", values_to = "n") %>%
  mutate(Percent = round(100 * n / sum(n), 1),
         Question = "Of the sources of information you mentioned, which one(s) do you trust the most? (n=203)")

# 8. How concerned if family member gets HB ------------------------
att8 <- DF1 %>%
  summarise(
    VeryConcerned = sum(VeryConcern == "Yes", na.rm = TRUE),
    SomewhatConcerned = sum(Somewhatconcern == "Yes", na.rm = TRUE),
    NotConcerned = sum(Notveryconcern == "Yes", na.rm = TRUE)
  ) %>%
  pivot_longer(cols = everything(), names_to = "Response", values_to = "n") %>%
  mutate(Percent = round(100 * n / sum(n), 1),
         Question = "How concerned are you if a family member may get sick with HB? (n=203)")



library(dplyr)
library(knitr)
library(kableExtra)

# Combine all attitude subsections
attitude_table <- bind_rows(att1, att2, att3, att4, att5, att6, att7, att8)

# Print nicely with kableExtra
attitude_table %>%
  kable("html", 
        caption = "Attitudes towards Hepatitis B among respondents (Baringo County, Kenya, 2019)",
        col.names = c("Question", "Response", "Frequency (n)", "Percent (%)")) %>%
  kable_styling(full_width = FALSE, 
                bootstrap_options = c("striped", "hover", "condensed", "responsive"),
                position = "center") %>%
  collapse_rows(columns = 1, valign = "top")









library(dplyr)
library(tidyr)
library(knitr)
library(kableExtra)

# 1. Ever screened
pr1 <- DF1 %>%
  summarise(
    Yes = sum(Yes13 == "Yes", na.rm = TRUE),
    No  = sum(No14 == "Yes", na.rm = TRUE),
    DontKnow = sum(DontKnow21 == "Yes", na.rm = TRUE)
  ) %>%
  pivot_longer(cols = everything(), names_to = "Response", values_to = "Frequency") %>%
  mutate(Percent = round(100 * Frequency / sum(Frequency), 1),
         Question = "Have you ever been screened for Hepatitis B? (n=206)")

# 2. Vaccination
pr2 <- DF1 %>%
  summarise(
    Yes = sum(Yes14 == "Yes", na.rm = TRUE),
    No  = sum(No15 == "Yes", na.rm = TRUE),
    DontKnow = sum(Dontknow22 == "Yes", na.rm = TRUE)
  ) %>%
  pivot_longer(cols = everything(), names_to = "Response", values_to = "Frequency") %>%
  mutate(Percent = round(100 * Frequency / sum(Frequency), 1),
         Question = "Have you been vaccinated against Hepatitis B? (n=272)")

# 3. Go to barbershop
pr3 <- DF1 %>%
  count(N81Doyougotoabarbershopregularly, name = "Frequency") %>%
  filter(!is.na(N81Doyougotoabarbershopregularly)) %>%
  mutate(Response = N81Doyougotoabarbershopregularly,
         Percent = round(100 * Frequency / sum(Frequency), 1),
         Question = "Do you go to a barber shop regularly? (n=272)") %>%
  select(Question, Response, Frequency, Percent)

# 4. Barber uses safe equipment
pr4 <- DF1 %>%
  summarise(
    Yes = sum(Yes14 == "Yes", na.rm = TRUE),
    No  = sum(No15 == "Yes", na.rm = TRUE),
    DontKnow = sum(Dontknow22 == "Yes", na.rm = TRUE)
  ) %>%
  pivot_longer(cols = everything(), names_to = "Response", values_to = "Frequency") %>%
  mutate(Percent = round(100 * Frequency / sum(Frequency), 1),
         Question = "Does your barber change/sanitize blade or use safe equipment for ear/nose piercing? (n=100)")

# Combine all
practice_table <- bind_rows(pr1, pr2, pr3, pr4) %>%
  select(Question, Response, Frequency, Percent)

# Nicely print
practice_table %>%
  kable("html", 
        caption = "Table 3: Practice related to Hepatitis B among the community, Baringo County, Kenya, 2019",
        col.names = c("Practice items", "Response", "Frequency", "%")) %>%
  kable_styling(full_width = FALSE, 
                bootstrap_options = c("striped", "hover", "condensed", "responsive"),
                position = "center") %>%
  collapse_rows(columns = 1, valign = "top")
