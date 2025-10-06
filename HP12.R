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





library(dplyr)
library(tidyr)
library(knitr)
library(kableExtra)

# Helper function to generate summary tables
make_table <- function(df, cols, question, n){
  df %>%
    summarise(across(all_of(cols), ~ sum(. == "Yes", na.rm = TRUE))) %>%
    pivot_longer(cols = everything(),
                 names_to = "Response",
                 values_to = "Frequency") %>%
    mutate(
      Percent = round(100 * Frequency / sum(Frequency), 1),
      Question = paste0(question, " (n=", n, ")")
    )
}

# 1. Ever heard of Hepatitis B -----------------
t1 <- make_table(DF1, c("Yes13", "No14", "DontKnow21"),
                 "Ever heard of Hepatitis B", 272)

# 2. Causes of Hepatitis B ---------------------
t2 <- make_table(DF1, c("Bacteria", "Virus", "Protozoa", "DontKnow12"),
                 "Causes of Hepatitis B", 203)

# 3. Signs & Symptoms --------------------------
t3 <- make_table(DF1, c("Fever1", "Fatique1", "Lossofappetite1", "Nausea1",
                        "Abdominalpain1", "Jointpain", "Darkurine1",
                        "Greycolouredstools", "Jaundice1"),
                 "Signs and Symptoms of HB disease", 248)

# 4. Know how HB is transmitted ----------------
t4 <- make_table(DF1, c("Yes", "No1", "Dontknow1"),
                 "Know how HB is transmitted", 201)

# 5. How is HB transmitted ---------------------
t5 <- make_table(DF1, c("Havingunprotectedsex", "Contactwithaninfectedperson",
                        "Bloodtransfusion", "Eatingcontaminatedfood","Duringchildbirth",
                        "Usingunsterilizedsyringesneedlesandsurgicalinstruments", "Sharingofneedlesearpiercingtoothextractionandtattooinstruments", "Dontknow15"),
                 "How is HB transmitted", 79)

# 6. Is HB preventable -------------------------
t6 <- make_table(DF1, c("Yes1", "No3", "Dontknow3"),
                 "Is Hepatitis B a preventable disease?", 200)

# 7. How is HB prevented -----------------------
t7 <- make_table(DF1, c("Vaccination", "Havingprotectedsex", "Notsharingcuttingequipment",
                        "Handwashing", "Usingsterilizedequipment", "Preparingfoodinhygienicconditions","Screeningbloodbeforetransfusion"),
                 "How is Hepatitis B prevented?", 87)

# 8. Do you know HB vaccination in routine immunization? ----
t8 <- make_table(DF1, c("Yes2", "No4", "DontKnow5"),
                 "Do you know that Hepatitis B vaccination is given as part of national routine immunization?", 200)



# Combine all parts ----------------------------
knowledge_table <- bind_rows(t1, t2, t3, t4, t5, t6, t7, t8)

# Print nicely ---------------------------------
kable(knowledge_table,
      caption = "Table 1: Responses to Hepatitis B knowledge among the community, Baringo County, Kenya, 2019") %>%
  kable_styling(full_width = FALSE, position = "center", bootstrap_options = c("striped", "hover", "condensed", "responsive"))


knowledge_table %>%
  kable(col.names = c("Knowledge items", "Response", "Frequency", "%"),
        caption = "Table 1: Responses to Hepatitis B knowledge among the community, Baringo County, Kenya, 2019") %>%
  kable_styling(full_width = FALSE, position = "center",
                bootstrap_options = c("striped", "hover", "condensed", "responsive"))







library(dplyr)
library(tidyr)
library(knitr)
library(kableExtra)

# Helper function
make_block <- function(df, cols, labels, question, n){
  df %>%
    summarise(across(all_of(cols), ~ sum(. == "Yes", na.rm = TRUE))) %>%
    pivot_longer(cols = everything(), values_to = "Frequency") %>%
    mutate(
      Response = labels,
      Percent = round(100 * Frequency / sum(Frequency), 1),
      Question = paste0(question, " (n=", n, ")")
    ) %>%
    select(Question, Response, Frequency, Percent)
}

# 1. Ever heard of HB
t1 <- make_block(DF1, c("Yes13","No14","DontKnow21"),
                 c("Yes","No","Don’t know"),
                 "Ever heard of Hepatitis B", 272)

# 2. Causes of HB
t2 <- make_block(DF1, c("Bacteria","Virus","DontKnow12","Aflaxtoxin","Dirt"),
                 c("Bacteria","Virus","Don’t know","Aflaxtoxin","Dirt"),
                 "Causes of Hepatitis B", 203)

# 3. Signs & Symptoms
t3 <- make_block(DF1, c("Fever1","Fatique1","Lossofappetite1","Abdominalpain1",
                        "Jointpain","Jaundice1","Vomiting","Dontknow13"),
                 c("Fever","Fatigue","Loss of appetite","Abdominal pain",
                   "Joint pain","Jaundice","Vomiting","Don’t know"),
                 "Signs and Symptoms of HB disease", 248)

# 4. Know how HB is transmitted
t4 <- make_block(DF1, c("Yes","No1"),
                 c("Yes","No"),
                 "Know how HB is transmitted", 201)

# 5. How is HB transmitted
t5 <- make_block(DF1, c("Unprotectedsex","Contactwithinfectedperson",
                        "Bloodtransfusion","Eatingcontaminatedfood",
                        "Unsterilizedequipment","Sharingneedles","Dontknow2"),
                 c("Having unprotected sex","Contact with an infected person",
                   "Blood transfusion","Eating contaminated food",
                   "Using un-sterilized equipment",
                   "Sharing of needles, ear piercing, tooth extraction and tattooing equipment",
                   "Don’t know"),
                 "How is HB transmitted", 79)

# 6. Is HB preventable
t6 <- make_block(DF1, c("Yes1","No3","Dontknow3"),
                 c("Yes","No","Don’t know"),
                 "Is Hepatitis B a preventable disease?", 200)

# 7. How is HB prevented
t7 <- make_block(DF1, c("Vaccination","Protectedsex","Notsharingequipment",
                        "Handwashing","Sterilizedequipment"),
                 c("Vaccination","Having protected sex","Not sharing cutting equipment",
                   "Hand washing","Using sterilized equipment"),
                 "How is Hepatitis B prevented?", 87)

# 8. Do you know HB vaccination is in national routine immunization
t8 <- make_block(DF1, c("Yes2","No4","DontKnow5"),
                 c("Yes","No","Don’t know"),
                 "Do you know that Hepatitis B vaccination is given as part of national routine immunization?", 200)

# Combine
knowledge_table <- bind_rows(t1,t2,t3,t4,t5,t6,t7,t8)

# Format like your example
kable(knowledge_table,
      col.names = c("Knowledge items","Response","Frequency","%"),
      caption = "Table 1: Responses to Hepatitis B knowledge among the community, Baringo County, Kenya, 2019") %>%
  kable_styling(full_width = FALSE, position = "center",
                bootstrap_options = c("striped","hover","condensed","responsive"))












# Load libraries
library(dplyr)
library(broom)
library(kableExtra)

# ---------------------------------------------
# 1. Outcome variable
# ---------------------------------------------
DF2 <- DF1 %>%
  mutate(HBV_status = case_when(
    PositiveforHepatitisB == "Yes" ~ 1,
    NegativeforHepatitisB == "Yes" ~ 0,
    TRUE ~ NA_real_
  ))

# ---------------------------------------------
# 2. Predictors (recoded like in your table)
# ---------------------------------------------
DF3 <- DF2 %>%
  mutate(
    # Sex
    Sex = factor(N2Sex1, levels = c("Female", "Male")),
    
    # Age groups
    AgeGroup = cut(N1Ageinyears1, breaks = c(0,24,34,44,54,200),
                   labels = c("<25","25-34","35-44","45-54","≥55"),
                   right = TRUE, include.lowest = TRUE),
    
    # Marital status
    Marital = factor(N10Whatisyourmaritalstatus, levels = c("Never married","Ever married")),
    
    # Type of marriage
    MarriageType = factor(Ifmarriedwhichtypeofmarrige, levels = c("Monogamous","Polygamous")),
    
    # Education
    Education = case_when(
      N7Haveyouhadformaleducation1 == "Not documented" ~ "Not documented",
      N7Haveyouhadformaleducation1 == "Yes" ~ "Yes",
      N7Haveyouhadformaleducation1 == "No" ~ "No"
    ),
    
    # Circumcision
    Circumcised = factor(N49Haveyoubeencircumcised, levels = c("No","Yes")),
    
    # Living with sexual partner
    Partner = factor(N19Doyoucurrentlylivewithasexualpartner, levels = c("No","Yes")),
    
    # Sexual partners
    MultiPartners = ifelse(N20Howmanysexualpartnershaveyouhadinthelast12months >= 2, "≥2", "<2"),
    MultiPartners = factor(MultiPartners, levels = c("<2","≥2")),
    
    # Living with HBV case
    LiveWithHBV = factor(N21HaveyoueverlivedwithsomeonewhohasbeendiagnosedwithhepatitisBy,
                         levels = c("No","Yes","Dontknow")),
    
    # Family member died of HBV
    FamilyDeathHBV = factor(N23Hasanymemberofyourfamilyyouknowdiedfromthisdiseaseyellownesso,
                            levels = c("No","Yes","Dontknow")),
    
    # Shared toothbrush
    Toothbrush = factor(N34HaveyoueversharedatoothbrushtoothbrushingtwigGetlocalnameoftw,
                        levels = c("No","Yes")),
    
    # Traditional mark/tattoo/piercing
    Tattoo = factor(N54Haveyouhadanytraditionalmarkstattoosbodyincisionsorpiercingsd,
                    levels = c("No","Yes")),
    
    # Traditional tooth extraction
    ToothExtraction = factor(N32Doyouknowanyonewhowasillwiththisdiseaseyellownessoftheeyesiny,
                             levels = c("No","Yes")),
    
    # Shaving in barbershop
    Barbershop = factor(N81Doyougotoabarbershopregularly, levels = c("No","Yes")),
    
    # Alcohol
    Alcohol = factor(N15Doyouconsumealcohol, levels = c("No","Yes")),
    
    
    # Medical attention past 6 months
    MedAttention = factor(N55Haveyousoughtanymedicalattentioninanyhealthfacilityinthelast6,
                          levels = c("No","Yes"))
  )

# ---------------------------------------------
# 3. Function to run bivariate logistic regressions
# ---------------------------------------------
run_bivariate <- function(var){
  formula <- as.formula(paste("HBV_status ~", var))
  glm(formula, data = DF3, family = binomial) %>%
    tidy(exp = TRUE, conf.int = TRUE) %>%
    filter(term != "(Intercept)") %>%
    mutate(Variable = var)
}

# List of variables
vars <- c("Sex", "AgeGroup", "Marital", "MarriageType", "Education",
          "Circumcised", "Partner", "MultiPartners", "LiveWithHBV",
          "FamilyDeathHBV", "Toothbrush", "Tattoo", "ToothExtraction",
          "Barbershop", "Alcohol", "HIV", "MedAttention")
library(dplyr)
library(broom)

run_bivariate <- function(var){
  x <- DF1[[var]]
  
  # skip variables with < 2 levels
  if(length(unique(na.omit(x))) < 2){
    return(NULL)
  }
  
  formula <- as.formula(paste("HBV_status ~", var))
  model <- glm(formula, data = DF1, family = binomial)
  
  broom::tidy(model, exponentiate = TRUE, conf.int = TRUE) %>%
    filter(term != "(Intercept)") %>%
    mutate(Variable = var) %>%
    rename(uOR = estimate,
           uOR_low = conf.low,
           uOR_high = conf.high)
}

# Run for all vars
biv_results <- lapply(vars, run_bivariate) %>%
  bind_rows()


sapply(DF3[, c("Sex","AgeGroup","Education",
               "Circumcised","Partner","MultiPartners",
               "LiveWithHBV","FamilyDeathHBV","Toothbrush",
               "Tattoo","ToothExtraction","Barbershop",
               "Alcohol","MedAttention")],
       function(x) table(x, useNA="ifany"))




# ---------------------------------------------
# 4. Multivariate logistic regression
# ---------------------------------------------
multi_model <- glm(HBV_status ~ Sex + AgeGroup + Education +
                     Circumcised + Partner + MultiPartners +
                     LiveWithHBV + FamilyDeathHBV + Toothbrush +
                     Tattoo + ToothExtraction + Barbershop +
                     Alcohol + MedAttention,
                   data = DF3, family = binomial)


multi_results <- tidy(multi_model, exp = TRUE, conf.int = TRUE) %>%
  filter(term != "(Intercept)") %>%
  rename(aOR = estimate, aOR_low = conf.low, aOR_high = conf.high)

# ---------------------------------------------
# 5. Combine uOR and aOR
# ---------------------------------------------
final_table <- biv_results %>%
  left_join(multi_results, by = "term") %>%
  mutate(
    uOR_CI = paste0(round(uOR,1)," (",round(uOR_low,1),"-",round(uOR_high,1),")"),
    aOR_CI = ifelse(!is.na(aOR), paste0(round(aOR,1)," (",round(aOR_low,1),"-",round(aOR_high,1),")"), "")
  ) %>%
  select(Variable, term, uOR_CI, aOR_CI)

# ---------------------------------------------
# 6. Display nicely
# ---------------------------------------------
kable(final_table,
      caption = "Bivariate and multivariate analysis of factors associated with Hepatitis B infection (Baringo & Elgeyo-Marakwet, 2019)",
      col.names = c("Characteristic", "Category", "uOR (95% CI)", "aOR (95% CI)")
) %>%
  kable_styling(full_width = FALSE,
                bootstrap_options = c("striped","hover","condensed","responsive"))




sapply(DF4[, vars_to_use], function(x) table(x, useNA="ifany"))



# Keep only predictors with >= 2 levels
valid_vars <- vars_to_use[sapply(DF4[, vars_to_use], function(x) length(unique(na.omit(x))) > 1)]

valid_vars


# Keep only rows with complete data across valid_vars
DF4_complete <- DF4 %>% dplyr::select(all_of(c("HBV_status", valid_vars))) %>% na.omit()

sapply(DF4_complete[, valid_vars], function(x) length(unique(x)))


valid_vars_final <- c("Sex", "AgeGroup", "MultiPartners", 
                      "LiveWithHBV", "ToothExtraction", "Barbershop")

fmla <- as.formula(
  paste("HBV_status ~", paste(valid_vars_final, collapse = " + "))
)

multi_model <- glm(fmla, data = DF4_complete, family = binomial)
summary(multi_model)








library(sf)
library(dplyr)
library(ggplot2)

# Correct path with forward slashes
shapefile_path <- "D:/Documents/R Studio/R_Statistical-Language/shapefiles/ke_subcounty(2)/ke_subcounty.shp"

# Load shapefile
subcounty_shape <- st_read(shapefile_path)

# Filter to Baringo County only (assuming a column 'COUNTY' exists)
baringo_shape <- subcounty_shape %>%
  filter(county == "Baringo")


baringo_shape <- baringo_shape %>%
  mutate(subcounty = gsub(" Sub County", "", subcounty))


unique(baringo_shape$subcounty)


ggplot(data = baringo_shape) +
  geom_sf(fill = "lightblue", color = "black") +
  geom_sf_text(aes(label = subcounty), size = 3, color = "darkred") +
  labs(title = "Subcounties of Baringo County, Kenya") +
  theme_minimal()



install.packages("ggspatial")


library(ggplot2)
library(sf)
library(dplyr)
library(ggspatial)
library(ggtext)

# Clean subcounty names
baringo_shape <- subcounty_shape %>%
  filter(county == "Baringo") %>%
  mutate(subcounty = gsub(" Sub County", "", subcounty))

# Plot

ggplot(data = baringo_shape) +
  geom_sf(aes(fill = subcounty), color = "white", linewidth = 0.6) +
  scale_fill_brewer(palette = "Set3", name = "Subcounties") +
  
  # Labels
  geom_sf_text(aes(label = subcounty), size = 3.5, color = "black", fontface = "bold") +
  
  # Title and layout
  labs(
    title = "Subcounties of <span style='color:#006d2c;'>Baringo County</span>, Kenya",
    subtitle = "Administrative boundaries showing the seven subcounties",
    caption = "Source: Kenya Open Data / GADM | Map: Your Name"
  ) +
  
  # Themes and styling
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_markdown(size = 16, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 11, hjust = 0.5, color = "gray40"),
    legend.position = "right",
    panel.grid.major = element_line(color = "gray90", linewidth = 0.3),
    panel.background = element_rect(fill = "aliceblue", color = NA),
    plot.background = element_rect(fill = "white", color = NA)
  ) +
  
  # Add north arrow & scale bar
  annotation_north_arrow(
    location = "tr", which_north = "true",
    style = north_arrow_fancy_orienteering
  ) +
  annotation_scale(location = "bl", width_hint = 0.3)

















# Join with positivity data (replace NAME_2 with the actual subcounty column in shapefile)
map_data <- baringo_shape %>%
  left_join(
    summary_table %>% filter(N6SubCounty != "Total") %>% select(N6SubCounty, Positivity_rate),
    by = c("Mogotio Sub County" = "N6SubCounty")
  )

# Plot choropleth map
ggplot(map_data) +
  geom_sf(aes(fill = Positivity_rate), color = "black") +
  scale_fill_viridis_c(option = "plasma", na.value = "grey90") +
  theme_minimal() +
  labs(title = "Hepatitis B Positivity Rate in Baringo Sub-Counties",
       fill = "Positivity (%)")


