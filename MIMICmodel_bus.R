# ################################################################# #
#### LOAD LIBRARY AND DEFINE CORE SETTINGS                       ####
# ################################################################# #

### Clear memory
rm(list = ls())

### Load necessary libraries
library(apollo)
library(fastDummies)
library(lavaan)

### Initialise Apollo
apollo_initialise()

### Set Apollo core controls
apollo_control = list(
  modelName  = "MNL",
  modelDescr = "Simple MNL model on mode choice SP data",
  indivID    = "ID"
)

# ################################################################# #
#### LOAD DATA AND APPLY ANY TRANSFORMATIONS                     ####
# ################################################################# #

database = read.csv("D:/Thesis/RP/New/Final Data/Main_Don'tchangeanything_BustoBusMRTT.csv")

### Display summary of the dataset
summary(database)

# Standardize selected variables
variables_to_standardize <- c("personal_income", "family_income", "trans_expn_3months", "main_distance", "access_distance", "fare_bus", "fare_MRT", "time_bus", "time_MRT", "Access_egress_Fare")
database[variables_to_standardize] <- scale(database[variables_to_standardize])

# Create dummy variables for categorical variables, removing the first dummy column
database <- dummy_cols(database, select_columns = c("age", "education"), remove_first_dummy = TRUE)

### Display summary of the modified dataset
summary(database)

# Check for missing values in each column
sapply(database, function(x) sum(is.na(x)))

# ################################################################# #
#### DEFINE AND ESTIMATE MIMIC MODEL                             ####
# ################################################################# #

# Define the MIMIC model
mimic_model <- '
  # Measurement model
  Flexibility_and_Convenience_m =~ I2 + I3 + I4 + I5 
  Family_Inclined_People_m =~ I6 + I7 + I8 + I9 
  #Pedestrian_Infrastructure_m =~ I10 + I11 + I12 + I13
  Reliability_m =~ I14 + I15 + I16 
  Safety_m =~ I17 + I18 + I19 + I20
  Inflation_m =~ I21 + I22 + I23 
  Walking_Benefits_m =~ I24 + I25 + I26 + I27
  Social_Media_m =~ I10 + I11 + I12 + I13

 
  # Regressions (sociodemographic variables affecting the latent variables)
  Flexibility_and_Convenience_m ~ age_2+ age_3 + age_4 + age_5  + sex + education_2 + education_3 + education_4 + education_5 + household_size + AvailablePV + personal_income + family_income 
  Family_Inclined_People_m ~ age_2+ age_3 + age_4 + age_5  + sex + education_2 + education_3 + education_4 + education_5 + household_size + AvailablePV + personal_income + family_income 
  #Pedestrian_Infrastructure_m ~ age_2+ age_3 + age_4 + age_5  + sex + education_2 + education_3 + education_4 + education_5 + household_size + AvailablePV + personal_income + family_income
  Reliability_m ~ age_2+ age_3 + age_4 + age_5  + sex + education_2 + education_3 + education_4 + education_5 + household_size + AvailablePV + personal_income + family_income
  Safety_m ~ age_2+ age_3 + age_4 + age_5  + sex + education_2 + education_3 + education_4 + education_5 + household_size + AvailablePV + personal_income + family_income
  Inflation_m ~ age_2+ age_3 + age_4 + age_5  + sex + education_2 + education_3 + education_4 + education_5 + household_size + AvailablePV + personal_income + family_income
  Walking_Benefits_m ~ age_2+ age_3 + age_4 + age_5  + sex + education_2 + education_3 + education_4 + education_5 + household_size + AvailablePV + personal_income + family_income
  Social_Media_m ~ age_2+ age_3 + age_4 + age_5  + sex + education_2 + education_3 + education_4 + education_5 + household_size + AvailablePV + personal_income + family_income
'

# Estimate the model using lavaan
fit <- sem(mimic_model, data = database)

# Summarize the results
summary(fit, fit.measures = TRUE, standardized = TRUE)


