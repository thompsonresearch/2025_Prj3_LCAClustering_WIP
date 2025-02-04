#-------------------------------------------------------------------------------------------
#-------------------------------------------------------------------------------------------
# 1. Clear the data environment
#-------------------------------------------------------------------------------------------
#-------------------------------------------------------------------------------------------

rm(list = ls())

#-------------------------------------------------------------------------------------------
#-------------------------------------------------------------------------------------------
# 2. Install and load packages using pacman
#-------------------------------------------------------------------------------------------
#-------------------------------------------------------------------------------------------

if(!require("pacman")) install.packages("pacman")

pacman::p_load(
  rlang,          # Tools for programming in R
  tidyverse,      # Collection of R packages for data manipulation and visualization
  dplyr,          # Data manipulation package
  plyr,           # Tools for splitting, applying, and combining data
  tidyr,          # Data tidying and reshaping
  stringr,        # String manipulation functions
  kableExtra,     # Enhancements for 'knitr' tables
  knitr,          # Dynamic report generation
  log4r,          # Logging utility for R
  ggplot2,        # Data visualization using the grammar of graphics
  plotly,         # Interactive data visualizations
  survey,         # Tools for complex survey analysis
  mice,           # Multiple imputation for missing data
  VIM,            # Visualizing and imputing missing data
  VGAM,           # Vector generalized linear and additive models (used for Tobit)
  nnet,           # Neural networks and multinomial log-linear models
  tinytex,        # Lightweight LaTeX distribution for R Markdown
  stargazer,      # Create tables for regression output and summary statistics
  broom,          # Tidy tools for summarizing statistical models
  xtable,         # Export tables to LaTeX or HTML
  rmarkdown,      # Dynamic document generation with R
  purrr,          # Functional programming tools
  gt,             # Create formatted tables in R
  poLCA           # Latent Class Analysis for determing groups
  )

#-------------------------------------------------------------------------------------------
#-------------------------------------------------------------------------------------------
# 3. Load People and Nature Survey - used safeguarded data accessed from UK Data Service
#-------------------------------------------------------------------------------------------
#-------------------------------------------------------------------------------------------

data_url <- "https://beta.ukdataservice.ac.uk/Umbraco/Surface/Project/GetDownload?studyNumber=9093&projectId=db8d7bb6-57d4-4358-8688-caf6a6df13c8&fileName=9093excel_4D6CFD311388712DF0255DC8D7CE65C8B4528D75096267FE4D6AC3A1E10D1699_V1.zip&fileSize=196"

data_dest <- "data/adult/pans-adult-2024"

download.file(data_url, data_dest)

# extract .zip file and rename to pans-adult-2024.csv


# convert to csv external to R Studio. 
cat("have you extracted and converted data to csv?")

pans_adult_og <- read.csv("data/adult/pans-adult-2024.csv")

#-------------------------------------------------------------------------------------------
#-------------------------------------------------------------------------------------------
# 4. Subset data for all green space related questions, wellbeing variables and demographics
#-------------------------------------------------------------------------------------------
#-------------------------------------------------------------------------------------------

pans_adult <- pans_adult_og %>%
  dplyr::mutate(across(where(is.character), ~ na_if(., "")))

colnames(pans_adult)

pans_adult <- pans_adult %>%
  dplyr::select(
    
    Respondent_ID,
    Weight_Percent,
    
    # wellbeing outcomes:
    Wellbeing_lonely,
    Wellbeing_satisfied,
    Wellbeing_worthwhile,
    Wellbeing_happy,
    # Wellbeing_anxious, not included, as does not fill entire study period with other wellbeing indicators
    # Wellbeing_worried, commented out, subset (short period)
    
    # green space questions from data:
    No_Of_Visits, # frequency of visits to green or natural spaces by an individual
    M2A_Q2,       # the main type of green (natural) space visited by an individual
    M2A_Q8B,      # the main activity during a green (natural) space visit by an individual
    M2A_Q8C,      # the duration of these activities during a visit to a green (natural) space by an individual
    
    # socio demographic / economic confounders
    Region,
    Age_Band,
    Gender,
    # Qualification,
    # Marital_Status,
    # No_Of_Children,
    # Work_Status,
    Income,
    # Ethnicity,
    # No_Of_Vehicles,
    # Dog,
    # Illnesses,
    # Illnesses_Impact, # omitted due to amount of NA responses - unreliable variable
    General_Health,
    Activity
    
  )

#-------------------------------------------------------------------------------------------
#-------------------------------------------------------------------------------------------
# 5. Recode variables for data imputation and analysis
#-------------------------------------------------------------------------------------------
#-------------------------------------------------------------------------------------------

pans_adult <- pans_adult %>%
  mutate(
    
    No_Of_Visits = case_when(
      is.na(No_Of_Visits) ~ NA_real_,                  # Treat missing responses as NA
      No_Of_Visits == "Prefer not to say" ~ -2,        # Recode "Prefer not to say" as -2
      No_Of_Visits == "Don’t know" ~ -1,               # Recode "Don’t know" as -1
      TRUE ~ as.numeric(No_Of_Visits)                 # Retain numeric responses
    ),
    
    M2A_Q2 = recode(M2A_Q2,
                    "Prefer not to say" = -2,
                    "Don’t know" = -1,
                    "Urban green space (such as a park, field or playground)" = 1,
                    "Grounds of a historic property or country park" = 2,
                    "Allotment or community garden" = 3,
                    "Woodland or forest" = 4,
                    "River, lake or canal" = 5,
                    "Hill, mountain or moorland" = 6,
                    "Beach / other coastline / sea" = 7,
                    "Nature / wildlife reserve" = 8,
                    "Fields / farmland / countryside" = 9,
                    "Other specify" = 10,
                    .default = NA_real_),
    
    M2A_Q8B = recode(M2A_Q8B,
                     "Eating or drinking out / picnicking" = 1,
                     "Playing with children" = 2,
                     "Walking (including taking a dog for a walk)" = 3,
                     "Cycling or running" = 4,
                     "Fishing" = 5,
                     "Appreciating scenery from a car" = 6,
                     "Horse-riding" = 7,
                     "Shooting / hunting" = 8,
                     "Sports and games" = 9,
                     "Visiting an attraction" = 10,
                     "Boating, water sports or swimming outdoors" = 11,
                     "Wildlife watching" = 12,
                     "Other" = 13,
                     "Cycling" = 14,
                     "Running" = 15,
                     .default = NA_real_),
    
    M2A_Q8C = recode(M2A_Q8C,
                     "Prefer not to say" = -2,
                     "Don’t know" = -1,
                     "Up to 30 minutes" = 1,
                     "Over 30 minutes and up to an hour" = 2,
                     "Over 1 hour and up to 2 hours" = 3,
                     "Over 2 hours and up to 3 hours" = 4,
                     "Over 3 hours and up to 5 hours" = 5,
                     "Over 5 hours" = 6,
                     .default = NA_real_),
    
    Wellbeing_lonely = stringr::str_trim(Wellbeing_lonely),  # Remove extra spaces
    Wellbeing_lonely = str_replace_all(Wellbeing_lonely, "’", "'"),
    Wellbeing_lonely = recode(Wellbeing_lonely,
                              "Prefer not to say" = -2,
                              "Don’t know" = -1,
                              "Often/always" = 1,
                              "Sometimes" = 2,
                              "Occasionally" = 3,
                              "Hardly ever" = 4,
                              "Never" = 5,
                              .default = NA_real_),
    
    Wellbeing_satisfied = recode(Wellbeing_satisfied,
                                 "Prefer not to say" = -2,
                                 "0 – Not at all" = 1,
                                 "1" = 2,
                                 "2" = 3,
                                 "3" = 4,
                                 "4" = 5,
                                 "5" = 6,
                                 "6" = 7,
                                 "7" = 8,
                                 "8" = 9,
                                 "9" = 10,
                                 "10 – Completely" = 11,
                                 .default = NA_real_),
    
    Wellbeing_worthwhile = recode(Wellbeing_worthwhile,
                                  "Prefer not to say" = -2,
                                  "0 – Not at all" = 1,
                                  "1" = 2,
                                  "2" = 3,
                                  "3" = 4,
                                  "4" = 5,
                                  "5" = 6,
                                  "6" = 7,
                                  "7" = 8,
                                  "8" = 9,
                                  "9" = 10,
                                  "10 – Completely" = 11,
                                  .default = NA_real_),
    
    Wellbeing_happy = recode(Wellbeing_happy,
                             "Prefer not to say" = -2,
                             "0 – Not at all" = 1,
                             "1" = 2,
                             "2" = 3,
                             "3" = 4,
                             "4" = 5,
                             "5" = 6,
                             "6" = 7,
                             "7" = 8,
                             "8" = 9,
                             "9" = 10,
                             "10 – Completely" = 11,
                             .default = NA_real_),
    
    Region = recode(Region,
                    "North East" = 1,
                    "North West" = 2,
                    "Yorkshire and the Humber" = 3,
                    "East Midlands" = 4,
                    "West Midlands" = 5,
                    "East" = 6,
                    "London" = 7,
                    "South East" = 8,
                    "South West" = 9,
                    .default = NA_real_),
    
    Age_Band = recode(Age_Band,
                      "16-24" = 1,
                      "25-39" = 2,
                      "40-54" = 3,
                      "55-64" = 4,
                      "65+" = 5,
                      .default = NA_real_),
    
    Gender = recode(Gender,
                    "Male" = 1,
                    "Female" = 2,
                    "In another way (specify)" = 3,
                    .default = NA_real_),
    
    Income = recode(Income,
                    "Prefer not to say" = -2,
                    "Don't know" = -1,
                    "£0–14,999" = 1,
                    "£15,000–19,999" = 2,
                    "£20,000–29,999" = 3,
                    "£30,000–39,999" = 4,
                    "£40,000–49,999" = 5,
                    "£50,000 +" = 6,
                    .default = NA_real_),
    
    General_Health = recode(General_Health,
                            "Prefer not to say" = -2,
                            "Don't know" = -1,
                            "Very good" = 1,
                            "Good" = 2,
                            "Fair" = 3,
                            "Bad" = 4,
                            "Very bad" = 5,
                            .default = NA_real_),
    
    Activity = case_when(
      is.na(Activity) ~ NA_real_,                  # Treat missing responses as NA
      Activity == "Prefer not to say" ~ -2,        # Recode "Prefer not to say" as -2
      Activity == "Don’t know" ~ -1,               # Recode "Don’t know" as -1
      TRUE ~ as.numeric(Activity)                 # Retain numeric responses
    )
  )

# set non-information data to NA (e.g. -2, -1, Don't know, Prefer not to say)

pans_adult <- pans_adult %>%
  mutate(
    Wellbeing_lonely = na_if(Wellbeing_lonely, -2),  # Convert -2 to NA
    Wellbeing_lonely = na_if(Wellbeing_lonely, -1),  # Convert -1 to NA
    Wellbeing_satisfied = na_if(Wellbeing_satisfied, -2),
    Wellbeing_worthwhile = na_if(Wellbeing_worthwhile, -2),
    Wellbeing_happy = na_if(Wellbeing_happy, -2),
    Activity = na_if(Activity, -1),
    Activity = na_if(Activity, -2),
    Income = na_if(Income, -1),
    Income = na_if(Income, -2),
    General_Health = na_if(General_Health, -1),
    General_Health = na_if(General_Health, -2),
    No_Of_Visits = na_if(No_Of_Visits, -1),
    No_Of_Visits = na_if(No_Of_Visits, -2),
    M2A_Q2 = na_if(M2A_Q2, -1),
    M2A_Q2 = na_if(M2A_Q2, -2),
    M2A_Q8C = na_if(M2A_Q8C, -1),
    M2A_Q8C = na_if(M2A_Q8C, -2)
  )

#-------------------------------------------------------------------------------------------
#-------------------------------------------------------------------------------------------
# 6. Impute missing data
#-------------------------------------------------------------------------------------------
#-------------------------------------------------------------------------------------------

pans_for_imputation <- pans_adult %>% 
  select(Wellbeing_satisfied, Wellbeing_worthwhile, 
         Wellbeing_happy, Wellbeing_lonely, No_Of_Visits, M2A_Q2, M2A_Q8B, M2A_Q8C, 
         Region, Age_Band, Gender, Income, General_Health, Activity) %>%
  # Replace "NA" strings in character columns
  mutate(across(where(is.character), ~ na_if(., "NA"))) %>%
  # Convert character columns to factors
  mutate(across(where(is.character), as.factor)) %>%
  # Ensure numeric columns are numeric
  mutate(across(where(is.numeric), as.numeric))

# Generate the missing data pattern plot
md_plot <- md.pattern(pans_for_imputation, plot = TRUE)

# Generate the default method vector based on pans_for_imputation
impute_methods <- make.method(pans_for_imputation)

# Ensure all other variables use "pmm" for imputation
# impute_methods[impute_methods == ""] <- "pmm"   # this is incorrect and would overide the ignore for Respondent_ID and Weight_Percent

# Set specific columns to not be imputed
# impute_methods["Respondent_ID"] <- ""     # Do not impute Respondent_ID
# impute_methods["Weight_Percent"] <- ""    # Do not impute Weight_Percent

# Print to verify alignment
print(impute_methods)

# Perform the imputation
tryCatch({
  pans_imputed <- mice(pans_for_imputation, m = 100, method = impute_methods, seed = 080299)
  
  # Extract completed datasets
  pans_imputed_datasets <- lapply(1:100, function(i) complete(pans_imputed, i))
  
  # Check the first completed dataset
  print(head(pans_imputed_datasets[[1]]))
}, error = function(e) {
  print(paste("Error in imputation:", e))
})

# save the imputed dataset
date_pfix <- format(Sys.Date(), "%d-%m-%y")
fnme_rds <- paste0(date_pfix, "_pans-imputed.rds")
saveRDS(pans_imputed_datasets, file = fnme_rds)

# save the environment
fnme <- paste0(date_pfix, "_pans-env.RData")
save(list = ls(), file = fnme)

#-------------------------------------------------------------------------------------------
#-------------------------------------------------------------------------------------------
# 7. Attach weight and respondent id variables
#-------------------------------------------------------------------------------------------
#-------------------------------------------------------------------------------------------

# Reattach columns to each imputed dataset
imputed_list <- lapply(pans_imputed_datasets, function(dataset) {
  dataset <- dataset %>%
    mutate(
      Respondent_ID = pans_adult$Respondent_ID,
      Weight_Percent = pans_adult$Weight_Percent
    )
  return(dataset)
})

#-------------------------------------------------------------------------------------------
#-------------------------------------------------------------------------------------------
# 8.a. Determine Optimal Number of Classes
#-------------------------------------------------------------------------------------------
#-------------------------------------------------------------------------------------------

# Function to fit LCA and extract fit indices
run_lca <- function(dataset, nclass) {
  lca_formula <- cbind(M2A_Q2, M2A_Q8B, M2A_Q8C, Region, Age_Band, Gender, Income, General_Health, Activity) ~ 1
  lca_model <- poLCA(lca_formula, data = dataset, nclass = nclass, maxiter = 1000)
  return(list(
    model = lca_model,
    AIC = lca_model$aic,
    BIC = lca_model$bic
  ))
}

# Evaluate models for 1 to 6 classes
class_range <- 1:6  # Change as needed
fit_indices <- data.frame(Classes = class_range, AIC = NA, BIC = NA)

for (k in class_range) {
  result <- run_lca(your_dataset, k)
  fit_indices$AIC[k] <- result$AIC
  fit_indices$BIC[k] <- result$BIC
}

# Plot AIC and BIC to find the "elbow point"
plot(fit_indices$Classes, fit_indices$AIC, type = "b", xlab = "Number of Classes", ylab = "AIC", main = "AIC by Number of Classes")
lines(fit_indices$Classes, fit_indices$BIC, type = "b", col = "red")
legend("topright", legend = c("AIC", "BIC"), col = c("black", "red"), lty = 1)

#-------------------------------------------------------------------------------------------
#-------------------------------------------------------------------------------------------
# 8.b. Latent Class Analysis (LCA) - classify groups of individuals using categorical data
# Undertaken across all imputed iterations
#-------------------------------------------------------------------------------------------
#-------------------------------------------------------------------------------------------

# Define a function to run LCA
run_lca <- function(dataset, nclass) {
  lca_formula <- cbind(M2A_Q2, M2A_Q8B, M2A_Q8C, Region, Age_Band, Gender, Income, General_Health, Activity) ~ 1
  lca_model <- poLCA(lca_formula, data = dataset, nclass = nclass, maxiter = 1000)
  return(lca_model)
}

# Set the optimal number of latent classes (determined beforehand)
nclass <- 3  # Replace with the optimal number of classes

# Apply LCA to each imputed dataset
lca_results <- lapply(imputed_list, function(dataset) {
  run_lca(dataset, nclass)
})

#-------------------------------------------------------------------------------------------
#-------------------------------------------------------------------------------------------
# 8.c. Align class labels across imputations
#-------------------------------------------------------------------------------------------
#-------------------------------------------------------------------------------------------

# Function to align class labels using reference probabilities
align_classes <- function(lca_results) {
  reference_probs <- lca_results[[1]]$probs  # Use the first imputation as the reference
  
  for (i in 2:length(lca_results)) {
    current_probs <- lca_results[[i]]$probs
    
    # Calculate similarity between current and reference class profiles
    similarity_matrix <- cor(t(reference_probs), t(current_probs))
    alignment <- apply(similarity_matrix, 1, which.max)  # Find best alignment
    
    # Reorder classes in the current result to match the reference
    lca_results[[i]]$predclass <- factor(lca_results[[i]]$predclass, levels = alignment)
  }
  
  return(lca_results)
}

lca_results <- align_classes(lca_results)

#-------------------------------------------------------------------------------------------
#-------------------------------------------------------------------------------------------
# 8.d. Pool class membership probabilities
#-------------------------------------------------------------------------------------------
#-------------------------------------------------------------------------------------------

# Extract class membership probabilities
class_probs_list <- lapply(lca_results, function(model) model$posterior)

# Average probabilities across imputations
pooled_probs <- Reduce("+", class_probs_list) / length(class_probs_list)

# Assign individuals to the most likely class based on pooled probabilities
pooled_classes <- apply(pooled_probs, 1, which.max)

# Add pooled class assignments to the original dataset
pans_adult$Pooled_Class <- pooled_classes

# Save the final clustered dataset
write.csv(pans_adult, "final_clusters_lca.csv", row.names = FALSE)

#-------------------------------------------------------------------------------------------
#-------------------------------------------------------------------------------------------
# 9. Unadjusted Tobit regression for classified groups (weighted)
#-------------------------------------------------------------------------------------------
#-------------------------------------------------------------------------------------------

# Fit a Tobit model for a specified outcome
fit_tobit_model_unad <- function(dataset, outcome) {
  formula <- as.formula(paste(outcome, "~ Pooled_Class"))
  
  vglm(
    formula,
    family = tobit(Lower = 0, Upper = 11),  # Bounds for the Tobit model
    data = dataset,
    weights = dataset$Weight_Percent       # Include weights
  )
}

# Defined list of outcomes and var names here
outcomes <- c("Wellbeing_satisfied", "Wellbeing_happy", "Wellbeing_worthwhile", "Wellbeing_lonely")
variable_names <- c("satisfied", "happy", "worthwhile", "lonely")  # Desired suffix for object names

# Loop through each outcome
unadjusted_tobit_models <- list()
for (i in seq_along(outcomes)) {
  outcome <- outcomes[i]
  var_name <- variable_names[i]
  
  # Fit Tobit models for all imputed datasets for the current outcome
  tobit_models <- lapply(imputed_list, fit_tobit_model_unad, outcome = outcome)
  
  # Extract coefficients and pool manually
  pooled_coefficients <- Reduce("+", lapply(tobit_models, coef)) / length(tobit_models)
  
  # Store the pooled model results
  unadjusted_tobit_models[[var_name]] <- pooled_coefficients
}

# Print unadjusted results
cat("\nPooled Unadjusted Tobit Results:\n")
print(unadjusted_tobit_models)

#-------------------------------------------------------------------------------------------
#-------------------------------------------------------------------------------------------
# 10. Adjusted Tobit regression for classified groups (weighted)
#-------------------------------------------------------------------------------------------
#-------------------------------------------------------------------------------------------

# Fit an adjusted Tobit model for a specified outcome
fit_tobit_model_ad <- function(dataset, outcome) {
  formula <- as.formula(paste(outcome, "~ Pooled_Class + Age_Band + Gender + Income + General_Health + Activity"))
  
  vglm(
    formula,
    family = tobit(Lower = 0, Upper = 11),  # Bounds for the Tobit model
    data = dataset,
    weights = dataset$Weight_Percent       # Include weights
  )
}

# Loop through each outcome
adjusted_tobit_models <- list()
for (i in seq_along(outcomes)) {
  outcome <- outcomes[i]
  var_name <- variable_names[i]
  
  # Fit adjusted Tobit models for all imputed datasets
  tobit_models_adj <- lapply(imputed_list, fit_tobit_model_ad, outcome = outcome)
  
  # Extract coefficients and pool manually
  pooled_coefficients <- Reduce("+", lapply(tobit_models_adj, coef)) / length(tobit_models_adj)
  
  # Store the pooled model results
  adjusted_tobit_models[[var_name]] <- pooled_coefficients
}

# Print adjusted results
cat("\nPooled Adjusted Tobit Results:\n")
print(adjusted_tobit_models)

# Save the regression models
saveRDS(unadjusted_tobit_models, "unadjusted_tobit_models_lca.rds")
saveRDS(adjusted_tobit_models, "adjusted_tobit_models_lca.rds")

#-------------------------------------------------------------------------------------------
#-------------------------------------------------------------------------------------------
# 13. Export faceted coefficient plots for pooled results (unadjusted and adjusted)
# Coef plots with CI percentages etc. 
#-------------------------------------------------------------------------------------------
#-------------------------------------------------------------------------------------------



#-------------------------------------------------------------------------------------------
#-------------------------------------------------------------------------------------------
# 14. END OF ANALYSIS
#-------------------------------------------------------------------------------------------
#-------------------------------------------------------------------------------------------
