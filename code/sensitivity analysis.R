####################################################################
# SNAP PROJECT - COMPLETE PIPELINE
# Data Cleaning + Survey-Weighted Ordered Logit (POLR)
# Using MASS::polr() for proportional odds model
# With Weighted Risk Stratification & Missed Household Analysis
# January 2026
####################################################################

# Load libraries in CORRECT ORDER to avoid namespace conflicts
library(MASS)      # Load MASS first (has select conflict)
library(survey)
library(erer)
library(tidyverse) # Load tidyverse LAST to override any conflicts
library(gtsummary) # For publication-ready tables
library(flextable) # For Word document formatting
library(officer)   # For page_size and prop_section (landscape orientation)

# Set working directory
setwd("C:/Users/natmaxey/OneDrive - Indiana University/Desktop/snap project/data")
results_dir <- "C:/Users/natmaxey/OneDrive - Indiana University/Desktop/snap project/sensitivity analysis"

if (!dir.exists(results_dir)) { 
  dir.create(results_dir, recursive = TRUE) 
}

set.seed(42)

####################################################################
# PHASE 1: DATA CLEANING
# Complete Data Cleaning with Survey Weights
# COMPREHENSIVE VERSION - All variables properly recoded
# Respects measurement levels: Nominal, Ordinal, Continuous
####################################################################

cat("\n" %+% strrep("=", 80) %+% "\n")
cat("PHASE 1: DATA CLEANING\n")
cat(strrep("=", 80) %+% "\n\n")

####################################################################
# 1. LOAD RAW DATA
####################################################################

cat("Loading HPS data files...\n")

hps_oct_2024 <- read_csv("HPS_OCTOBER2024_PUF.csv")
hps_dec_2024 <- read_csv("HPS_DECEMBER2024_PUF.csv")
hps_phase42 <- read_csv("hps_04_02_09_puf.csv")

cat("✓ Data loaded successfully\n")
cat(sprintf("  October 2024: %d rows × %d cols\n", nrow(hps_oct_2024), ncol(hps_oct_2024)))
cat(sprintf("  December 2024: %d rows × %d cols\n", nrow(hps_dec_2024), ncol(hps_dec_2024)))
cat(sprintf("  Phase 4.2 Cycle 09: %d rows × %d cols\n", nrow(hps_phase42), ncol(hps_phase42)))

####################################################################
# 2. DEFINE COLUMNS TO KEEP
####################################################################

columns_to_keep <- c(
  "SCRAMID",
  "TAGE1", "TBIRTH_YEAR", "A_SEX1", "RHISPANIC1", "RRACE1", "MARITAL1",
  "ANXIOUS", "WORRY",
  "WRKLOSSRV", "ANYWORK", "EXPNS_DIF",
  "CURFOODSUF",
  "RENTCUR", "MORTCUR", "EVICT",
  "HLTHINS1",
  "FOODRSNRV1", "FOODRSNRV2", "FOODRSNRV3", "FREEFOOD",
  "FDBENEFIT1", "FDBENEFIT2", "FDBENEFIT3", "FDBENEFIT4", "FDBENEFIT5",
  "SCHLFDEXPNS",
  "ACCESS_TRANSP",
  "KIDS_LT1Y", "KIDS_1_4Y", "KIDS_5_11Y", "KIDS_12_17Y"
)

####################################################################
# 3. DATA CLEANING FUNCTION - COMPREHENSIVE RECODING
####################################################################

clean_hps_data <- function(df, wave_name) {
  
  cat(sprintf("Cleaning %s data...\n", wave_name))
  
  # Fix HTOPS/Phase 4.2 ID column name
  if("SCRAM" %in% names(df) & !("SCRAMID" %in% names(df))) {
    df <- df %>% dplyr::rename(SCRAMID = SCRAM)
  }
  
  # Select available columns - use explicit dplyr namespace
  available_cols <- intersect(columns_to_keep, names(df))
  
  df_clean <- df %>%
    dplyr::select(all_of(available_cols)) %>%
    dplyr::mutate(wave_source = wave_name)
  
  # Handle age variable: Oct/Dec use TAGE1, Phase 4.2 uses TBIRTH_YEAR
  if("TAGE1" %in% names(df_clean)) {
    df_clean$age <- df_clean$TAGE1
    df_clean <- df_clean %>% dplyr::select(-TAGE1)
  } else if("TBIRTH_YEAR" %in% names(df_clean)) {
    df_clean$age <- 2024 - df_clean$TBIRTH_YEAR
    df_clean <- df_clean %>% dplyr::select(-TBIRTH_YEAR)
  }
  
  # Add PWEIGHT
  if("PWEIGHT" %in% names(df)) {
    df_clean$PWEIGHT <- df$PWEIGHT
  } else {
    df_clean$PWEIGHT <- NA_real_
  }
  
  # Convert missing codes to NA
  numeric_cols <- df_clean %>% dplyr::select(where(is.numeric)) %>% names()
  numeric_cols <- setdiff(numeric_cols, c("PWEIGHT", "TAGE1"))
  
  for(col in numeric_cols) {
    df_clean[[col]][df_clean[[col]] == -99] <- NA
    df_clean[[col]][df_clean[[col]] == -88] <- NA
    df_clean[[col]][df_clean[[col]] == -77] <- NA
  }
  
  # ================================================================
  # RECODE ALL VARIABLES BY MEASUREMENT LEVEL
  # ================================================================
  
  # DEMOGRAPHICS - NOMINAL
  if("RHISPANIC1" %in% names(df_clean)) {
    df_clean$RHISPANIC1 <- case_when(
      df_clean$RHISPANIC1 == 1 ~ "Not Hispanic",
      df_clean$RHISPANIC1 == 2 ~ "Hispanic",
      is.na(df_clean$RHISPANIC1) ~ NA_character_,
      TRUE ~ "Unknown"
    )
  }
  
  if("RRACE1" %in% names(df_clean)) {
    df_clean$RRACE1 <- case_when(
      df_clean$RRACE1 == 1 ~ "White, Alone",
      df_clean$RRACE1 == 2 ~ "Black, Alone",
      df_clean$RRACE1 == 3 ~ "Asian, Alone",
      df_clean$RRACE1 == 4 ~ "Other/Multiple",
      is.na(df_clean$RRACE1) ~ NA_character_,
      TRUE ~ "Unknown"
    )
  }
  
  if("MARITAL1" %in% names(df_clean)) {
    df_clean$MARITAL1 <- case_when(
      df_clean$MARITAL1 == 1 ~ "Married",
      df_clean$MARITAL1 == 2 ~ "Domestic partnership",
      df_clean$MARITAL1 == 4 ~ "Divorced",
      df_clean$MARITAL1 == 5 ~ "Separated",
      df_clean$MARITAL1 == 6 ~ "Never married",
      is.na(df_clean$MARITAL1) ~ NA_character_,
      TRUE ~ "Unknown"
    )
  }
  
  # MENTAL HEALTH - ORDINAL (1-4 Likert scale)
  if("ANXIOUS" %in% names(df_clean)) {
    df_clean$ANXIOUS_cat <- case_when(
      df_clean$ANXIOUS == 1 ~ "Not at all",
      df_clean$ANXIOUS == 2 ~ "Several days",
      df_clean$ANXIOUS == 3 ~ "More than half the days",
      df_clean$ANXIOUS == 4 ~ "Nearly every day",
      is.na(df_clean$ANXIOUS) ~ NA_character_,
      TRUE ~ "Unknown"
    )
    df_clean$ANXIOUS_numeric <- df_clean$ANXIOUS
    df_clean$ANXIOUS <- df_clean$ANXIOUS_cat
  }
  
  if("WORRY" %in% names(df_clean)) {
    df_clean$WORRY_cat <- case_when(
      df_clean$WORRY == 1 ~ "Not at all",
      df_clean$WORRY == 2 ~ "Several days",
      df_clean$WORRY == 3 ~ "More than half the days",
      df_clean$WORRY == 4 ~ "Nearly every day",
      is.na(df_clean$WORRY) ~ NA_character_,
      TRUE ~ "Unknown"
    )
    df_clean$WORRY_numeric <- df_clean$WORRY
    df_clean$WORRY <- df_clean$WORRY_cat
  }
  
  # EMPLOYMENT - BINARY
  if("ANYWORK" %in% names(df_clean)) {
    # Create labeled version
    df_clean$ANYWORK_labeled <- haven::labelled(
      as.integer(case_when(
        df_clean$ANYWORK == "Yes" ~ 1,
        df_clean$ANYWORK == "No" ~ 2,
        TRUE ~ NA_integer_
      )),
      c("1) Yes" = 1, "2) No" = 2)
    )
    # Keep character version for tables
    df_clean$ANYWORK <- case_when(
      df_clean$ANYWORK == 1 ~ "1) Yes",
      df_clean$ANYWORK == 2 ~ "2) No",
      is.na(df_clean$ANYWORK) ~ NA_character_,
      TRUE ~ "Unknown"
    )
  }
  
  if("WRKLOSSRV" %in% names(df_clean)) {
    # Create labeled version
    df_clean$WRKLOSSRV_labeled <- haven::labelled(
      as.integer(case_when(
        df_clean$WRKLOSSRV == "Yes" ~ 1,
        df_clean$WRKLOSSRV == "No" ~ 2,
        TRUE ~ NA_integer_
      )),
      c("1) Yes" = 1, "2) No" = 2)
    )
    # Keep character version for tables
    df_clean$WRKLOSSRV <- case_when(
      df_clean$WRKLOSSRV == 1 ~ "1) Yes",
      df_clean$WRKLOSSRV == 2 ~ "2) No",
      is.na(df_clean$WRKLOSSRV) ~ NA_character_,
      TRUE ~ "Unknown"
    )
  }
  
  # ECONOMIC STRESS - ORDINAL
  if("EXPNS_DIF" %in% names(df_clean)) {
    df_clean$EXPNS_DIF_cat <- case_when(
      df_clean$EXPNS_DIF == 1 ~ "1) Not at all difficult",
      df_clean$EXPNS_DIF == 2 ~ "2) A little difficult",
      df_clean$EXPNS_DIF == 3 ~ "3) Somewhat difficult",
      df_clean$EXPNS_DIF == 4 ~ "4) Very difficult",
      is.na(df_clean$EXPNS_DIF) ~ NA_character_,
      TRUE ~ "Unknown"
    )
    df_clean$EXPNS_DIF_numeric <- df_clean$EXPNS_DIF
    # Add value labels to numeric version
    df_clean$EXPNS_DIF_numeric <- haven::labelled(
      df_clean$EXPNS_DIF_numeric,
      c("Not at all difficult" = 1, 
        "A little difficult" = 2, 
        "Somewhat difficult" = 3, 
        "Very difficult" = 4)
    )
    df_clean$EXPNS_DIF <- df_clean$EXPNS_DIF_cat
  }
  
  # PRIMARY OUTCOME - FOOD SECURITY - ORDINAL
  if("CURFOODSUF" %in% names(df_clean)) {
    df_clean$CURFOODSUF_cat <- case_when(
      df_clean$CURFOODSUF == 1 ~ "1) Enough, kinds wanted (Secure)",
      df_clean$CURFOODSUF == 2 ~ "2) Enough, not kinds wanted (Marginal)",
      df_clean$CURFOODSUF == 3 ~ "3) Sometimes not enough (Sometimes Insecure)",
      df_clean$CURFOODSUF == 4 ~ "4) Often not enough (Often Insecure)",
      is.na(df_clean$CURFOODSUF) ~ NA_character_,
      TRUE ~ "Unknown"
    )
    df_clean$CURFOODSUF_numeric <- df_clean$CURFOODSUF
    # Add value labels to numeric version
    df_clean$CURFOODSUF_numeric <- haven::labelled(
      df_clean$CURFOODSUF_numeric,
      c("Secure" = 1, 
        "Marginal" = 2, 
        "Sometimes Insecure" = 3, 
        "Often Insecure" = 4)
    )
    df_clean$CURFOODSUF <- df_clean$CURFOODSUF_cat
  }
  
  # HOUSING - BINARY
  if("RENTCUR" %in% names(df_clean)) {
    df_clean$RENTCUR <- case_when(
      df_clean$RENTCUR == 1 ~ "Current on rent",
      df_clean$RENTCUR == 2 ~ "Behind on rent",
      is.na(df_clean$RENTCUR) ~ NA_character_,
      TRUE ~ "Unknown"
    )
  }
  
  if("MORTCUR" %in% names(df_clean)) {
    df_clean$MORTCUR <- case_when(
      df_clean$MORTCUR == 1 ~ "Current on mortgage",
      df_clean$MORTCUR == 2 ~ "Behind on mortgage",
      is.na(df_clean$MORTCUR) ~ NA_character_,
      TRUE ~ "Unknown"
    )
  }
  
  # EVICTION RISK - ORDINAL
  if("EVICT" %in% names(df_clean)) {
    df_clean$EVICT_cat <- case_when(
      df_clean$EVICT == 1 ~ "Very likely",
      df_clean$EVICT == 2 ~ "Somewhat likely",
      df_clean$EVICT == 3 ~ "Not very likely",
      df_clean$EVICT == 4 ~ "Not likely at all",
      is.na(df_clean$EVICT) ~ NA_character_,
      TRUE ~ "Unknown"
    )
    df_clean$EVICT_numeric <- df_clean$EVICT
    df_clean$EVICT <- df_clean$EVICT_cat
  }
  
  # HEALTH INSURANCE - BINARY
  if("HLTHINS1" %in% names(df_clean)) {
    df_clean$HLTHINS1 <- case_when(
      df_clean$HLTHINS1 == 1 ~ "Has employer insurance",
      df_clean$HLTHINS1 == 2 ~ "No employer insurance",
      is.na(df_clean$HLTHINS1) ~ NA_character_,
      TRUE ~ "Unknown"
    )
  }
  
  # FOOD BARRIERS - BINARY
  for(col in c("FOODRSNRV1", "FOODRSNRV2", "FOODRSNRV3")) {
    if(col %in% names(df_clean)) {
      df_clean[[col]] <- case_when(
        df_clean[[col]] == 1 ~ "Yes",
        is.na(df_clean[[col]]) ~ NA_character_,
        TRUE ~ "No"
      )
    }
  }
  
  # EMERGENCY FOOD USE - BINARY
  if("FREEFOOD" %in% names(df_clean)) {
    df_clean$FREEFOOD <- case_when(
      df_clean$FREEFOOD == 1 ~ "Yes",
      df_clean$FREEFOOD == 2 ~ "No",
      is.na(df_clean$FREEFOOD) ~ NA_character_,
      TRUE ~ "Unknown"
    )
  }
  
  # FOOD PROGRAM BENEFITS - BINARY
  for(col in c("FDBENEFIT1", "FDBENEFIT2", "FDBENEFIT3", "FDBENEFIT4", "FDBENEFIT5")) {
    if(col %in% names(df_clean)) {
      df_clean[[col]] <- case_when(
        df_clean[[col]] == 1 ~ "Yes",
        is.na(df_clean[[col]]) ~ NA_character_,
        TRUE ~ "No"
      )
    }
  }
  
  # SCHOOL FOOD EXPENSES - NOMINAL
  if("SCHLFDEXPNS" %in% names(df_clean)) {
    df_clean$SCHLFDEXPNS <- case_when(
      df_clean$SCHLFDEXPNS == 1 ~ "1) Yes, difficulty",
      df_clean$SCHLFDEXPNS == 2 ~ "2) No difficulty",
      df_clean$SCHLFDEXPNS == 3 ~ "3) N/A (no school food costs)",
      is.na(df_clean$SCHLFDEXPNS) ~ NA_character_,
      TRUE ~ "Unknown"
    )
  }
  
  # TRANSPORTATION ACCESS - ORDINAL
  if("ACCESS_TRANSP" %in% names(df_clean)) {
    df_clean$ACCESS_TRANSP_cat <- case_when(
      df_clean$ACCESS_TRANSP == 1 ~ "1) Enough",
      df_clean$ACCESS_TRANSP == 2 ~ "2) Enough, not preferred kinds",
      df_clean$ACCESS_TRANSP == 3 ~ "3) Sometimes not enough",
      df_clean$ACCESS_TRANSP == 4 ~ "4) Often not enough",
      df_clean$ACCESS_TRANSP == 5 ~ "5) Always not enough",
      is.na(df_clean$ACCESS_TRANSP) ~ NA_character_,
      TRUE ~ "Unknown"
    )
    df_clean$ACCESS_TRANSP_numeric <- df_clean$ACCESS_TRANSP
    # Add value labels to numeric version
    df_clean$ACCESS_TRANSP_numeric <- haven::labelled(
      df_clean$ACCESS_TRANSP_numeric,
      c("Enough" = 1, 
        "Enough, not preferred" = 2, 
        "Sometimes not enough" = 3, 
        "Often not enough" = 4,
        "Always not enough" = 5)
    )
    df_clean$ACCESS_TRANSP <- df_clean$ACCESS_TRANSP_cat
  }
  
  # HOUSEHOLD CHILDREN - BINARY
  for(col in c("KIDS_LT1Y", "KIDS_1_4Y", "KIDS_5_11Y", "KIDS_12_17Y")) {
    if(col %in% names(df_clean)) {
      df_clean[[col]] <- case_when(
        df_clean[[col]] == 1 ~ "Yes",
        is.na(df_clean[[col]]) ~ NA_character_,
        TRUE ~ "No"
      )
    }
  }
  
  # ================================================================
  # CREATE COMPOSITE VARIABLES
  # ================================================================
  
  if("KIDS_LT1Y" %in% names(df_clean)) {
    df_clean$has_children <- ifelse(
      (df_clean$KIDS_LT1Y == "Yes") | (df_clean$KIDS_1_4Y == "Yes") | 
        (df_clean$KIDS_5_11Y == "Yes") | (df_clean$KIDS_12_17Y == "Yes"), 
      1, 0
    )
    
    df_clean$has_school_age <- ifelse(
      (df_clean$KIDS_5_11Y == "Yes") | (df_clean$KIDS_12_17Y == "Yes"), 
      1, 0
    )
    
    df_clean$has_young_children <- ifelse(
      (df_clean$KIDS_LT1Y == "Yes") | (df_clean$KIDS_1_4Y == "Yes"), 
      1, 0
    )
  }
  
  # Housing stability
  if("RENTCUR" %in% names(df_clean) & "MORTCUR" %in% names(df_clean)) {
    df_clean$housing_stable <- ifelse(
      (df_clean$RENTCUR == "Current on rent") | (df_clean$MORTCUR == "Current on mortgage"), 
      1, 
      ifelse((df_clean$RENTCUR == "Behind on rent") | (df_clean$MORTCUR == "Behind on mortgage"), 0, NA)
    )
  }
  
  # ================================================================
  # REORDER COLUMNS
  # ================================================================
  
  col_order <- c("SCRAMID", "PWEIGHT", "wave_source", "age")
  other_cols <- setdiff(names(df_clean), col_order)
  df_clean <- df_clean %>% dplyr::select(all_of(col_order), all_of(other_cols))
  
  cat(sprintf("✓ %s cleaned: %d rows × %d cols\n", wave_name, nrow(df_clean), ncol(df_clean)))
  
  return(df_clean)
}

####################################################################
# 4. CLEAN ALL THREE HPS DATASETS
####################################################################

cat("\nCleaning all three HPS datasets...\n")
cat("-" %+% strrep("-", 78) %+% "\n\n")

hps_october_clean <- clean_hps_data(hps_oct_2024, "October 2024")
hps_december_clean <- clean_hps_data(hps_dec_2024, "December 2024")
hps_phase42_clean <- clean_hps_data(hps_phase42, "Phase 4.2 (Cycle 09)")

####################################################################
# 5. SAVE CLEANED DATASETS
####################################################################

cat("\nSaving cleaned datasets...\n")
cat("-" %+% strrep("-", 78) %+% "\n\n")

write_csv(hps_october_clean, file.path(results_dir, "hps_october2024_clean.csv"))
write_csv(hps_december_clean, file.path(results_dir, "hps_december2024_clean.csv"))
write_csv(hps_phase42_clean, file.path(results_dir, "hps_phase42_clean.csv"))

cat(sprintf("✓ Saved: hps_october2024_clean.csv (%d rows)\n", nrow(hps_october_clean)))
cat(sprintf("✓ Saved: hps_december2024_clean.csv (%d rows)\n", nrow(hps_december_clean)))
cat(sprintf("✓ Saved: hps_phase42_clean.csv (%d rows)\n\n", nrow(hps_phase42_clean)))

# Also save as RData
save(hps_october_clean, hps_december_clean, hps_phase42_clean,
     file = file.path(results_dir, "hps_cleaned_datasets.RData"))

cat(sprintf("✓ Saved: hps_cleaned_datasets.RData\n\n"))

####################################################################
# PHASE 2: SURVEY-WEIGHTED ORDERED LOGIT (POLR) ANALYSIS
####################################################################

cat("\n" %+% strrep("=", 80) %+% "\n")
cat("PHASE 2: SURVEY-WEIGHTED ORDERED LOGIT (POLR) ANALYSIS\n")
cat(strrep("=", 80) %+% "\n\n")

####################################################################
# STEP 1: LOAD AND PREPARE DATA
####################################################################

cat("\n" %+% strrep("=", 80) %+% "\n")
cat("STEP 1: LOADING AND PREPARING DATA\n")
cat(strrep("=", 80) %+% "\n\n")

# Create household size of children variable
add_hhld_numkid <- function(df) {
  df %>%
    dplyr::rowwise() %>%
    dplyr::mutate(
      HHLD_NUMKID = sum(
        c(KIDS_LT1Y, KIDS_1_4Y, KIDS_5_11Y, KIDS_12_17Y) == "Yes",
        na.rm = TRUE
      )
    ) %>%
    dplyr::ungroup()
}

# Standardize household child count across all datasets
standardize_hhld_numkid <- function(df) {
  if ("THHLD_NUMKID" %in% names(df) && !("HHLD_NUMKID" %in% names(df))) {
    df <- df %>% dplyr::rename(HHLD_NUMKID = THHLD_NUMKID)
  }
  if ("AHHLD_NUMKID" %in% names(df) && !("HHLD_NUMKID" %in% names(df))) {
    df <- df %>% dplyr::rename(HHLD_NUMKID = AHHLD_NUMKID)
  }
  return(df)
}

# Combine training datasets
training_raw <- dplyr::bind_rows(
  hps_october_clean %>% add_hhld_numkid() %>% standardize_hhld_numkid() %>% dplyr::mutate(dataset = "October 2024"),
  hps_phase42_clean %>% add_hhld_numkid() %>% standardize_hhld_numkid() %>% dplyr::mutate(dataset = "Phase 4.2")
)

# Test dataset
test_raw <- hps_december_clean %>%
  add_hhld_numkid() %>%
  standardize_hhld_numkid() %>%
  dplyr::mutate(dataset = "December 2024")

# Define analysis variables
predictors <- c(
  "EXPNS_DIF_numeric", "WRKLOSSRV", "ANYWORK", "SCHLFDEXPNS",
  "ACCESS_TRANSP_numeric", "HHLD_NUMKID"
)

outcome <- "CURFOODSUF_numeric"

assistance_vars <- c("FDBENEFIT1", "FDBENEFIT2", "FDBENEFIT3", 
                     "FDBENEFIT4", "FREEFOOD")

# Prepare training data
training_data <- training_raw %>%
  dplyr::select(SCRAMID, PWEIGHT, all_of(c(outcome, predictors, assistance_vars)), dataset) %>%
  dplyr::filter(!is.na(!!sym(outcome))) %>%
  tidyr::drop_na(all_of(c(outcome, predictors, "PWEIGHT")))

# Prepare test data
test_data <- test_raw %>%
  dplyr::select(SCRAMID, PWEIGHT, all_of(c(outcome, predictors, assistance_vars)), dataset) %>%
  dplyr::filter(!is.na(!!sym(outcome))) %>%
  tidyr::drop_na(all_of(c(outcome, predictors, "PWEIGHT")))

cat(sprintf("Training: %d rows\n", nrow(training_data)))
cat(sprintf("  - Weighted sum (PWEIGHT): %.0f\n", sum(training_data$PWEIGHT)))
cat(sprintf("  - Mean weight (before scaling): %.2f\n\n", mean(training_data$PWEIGHT)))

# Scale weights for numerical stability in polr
training_data <- training_data %>%
  dplyr::mutate(PWEIGHT_scaled = PWEIGHT / mean(PWEIGHT))

test_data <- test_data %>%
  dplyr::mutate(PWEIGHT_scaled = PWEIGHT / mean(PWEIGHT))

cat(sprintf("Test: %d rows\n", nrow(test_data)))
cat(sprintf("After scaling for numerical stability:\n"))
cat(sprintf("  - Training weighted sum: %.0f\n", sum(training_data$PWEIGHT_scaled)))
cat(sprintf("  - Training mean weight: %.2f\n", mean(training_data$PWEIGHT_scaled)))
cat(sprintf("  - Test weighted sum: %.0f\n", sum(test_data$PWEIGHT_scaled)))
cat(sprintf("  - Test mean weight: %.2f\n\n", mean(test_data$PWEIGHT_scaled)))

cat("Note: Weights scaled to mean=1 for convergence. Statistical inference\n")
cat("remains valid; relative weights preserved. Final estimates use original\n")
cat("PWEIGHT for population inference.\n\n")

####################################################################
# STEP 2: COERCE OUTCOME TO ORDERED FACTOR
####################################################################

cat("Coercing outcome to ordered factor...\n\n")

training_data <- training_data %>%
  dplyr::mutate(!!outcome := ordered(!!sym(outcome), levels = 1:4,
                                     labels = c("Secure", "Marginal", "Sometimes Insecure", "Often Insecure")))

test_data <- test_data %>%
  dplyr::mutate(!!outcome := ordered(!!sym(outcome), levels = 1:4,
                                     labels = c("Secure", "Marginal", "Sometimes Insecure", "Often Insecure")))

# Coerce factors for proper statistical treatment
# EXPNS_DIF_numeric: Ordinal (1-4 scale)
# ACCESS_TRANSP_numeric: Ordinal (1-5 scale)
# SCHLFDEXPNS: Categorical with explicit reference level

training_data <- training_data %>%
  dplyr::mutate(
    # Ordinal: Expense difficulty
    EXPNS_DIF_numeric = ordered(EXPNS_DIF_numeric, 
                                levels = 1:4,
                                labels = c("Not at all difficult", "A little difficult", 
                                           "Somewhat difficult", "Very difficult")),
    # Ordinal: Transportation access
    ACCESS_TRANSP_numeric = ordered(ACCESS_TRANSP_numeric, 
                                    levels = 1:5,
                                    labels = c("Enough", "Enough, not preferred", 
                                               "Sometimes not enough", "Often not enough", 
                                               "Always not enough")),
    # Categorical: School food expenses - Reference: "Yes, difficulty"
    SCHLFDEXPNS = factor(SCHLFDEXPNS, 
                         levels = c("1) Yes, difficulty", "2) No difficulty", 
                                    "3) N/A (no school food costs)"))
  )

test_data <- test_data %>%
  dplyr::mutate(
    # Ordinal: Expense difficulty
    EXPNS_DIF_numeric = ordered(EXPNS_DIF_numeric, 
                                levels = 1:4,
                                labels = c("Not at all difficult", "A little difficult", 
                                           "Somewhat difficult", "Very difficult")),
    # Ordinal: Transportation access
    ACCESS_TRANSP_numeric = ordered(ACCESS_TRANSP_numeric, 
                                    levels = 1:5,
                                    labels = c("Enough", "Enough, not preferred", 
                                               "Sometimes not enough", "Often not enough", 
                                               "Always not enough")),
    # Categorical: School food expenses - Reference: "Yes, difficulty"
    SCHLFDEXPNS = factor(SCHLFDEXPNS, 
                         levels = c("1) Yes, difficulty", "2) No difficulty", 
                                    "3) N/A (no school food costs)"))
  )

####################################################################
# STEP 3: FIT SURVEY-WEIGHTED ORDERED LOGIT (POLR)
####################################################################

cat("\n" %+% strrep("=", 80) %+% "\n")
cat("STEP 3: FITTING SURVEY-WEIGHTED ORDERED LOGIT (POLR)\n")
cat(strrep("=", 80) %+% "\n\n")

cat("Model Specification:\n")
cat(sprintf("  Package: MASS::polr()\n"))
cat(sprintf("  Outcome: %s (4-category ordinal)\n", outcome))
cat(sprintf("  Predictors: %s\n", paste(predictors, collapse = ", ")))
cat("  Weights: PWEIGHT (survey weights from HPS)\n")
cat("  Link: logit (proportional odds model)\n\n")

# Build formula with polynomial contrasts for ordinal variables
# Create polynomial contrasts for EXPNS_DIF_numeric (4 levels → 3 contrasts)
contrasts(training_data$EXPNS_DIF_numeric) <- contr.poly(4)
contrasts(test_data$EXPNS_DIF_numeric) <- contr.poly(4)

# Create polynomial contrasts for ACCESS_TRANSP_numeric (5 levels → 4 contrasts)
contrasts(training_data$ACCESS_TRANSP_numeric) <- contr.poly(5)
contrasts(test_data$ACCESS_TRANSP_numeric) <- contr.poly(5)

# Build formula - polynomial contrasts will be automatically applied
formula_spec <- as.formula(paste(outcome, "~", paste(predictors, collapse = " + ")))

# Fit survey-weighted POLR
polr_model <- polr(
  formula_spec, 
  data = training_data, 
  weights = PWEIGHT_scaled,
  Hess = TRUE
)

cat("✓ Survey-weighted ordered logit fitted successfully\n")
cat("✓ Polynomial contrasts applied to ordinal variables\n\n")

# Model diagnostics
cat("Model Fit:\n")
cat(sprintf("  Log-likelihood: %.2f\n", logLik(polr_model)))
cat(sprintf("  AIC: %.2f\n", AIC(polr_model)))
cat(sprintf("  Residual Deviance: %.2f\n\n", polr_model$deviance))

model_summary <- summary(polr_model)
print(model_summary)

# Extract and display coefficients with p-values
coef_table <- data.frame(coef(model_summary))
coef_table$p_value <- round((pnorm(abs(coef_table$t.value), lower.tail = FALSE) * 2), 4)
coef_table$Odds_Ratio <- round(exp(coef_table$Value), 4)

cat("\n\nCoefficients with Odds Ratios & P-values:\n")
cat("-" %+% strrep("-", 78) %+% "\n")
print(coef_table)

# Create Adjusted Odds Ratios Table with 95% CI using gtsummary
cat("\n\n" %+% strrep("=", 80) %+% "\n")
cat("TABLE 3: ADJUSTED ODDS RATIOS WITH 95% CONFIDENCE INTERVALS\n")
cat(strrep("=", 80) %+% "\n\n")

# Custom tidy function: converts polr t-values to two-sided p-values via
# normal approximation (standard approach for MASS::polr, which gives z/t
# but no p-value by default). Exponentiates estimates to Odds Ratios and
# computes 95% CIs on the OR scale manually.
polr_tidy_with_p <- function(x, exponentiate = FALSE, ...) {
  s <- summary(x)
  ct <- data.frame(coef(s))
  # Keep only predictor rows (not intercepts/zeta)
  ct <- ct[!rownames(ct) %in% names(x$zeta), , drop = FALSE]
  
  log_est  <- ct$Value
  se       <- ct$Std..Error
  z        <- ct$t.value
  p_val    <- pnorm(abs(z), lower.tail = FALSE) * 2
  
  # Compute 95% CI on log scale then exponentiate
  ci_low  <- log_est - 1.96 * se
  ci_high <- log_est + 1.96 * se
  
  dplyr::tibble(
    term      = rownames(ct),
    estimate  = exp(log_est),   # OR
    conf.low  = exp(ci_low),
    conf.high = exp(ci_high),
    std.error = se,
    statistic = z,
    p.value   = p_val
  )
}

# Create the regression table with exponentiated ORs, 95% CIs, and p-values
table3 <- tbl_regression(
  polr_model,
  tidy_fun     = polr_tidy_with_p,
  exponentiate = FALSE,   # already exponentiated inside tidy_fun
  label = list(
    WRKLOSSRV ~ "Work Loss or Reservation",
    ANYWORK ~ "Any Employment",
    SCHLFDEXPNS ~ "School Food Expenses",
    HHLD_NUMKID ~ "Number of Children"
  )
) %>%
  bold_labels() %>%
  italicize_levels() %>%
  bold_p() %>%
  modify_table_styling(
    columns = label,
    footnote = "Polynomial contrasts applied to ordinal variables: L = Linear, Q = Quadratic, C = Cubic, ^4 = Quartic trends. P-values derived from t-values via two-sided normal approximation (standard for MASS::polr)."
  )

print(table3)

# Save Table 3 as HTML
cat("\nSaving Table 3 as HTML...\n")
table3_gt <- table3 %>% as_gt() %>%
  gt::tab_header(
    title = "Table 3: Adjusted Odds Ratios with 95% Confidence Intervals",
    subtitle = "Survey-Weighted Proportional Odds Logistic Regression Model"
  ) %>%
  gt::tab_style(
    style = gt::cell_borders(sides = "bottom", weight = gt::px(2)),
    locations = gt::cells_column_labels()
  )
gt::gtsave(table3_gt, file.path(results_dir, "Table3_Adjusted_Odds_Ratios.html"))
cat("✓ Saved: Table3_Adjusted_Odds_Ratios.html\n")

# Save Table 3 as Word (landscape orientation)
cat("Saving Table 3 as Word...\n")
table3_flex <- table3 %>% as_flex_table() %>%
  flextable::set_table_properties(layout = "autofit", width = 1)
save_as_docx("Table 3: Adjusted Odds Ratios with 95% Confidence Intervals" = table3_flex,
             path = file.path(results_dir, "Table3_Adjusted_Odds_Ratios.docx"),
             pr_section = prop_section(page_size = page_size(orient = "landscape", width = 8.3, height = 11.7)))
cat("✓ Saved: Table3_Adjusted_Odds_Ratios.docx\n\n")

cat("\n\nThresholds (Intercepts):\n")
cat("-" %+% strrep("-", 78) %+% "\n")
intercepts <- data.frame(Threshold = polr_model$zeta)
print(intercepts)

cat("\n\nInterpretation of Coefficients:\n")
cat("When a predictor increases by 1 unit, the log-odds of being in a higher\n")
cat("category (more food insecure) changes by the coefficient value.\n")
cat("Odds ratios > 1 indicate increased likelihood of higher insecurity.\n\n")

####################################################################
# STEP 6B: MARGINAL EFFECTS PLOT (Reviewer Response)
####################################################################

cat("\n" %+% strrep("=", 80) %+% "\n")
cat("STEP 6B: MARGINAL EFFECTS PLOT\n")
cat(strrep("=", 80) %+% "\n\n")

if (!requireNamespace("ggeffects", quietly = TRUE)) install.packages("ggeffects")
library(ggeffects)

# Predicted probabilities holding all other vars at their mode/mean
mep_raw <- ggpredict(polr_model, terms = "EXPNS_DIF_numeric")

# Stack into tidy df (ggeffects returns list for polr, single df for newer versions)
if (inherits(mep_raw, "list")) {
  mep_df <- dplyr::bind_rows(
    lapply(names(mep_raw), function(lvl) {
      df <- as.data.frame(mep_raw[[lvl]])
      df$response.level <- lvl
      df
    })
  )
} else {
  mep_df <- as.data.frame(mep_raw)
}

# Clean up x-axis and outcome labels
mep_df <- mep_df %>%
  dplyr::mutate(
    x_label = dplyr::case_when(
      as.character(x) %in% c("1", "Not at all difficult") ~ "Not at all\ndifficult",
      as.character(x) %in% c("2", "A little difficult")   ~ "A little\ndifficult",
      as.character(x) %in% c("3", "Somewhat difficult")   ~ "Somewhat\ndifficult",
      as.character(x) %in% c("4", "Very difficult")       ~ "Very\ndifficult",
      TRUE ~ as.character(x)
    ),
    x_label = factor(x_label,
                     levels = c("Not at all\ndifficult", "A little\ndifficult",
                                "Somewhat\ndifficult", "Very\ndifficult")),
    outcome_level = dplyr::case_when(
      grepl("Secure", response.level, ignore.case = TRUE) &
        !grepl("Marginal|Insecure", response.level, ignore.case = TRUE) ~ "Food Secure",
      grepl("Marginal",  response.level, ignore.case = TRUE) ~ "Marginally Food Secure",
      grepl("Sometimes", response.level, ignore.case = TRUE) ~ "Sometimes Food Insecure",
      grepl("Often",     response.level, ignore.case = TRUE) ~ "Often Food Insecure",
      TRUE ~ response.level
    ),
    outcome_level = factor(outcome_level,
                           levels = c("Food Secure", "Marginally Food Secure",
                                      "Sometimes Food Insecure", "Often Food Insecure"))
  )

cat(sprintf("  Marginal effects: %d rows across %d outcome levels\n",
            nrow(mep_df), length(unique(mep_df$outcome_level))))

fig_mep <- ggplot(mep_df, aes(x = x_label, y = predicted,
                              colour = outcome_level, group = outcome_level)) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high, fill = outcome_level),
              alpha = 0.12, colour = NA) +
  geom_line(linewidth = 1.1) +
  geom_point(size = 3) +
  scale_y_continuous(
    labels = scales::percent_format(accuracy = 1),
    limits = c(0, NA),
    expand = expansion(mult = c(0, 0.05))
  ) +
  scale_colour_manual(
    name = "Food Security Status",
    values = c(
      "Food Secure"             = "#2166ac",
      "Marginally Food Secure"  = "#74add1",
      "Sometimes Food Insecure" = "#f46d43",
      "Often Food Insecure"     = "#d73027"
    )
  ) +
  scale_fill_manual(
    name = "Food Security Status",
    values = c(
      "Food Secure"             = "#2166ac",
      "Marginally Food Secure"  = "#74add1",
      "Sometimes Food Insecure" = "#f46d43",
      "Often Food Insecure"     = "#d73027"
    )
  ) +
  labs(
    title    = "Predicted Probability of Food Security Status\nby Household Expense Difficulty",
    subtitle = "Survey-weighted proportional odds model; 95% CIs shown. All other predictors held at observed means/modes.",
    x        = "Expense Difficulty",
    y        = "Predicted Probability",
    caption  = "Source: Household Pulse Survey (HPS). Model: MASS::polr() with PWEIGHT survey weights."
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title         = element_text(face = "bold", size = 14, hjust = 0),
    plot.subtitle      = element_text(size = 10, colour = "grey40", hjust = 0),
    plot.caption       = element_text(size = 9,  colour = "grey50", hjust = 0),
    axis.title         = element_text(face = "bold"),
    legend.position    = "bottom",
    legend.title       = element_text(face = "bold"),
    legend.key.width   = unit(1.5, "cm"),
    panel.grid.minor   = element_blank(),
    panel.grid.major.x = element_blank()
  ) +
  guides(colour = guide_legend(nrow = 2, byrow = TRUE),
         fill   = guide_legend(nrow = 2, byrow = TRUE))

ggsave(file.path(results_dir, "Figure4_Marginal_Effects_ExpenseDifficulty.png"),
       plot = fig_mep, width = 9, height = 6, dpi = 300, bg = "white")
ggsave(file.path(results_dir, "Figure4_Marginal_Effects_ExpenseDifficulty.pdf"),
       plot = fig_mep, width = 9, height = 6, bg = "white")

cat("Saved: Figure4_Marginal_Effects_ExpenseDifficulty.png\n")
cat("Saved: Figure4_Marginal_Effects_ExpenseDifficulty.pdf\n\n")

####################################################################
# STEP 7: PREDICTIONS & RISK STRATIFICATION
####################################################################

cat("\n" %+% strrep("=", 80) %+% "\n")
cat("STEP 7: PREDICTIONS & RISK STRATIFICATION\n")
cat(strrep("=", 80) %+% "\n\n")

cat("Prediction approach:\n")
cat("  - Method: Predicted probabilities via predict(type='probs')\n")
cat("  - Output: P(Secure), P(Marginal), P(Sometimes), P(Often)\n")
cat("  - Food insecurity: P(Sometimes) + P(Often)\n\n")

# Generate probability predictions
pred_probs <- predict(polr_model, newdata = test_data, type = "probs")

# Ensure it's a matrix
if (!is.matrix(pred_probs)) {
  pred_probs <- as.matrix(pred_probs)
}

cat(sprintf("  Prediction matrix dimensions: %d rows × %d cols\n", nrow(pred_probs), ncol(pred_probs)))
cat(sprintf("  Columns: %s\n\n", paste(colnames(pred_probs), collapse = ", ")))

# Sanity checks
if (nrow(pred_probs) != nrow(test_data)) {
  stop(sprintf("ERROR: Row count mismatch (%d pred vs %d test)", nrow(pred_probs), nrow(test_data)))
}
if (ncol(pred_probs) != 4) {
  stop(sprintf("ERROR: Expected 4 probability columns, got %d", ncol(pred_probs)))
}

# Compute food insecurity probability
prob_insecure <- pred_probs[, 3] + pred_probs[, 4]

cat("Computing food insecurity probability...\n")
cat(sprintf("  Mean P(insecure): %.4f\n", mean(prob_insecure)))
cat(sprintf("  Median P(insecure): %.4f\n", median(prob_insecure)))
cat(sprintf("  SD P(insecure): %.4f\n", sd(prob_insecure)))
cat(sprintf("  Range: [%.4f, %.4f]\n", min(prob_insecure), max(prob_insecure)))
cat(sprintf("  Quantiles: 25%%=%.4f, 50%%=%.4f, 75%%=%.4f, 90%%=%.4f\n\n",
            quantile(prob_insecure, 0.25),
            quantile(prob_insecure, 0.50),
            quantile(prob_insecure, 0.75),
            quantile(prob_insecure, 0.90)))

# Add to test data
test_data <- test_data %>%
  dplyr::mutate(prob_insecure = prob_insecure)

# Survey-weighted 70th percentile risk threshold
cat("Calculating survey-weighted risk threshold...\n")

test_design <- svydesign(ids = ~1, weights = ~PWEIGHT, data = test_data)
risk_threshold_result <- svyquantile(~prob_insecure, test_design, quantiles = 0.80)

if (is.matrix(risk_threshold_result)) {
  risk_threshold <- as.numeric(risk_threshold_result[1, 1])
} else if (is.list(risk_threshold_result)) {
  risk_threshold <- as.numeric(risk_threshold_result[[1]][1])
} else {
  risk_threshold <- as.numeric(risk_threshold_result[1])
}

unweighted_70 <- quantile(prob_insecure, 0.80)

cat(sprintf("  Survey-weighted 70th percentile: %.4f\n", risk_threshold))
cat(sprintf("  Unweighted 70th percentile: %.4f\n", unweighted_70))
cat(sprintf("  Difference: %.4f\n\n", risk_threshold - unweighted_70))

# Risk classification
test_data <- test_data %>%
  dplyr::mutate(
    risk_flag = as.numeric(prob_insecure >= risk_threshold),
    risk_level = if_else(risk_flag == 1, "High Risk", "Standard Risk")
  )

# Check distribution
risk_dist <- test_data %>%
  dplyr::group_by(risk_level) %>%
  dplyr::summarise(
    n = n(),
    pct = 100 * n() / nrow(test_data),
    mean_prob = mean(prob_insecure),
    sd_prob = sd(prob_insecure),
    .groups = "drop"
  )

cat("Risk Distribution (Unweighted):\n")
print(risk_dist)

# Survey-weighted distribution
test_design_with_risk <- svydesign(ids = ~1, weights = ~PWEIGHT, data = test_data)
cat("\nRisk Distribution (Survey-Weighted):\n")
risk_dist_weighted <- svytable(~risk_level, test_design_with_risk)
print(risk_dist_weighted)

risk_pct_weighted <- prop.table(risk_dist_weighted) * 100
cat("\nPercentages (Survey-Weighted):\n")
print(risk_pct_weighted)
cat("\n")

####################################################################
# STEP 8: ASSISTANCE PROGRAM IDENTIFICATION
####################################################################

cat("\n" %+% strrep("=", 80) %+% "\n")
cat("STEP 8: ASSISTANCE PROGRAM IDENTIFICATION\n")
cat(strrep("=", 80) %+% "\n\n")

test_data <- test_data %>%
  dplyr::mutate(
    receives_any_assistance = as.numeric(
      (FDBENEFIT1 == "Yes") | (FDBENEFIT2 == "Yes") | (FDBENEFIT3 == "Yes") |
        (FDBENEFIT4 == "Yes") | (FREEFOOD == "Yes")
    ),
    receives_any_assistance = if_else(is.na(receives_any_assistance), 0, receives_any_assistance),
    assistance_status = case_when(
      risk_flag == 0 ~ "Not High-Risk",
      risk_flag == 1 & receives_any_assistance == 1 ~ "Captured (High-Risk + Assistance)",
      risk_flag == 1 & receives_any_assistance == 0 ~ "MISSED (High-Risk + No Assistance)"
    ),
    num_programs = rowSums(dplyr::select(., FDBENEFIT1, FDBENEFIT2, FDBENEFIT3, 
                                         FDBENEFIT4, FREEFOOD) == "Yes", na.rm = TRUE)
  )

cat("Summary of Assistance Status:\n")
assistance_summary <- test_data %>%
  dplyr::group_by(assistance_status) %>%
  dplyr::summarise(
    n = n(),
    pct = 100 * n() / nrow(test_data),
    mean_risk = mean(prob_insecure),
    mean_programs = mean(num_programs),
    .groups = "drop"
  )
print(assistance_summary)

####################################################################
# STEP 9: MISSED HOUSEHOLD ANALYSIS
####################################################################

cat("\n\n" %+% strrep("=", 80) %+% "\n")
cat("STEP 9: MISSED HIGH-RISK HOUSEHOLDS\n")
cat(strrep("=", 80) %+% "\n\n")

high_risk_data <- test_data %>% filter(risk_flag == 1)

cat(sprintf("High-Risk Households (n=%d):\n\n", nrow(high_risk_data)))

missed_summary <- high_risk_data %>%
  dplyr::group_by(assistance_status) %>%
  dplyr::summarise(
    n = n(),
    pct_high_risk = 100 * n() / nrow(high_risk_data),
    mean_risk = mean(prob_insecure),
    median_risk = median(prob_insecure),
    mean_programs = mean(num_programs),
    .groups = "drop"
  )

print(missed_summary)

####################################################################
# TABLE 4: RISK STRATIFICATION & ASSISTANCE COVERAGE (gtsummary)
####################################################################

cat("\n\n" %+% strrep("=", 80) %+% "\n")
cat("TABLE 4: RISK STRATIFICATION & ASSISTANCE PROGRAM COVERAGE\n")
cat(strrep("=", 80) %+% "\n\n")

# Create comprehensive Table 4 with survey-weighted estimates
table4_data <- test_data %>%
  dplyr::group_by(assistance_status) %>%
  dplyr::summarise(
    weighted_n = sum(PWEIGHT),
    unweighted_n = n(),
    pct_population = 100 * sum(PWEIGHT) / sum(test_data$PWEIGHT),
    mean_risk = mean(prob_insecure),
    pct_assistance = 100 * sum(receives_any_assistance) / n(),
    .groups = "drop"
  ) %>%
  dplyr::mutate(
    Group = dplyr::case_when(
      assistance_status == "Not High-Risk" ~ "Not High-Risk",
      assistance_status == "Captured (High-Risk + Assistance)" ~ "High-Risk + Assistance",
      assistance_status == "MISSED (High-Risk + No Assistance)" ~ "High-Risk + No Assistance"
    ),
    Group = factor(Group, levels = c("Not High-Risk", "High-Risk + Assistance", "High-Risk + No Assistance"))
  ) %>%
  dplyr::select(Group, weighted_n, pct_population, mean_risk, pct_assistance) %>%
  dplyr::arrange(Group)

# Convert to data frame for gtsummary
table4_display <- data.frame(
  Group = table4_data$Group,
  `Weighted N` = round(table4_data$weighted_n, 0),
  `% of Population` = sprintf("%.1f%%", table4_data$pct_population),
  `Mean Risk Score` = sprintf("%.3f", table4_data$mean_risk),
  `% Receiving Assistance` = sprintf("%.1f%%", table4_data$pct_assistance),
  check.names = FALSE
)

# Create gt table
table4 <- table4_display %>%
  gt::gt() %>%
  gt::tab_header(
    title = "Table 4: Risk Stratification and Assistance Program Coverage",
    subtitle = "Survey-Weighted Distribution of High-Risk Households"
  ) %>%
  gt::tab_style(
    style = gt::cell_text(weight = "bold"),
    locations = gt::cells_column_labels()
  ) %>%
  gt::tab_style(
    style = gt::cell_borders(sides = "bottom", weight = gt::px(2)),
    locations = gt::cells_column_labels()
  ) %>%
  gt::cols_align(align = "center", columns = c(2, 3, 4, 5))

print(table4_display)

# Save Table 4 as HTML
cat("\nSaving Table 4 as HTML...\n")
gt::gtsave(table4, file.path(results_dir, "Table4_Risk_Stratification.html"))
cat("✓ Saved: Table4_Risk_Stratification.html\n")

# Save Table 4 as Word (landscape orientation)
cat("Saving Table 4 as Word...\n")
table4_flex <- table4_display %>%
  flextable::flextable() %>%
  flextable::set_table_properties(layout = "autofit", width = 1) %>%
  flextable::bold(part = "header")

save_as_docx("Table 4: Risk Stratification and Assistance Program Coverage" = table4_flex,
             path = file.path(results_dir, "Table4_Risk_Stratification.docx"),
             pr_section = prop_section(page_size = page_size(orient = "landscape", width = 8.3, height = 11.7)))
cat("✓ Saved: Table4_Risk_Stratification.docx\n\n")


####################################################################
# STEP 10: SAVE RESULTS
####################################################################

cat("\n\n" %+% strrep("=", 80) %+% "\n")
cat("STEP 10: SAVING RESULTS\n")
cat(strrep("=", 80) %+% "\n\n")

# Full predictions
predictions_export <- test_data %>%
  dplyr::select(SCRAMID, PWEIGHT, all_of(outcome), prob_insecure, risk_level,
                receives_any_assistance, num_programs, assistance_status, all_of(predictors)) %>%
  dplyr::arrange(desc(prob_insecure))

write_csv(predictions_export, 
          file.path(results_dir, "december_predictions_risk_classification.csv"))
cat("✓ Saved: december_predictions_risk_classification.csv\n")

# Risk stratification table
risk_table <- test_data %>%
  dplyr::group_by(risk_level) %>%
  dplyr::summarise(
    n = n(),
    pct_all = 100 * n() / nrow(test_data),
    sum_pweight = sum(PWEIGHT),
    mean_risk = mean(prob_insecure),
    median_risk = median(prob_insecure),
    sd_risk = sd(prob_insecure),
    .groups = "drop"
  )

write_csv(risk_table,
          file.path(results_dir, "risk_stratification_summary.csv"))
cat("✓ Saved: risk_stratification_summary.csv\n")

# Assistance status detail
assistance_detail <- test_data %>%
  dplyr::group_by(assistance_status) %>%
  dplyr::summarise(
    n = n(),
    pct_all = 100 * n() / nrow(test_data),
    sum_pweight = sum(PWEIGHT),
    mean_risk = mean(prob_insecure),
    median_risk = median(prob_insecure),
    mean_programs = mean(num_programs),
    .groups = "drop"
  )

write_csv(assistance_detail,
          file.path(results_dir, "assistance_status_detail.csv"))
cat("✓ Saved: assistance_status_detail.csv\n")

# Missed households
missed_households <- test_data %>%
  dplyr::filter(assistance_status == "MISSED (High-Risk + No Assistance)") %>%
  dplyr::select(SCRAMID, prob_insecure, PWEIGHT, all_of(predictors), num_programs) %>%
  dplyr::arrange(desc(prob_insecure))

write_csv(missed_households,
          file.path(results_dir, "missed_high_risk_households.csv"))
cat("✓ Saved: missed_high_risk_households.csv\n")

# Model summary
sink(file.path(results_dir, "polr_survey_weighted_model_summary.txt"))
cat("SURVEY-WEIGHTED ORDERED LOGIT (POLR) MODEL\n")
cat(strrep("=", 80) %+% "\n\n")
cat("Model Specification:\n")
cat("  Package: MASS::polr()\n")
cat("  Outcome: CURFOODSUF_numeric (4-category ordinal)\n")
cat("  Weights: PWEIGHT (HPS survey weights)\n")
cat("  Link: logit (proportional odds)\n\n")
cat("Data:\n")
cat(sprintf("  Training observations: %d\n", nrow(training_data)))
cat(sprintf("  Training weighted sum: %.0f\n", sum(training_data$PWEIGHT)))
cat(sprintf("  Test observations: %d\n", nrow(test_data)))
cat(sprintf("  Test weighted sum: %.0f\n\n", sum(test_data$PWEIGHT)))
cat("Model Fit:\n")
cat(sprintf("  Log-likelihood: %.2f\n", logLik(polr_model)))
cat(sprintf("  AIC: %.2f\n", AIC(polr_model)))
cat(sprintf("  Residual Deviance: %.2f\n\n", polr_model$deviance))
print(summary(polr_model))
sink()

cat("✓ Saved: polr_survey_weighted_model_summary.txt\n")

# Results summary
results_summary <- tibble(
  Metric = c(
    "Training sample size (n)",
    "Test sample size (n)",
    "Number of predictors",
    "Risk threshold (weighted 70th %ile)",
    "High-risk households (n)",
    "High-risk households (%)",
    "Captured (high-risk + assistance)",
    "Captured (%)",
    "MISSED (high-risk + no assistance)",
    "MISSED (%)",
    "Mean risk score",
    "Median risk score"
  ),
  Value = c(
    nrow(training_data),
    nrow(test_data),
    length(predictors),
    sprintf("%.4f", risk_threshold),
    nrow(high_risk_data),
    sprintf("%.1f%%", 100 * nrow(high_risk_data) / nrow(test_data)),
    sum(high_risk_data$receives_any_assistance),
    sprintf("%.1f%%", 100 * sum(high_risk_data$receives_any_assistance) / nrow(high_risk_data)),
    sum(high_risk_data$receives_any_assistance == 0),
    sprintf("%.1f%%", 100 * sum(high_risk_data$receives_any_assistance == 0) / nrow(high_risk_data)),
    sprintf("%.3f", mean(test_data$prob_insecure)),
    sprintf("%.3f", median(test_data$prob_insecure))
  )
)

write_csv(results_summary,
          file.path(results_dir, "analysis_results_summary.csv"))
cat("✓ Saved: analysis_results_summary.csv\n")

####################################################################
# STEP 11: SURVEY-WEIGHTED DESCRIPTIVE STATISTICS (TABLE 1)
####################################################################

cat("\n\n" %+% strrep("=", 80) %+% "\n")
cat("STEP 11: SURVEY-WEIGHTED DESCRIPTIVE STATISTICS\n")
cat(strrep("=", 80) %+% "\n\n")

# Install gtsummary if needed
if (!require("gtsummary", quietly = TRUE)) {
  cat("Installing gtsummary for table generation...\n")
  install.packages("gtsummary")
  library(gtsummary)
}

# Create Table 1: Descriptive Statistics (Survey-Weighted with svydesign)
cat("Creating Table 1: Survey-Weighted Descriptive Statistics...\n")

# Create survey design object with PWEIGHT
cat("  Creating survey design with PWEIGHT...\n")
training_survey_design <- svydesign(
  ids = ~1,              # No clustering (simple random sample within strata)
  weights = ~PWEIGHT,    # Person weights from HPS
  data = training_data
)

cat(sprintf("  Sample size: %d (weighted to ~%.0f households)\n", 
            nrow(training_data),
            sum(training_data$PWEIGHT, na.rm = TRUE)))
cat(sprintf("  Outcome: 4-category ordered factor\n"))
cat(sprintf("  All statistics: SURVEY-WEIGHTED\n\n"))

# Create Table 1 using survey design
table1 <- training_survey_design %>%
  tbl_svysummary(
    by = NULL,  # No stratification for Table 1
    include = c(!!sym(outcome), EXPNS_DIF_numeric, WRKLOSSRV, ANYWORK, 
                SCHLFDEXPNS, ACCESS_TRANSP_numeric, HHLD_NUMKID),
    label = list(
      CURFOODSUF_numeric ~ "Food Security Status",
      EXPNS_DIF_numeric ~ "Expense Difficulty (1-4)",
      WRKLOSSRV ~ "Work Loss or Reservation",
      ANYWORK ~ "Any Employment",
      SCHLFDEXPNS ~ "School Food Expenses",
      ACCESS_TRANSP_numeric ~ "Transportation Access (1-5)",
      HHLD_NUMKID ~ "Number of Children"
    ),
    statistic = list(
      all_categorical() ~ "{n} ({p}%)",
      all_continuous() ~ "{mean} ({sd})"
    ),
    digits = list(
      all_categorical() ~ c(0, 1),
      all_continuous() ~ c(2, 2)
    ),
    missing = "no",
    percent = "column"
  ) %>%
  add_n(weights = ~PWEIGHT)

cat("✓ Table 1 created with survey design (PWEIGHT applied)\n\n")

# Save Table 1 as HTML
cat("Saving Table 1 as HTML...\n")
table1_gt <- table1 %>% as_gt()
gt::gtsave(table1_gt, file.path(results_dir, "Table1_Descriptive_Statistics.html"))
cat("✓ Saved: Table1_Descriptive_Statistics.html\n")

# Save Table 1 as Word document
if (!require("flextable", quietly = TRUE)) {
  install.packages("flextable")
  library(flextable)
}

cat("Saving Table 1 as Word...\n")
table1_flex <- table1 %>% as_flex_table()
save_as_docx("Table 1: Survey-Weighted Descriptive Statistics" = table1_flex,
             path = file.path(results_dir, "Table1_Descriptive_Statistics.docx"))
cat("✓ Saved: Table1_Descriptive_Statistics.docx\n")

####################################################################
# STEP 12: SURVEY-WEIGHTED BIVARIATE ANALYSIS (TABLE 2)
####################################################################

cat("\n\n" %+% strrep("=", 80) %+% "\n")
cat("STEP 12: SURVEY-WEIGHTED BIVARIATE ANALYSIS\n")
cat(strrep("=", 80) %+% "\n\n")

cat("Creating Table 2: Survey-Weighted Bivariate Associations...\n")

# Create bivariate table by outcome (4-category ordinal)
cat("Note: CURFOODSUF_numeric is 4-category ordinal outcome\n")
cat("Stratifying by all 4 food security levels (NO binary simplification)\n")
cat("Using survey design with PWEIGHT (person weights)\n\n")

# Create table2 using survey design object
# The survey_design already has PWEIGHT built in
table2 <- training_survey_design %>%
  tbl_svysummary(
    by = !!sym(outcome),  # Stratify by the ordered factor outcome
    include = c(EXPNS_DIF_numeric, WRKLOSSRV, ANYWORK, SCHLFDEXPNS, 
                ACCESS_TRANSP_numeric, HHLD_NUMKID),
    label = list(
      EXPNS_DIF_numeric ~ "Expense Difficulty (1-4)",
      WRKLOSSRV ~ "Work Loss or Reservation",
      ANYWORK ~ "Any Employment",
      SCHLFDEXPNS ~ "School Food Expenses",
      ACCESS_TRANSP_numeric ~ "Transportation Access (1-5)",
      HHLD_NUMKID ~ "Number of Children"
    ),
    statistic = list(
      all_categorical() ~ "{n} ({p}%)",
      all_continuous() ~ "{mean} ({sd})"
    ),
    digits = list(
      all_categorical() ~ c(0, 1),
      all_continuous() ~ c(2, 2)
    ),
    missing = "no",
    percent = "column"
  ) %>%
  add_overall(last = TRUE) %>%
  bold_labels() %>%
  italicize_levels() %>%
  add_stat_label(label = all_categorical() ~ "No. (%)")

# Variables: categorical use svychisq, continuous use svyranktest (Kruskal-Wallis)
t2_cat_vars  <- c("WRKLOSSRV", "ANYWORK", "SCHLFDEXPNS")
t2_cont_vars <- c("EXPNS_DIF_numeric", "ACCESS_TRANSP_numeric", "HHLD_NUMKID")
t2_vars <- c("EXPNS_DIF_numeric", "WRKLOSSRV", "ANYWORK",
             "SCHLFDEXPNS", "ACCESS_TRANSP_numeric", "HHLD_NUMKID")

t2_pvals <- sapply(t2_vars, function(v) {
  tryCatch({
    if (v %in% t2_cat_vars) {
      frm <- as.formula(paste0("~", v, " + ", outcome))
      svychisq(frm, design = training_survey_design, statistic = "Chisq")$p.value
    } else {
      # Survey-weighted Kruskal-Wallis via svyranktest
      frm <- as.formula(paste0("~", v, " + ", outcome))
      svyranktest(frm, design = training_survey_design)$p.value
    }
  }, error = function(e) NA_real_)
})

t2_pval_df <- dplyr::tibble(
  variable = t2_vars,
  p_fmt    = style_pvalue(unname(t2_pvals), digits = 3)
)

table2 <- table2 %>%
  modify_table_body(
    ~ .x %>%
      dplyr::left_join(t2_pval_df, by = "variable") %>%
      dplyr::mutate(p_fmt = ifelse(row_type == "label", p_fmt, NA_character_))
  ) %>%
  modify_header(p_fmt = "**p-value**") %>%
  modify_column_alignment(columns = p_fmt, align = "center")

cat("✓ Table 2 created with survey design (PWEIGHT applied by outcome)\n\n")

# Save Table 2 as HTML
cat("Saving Table 2 as HTML...\n")
table2_gt <- table2 %>% as_gt() %>%
  gt::tab_header(
    title = "Table 2: Survey-Weighted Bivariate Associations",
    subtitle = "by 4-Category Food Security Status"
  ) %>%
  gt::tab_style(
    style = gt::cell_borders(sides = "bottom", weight = gt::px(2)),
    locations = gt::cells_column_labels()
  )
gt::gtsave(table2_gt, file.path(results_dir, "Table2_Bivariate_Associations.html"))
cat("✓ Saved: Table2_Bivariate_Associations.html\n")

# Save Table 2 as Word (landscape orientation for better readability)
cat("Saving Table 2 as Word...\n")
table2_flex <- table2 %>% as_flex_table() %>%
  flextable::set_table_properties(layout = "autofit", width = 1)
save_as_docx("Table 2: Survey-Weighted Bivariate Associations (by 4-category Food Security)" = table2_flex,
             path = file.path(results_dir, "Table2_Bivariate_Associations.docx"),
             pr_section = prop_section(page_size = page_size(orient = "landscape", width = 8.3, height = 11.7)))
cat("✓ Saved: Table2_Bivariate_Associations.docx\n")

####################################################################
# STEP 12B: TABLES 1 & 2 FOR ALL THREE WAVES COMBINED
####################################################################

cat("\n\n" %+% strrep("=", 80) %+% "\n")
cat("STEP 12B: TABLES 1 & 2 - ALL THREE WAVES (Oct + Phase4.2 + Dec)\n")
cat(strrep("=", 80) %+% "\n\n")

# Create combined dataset from all three waves
all_waves_data <- dplyr::bind_rows(
  hps_october_clean %>% add_hhld_numkid() %>% standardize_hhld_numkid() %>% dplyr::mutate(dataset = "October 2024"),
  hps_phase42_clean %>% add_hhld_numkid() %>% standardize_hhld_numkid() %>% dplyr::mutate(dataset = "Phase 4.2"),
  hps_december_clean %>% add_hhld_numkid() %>% standardize_hhld_numkid() %>% dplyr::mutate(dataset = "December 2024")
)

# Prepare all waves data
all_waves_for_tables <- all_waves_data %>%
  dplyr::select(SCRAMID, PWEIGHT, all_of(c(outcome, predictors)), dataset) %>%
  dplyr::filter(!is.na(!!sym(outcome))) %>%
  tidyr::drop_na(all_of(c(outcome, predictors, "PWEIGHT"))) %>%
  # Coerce outcome to ordered factor (same as training data)
  dplyr::mutate(!!outcome := ordered(!!sym(outcome), levels = 1:4,
                                     labels = c("Secure", "Marginal", "Sometimes Insecure", "Often Insecure")))

cat(sprintf("All three waves combined: %d rows\n", nrow(all_waves_for_tables)))
cat(sprintf("  - October 2024: %d\n", sum(all_waves_for_tables$dataset == "October 2024")))
cat(sprintf("  - Phase 4.2: %d\n", sum(all_waves_for_tables$dataset == "Phase 4.2")))
cat(sprintf("  - December 2024: %d\n\n", sum(all_waves_for_tables$dataset == "December 2024")))

# TABLE 1 - ALL WAVES (with survey design and PWEIGHT)
cat("Creating Table 1 (All Waves) - with survey design...\n")

# Create survey design for all waves data
all_waves_survey_design <- svydesign(
  ids = ~1,
  weights = ~PWEIGHT,
  data = all_waves_for_tables
)

cat(sprintf("  Survey design created: %d observations\n", nrow(all_waves_for_tables)))
cat(sprintf("  Weighted to ~%.0f households\n", sum(all_waves_for_tables$PWEIGHT, na.rm = TRUE)))
cat(sprintf("  All statistics: SURVEY-WEIGHTED\n\n"))

table1_all_waves <- all_waves_survey_design %>%
  tbl_svysummary(
    by = NULL,
    include = c(!!sym(outcome), EXPNS_DIF_numeric, WRKLOSSRV, ANYWORK, 
                SCHLFDEXPNS, ACCESS_TRANSP_numeric, HHLD_NUMKID),
    label = list(
      CURFOODSUF_numeric ~ "Food Security Status",
      EXPNS_DIF_numeric ~ "Expense Difficulty (1-4)",
      WRKLOSSRV ~ "Work Loss or Reservation",
      ANYWORK ~ "Any Employment",
      SCHLFDEXPNS ~ "School Food Expenses",
      ACCESS_TRANSP_numeric ~ "Transportation Access (1-5)",
      HHLD_NUMKID ~ "Number of Children"
    ),
    statistic = list(
      all_categorical() ~ "{n} ({p}%)",
      all_continuous() ~ "{mean} ({sd})"
    ),
    digits = list(
      all_categorical() ~ c(0, 1),
      all_continuous() ~ c(2, 2)
    ),
    missing = "no",
    percent = "column"
  ) %>%
  add_n(weights = ~PWEIGHT)

cat("✓ Table 1 (All Waves) created with survey design (PWEIGHT applied)\n")

# Save Table 1 - All Waves
cat("Saving Table 1 (All Waves) as HTML...\n")
table1_all_gt <- table1_all_waves %>% as_gt()
gt::gtsave(table1_all_gt, file.path(results_dir, "Table1_Descriptive_AllWaves.html"))
cat("✓ Saved: Table1_Descriptive_AllWaves.html\n")

cat("Saving Table 1 (All Waves) as Word...\n")
table1_all_flex <- table1_all_waves %>% as_flex_table()
save_as_docx("Table 1: Descriptive Statistics - All Three Waves Combined" = table1_all_flex,
             path = file.path(results_dir, "Table1_Descriptive_AllWaves.docx"))
cat("✓ Saved: Table1_Descriptive_AllWaves.docx\n\n")

# TABLE 2 - ALL WAVES (with survey design and PWEIGHT)
cat("Creating Table 2 (All Waves Bivariate) - with survey design...\n")

table2_all_waves <- all_waves_survey_design %>%
  tbl_svysummary(
    by = !!sym(outcome),
    include = c(EXPNS_DIF_numeric, WRKLOSSRV, ANYWORK, SCHLFDEXPNS, 
                ACCESS_TRANSP_numeric, HHLD_NUMKID),
    label = list(
      EXPNS_DIF_numeric ~ "Expense Difficulty (1-4)",
      WRKLOSSRV ~ "Work Loss or Reservation",
      ANYWORK ~ "Any Employment",
      SCHLFDEXPNS ~ "School Food Expenses",
      ACCESS_TRANSP_numeric ~ "Transportation Access (1-5)",
      HHLD_NUMKID ~ "Number of Children"
    ),
    statistic = list(
      all_categorical() ~ "{n} ({p}%)",
      all_continuous() ~ "{mean} ({sd})"
    ),
    digits = list(
      all_categorical() ~ c(0, 1),
      all_continuous() ~ c(2, 2)
    ),
    missing = "no",
    percent = "column"
  ) %>%
  add_overall(last = TRUE) %>%
  bold_labels() %>%
  italicize_levels()

# All-waves: categorical use svychisq, continuous use svyranktest (Kruskal-Wallis)
t2aw_pvals <- sapply(t2_vars, function(v) {
  tryCatch({
    if (v %in% t2_cat_vars) {
      frm <- as.formula(paste0("~", v, " + ", outcome))
      svychisq(frm, design = all_waves_survey_design, statistic = "Chisq")$p.value
    } else {
      frm <- as.formula(paste0("~", v, " + ", outcome))
      svyranktest(frm, design = all_waves_survey_design)$p.value
    }
  }, error = function(e) NA_real_)
})

t2aw_pval_df <- dplyr::tibble(
  variable = t2_vars,
  p_fmt    = style_pvalue(unname(t2aw_pvals), digits = 3)
)

table2_all_waves <- table2_all_waves %>%
  modify_table_body(
    ~ .x %>%
      dplyr::left_join(t2aw_pval_df, by = "variable") %>%
      dplyr::mutate(p_fmt = ifelse(row_type == "label", p_fmt, NA_character_))
  ) %>%
  modify_header(p_fmt = "**p-value**") %>%
  modify_column_alignment(columns = p_fmt, align = "center")

cat("✓ Table 2 (All Waves) created with survey design (PWEIGHT applied by outcome)\n")

# Save Table 2 - All Waves
cat("Saving Table 2 (All Waves Bivariate) as HTML...\n")
table2_all_gt <- table2_all_waves %>% as_gt()
gt::gtsave(table2_all_gt, file.path(results_dir, "Table2_Bivariate_AllWaves.html"))
cat("✓ Saved: Table2_Bivariate_AllWaves.html\n")

cat("Saving Table 2 (All Waves Bivariate) as Word...\n")
table2_all_flex <- table2_all_waves %>% as_flex_table()
save_as_docx("Table 2: Bivariate Associations - All Three Waves Combined" = table2_all_flex,
             path = file.path(results_dir, "Table2_Bivariate_AllWaves.docx"))
cat("✓ Saved: Table2_Bivariate_AllWaves.docx\n")

####################################################################
# STEP 13: CALIBRATION PLOTS
####################################################################

cat("\n\n" %+% strrep("=", 80) %+% "\n")
cat("STEP 13: ORDINAL LOGISTIC REGRESSION CALIBRATION PLOTS\n")
cat(strrep("=", 80) %+% "\n\n")

# Install ggplot2 if needed
if (!require("ggplot2", quietly = TRUE)) {
  install.packages("ggplot2")
  library(ggplot2)
}

# ────────────────────────────────────────────────────────────────
# FIGURE 1: CATEGORY-SPECIFIC CALIBRATION
# ────────────────────────────────────────────────────────────────

cat("Creating Figure 1: Category-Specific Calibration Plot...\n")

# Compute predicted probabilities for each category
pred_probs_test <- predict(polr_model, newdata = test_data, type = "probs")

if (!is.matrix(pred_probs_test)) {
  pred_probs_test <- as.matrix(pred_probs_test)
}

category_labels <- c("Secure", "Marginal", "Sometimes Insecure", "Often Insecure")

# Create calibration data for each category
calibration_cat <- data.frame()

for (k in 1:4) {
  # Predicted probability for category k
  pred_k <- pred_probs_test[, k]
  
  # Observed: indicator for category k
  obs_k <- as.numeric(test_data[[outcome]] == category_labels[k])
  
  # Bin into deciles
  pred_quantiles <- ntile(pred_k, 5)
  quantile_labels <- paste0("Q", pred_quantiles)
  
  cal_data_k <- data.frame(
    category = k,
    category_label = category_labels[k],
    predicted = pred_k,
    observed = obs_k,
    decile = pred_quantiles,
    weight = test_data$PWEIGHT
  )
  
  # Summarize by quantile (survey-weighted)
  cal_summary_k <- cal_data_k %>%
    dplyr::filter(!is.na(decile)) %>%
    dplyr::group_by(category, category_label, decile) %>%
    dplyr::summarise(
      pred_mean = weighted.mean(predicted, w = weight, na.rm = TRUE),
      obs_mean = weighted.mean(observed, w = weight, na.rm = TRUE),
      n = n(),
      weight_sum = sum(weight),
      .groups = "drop"
    )
  
  calibration_cat <- rbind(calibration_cat, cal_summary_k)
}

# Plot Figure 1
fig1 <- ggplot(calibration_cat, aes(x = pred_mean, y = obs_mean)) +
  geom_point(size = 3, color = "steelblue", alpha = 0.7) +
  geom_smooth(method = "loess", se = TRUE, color = "darkblue", fill = "lightblue", alpha = 0.2) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "red", size = 1) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "red", size = 1) +
  facet_wrap(~category_label, nrow = 2, ncol = 2) +
  labs(
    title = "Figure 1: Category-Specific Calibration Plot",
    subtitle = "Survey-Weighted Predicted vs Observed Probabilities",
    x = "Predicted P(Category)",
    y = "Observed Proportion",
    caption = "Perfect calibration: points lie on diagonal. LOESS smoother with 95% CI."
  ) +
  xlim(0, 1) + ylim(0, 1) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(size = 12),
    facet.text = element_text(face = "bold"),
    panel.grid.minor = element_blank()
  )

ggsave(file.path(results_dir, "Figure1_Category_Calibration.png"), fig1, 
       width = 12, height = 10, dpi = 300)
ggsave(file.path(results_dir, "Figure1_Category_Calibration.pdf"), fig1, 
       width = 12, height = 10)

cat("✓ Saved: Figure1_Category_Calibration.png/pdf\n")

# ────────────────────────────────────────────────────────────────
# FIGURE 2: CUMULATIVE PROBABILITY CALIBRATION (ORDINAL-AWARE)
# ────────────────────────────────────────────────────────────────

cat("\nCreating Figure 2: Cumulative Probability Calibration Plot...\n")

# Compute cumulative probabilities
cum_pred_probs <- apply(pred_probs_test, 1, cumsum)
cum_pred_probs <- t(cum_pred_probs)

# Observed cumulative probabilities
outcome_numeric <- as.numeric(test_data[[outcome]])
cum_obs_probs <- matrix(0, nrow = nrow(test_data), ncol = 3)
for (k in 1:3) {
  cum_obs_probs[, k] <- as.numeric(outcome_numeric <= (k + 1))
}

# Threshold labels
threshold_labels <- c(
  "Secure | Marginal",
  "Marginal | Sometimes Insecure",
  "Sometimes Insecure | Often Insecure"
)

# Create calibration data for cumulative probabilities
calibration_cum <- data.frame()

for (k in 1:3) {
  pred_k <- cum_pred_probs[, k]
  obs_k <- cum_obs_probs[, k]
  
  # Bin into deciles
  pred_quantiles <- ntile(pred_k, 5)
  quantile_labels <- paste0("Q", pred_quantiles)
  
  cal_data_k <- data.frame(
    threshold = k,
    threshold_label = threshold_labels[k],
    predicted = pred_k,
    observed = obs_k,
    decile = pred_quantiles,
    weight = test_data$PWEIGHT
  )
  
  # Summarize by quantile
  cal_summary_k <- cal_data_k %>%
    dplyr::filter(!is.na(decile)) %>%
    dplyr::group_by(threshold, threshold_label, decile) %>%
    dplyr::summarise(
      pred_mean = weighted.mean(predicted, w = weight, na.rm = TRUE),
      obs_mean = weighted.mean(observed, w = weight, na.rm = TRUE),
      n = n(),
      weight_sum = sum(weight),
      mae = weighted.mean(abs(predicted - observed), w = weight, na.rm = TRUE),
      .groups = "drop"
    )
  
  calibration_cum <- rbind(calibration_cum, cal_summary_k)
}

# Plot Figure 2
fig2 <- ggplot(calibration_cum, aes(x = pred_mean, y = obs_mean)) +
  geom_point(size = 3, color = "darkgreen", alpha = 0.7) +
  geom_smooth(method = "loess", se = TRUE, color = "darkgreen", fill = "lightgreen", alpha = 0.2) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "red", size = 1) +
  facet_wrap(~threshold_label, nrow = 1, ncol = 3) +
  labs(
    title = "Figure 2: Cumulative Probability Calibration (Ordinal-Aware)",
    subtitle = "Most Statistically Correct for Proportional Odds Model",
    x = "Predicted P(Y ≤ k)",
    y = "Observed P(Y ≤ k)",
    caption = "Tests proportional odds assumption. Perfect calibration: points on diagonal."
  ) +
  xlim(0, 1) + ylim(0, 1) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(size = 12, color = "darkgreen"),
    facet.text = element_text(face = "bold"),
    panel.grid.minor = element_blank()
  )

ggsave(file.path(results_dir, "Figure2_Cumulative_Calibration.png"), fig2, 
       width = 14, height = 5, dpi = 300)
ggsave(file.path(results_dir, "Figure2_Cumulative_Calibration.pdf"), fig2, 
       width = 14, height = 5)

cat("✓ Saved: Figure2_Cumulative_Calibration.png/pdf\n")

# ────────────────────────────────────────────────────────────────
# FIGURE 3: GROUPED RISK CALIBRATION (DECILE PLOT)
# ────────────────────────────────────────────────────────────────

cat("\nCreating Figure 3: Grouped Risk Calibration Plot...\n")

# Define risk as P(Y >= "Sometimes Insecure") = P(Y=3) + P(Y=4)
risk_scores <- pred_probs_test[, 3] + pred_probs_test[, 4]

calibration_risk <- data.frame(
  predicted_risk = risk_scores,
  observed_hardship = as.numeric(test_data[[outcome]] %in% c("Sometimes Insecure", "Often Insecure")),
  weight = test_data$PWEIGHT,
  high_risk_flag = as.numeric(risk_scores >= risk_threshold)
)

# Create decile groups
calibration_risk <- calibration_risk %>%
  dplyr::arrange(predicted_risk) %>%
  dplyr::mutate(
    quintile = ntile(predicted_risk, 5)
  )

# Summarize by quantile
decile_summary <- calibration_risk %>%
  dplyr::group_by(quintile) %>%
  dplyr::summarise(
    pred_risk = weighted.mean(predicted_risk, w = weight, na.rm = TRUE),
    obs_risk = weighted.mean(observed_hardship, w = weight, na.rm = TRUE),
    n = n(),
    weight_sum = sum(weight),
    in_top20 = mean(high_risk_flag),
    .groups = "drop"
  ) %>%
  dplyr::mutate(
    quintile_label = paste0("Q", quintile),
    top20_color = ifelse(quintile >= 4, "Top 30%", "Lower 70%")
  )

# Plot Figure 3
fig3 <- ggplot(decile_summary, aes(x = pred_risk, y = obs_risk, color = top20_color, size = weight_sum)) +
  geom_point(alpha = 0.7) +
  geom_smooth(method = "loess", se = TRUE, color = "darkblue", fill = "lightblue", 
              alpha = 0.2, inherit.aes = FALSE, aes(x = pred_risk, y = obs_risk)) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "red", size = 1) +
  geom_vline(xintercept = risk_threshold, linetype = "dotted", color = "orange", size = 1,
             label = paste0("70th %ile = ", round(risk_threshold, 3))) +
  scale_color_manual(values = c("Top 30%" = "#d62728", "Lower 70%" = "steelblue")) +
  scale_size_continuous(name = "Weighted N", breaks = c(5e4, 1e5, 1.5e5)) +
  geom_text(aes(label = quintile_label), nudge_x = 0.02, nudge_y = 0.02, size = 3) +
  labs(
    title = "Figure 3: Grouped Risk Calibration Plot",
    subtitle = "Decile-Binned Survey-Weighted Predictions vs Observed Hardship",
    x = "Mean Predicted P(Food Hardship)",
    y = "Observed Proportion with Hardship",
    color = "Risk Group",
    caption = paste0("Vertical line: 70th percentile threshold (", round(risk_threshold, 3), 
                     "). Size of point: weighted sample size per decile.")
  ) +
  xlim(0, 1) + ylim(0, 1) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(size = 12),
    legend.position = "right",
    panel.grid.minor = element_blank()
  )

ggsave(file.path(results_dir, "Figure3_Grouped_Risk_Calibration.png"), fig3, 
       width = 12, height = 9, dpi = 300)
ggsave(file.path(results_dir, "Figure3_Grouped_Risk_Calibration.pdf"), fig3, 
       width = 12, height = 9)

cat("✓ Saved: Figure3_Grouped_Risk_Calibration.png/pdf\n")

# Save calibration summary to CSV
cat("\nSaving calibration summaries...\n")

write_csv(calibration_cat, file.path(results_dir, "calibration_category_summary.csv"))
cat("✓ Saved: calibration_category_summary.csv\n")

write_csv(calibration_cum, file.path(results_dir, "calibration_cumulative_summary.csv"))
cat("✓ Saved: calibration_cumulative_summary.csv\n")

write_csv(decile_summary, file.path(results_dir, "calibration_risk_decile_summary.csv"))
cat("✓ Saved: calibration_risk_decile_summary.csv\n")

####################################################################
# FINAL SUMMARY
####################################################################

cat("\n\n" %+% strrep("=", 80) %+% "\n")
cat("ANALYSIS COMPLETE - FINAL SUMMARY\n")
cat(strrep("=", 80) %+% "\n\n")

print(results_summary)

cat("\n\nKEY FINDINGS:\n")
cat("-" %+% strrep("-", 78) %+% "\n\n")

cat(sprintf("1. Risk Threshold (Weighted 70th Percentile): %.4f\n", risk_threshold))
cat(sprintf("   → Households with P(food insecurity) ≥ %.4f classified as HIGH-RISK\n", risk_threshold))
cat(sprintf("   → Represents top 30%% of population by predicted risk\n\n"))

cat(sprintf("2. High-Risk Population: %d households (%.1f%% of test sample)\n",
            nrow(high_risk_data),
            100 * nrow(high_risk_data) / nrow(test_data)))

cat(sprintf("3. Program Reach:\n"))
cat(sprintf("   → Captured: %d households (%.1f%% of high-risk)\n",
            sum(high_risk_data$receives_any_assistance),
            100 * sum(high_risk_data$receives_any_assistance) / nrow(high_risk_data)))
cat(sprintf("   → MISSED: %d households (%.1f%% of high-risk)\n\n",
            sum(high_risk_data$receives_any_assistance == 0),
            100 * sum(high_risk_data$receives_any_assistance == 0) / nrow(high_risk_data)))

cat("4. Model Fit:\n")
cat(sprintf("   → Log-likelihood: %.2f\n", logLik(polr_model)))
cat(sprintf("   → AIC: %.2f\n", AIC(polr_model)))
cat(sprintf("   → Residual Deviance: %.2f\n\n", polr_model$deviance))

cat("5. Prediction Quality:\n")
cat(sprintf("   → Mean predicted risk: %.3f\n", mean(test_data$prob_insecure)))
cat(sprintf("   → Risk range: [%.3f, %.3f]\n", 
            min(test_data$prob_insecure), max(test_data$prob_insecure)))

cat("\n\nOUTPUT FILES (23 total):\n")
cat("-" %+% strrep("-", 78) %+% "\n\n")

cat("📊 DATA CLEANING:\n")
cat("  1. hps_october2024_clean.csv\n")
cat("  2. hps_december2024_clean.csv\n")
cat("  3. hps_phase42_clean.csv\n")
cat("  4. hps_cleaned_datasets.RData\n\n")

cat("📈 ANALYSIS TABLES & FIGURES:\n")
cat("  5. Table1_Descriptive_Statistics.html\n")
cat("  6. Table1_Descriptive_Statistics.docx\n")
cat("  7. Table2_Bivariate_Associations.html\n")
cat("  8. Table2_Bivariate_Associations.docx\n")
cat("  9. Figure1_Category_Calibration.png\n")
cat(" 10. Figure1_Category_Calibration.pdf\n")
cat(" 11. Figure2_Cumulative_Calibration.png\n")
cat(" 12. Figure2_Cumulative_Calibration.pdf\n")
cat(" 13. Figure3_Grouped_Risk_Calibration.png\n")
cat(" 14. Figure3_Grouped_Risk_Calibration.pdf\n\n")

cat("📋 PREDICTIONS & RESULTS:\n")
cat(" 15. december_predictions_risk_classification.csv\n")
cat(" 16. risk_stratification_summary.csv\n")
cat(" 17. assistance_status_detail.csv\n")
cat(" 18. missed_high_risk_households.csv\n")
cat(" 19. calibration_category_summary.csv\n")
cat(" 20. calibration_cumulative_summary.csv\n")
cat(" 21. calibration_risk_decile_summary.csv\n")
cat(" 22. polr_survey_weighted_model_summary.txt\n")
cat(" 23. analysis_results_summary.csv\n\n")

cat("✅ COMPLETE SNAP ANALYSIS PIPELINE FINISHED\n")
cat("✅ Survey-weighted descriptive & bivariate analysis complete\n")
cat("✅ Ordinal logistic regression with 3 calibration plots\n")
cat("✅ Publication-ready tables (HTML & Word)\n")
cat("✅ All outputs saved to results directory\n")
cat("✅ Ready for journal submission and policy briefing\n\n")