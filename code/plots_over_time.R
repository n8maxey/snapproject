####################################################################
# TEMPORAL PLOT - 3 WAVES (Chronological: Phase 4.2 → Oct → Dec)
# PUBLICATION QUALITY REFACTOR
# Features: 95% CIs, Colorblind-safe palettes, varying shapes/lines
####################################################################

library(MASS)
library(survey)
library(tidyverse)
library(scales)

# patchwork is used to compose multiple ggplots side-by-side
if (!requireNamespace("patchwork", quietly = TRUE)) {
  install.packages("patchwork")
}
library(patchwork)

# Paths
setwd("C:/Users/natmaxey/OneDrive - Indiana University/Desktop/snap project/data")
results_dir <- "C:/Users/natmaxey/OneDrive - Indiana University/Desktop/snap project/results"
if (!dir.exists(results_dir)) { dir.create(results_dir, recursive = TRUE) }
set.seed(42)

####################################################################
# 1. LOAD RAW DATA
####################################################################
cat("Loading HPS data files...\n")
hps_oct_2024 <- read_csv("HPS_OCTOBER2024_PUF.csv")
hps_dec_2024 <- read_csv("HPS_DECEMBER2024_PUF.csv")
hps_phase42  <- read_csv("hps_04_02_09_puf.csv")

####################################################################
# 2. COLUMNS TO KEEP
####################################################################
columns_to_keep <- c(
  "SCRAMID", "TAGE1", "TBIRTH_YEAR", "A_SEX1", "RHISPANIC1", "RRACE1", "MARITAL1",
  "ANXIOUS", "WORRY", "WRKLOSSRV", "ANYWORK", "EXPNS_DIF", "CURFOODSUF",
  "RENTCUR", "MORTCUR", "EVICT", "HLTHINS1",
  "FOODRSNRV1", "FOODRSNRV2", "FOODRSNRV3", "FREEFOOD",
  "FDBENEFIT1", "FDBENEFIT2", "FDBENEFIT3", "FDBENEFIT4", "FDBENEFIT5",
  "SCHLFDEXPNS", "ACCESS_TRANSP",
  "KIDS_LT1Y", "KIDS_1_4Y", "KIDS_5_11Y", "KIDS_12_17Y"
)

####################################################################
# 3. CLEANING FUNCTION - SAME DICTIONARIES, TABLE 1 LABELS
####################################################################
clean_hps_data <- function(df, wave_name) {
  
  cat(sprintf("Cleaning %s data...\n", wave_name))
  
  if("SCRAM" %in% names(df) & !("SCRAMID" %in% names(df))) {
    df <- df %>% dplyr::rename(SCRAMID = SCRAM)
  }
  
  available_cols <- intersect(columns_to_keep, names(df))
  df_clean <- df %>%
    dplyr::select(all_of(available_cols)) %>%
    dplyr::mutate(wave_source = wave_name)
  
  if("PWEIGHT" %in% names(df)) {
    df_clean$PWEIGHT <- df$PWEIGHT
  } else {
    df_clean$PWEIGHT <- NA_real_
  }
  
  numeric_cols <- df_clean %>% dplyr::select(where(is.numeric)) %>% names()
  numeric_cols <- setdiff(numeric_cols, c("PWEIGHT"))
  for(col in numeric_cols) {
    df_clean[[col]][df_clean[[col]] %in% c(-99, -88, -77)] <- NA
  }
  
  # FOOD SECURITY 
  if("CURFOODSUF" %in% names(df_clean)) {
    df_clean$CURFOODSUF <- case_when(
      df_clean$CURFOODSUF == 1 ~ "Secure",
      df_clean$CURFOODSUF == 2 ~ "Marginal",
      df_clean$CURFOODSUF == 3 ~ "Sometimes Insecure",
      df_clean$CURFOODSUF == 4 ~ "Often Insecure",
      TRUE ~ NA_character_
    )
  }
  
  # EXPENSE DIFFICULTY 
  if("EXPNS_DIF" %in% names(df_clean)) {
    df_clean$EXPNS_DIF <- case_when(
      df_clean$EXPNS_DIF == 1 ~ "Not at all difficult",
      df_clean$EXPNS_DIF == 2 ~ "A little difficult",
      df_clean$EXPNS_DIF == 3 ~ "Somewhat difficult",
      df_clean$EXPNS_DIF == 4 ~ "Very difficult",
      TRUE ~ NA_character_
    )
  }
  
  # EMPLOYMENT
  if("ANYWORK" %in% names(df_clean)) {
    df_clean$ANYWORK <- case_when(
      df_clean$ANYWORK == 1 ~ "Yes",
      df_clean$ANYWORK == 2 ~ "No",
      TRUE ~ NA_character_
    )
  }
  if("WRKLOSSRV" %in% names(df_clean)) {
    df_clean$WRKLOSSRV <- case_when(
      df_clean$WRKLOSSRV == 1 ~ "Yes",
      df_clean$WRKLOSSRV == 2 ~ "No",
      TRUE ~ NA_character_
    )
  }
  
  # SCHOOL FOOD EXPENSES
  if("SCHLFDEXPNS" %in% names(df_clean)) {
    df_clean$SCHLFDEXPNS <- case_when(
      df_clean$SCHLFDEXPNS == 1 ~ "Yes, difficulty",
      df_clean$SCHLFDEXPNS == 2 ~ "No difficulty",
      df_clean$SCHLFDEXPNS == 3 ~ "N/A (no school food costs)",
      TRUE ~ NA_character_
    )
  }
  
  # TRANSPORTATION ACCESS
  if("ACCESS_TRANSP" %in% names(df_clean)) {
    df_clean$ACCESS_TRANSP <- case_when(
      df_clean$ACCESS_TRANSP == 1 ~ "Enough transportation to meet your needs",
      df_clean$ACCESS_TRANSP == 2 ~ "Enough transportation, but not always the kinds you want to use",
      df_clean$ACCESS_TRANSP == 3 ~ "Sometimes not enough transportation to meet your needs",
      df_clean$ACCESS_TRANSP == 4 ~ "Often not enough transportation to meet your needs",
      df_clean$ACCESS_TRANSP == 5 ~ "Always not enough transportation to meet your needs",
      TRUE ~ NA_character_
    )
  }
  
  # HOUSEHOLD CHILDREN
  for(col in c("KIDS_LT1Y", "KIDS_1_4Y", "KIDS_5_11Y", "KIDS_12_17Y")) {
    if(col %in% names(df_clean)) {
      df_clean[[col]] <- case_when(
        df_clean[[col]] == 1 ~ "Yes",
        is.na(df_clean[[col]]) ~ NA_character_,
        TRUE ~ "No"
      )
    }
  }
  
  cat(sprintf("✓ %s cleaned: %d rows\n", wave_name, nrow(df_clean)))
  return(df_clean)
}

####################################################################
# 4. CLEAN ALL THREE WAVES & COMBINE
####################################################################
hps_october_clean  <- clean_hps_data(hps_oct_2024, "October 2024")
hps_december_clean <- clean_hps_data(hps_dec_2024, "December 2024")
hps_phase42_clean  <- clean_hps_data(hps_phase42,  "Phase 4.2 (Cycle 09)")

add_hhld_numkid <- function(df) {
  df %>%
    dplyr::rowwise() %>%
    dplyr::mutate(
      HHLD_NUMKID = sum(c(KIDS_LT1Y, KIDS_1_4Y, KIDS_5_11Y, KIDS_12_17Y) == "Yes", na.rm = TRUE)
    ) %>%
    dplyr::ungroup()
}

wave_levels <- c("Wave 1: Phase 4.2", "Wave 2: October 2024", "Wave 3: December 2024")
wave_labels <- c("Phase 4.2\n(Cycle 09)", "October\n2024", "December\n2024")

waves_combined <- dplyr::bind_rows(
  hps_phase42_clean  %>% add_hhld_numkid() %>% dplyr::mutate(wave = "Wave 1: Phase 4.2"),
  hps_october_clean  %>% add_hhld_numkid() %>% dplyr::mutate(wave = "Wave 2: October 2024"),
  hps_december_clean %>% add_hhld_numkid() %>% dplyr::mutate(wave = "Wave 3: December 2024")
) %>%
  dplyr::mutate(
    wave = factor(wave, levels = wave_levels),
    wave_num = as.integer(wave)
  ) %>%
  dplyr::filter(!is.na(PWEIGHT))

####################################################################
# 5. SURVEY-WEIGHTED % WITH STANDARD ERRORS (FOR CIs)
####################################################################
weighted_pct_by_wave <- function(full_data, var, characteristic, level_order = NULL) {
  df <- full_data[!is.na(full_data[[var]]) & !is.na(full_data$PWEIGHT), ]
  if (nrow(df) == 0) return(NULL)
  
  df[[var]] <- as.factor(df[[var]])
  des <- svydesign(ids = ~1, weights = ~PWEIGHT, data = df)
  
  fmla <- as.formula(paste0("~", var))
  res <- svyby(fmla, ~wave, des, svymean, na.rm = TRUE)
  
  levels_var <- levels(df[[var]])
  out_list <- list()
  
  for (lvl in levels_var) {
    col_mean <- paste0(var, lvl)
    col_se   <- paste0("se.", var, lvl)
    
    if (col_mean %in% names(res)) {
      temp <- res[, c("wave", col_mean, col_se)]
      names(temp) <- c("wave", "pct", "se")
      temp$pct <- temp$pct * 100
      temp$se <- temp$se * 100
      temp$level <- lvl
      out_list[[length(out_list) + 1]] <- temp
    }
  }
  
  out <- dplyr::bind_rows(out_list) %>%
    dplyr::mutate(characteristic = characteristic, level = as.character(level))
  
  if (!is.null(level_order)) { out$level <- factor(out$level, levels = level_order) }
  return(out)
}

weighted_mean_by_wave <- function(full_data, var, characteristic) {
  df <- full_data[!is.na(full_data[[var]]) & !is.na(full_data$PWEIGHT), ]
  des <- svydesign(ids = ~1, weights = ~PWEIGHT, data = df)
  m <- svyby(as.formula(paste0("~", var)), ~wave, des, svymean, na.rm = TRUE)
  data.frame(wave = m$wave, characteristic = characteristic, mean_value = m[[var]], se = m$se)
}

####################################################################
# 6. COMPUTE ESTIMATES
####################################################################

food_sec <- weighted_pct_by_wave(waves_combined, "CURFOODSUF", "Food Security Status", 
                                 c("Secure", "Marginal", "Sometimes Insecure", "Often Insecure"))
expns_dif <- weighted_pct_by_wave(waves_combined, "EXPNS_DIF", "Expense Difficulty", 
                                  c("Not at all difficult", "A little difficult", "Somewhat difficult", "Very difficult"))
wrkloss <- weighted_pct_by_wave(waves_combined, "WRKLOSSRV", "Work Loss", c("Yes", "No"))
anywork <- weighted_pct_by_wave(waves_combined, "ANYWORK", "Any Employment", c("Yes", "No"))

# Removed the "N/A" category strictly to look at households with expenses
schlfd <- weighted_pct_by_wave(waves_combined, "SCHLFDEXPNS", "School Food Expenses", 
                               c("No difficulty", "Yes, difficulty", "N/A (no school food costs)")) %>%
  dplyr::filter(level != "N/A (no school food costs)")

transp <- weighted_pct_by_wave(waves_combined, "ACCESS_TRANSP", "Transportation Access", 
                               c("Enough transportation to meet your needs", 
                                 "Enough transportation, but not always the kinds you want to use", 
                                 "Sometimes not enough transportation to meet your needs", 
                                 "Often not enough transportation to meet your needs", 
                                 "Always not enough transportation to meet your needs"))

num_kids_mean <- weighted_mean_by_wave(waves_combined, "HHLD_NUMKID", "Number of Children (Mean)")

# Pre-processing plot data
prep_plot_data <- function(df) {
  df %>% dplyr::mutate(wave = factor(wave, levels = wave_levels), wave_num = as.integer(wave))
}
food_sec <- prep_plot_data(food_sec); expns_dif <- prep_plot_data(expns_dif)
wrkloss <- prep_plot_data(wrkloss); anywork <- prep_plot_data(anywork)
schlfd <- prep_plot_data(schlfd); transp <- prep_plot_data(transp)

####################################################################
# 7. PUBLICATION QUALITY THEMES & COLORBLIND PALETTES
####################################################################

theme_hps_pub <- function(base_size = 12) {
  theme_minimal(base_size = base_size, base_family = "sans") +
    theme(
      plot.title         = element_text(face = "bold", size = base_size + 1),
      plot.subtitle      = element_text(size = base_size - 1, color = "grey30", margin = margin(b=10)),
      axis.title         = element_text(size = base_size - 1, face = "bold", color = "black"),
      axis.text          = element_text(color = "black", size = base_size - 2),
      axis.text.x        = element_text(face = "bold"),
      panel.border       = element_rect(color = "black", fill = NA, linewidth = 0.5), # Clear box
      panel.grid.major.y = element_line(color = "grey85", linetype = "dashed"),
      panel.grid.major.x = element_blank(),
      panel.grid.minor   = element_blank(),
      legend.position    = "bottom",
      legend.title       = element_blank(),
      legend.text        = element_text(size = base_size - 2),
      legend.key.width   = unit(2.5, "lines"),
      plot.margin        = margin(t=10, r=15, b=10, l=10)
    )
}

wrap_lbl <- function(x, width = 35) stringr::str_wrap(x, width = width)

# Colorblind-safe palettes (Okabe-Ito inspired & customized Viridis)
palette_foodsec <- c("Secure" = "#0072B2", "Marginal" = "#56B4E9", 
                     "Sometimes Insecure" = "#E69F00", "Often Insecure" = "#D55E00")
palette_expns   <- c("Not at all difficult" = "#0072B2", "A little difficult" = "#56B4E9", 
                     "Somewhat difficult" = "#E69F00", "Very difficult" = "#D55E00")
palette_yesno   <- c("Yes" = "#D55E00", "No" = "#0072B2")
palette_school  <- c("No difficulty" = "#0072B2", "Yes, difficulty" = "#D55E00")
palette_transp  <- c("Enough transportation to meet your needs" = "#009E73",
                     "Enough transportation, but not always the kinds you want to use" = "#56B4E9",
                     "Sometimes not enough transportation to meet your needs" = "#F0E442",
                     "Often not enough transportation to meet your needs" = "#E69F00",
                     "Always not enough transportation to meet your needs" = "#D55E00")

# Vector shapes and linestyles to map redundantly for print accessibility
shapes_vec    <- c(21, 22, 24, 23, 25)
linetypes_vec <- c("solid", "longdash", "dotdash", "dashed", "dotted")

build_pub_panel <- function(data, title, palette_vec, legend_ncol = 2, wrap_width = 35) {
  
  named_palette <- palette_vec
  names(named_palette) <- wrap_lbl(names(palette_vec), wrap_width)
  
  data <- data %>%
    dplyr::mutate(
      level_wrapped = factor(wrap_lbl(as.character(level), wrap_width), levels = names(named_palette)),
      lower_ci = pmax(0, pct - 1.96 * se),
      upper_ci = pmin(100, pct + 1.96 * se)
    ) %>% drop_na(level_wrapped)
  
  ggplot(data, aes(x = wave_num, y = pct, color = level_wrapped, 
                   shape = level_wrapped, linetype = level_wrapped)) +
    geom_line(linewidth = 0.9, alpha = 0.85) +
    # 95% Confidence Interval Error Bars
    geom_errorbar(aes(ymin = lower_ci, ymax = upper_ci), 
                  width = 0.1, linewidth = 0.6, alpha = 0.75, show.legend = FALSE) +
    # White-filled points to make lines pop
    geom_point(size = 2.8, fill = "white", stroke = 1.2) +
    scale_x_continuous(breaks = 1:3, labels = wave_labels, expand = expansion(mult = c(0.12, 0.12))) +
    scale_y_continuous(labels = label_percent(scale = 1), expand = expansion(mult = c(0.1, 0.1))) +
    scale_color_manual(values = named_palette, drop = FALSE) +
    scale_shape_manual(values = shapes_vec[1:length(palette_vec)], drop = FALSE) +
    scale_linetype_manual(values = linetypes_vec[1:length(palette_vec)], drop = FALSE) +
    labs(title = title, x = NULL, y = "Weighted % (95% CI)") +
    theme_hps_pub() +
    guides(color = guide_legend(ncol = legend_ncol, byrow = TRUE),
           shape = guide_legend(ncol = legend_ncol, byrow = TRUE),
           linetype = guide_legend(ncol = legend_ncol, byrow = TRUE))
}

####################################################################
# 8. BUILD PANELS & COMPOSE
####################################################################

p_food  <- build_pub_panel(food_sec,  "Food Security Status", palette_foodsec, legend_ncol = 2)
p_expns <- build_pub_panel(expns_dif, "Expense Difficulty", palette_expns, legend_ncol = 2)
p_wrk   <- build_pub_panel(wrkloss,   "Work Loss or Reservation", palette_yesno, legend_ncol = 2)
p_any   <- build_pub_panel(anywork,   "Any Employment", palette_yesno, legend_ncol = 2)
p_schl  <- build_pub_panel(schlfd,    "School Food Expenses (Excl. N/A)", palette_school, legend_ncol = 2)
p_trans <- build_pub_panel(transp,    "Transportation Access", palette_transp, legend_ncol = 1, wrap_width = 45)

p_main <- (p_food  | p_expns) /
  (p_wrk   | p_any)   /
  (p_schl  | p_trans) +
  plot_annotation(
    title    = "Temporal Trends in Household Pulse Survey Estimates",
    subtitle = "Survey-weighted proportions with 95% Confidence Intervals (Phase 4.2 to Dec 2024)",
    caption  = "Data: U.S. Census Bureau HPS. Point estimates use person-level replicate weights. Error bars denote 95% CIs.",
    theme    = theme(plot.title   = element_text(face = "bold", size = 16, margin = margin(b=5)),
                     plot.subtitle = element_text(size = 12, color = "grey20", margin = margin(b=15)),
                     plot.caption  = element_text(size = 10, color = "grey40", hjust = 0))
  )

####################################################################
# 9. SAVE OUTPUTS (High DPI for Print)
####################################################################

ggsave(file.path(results_dir, "Figure_Temporal_Table1_Categorical_Pub.png"), p_main, width = 16, height = 15, dpi = 600)
ggsave(file.path(results_dir, "Figure_Temporal_Table1_Categorical_Pub.pdf"), p_main, width = 16, height = 15)

cat("\n✓ Saved high-resolution, publication-ready figures.\n")