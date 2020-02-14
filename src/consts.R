# Shared constants and paths
set.seed(123)

# Constant values --------------------------------------------------------------
N_CASES <-
  52  # Number of cases shown per user (including dummy questions)
REAL_N_CASES <-
  50  # Number of cases shown per user (excludeing dummy questions)
DUMMY_CASE_DF <- tribble(~ id, ~ outcome,
                         "dummy1", 0,
                         "dummy2", 1)
LMH_LABELS <- c("low", "med", "high")
EXP_NAMES <- c("df", "df_violent", "lsi_low_br", "lsi_balanced_br")
EXP_NAME_LABELS <-
  c("COMPAS balanced BR",
    "COMPAS low BR",
    "LSI-R low BR",
    "LSI-R balanced BR")


# Different scales used in experiments
SURVEY_SCALE <- c( 2,  5,  8, 12, 15,
                  19, 22, 25, 28, 31,
                  35, 38, 42, 45, 48,
                  52, 55, 58, 62, 65,
                  69, 72, 75, 78, 81,
                  85, 88, 92, 95, 98) / 100

DF_SELECTED_MODEL <- "logit"
LSI_SELECTED_MODEL <- "logit"

FIT_MODELS <- c("logit")
TARGET_SITES <- c("low_br", "balanced_br")

N_SAMPLE_DRAW <- 1000

# Paths ------------------------------------------------------------------------
DATA_DIR <- normalizePath("../data/public/")
PRIVATE_DATA_DIR <- normalizePath("../data/private/")
dir.create(DATA_DIR, showWarnings = FALSE)
# Plot paths
PLOT_DIR <- normalizePath("../fig/")
dir.create(PLOT_DIR, showWarnings = FALSE)
# Derived/generated data paths
DERIVED_DIR <- normalizePath("../data/public/derived/")
dir.create(DERIVED_DIR, showWarnings = FALSE)
dir.create(file.path(DERIVED_DIR, "compas/"), showWarnings = FALSE)
dir.create(file.path(DERIVED_DIR, "lsi/"), showWarnings = FALSE)

# User and responses data ------------------------------------------------------
USER_PATH <- file.path(DATA_DIR, "surveys/df_user.csv")
RESP_PATH <- file.path(DATA_DIR, "surveys/df_response.csv")
VIOLENT_USER_PATH <-
  file.path(DATA_DIR, "surveys/df_violent_user.csv")
VIOLENT_RESP_PATH <-
  file.path(DATA_DIR, "surveys/df_violent_response.csv")
LSI_LOW_BR_USER_PATH <- file.path(DATA_DIR, "surveys/lsi_low_br_user.csv")
LSI_LOW_BR_RESP_PATH <- file.path(DATA_DIR, "surveys/lsi_low_br_response.csv")
LSI_BALANCED_BR_USER_PATH <-
  file.path(DATA_DIR, "surveys/lsi_balanced_br_user.csv")
LSI_BALANCED_BR_RESP_PATH <- 
  file.path(DATA_DIR, "surveys/lsi_balanced_br_response.csv")

# Risk assessment tool and vignettes paths -------------------------------------
# Dressel and Farid replication data paths
VIGN_PATH <- file.path(DATA_DIR, "individuals/compas_vignettes.csv")
VIOLENT_VIGN_PATH <-
  file.path(DERIVED_DIR, "compas/compas_violent_vignettes.csv")
COMPAS_SCORE_PATH <-
  file.path(DATA_DIR, "individuals/compas_scores.csv")
BROWARD_CLEAN_PATH <-
  file.path(DATA_DIR, "individuals/broward_clean_fixed.csv")
LSI_LOW_BR_VIGN_PATH <-
  file.path(PRIVATE_DATA_DIR, "individuals/lsi_vignettes_site_low_br.csv")
LSI_BALANCED_BR_VIGN_PATH <-
  file.path(PRIVATE_DATA_DIR, "individuals/lsi_vignettes_site_balanced_br.csv")
LSI_LOW_BR_VIGN_OUTCOME_ONLY_PATH <-
  file.path(DATA_DIR, "individuals/lsi_vignettes_site_low_br_outcome_only.csv")
LSI_BALANCED_BR_VIGN_OUTCOME_ONLY_PATH <-
  file.path(DATA_DIR, 
            "individuals/lsi_vignettes_site_balanced_br_outcome_only.csv")

EXP_RESULTS_RDS <- file.path(DERIVED_DIR, "exp_results.rds")
# Processed D&F repcalition data and LSI data
DF_FULL_RDS <-
  file.path(DERIVED_DIR, "compas/compas_fulldata.rds")
LSI_ALL_RDS <- file.path(DERIVED_DIR, "lsi/lsi_fulldata.rds")

# Model prediction RDS
DF_PREDS_RDS <- file.path(DERIVED_DIR, "compas/preds.rds")
LSI_ALL_PREDS_RDS <- file.path(DERIVED_DIR, "lsi/all_preds.rds")
# Model posterior draw performance RDS for plots
DF_POSTERIOR_SAMPLE_PERF_RDS <-
  file.path(DERIVED_DIR, "compas/pos_sample_perf.rds")
LSI_ALL_POS_PERF_RDS <-
  file.path(DERIVED_DIR, "lsi/all_pos_sample_perf.rds")
# Model posterior draw recall on the same set as user responses
DF_POS_RECALL_SD_RDS <-
  file.path(DERIVED_DIR, "compas/pos_sample_recall_sd.rds")
LSI_ALL_POS_RECALL_SD_RDS <-
  file.path(DERIVED_DIR, "lsi/all_pos_sample_recall_sd.rds")

# Human response model rds
HUMAN_RESPONSE_RECALL_SD_RDS <-
  file.path(DERIVED_DIR, "human_response_recall_sd.rds")

# Object saved for R Markdown output
PAPER_RESULTS_RDS <- file.path(DERIVED_DIR, "paper_results.rds")
