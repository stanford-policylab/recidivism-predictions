#!/usr/bin/env Rscript
# Build models and LOO predictions for all LSI sites
# TARGET_SITES are defined in const.R
# Only run this script if LSI_ALL_RDS exisits

if (file.exists(LSI_ALL_RDS)) {
  # Read data ------------------------------------------------------------------
  lsi_full_df <- read_rds(LSI_ALL_RDS)
  
  # Compute LOO risk estimates -------------------------------------------------
  get_lsi_all_loo_preds <- function(df) {
    df %>%
      group_by(site) %>%
      nest() %>%
      mutate(preds = map(
        data,
        ~ pred_loo(
          .x,
          fs = lsi_formulas,
          ms = model_specs,
          models = FIT_MODELS
        )
      )) %>%
      unnest(preds) %>%
      select(features, model, site, outcome,
             score = lsitotal, id, pred, choice) %>%
      mutate(target = lsi_target)
  }
  
  preds <- get_lsi_all_loo_preds(lsi_full_df)
  
  # Write preds to rds ---------------------------------------------------------
  write_rds(preds, LSI_ALL_PREDS_RDS)
} else {
  message("No LSI_ALL_RDS, skipping running preds_lsi_all.R...")
}
