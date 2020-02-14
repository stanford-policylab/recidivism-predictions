#!/usr/bin/env Rscript
# Compute (improved) COMPAS predictions

# Read data ---------------------------------------------------------------
full_df <- read_rds(DF_FULL_RDS)
train_df <- full_df %>%
  filter(fold == "train") # %>%

# Compute train/test risk estimates ---------------------------------------
get_df_preds <- function(train, test) {
  models_df <- expand.grid(
    features = names(df_covariates),
    target = df_targets,
    model = FIT_MODELS,
    stringsAsFactors = FALSE
  ) %>%
    as_tibble() %>%
    mutate(
      f = map2(features, target,
               ~ reformulate(df_covariates[[.x]], .y)),
      m = future_map2(f, model, ~ model_specs[[.y]](.x, train),
                      .progress = TRUE)
    )
  
  # Model prediction
  preds <- models_df %>%
    mutate(preds = pmap(list(model, m, f, list(test)), get_preds)) %>%
    unnest(preds) %>%
    mutate(
      method = "compas_split",
      choice = SURVEY_SCALE[vapply(pred,
                                   function(x)
                                     which.min(abs(x - SURVEY_SCALE)),
                                   integer(1))],
      outcome = ifelse(target == df_recid_target,
                       two_year_recid, is_violent_recid),
      score = ifelse(target == df_recid_target,
                     decile_score, v_decile_score)
    ) %>%
    select(features,
           target,
           model,
           index = fold,
           id,
           outcome,
           score,
           pred,
           choice)
  preds
}

preds <- get_df_preds(train_df, full_df)

# Write preds to rds ------------------------------------------------------
write_rds(preds, DF_PREDS_RDS)
