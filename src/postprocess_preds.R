#!/usr/bin/env Rscript

# Post-process script to obtain confidence intervals for figures using posterior
# model samples
message(sprintf("Running postprocess_preds.R... This may take a while."))

results_df <- read_rds(EXP_RESULTS_RDS)

# PART 1: For Compas data model variance for prop-recall plot/table -------------
# use posterior sample to estimate variance of LR model for
# proportion-recall curve

# Read COMPAS data
full_df <- read_rds(DF_FULL_RDS)
train_df <- full_df %>%
  filter(fold == "train") # %>%

# Model posterior prediction using posterior draws from the logit model --------
# Estimate SE for the COMPAS models' performance
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
    m = future_map2(f, model, ~ model_specs[[.y]](.x, train_df),
                    .progress = TRUE)
  )

df_posterior_preds <- models_df %>%
  mutate(pos_coef = map(m, ~ slot(sim(.x, n.sims = N_SAMPLE_DRAW), 'coef'))) %>%
  mutate(pos_sample_preds = future_pmap(list(list(full_df), f, pos_coef), mm_lr)) %>%
  unnest(pos_sample_preds) %>%
  mutate(
    pos_sample_choice = SURVEY_SCALE[vapply(pos_sample_preds,
                                            function(x)
                                              which.min(abs(x - SURVEY_SCALE)),
                                            integer(1))],
    outcome = ifelse(target == df_recid_target,
                     two_year_recid, is_violent_recid)
  ) %>%
  filter(fold == 'test') %>%
  select(
    features,
    target,
    model,
    index = fold,
    id,
    outcome,
    pos_sample_id,
    pos_sample_preds,
    pos_sample_choice
  )

df_posterior_preds_perf <- df_posterior_preds %>%
  group_by(features, target, model, pos_sample_id) %>%
  summarize(
    pos_sample_Accuracy = mean((pos_sample_choice > .5) == outcome),
    pos_sample_AUC = compute_auc(pos_sample_choice, outcome)
  ) %>%
  group_by(features, target, model) %>%
  summarize(
    pos_sample_Accuracy_mean = mean(pos_sample_Accuracy),
    pos_sample_Accuracy_sd = sd(pos_sample_Accuracy),
    pos_sample_AUC_mean = mean(pos_sample_AUC),
    pos_sample_AUC_sd = sd(pos_sample_AUC)
  ) %>%
  mutate(n_posterior_draw = N_SAMPLE_DRAW)

write_rds(df_posterior_preds_perf, DF_POSTERIOR_SAMPLE_PERF_RDS)

# replicate posterior sample draws based on user responses --------------------
# Estimate SE for table A6
compas_results_df <- results_df %>%
  filter(grepl("COMPAS", exp_name))
# Since it's in sample posterior recall samples, only use the SD
replicated_df_posterior_preds <- compas_results_df %>%
  select(features, target, model, id = individual_id, user_group) %>%
  mutate(id = as.integer(id),
         feedback = grepl("feedback", user_group)) %>%
  filter(!feedback) %>%
  inner_join(df_posterior_preds,
             by = c("features", "target", "model", "id")) %>%
  rename(individual_id = id) %>%
  recall_from_pos_samples()

write_rds(replicated_df_posterior_preds, DF_POS_RECALL_SD_RDS)

# PART 2: For LSI-R data model variance for prop-recall plot/table -------------
# use posterior sample to estimate variance of LR model for
# proportion-recall curve
# only execute this part if LSI_ALL_RDS

if (file.exists(LSI_ALL_RDS)) {
  full_lsi <- read_rds(LSI_ALL_RDS)
  
  # Compute risk estimates using posterior draws from --------------------------
  # Estimate SE for the LSI-R models' performance
  # the logit model trained on all data
  models_lsi <-
    expand.grid(
      f = lsi_formulas,
      model = FIT_MODELS,
      site = TARGET_SITES,
      stringsAsFactors = FALSE
    ) %>%
    as_tibble() %>%
    mutate(
      features = names(f),
      target = lsi_target,
      m = future_pmap(list(f, model, site), function(x, y, z) {
        model_specs[[y]](x, full_lsi %>% filter(site == z))
      },
      .progress = TRUE)
    )
  
  lsi_insample_posterior_preds <- models_lsi %>%
    mutate(pos_coef = future_map(m, ~ slot(sim(.x, n.sims = N_SAMPLE_DRAW), 'coef')),
           data = future_map(site, ~ full_lsi %>% filter(site == .x) 
                             %>% select(-site))) %>%
    mutate(pos_sample_preds = future_pmap(list(data, f, pos_coef), mm_lr)) %>%
    unnest(pos_sample_preds) %>%
    mutate(pos_sample_choice = SURVEY_SCALE[vapply(pos_sample_preds,
                                                   function(x)
                                                     which.min(abs(x - SURVEY_SCALE)),
                                                   integer(1))],
           fold = "LOO") %>%
    select(
      features,
      target,
      model,
      site,
      index = fold,
      id,
      outcome,
      pos_sample_id,
      pos_sample_preds,
      pos_sample_choice
    )
  
  lsi_insample_pos_perf <- lsi_insample_posterior_preds %>%
    group_by(features, target, model, pos_sample_id, site) %>%
    summarize(
      pos_sample_Accuracy = mean((pos_sample_choice > .5) == outcome),
      pos_sample_AUC = compute_auc(pos_sample_choice, outcome)
    ) %>%
    group_by(features, target, model, site) %>%
    summarize(
      pos_sample_Accuracy_mean = mean(pos_sample_Accuracy),
      pos_sample_Accuracy_sd = sd(pos_sample_Accuracy),
      pos_sample_AUC_mean = mean(pos_sample_AUC),
      pos_sample_AUC_sd = sd(pos_sample_AUC)
    ) %>%
    mutate(n_posterior_draw = N_SAMPLE_DRAW)
  
  write_rds(lsi_insample_pos_perf, LSI_ALL_POS_PERF_RDS)
  
  # replicate posterior sample draws based on user responses -------------------
  # Estimate SE for table A6
  lsi_results_df <- results_df %>%
    filter(site %in% TARGET_SITES)
  # Since it's in sample posterior recall samples, only use the SD
  replicated_lsi_insample_posterior_preds <- lsi_results_df %>%
    select(features, target, model, site, id = individual_id, user_group) %>%
    mutate(id = as.integer(id),
           feedback = grepl("feedback", user_group)) %>%
    filter(!feedback) %>%
    inner_join(lsi_insample_posterior_preds,
               by = c("features", "target", "model", "site", "id")) %>%
    rename(individual_id = id) %>%
    recall_from_pos_samples(site = TRUE)
  
  write_rds(replicated_lsi_insample_posterior_preds,
            LSI_ALL_POS_RECALL_SD_RDS)
} else {
  message("No LSI_ALL_RDS, skipping LSI posterior draws for estimating SD...")
}

# PART 3: Modeling human responses using a logit linear model to ---------------
# Estimate SE for the human part of table A6
# estimate human variance
user_question_df <- results_df %>%
  mutate(
    feedback = grepl("feedback", user_group),
    qid = paste(exp_name, user_group, individual_id, sep = "_"),
    predicted_decision = predicted_decision / 100,
    logit_pred_dec = logit(predicted_decision)
  ) %>%
  filter(!feedback)
# fit logit linear model
form <- predicted_decision ~ user_id + qid
logit_linear_form <- logit_pred_dec ~ user_id + qid
logit_linear_uq_model <-
  lm(logit_linear_form, data = user_question_df)
logit_linear_pred <- predict(logit_linear_uq_model)
sigma_hat <- sigma(logit_linear_uq_model)
noised_logit_linear_pred <- map_dfr(1:1000,
                                    ~ user_question_df %>%
                                      mutate(
                                        preds = inv.logit(logit_linear_pred +
                                                            rnorm(length(logit_linear_pred),
                                                                  sd = sigma_hat)),
                                        bs_id = .x
                                      ))

noised_logit_linear_pred <- noised_logit_linear_pred %>%
  mutate(human_response_model_choice =
           SURVEY_SCALE[vapply(preds,
                               function(x)
                                 which.min(abs(x - SURVEY_SCALE)),
                               integer(1))])

human_response_model_recall <- noised_logit_linear_pred %>%
  eval_recall(
    risk_col = "human_response_model_choice",
    outcome_col = "outcome",
    groupby_vars = c("exp_name", "features", "target", "user_group", "bs_id")
  ) %>%
  group_by(exp_name, features, target, user_group, prop) %>%
  summarize(recall_mean = mean(recall),
            recall_sd = sd(recall)) %>%
  ungroup()

write_rds(human_response_model_recall, HUMAN_RESPONSE_RECALL_SD_RDS)
