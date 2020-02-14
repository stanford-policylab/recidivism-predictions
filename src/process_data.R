#!/usr/bin/env Rscript
# Read and process all data

message(sprintf("Running process_data.R..."))

# Parameters --------------------------------------------------------------
user_df_paths <-
  c(USER_PATH, VIOLENT_USER_PATH, LSI_LOW_BR_USER_PATH, LSI_BALANCED_BR_USER_PATH)
resp_df_paths <-
  c(RESP_PATH, VIOLENT_RESP_PATH, LSI_LOW_BR_RESP_PATH, LSI_BALANCED_BR_RESP_PATH)

if (file.exists(LSI_LOW_BR_VIGN_PATH) & 
    file.exists(LSI_BALANCED_BR_VIGN_PATH)) {
  vign_df_paths <-
    c(VIGN_PATH, VIOLENT_VIGN_PATH, 
      LSI_LOW_BR_VIGN_PATH, LSI_BALANCED_BR_VIGN_PATH)
} else {
  vign_df_paths <-
    c(VIGN_PATH, VIOLENT_VIGN_PATH, 
      LSI_LOW_BR_VIGN_OUTCOME_ONLY_PATH, LSI_BALANCED_BR_VIGN_OUTCOME_ONLY_PATH)
}

# Aggregate experiment responses ------------------------------------------
all_df <- tibble()
all_attn_check <- tibble()

for (i in 1:length(EXP_NAMES)) {
  user_df <- read_csv(user_df_paths[i]) %>% select(-id)
  resp_df <- read_csv(resp_df_paths[i]) %>% select(-id) %>%
    mutate(bin_pred = if_else(predicted_decision > 50, 1, 0))
  vign_df <- read_csv(vign_df_paths[i],
                      col_types = cols(id = col_character())) %>%
    select(id, outcome) %>%
    bind_rows(DUMMY_CASE_DF) %>%
    rename(individual_id = id)
  
  cleaned <-
    clean_data(user_df, resp_df, vign_df, is_compas = FALSE)
  clean_df <- cleaned$clean_df
  attn_check <- cleaned$attn_check
  
  clean_df$exp_name <- EXP_NAMES[i]
  
  all_df <- bind_rows(all_df, clean_df)
  all_attn_check <- bind_rows(all_attn_check, attn_check)
  
  message(sprintf("Experiment: %s", EXP_NAMES[i]))
  print(eval_responses(clean_df))
}

message(sprintf("Attention check pass statistics"))
all_attn_check %>%
  summarize(
    total_n = n(),
    pass_n = sum(pass == 1),
    attn_pass_rate = pass_n / total_n
  )

all_df <- all_df %>%
  mutate(exp_name = factor(exp_name,
                           levels = EXP_NAMES,
                           labels = EXP_NAME_LABELS))

# Read and join appropriate prediction results ----------------------------
preds <- bind_rows(
  read_rds(DF_PREDS_RDS) %>%
    mutate(exp_name = if_else(
      target == "two_year_recid",
      "df", "df_violent"
    )) %>%
    filter(model == DF_SELECTED_MODEL),
  read_rds(LSI_ALL_PREDS_RDS) %>%
    mutate(exp_name = paste0("lsi_", site)) %>%
    filter(model == LSI_SELECTED_MODEL)
) %>%
  filter(exp_name %in% EXP_NAMES) %>%
  mutate(
    individual_id = as.character(id),
    exp_name = factor(exp_name,
                      levels = EXP_NAMES,
                      labels = EXP_NAME_LABELS)
  ) %>%
  select(-id) %>%
  rename(model_pred = pred,
         model_choice = choice)

joined_df <- all_df %>%
  mutate(features = if_else(grepl("short", user_group), "short", "long")) %>%
  left_join(preds, by = c("individual_id", "exp_name", "features", "outcome"))

# Write EXP_RESULTS_RDS ---------------------------------------------------
write_rds(joined_df, EXP_RESULTS_RDS)
