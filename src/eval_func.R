#!/usr/bin/env Rscript

compute_auc <- function(pred, label) {
  ifelse(length(unique(label)) < 2,
         NA,
         ROCR::performance(ROCR::prediction(pred, label), "auc")@y.values[[1]])
}

eval_responses <- function (d,
                            id = "user_id",
                            group = "user_group",
                            p_pred = "predicted_decision",
                            b_pred = "bin_pred",
                            label = "outcome") {
  # Evaluate the set of responses in d
  df <- d %>%
    rename(
      user_id = !!id,
      user_group = !!group,
      pp = !!p_pred,
      bp = !!b_pred,
      l = !!label
    )

  user_df <- df %>%
    group_by(user_id, user_group) %>%
    summarize(
      user_acc = mean(bp == l),
      user_auc = compute_auc(pp, l)
    ) %>%
    group_by(user_group) %>%
    summarize(
      mean_auc = mean(user_auc, na.rm=T),
      sd_user_acc = sd(user_acc),
      sd_user_auc = sd(user_auc, na.rm=T)
    )


  result <- df %>%
    group_by(!!rlang::sym(group)) %>%
    summarize(
      N = length(unique(user_id)),
      # mean_auc = mean(compute_auc(pp, l)),
      mean_acc = mean(bp == l)
    ) %>%
    # left_join(bs_df, by = c("user_group")) %>%
    left_join(user_df, by = c("user_group")) %>%
    mutate(
      se_acc = sd_user_acc/sqrt(N),
      se_auc = sd_user_auc/sqrt(N)
    ) %>%
    select(c(user_group, N,
             mean_acc, se_acc,
             mean_auc, se_auc))

  result
}

eval_recall <- function(d, risk_col, outcome_col, groupby_vars) {
  groupby_vars_with_pred <- c(groupby_vars, 'pred')
  d %>%
    rename(pred = !!risk_col, outcome = !!outcome_col) %>%
    group_by(!!!syms(groupby_vars_with_pred)) %>% 
    mutate(avg_outcome = sum(outcome)/n()) %>% 
    ungroup() %>% 
    group_by(!!!syms(groupby_vars)) %>% 
    arrange(desc(pred)) %>%
    mutate(count = 1,
           N = n(),
           recall = cumsum(avg_outcome) / sum(avg_outcome),
           prop = cumsum(count) / N) %>%
    select(-count) %>% 
    ungroup()
}

