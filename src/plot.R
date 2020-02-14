#!/usr/bin/env Rscript
# Generating plots and tables for the paper

message(sprintf("Running plot.R..."))

# Suppress warning initally, will turn off at the end
options(warn = -1)
theme_set(theme_bw())

# How many individuals to use for "last N" estimate
N_LAST <- 10

# Paper results object
paper_results <- list(
  t = list(), # Tables
  p = list()  # Plots
)

# Local functions --------------------------------------------------------------
calibrate <- function(score, target) {
  m <- glm(target ~ score, family = "binomial")
  predict(m, type = "response")
}

binomial_smooth <- function(...) {
  ggplot2::geom_smooth(method = "glm",
                       method.args = list(family = "binomial"),
                       ...)
}

# Read data --------------------------------------------------------------------
results_df <- read_rds(EXP_RESULTS_RDS)

# Read model posterior draw performance records
posterior_draw_perf <- bind_rows(
  read_rds(DF_POSTERIOR_SAMPLE_PERF_RDS) %>%
    mutate(exp_name = if_else(
      target == "two_year_recid",
      "df", "df_violent"
    )),
  read_rds(LSI_ALL_POS_PERF_RDS) %>%
    mutate(index = "LOO",
           exp_name = paste0("lsi_", site))
  ) %>%
  mutate(exp_name = factor(exp_name,
                           levels = EXP_NAMES,
                           labels = EXP_NAME_LABELS))

posterior_draw_perf <- posterior_draw_perf %>%
  rename(Accuracy = pos_sample_Accuracy_sd, AUC = pos_sample_AUC_sd) %>%
  gather(measure, sd, Accuracy, AUC) %>%
  select(features, target, method = model, exp_name, measure, sd)

# Read model posterior draw proportion-recall SDs
posterior_draw_recalls <- bind_rows(
  read_rds(DF_POS_RECALL_SD_RDS) %>%
    mutate(exp_name = if_else(
      target == "two_year_recid",
      "df", "df_violent"
    )),
  read_rds(LSI_ALL_POS_RECALL_SD_RDS) %>%
    mutate(index = "LOO",
           exp_name = paste0("lsi_", site))
  ) %>%
  mutate(
    exp_name = factor(exp_name,
                      levels = EXP_NAMES,
                      labels = EXP_NAME_LABELS),
    method = "model_choice"
  ) %>%
  select(method, features, exp_name, prop, model_recall_sd = recall_sd) %>%
  filter(features %in% c("long", "short"))

# Read human response model estimated human response recall SDs
human_response_model_recall <-
  read_rds(HUMAN_RESPONSE_RECALL_SD_RDS) %>%
  select(exp_name, features, prop, human_recall_sd = recall_sd) %>%
  mutate(method = "predicted_decision")

# Read model predictions
preds <- bind_rows(
  read_rds(DF_PREDS_RDS) %>%
    mutate(exp_name = if_else(
      target == "two_year_recid",
      "df", "df_violent"
    )),
  read_rds(LSI_ALL_PREDS_RDS) %>%
    mutate(index = "LOO",
           exp_name = paste0("lsi_", site))
  ) %>%
  mutate(exp_name = factor(exp_name,
                           levels = EXP_NAMES,
                           labels = EXP_NAME_LABELS))

model_perfs <- preds %>%
  filter(
    model == "logit",
    index %in% c("test", "LOO"),
    features != "full",
    (features == "short" & grepl("COMPAS", exp_name)) |
      !grepl("COMPAS", exp_name)
  ) %>%
  group_by(features, target, model, exp_name) %>%
  summarize(
    N = n(),
    base = mean(outcome),
    Accuracy = mean((choice > .5) == outcome),
    AUC = compute_auc(choice, outcome)
  ) %>%
  mutate(feedback = FALSE) %>%
  rename(method = model) %>%
  ungroup() %>%
  gather(measure, perf, AUC, Accuracy) %>%
  left_join(posterior_draw_perf,
            by = c("features", "target", "method", "exp_name", "measure"))



baserate_df <- model_perfs %>%
  mutate(
    features = factor(features, 
                      levels=c('short', 'long'), 
                      labels=c("streamlined", "enriched"))
  ) %>% 
  group_by(exp_name, features) %>%
  summarize(
    baserate = first(base),
    N = first(N)
  ) %>%
  ungroup()

predictions_df <- results_df %>%
  mutate(predicted_decision = predicted_decision / 100) %>%
  left_join(
    preds %>%
      filter(features == "full", model == "logit") %>%
      mutate(id = as.character(id)) %>%
      select(id, exp_name, tool = score),
    by = c(individual_id = "id", "exp_name")
  ) %>%
  gather(method,
         score,
         predicted_decision,
         model_choice,
         model_pred,
         tool) %>%
  mutate(
    features = if_else(grepl("short", user_group), "short", "long"),
    feedback = grepl("feedback", user_group),
    exp_name = fct_relevel(
      exp_name,
      "COMPAS balanced BR",
      "COMPAS low BR",
      "LSI-R balanced BR"
    ),
    features = fct_relevel(features, "short")
  )

# Create dataset summary table
num_resp <- predictions_df %>%
  filter(method == "predicted_decision") %>%
  group_by(features, exp_name, feedback) %>%
  summarize(n_resp = n()) %>% 
  ungroup() %>% 
  mutate(
    features = factor(features, 
                      levels=c('short', 'long'), 
                      labels=c("streamlined", "enriched"))
  )

data_summary_table <- num_resp %>% 
  left_join(baserate_df, by = c("features", "exp_name")) %>% 
  group_by(exp_name, feedback) %>% 
  summarize(
    features = ifelse(length(features) == 2, 
                      "streamlined/enriched", "streamlined"),
    n_resp = paste(n_resp, collapse="/"),
    baserate = first(baserate),
    N = first(N)
  ) %>% 
  ungroup()

data_summary_table <- data_summary_table %>% 
  filter(feedback) %>% 
  rename("# Resopnses (Feedback)" = n_resp) %>% 
  mutate(
    "# Resopnses (No needback)" = (data_summary_table %>% 
                                     filter(!feedback))$n_resp) %>% 
  select(exp_name, baserate, features, 
         "# Resopnses (No needback)", "# Resopnses (Feedback)") 
  
paper_results$t$data_summary_table <- data_summary_table

pays <- results_df %>%
  group_by(user_id) %>%
  summarize(
    exp_duration_min = (max(leave_time) - min(enter_time)) / 1000 / 60,
    brier = mean(1 - (outcome - predicted_decision / 100) ^ 2),
    pay = brier * 5 + 1,
    hourly_pay = pay * 60 / exp_duration_min
  )

# PRINT: Data and experiment summaries -----------------------------------------
message(rep("-", 80), "\nAverage payments:")
paper_results$t$pays <- pays %>% summarize(
  mean_duration_min = mean(exp_duration_min),
  mean_pay = mean(pay),
  mean_hourly_pay = mean(hourly_pay),
  median_hourly_pay = median(hourly_pay)
)
print(paper_results$t$pays)

message(rep("-", 80), "\nOutcome baserates:")
paper_results$t$baserate <- baserate_df %>%
  spread(exp_name, baserate)
print(paper_results$t$baserate)

message(rep("-", 80), "\nNumber of total responses:")
paper_results$t$num_resp <- predictions_df %>%
  filter(method == "predicted_decision") %>%
  group_by(features, exp_name, feedback) %>%
  summarize(n_resp = n()) %>%
  spread(exp_name, n_resp)
print(paper_results$t$num_resp)

message(rep("-", 80), "\nLogistic regression model performance:")
print(
  model_perfs %>%
    select(features, method, exp_name, feedback, measure, perf) %>%
    spread(measure, perf)
)

# PLOT: Vanilla AUC performance comparison -------------------------------------
ind_auc <- predictions_df %>%
  filter(method == "predicted_decision",!feedback) %>%
  group_by(method, features, feedback, exp_name, user_id) %>%
  summarize(auc = compute_auc(score, outcome))
mean_auc <- ind_auc %>%
  group_by(method, features, feedback, exp_name) %>%
  summarize(se = sd(auc, na.rm = TRUE) / sqrt(n()),
            auc = mean(auc, na.rm = TRUE)) %>%
  ungroup()

ind_feedback_auc <-
  predictions_df %>%
  filter(method == "predicted_decision", feedback) %>%
  group_by(user_id) %>%
  top_n(N_LAST, leave_time) %>%
  group_by(method, features, feedback, exp_name, user_id) %>%
  summarize(auc = compute_auc(score, outcome))
mean_feedback_auc <- ind_feedback_auc %>%
  group_by(method, features, feedback, exp_name) %>%
  summarize(se = sd(auc, na.rm = TRUE) / sqrt(n()),
            auc = mean(auc, na.rm = TRUE)) %>%
  ungroup()

auc_pd <-
  mean_auc %>%
  bind_rows(mean_feedback_auc) %>%
  mutate(
    lb = auc - qnorm(0.975) * se,
    ub = auc + qnorm(0.975) * se,
    feedback = fct_relevel(factor(if_else(
      feedback, "fb", "nofb"
    )), "fb")
  )

score_auc_pd <-
  preds %>%
  filter(features == "full", model == "logit", index %in% c("test", "LOO")) %>%
  group_by(target, exp_name) %>%
  summarize(auc = compute_auc(score, outcome))

message(rep("-", 80), "\nHuman AUC:")
print(
  auc_pd %>%
    select(features, feedback, exp_name, lb, auc, ub)
)

message(rep("-", 80), "\nExisting score AUC:")
print(
  score_auc_pd %>%
    ungroup() %>%
    select(exp_name, auc) %>%
    spread(exp_name, auc)
)

p_auc_model_perf_data <- model_perfs %>% 
  filter(measure == "AUC") %>% 
  rename(auc = perf)
p_auc_perf <-
  ggplot(auc_pd %>% filter(feedback == "nofb"), aes(x = features, y = auc)) +
  geom_hline(aes(yintercept = auc, linetype = "existing tool"),
             data = score_auc_pd,
             size = .4) +
  geom_linerange(
    aes(
      ymin = lb,
      ymax = ub,
      linetype = feedback
    ),
    position = position_nudge(x = -0.1),
    show.legend = FALSE
  ) +
  geom_point(aes(shape = feedback),
             size = 3,
             position = position_nudge(x = -0.1)) +
  geom_point(
    aes(shape = method, color = method, y = auc),
    data = p_auc_model_perf_data,
    position = position_nudge(x = 0.1),
    size = 3
  ) +
  geom_linerange(
    aes(
      ymin = auc - qnorm(0.975) * sd,
      ymax = auc + qnorm(0.975) * sd,
      color = method,
      linetype = method
    ),
    data = p_auc_model_perf_data,
    position = position_nudge(x = 0.1),
    show.legend = FALSE
  ) +
  scale_linetype_manual(
    element_blank(),
    limits = c("fb", "nofb", "logit", "existing tool"),
    values = c("solid", "solid", "solid", "dashed"),
    breaks = c("existing tool"),
    labels = c("Existing tool")
  ) +
  scale_color_manual(
    element_blank(),
    limits = c("nofb", "logit"),
    values = c("black", "red"),
    breaks = c("nofb", "logit"),
    labels = c("Humans (no feedback)", "Logistic regression")
  ) +
  scale_shape_manual(
    element_blank(),
    limits = c("nofb", "logit"),
    values = c(1, 15),
    breaks = c("nofb", "logit"),
    labels = c("Humans (no feedback)", "Logistic regression")
  ) +
  scale_x_discrete(element_blank(), labels = c("streamlined", "enriched")) +
  scale_y_continuous(
    "Performance (AUC)\n",
    labels = scales::percent_format(1),
    expand = c(0.1, .015)
  ) +
  facet_wrap(~ exp_name, scales = "free", ncol = 4) +
  theme(legend.position = "top") +
  guides(
    color = guide_legend(order = 1),
    shape = guide_legend(order = 1),
    linetype = guide_legend(order = 2)
  )
paper_results$p$p_auc_perf <- p_auc_perf

p_auc_publish <- p_auc_perf +
  labs(caption = element_blank())
ggsave(
  file.path(PLOT_DIR, "perf_auc.pdf"),
  p_auc_publish,
  width = 8,
  height = 3
)

p_auc_model_perf_data_lsi <- p_auc_model_perf_data %>% 
  filter(grepl("LSI-R", exp_name))
p_short_v_long <-
  ggplot(auc_pd %>% filter(grepl("LSI-R", exp_name), feedback == "nofb"),
         aes(x = features, y = auc)) +
  geom_point(aes(shape = feedback),
             size = 3,
             position = position_nudge(x = -0.1)) +
  geom_linerange(aes(ymin = lb, ymax = ub),
                 position = position_nudge(x = -0.1),
                 show.legend = FALSE) +
  geom_point(
    aes(shape = method, color = method, y = auc),
    data = p_auc_model_perf_data_lsi,
    size = 3,
    position = position_nudge(x = 0.1)
  ) +
  geom_linerange(
    aes(
      ymin = auc - qnorm(0.975) * sd,
      ymax = auc + qnorm(0.975) * sd,
      linetype = method,
      color = method
    ),
    data = p_auc_model_perf_data_lsi,
    show.legend = FALSE,
    position = position_nudge(x = 0.1)
  ) +
  geom_hline(
    aes(yintercept = auc, linetype = "existing tool"),
    size = .4,
    data = score_auc_pd %>%
      filter(grepl("LSI", exp_name))
  ) +
  scale_color_manual(
    element_blank(),
    limits = c("nofb", "logit"),
    values = c("black", "red"),
    breaks = c("nofb", "logit"),
    labels = c("Humans (no feedback)", "Logistic regression")
  ) +
  scale_shape_manual(
    element_blank(),
    limits = c("nofb", "logit"),
    values = c(1, 15),
    breaks = c("nofb", "logit"),
    labels = c("Humans (no feedback)", "Logistic regression")
  ) +
  scale_linetype_manual(
    element_blank(),
    limits = c("existing tool", "fb", "nofb", "logit"),
    values = c("dashed", "solid", "solid", "solid"),
    # breaks = NULL,
    # labels = NULL,
    breaks = c("existing tool"),
    labels = c("Existing tool")
  ) +
  scale_x_discrete(element_blank(), labels = c("streamlined", "enriched")) +
  scale_y_continuous(
    "Performance (AUC)\n",
    labels = scales::percent_format(1),
    expand = c(0.1, .015)
  ) +
  facet_wrap(~ exp_name, ncol = 2) +
  theme(legend.position = "top") +
  guides(
    color = guide_legend(order = 1),
    shape = guide_legend(order = 1),
    linetype = guide_legend(order = 2)
  )
paper_results$p$p_short_v_long <- p_short_v_long
ggsave(
  file.path(PLOT_DIR, "perf_short_long.pdf"),
  p_short_v_long,
  width = 5.5,
  height = 3
)

# PLOT: Vanilla performance comparison (Accuracy) ------------------------------
ind_acc <-
  predictions_df %>%
  filter(method == "predicted_decision",!feedback) %>%
  group_by(method, features, feedback, exp_name, user_id) %>%
  summarize(acc = mean((score >= .5) == outcome))

mean_acc <- ind_acc %>%
  group_by(method, features, feedback, exp_name) %>%
  summarize(se = sd(acc) / sqrt(n()),
            acc = mean(acc, na.rm = TRUE)) %>%
  ungroup()

ind_feedback_acc <-
  predictions_df %>%
  filter(method == "predicted_decision", feedback) %>%
  group_by(user_id) %>%
  top_n(N_LAST, leave_time) %>%
  group_by(method, features, feedback, exp_name, user_id) %>%
  summarize(acc = mean((score >= .5) == outcome))
mean_feedback_acc <- ind_feedback_acc %>%
  group_by(method, features, feedback, exp_name) %>%
  summarize(se = sd(acc, na.rm = TRUE) / sqrt(n()),
            acc = mean(acc, na.rm = TRUE)) %>%
  ungroup()

acc_pd <-
  mean_acc %>%
  bind_rows(mean_feedback_acc) %>%
  mutate(
    lb = acc - qnorm(0.975) * se,
    ub = acc + qnorm(0.975) * se,
    feedback = fct_relevel(factor(if_else(
      feedback, "fb", "nofb"
    )), "fb")
  )

score_acc_pd <-
  preds %>%
  filter(features == "full", model == "logit", index %in% c("test", "LOO")) %>%
  group_by(target, exp_name) %>%
  mutate(pred = calibrate(score, outcome)) %>%
  summarize(acc = mean((pred > .5) == outcome))

message(rep("-", 80), "\nHuman accuracy:")
print(
  acc_pd %>%
    select(features, feedback, exp_name, lb, acc, ub)
)

message(rep("-", 80), "\nExisting score accuracy:")
print(
  score_acc_pd %>%
    ungroup() %>%
    select(exp_name, acc) %>%
    spread(exp_name, acc)
)

p_acc_model_perf_data <- model_perfs %>% 
  filter(measure == "Accuracy") %>% 
  rename(acc = perf)
p_acc_perf <-
  ggplot(acc_pd, aes(x = features, y = acc)) +
  geom_hline(aes(yintercept = acc, linetype = "existing tool"),
             data = score_acc_pd,
             size = .4) +
  geom_linerange(
    aes(
      ymin = lb,
      ymax = ub,
      linetype = feedback
    ),
    position = position_dodge(.75),
    show.legend = FALSE
  ) +
  geom_point(aes(shape = feedback),
             size = 3,
             position = position_dodge(.75)) +
  geom_point(
    aes(shape = method, color = method, y = acc),
    data = p_acc_model_perf_data,
    size = 3
  ) +
  geom_linerange(
    aes(
      ymin = acc - qnorm(0.975) * sd,
      ymax = acc + qnorm(0.975) * sd,
      linetype = method,
      color = method
    ),
    data = p_acc_model_perf_data,
    show.legend = FALSE
  ) +
  scale_color_manual(
    element_blank(),
    limits = c("fb", "nofb", "logit"),
    values = c("black", "black", "red"),
    breaks = c("fb", "nofb", "logit"),
    labels = c("With feedback", "No feedback", "Logistic regression")
  ) +
  scale_shape_manual(
    element_blank(),
    limits = c("fb", "nofb", "logit"),
    values = c(16, 1, 15),
    breaks = c("fb", "nofb", "logit"),
    labels = c("With feedback", "No feedback", "Logistic regression")
  ) +
  scale_linetype_manual(
    element_blank(),
    limits = c("fb", "nofb", "existing tool", "logit"),
    values = c("solid", "solid", "dashed", "solid"),
    breaks = c("existing tool"),
    labels = c("Existing tool")
  ) +
  scale_x_discrete(element_blank(), labels = c("streamlined", "enriched")) +
  scale_y_continuous(
    "Classification accuracy\n",
    labels = scales::percent_format(1),
    expand = c(0.1, .015)
  ) +
  facet_wrap(~ exp_name, scales = "free_x", ncol = 4) +
  theme(legend.position = "top") +
  guides(
    color = guide_legend(order = 1),
    shape = guide_legend(order = 1),
    linetype = guide_legend(order = 2)
  ) +
  labs(caption = element_blank())
paper_results$p$p_acc_perf <- p_acc_perf

ggsave(
  file.path(PLOT_DIR, "perf_acc.pdf"),
  p_acc_perf,
  width = 8,
  height = 3
)


# PLOT: Distribution of relative performance (Accuracy) ------------------------
ind_acc_diff <-
  predictions_df %>%
  filter(method %in% c("predicted_decision", "model_choice"),!feedback) %>%
  mutate(acc = (score >= .5) == outcome) %>%
  select(-score) %>%
  spread(method, acc) %>%
  mutate(diff = predicted_decision - model_choice)

ind_feedback_acc_diff <-
  predictions_df %>%
  filter(method %in% c("predicted_decision", "model_choice"), feedback) %>%
  group_by(user_id) %>%
  top_n(N_LAST, leave_time) %>%
  mutate(acc = (score >= .5) == outcome) %>%
  select(-score) %>%
  spread(method, acc) %>%
  mutate(diff = predicted_decision - model_choice)

acc_diff_pd <-
  ind_acc_diff %>%
  bind_rows(ind_feedback_acc_diff) %>%
  group_by(features, feedback, exp_name, user_id) %>%
  summarize(
    N = n(),
    m = mean(diff),
    d = sum(diff),
    sd = sd(diff),
    t = if_else(m == 0, 0, mean(diff) / (sd / sqrt(N)))
  ) %>%
  ungroup() %>%
  mutate(feedback = fct_relevel(factor(
    if_else(feedback, "Feedback", "No feedback")
  ), "fb"))
p_t_dist <- ggplot(acc_diff_pd, aes(x = t)) +
  geom_histogram(binwidth = 0.8) +
  geom_vline(xintercept = 2, linetype = "dashed") + 
  scale_y_continuous("Count")
paper_results$p$p_t_dist <- p_t_dist

ggsave(
  file.path(PLOT_DIR, "dist_acc_diff.pdf"),
  p_t_dist,
  width = 5,
  height = 3
)

# PLOT: Comparing risk rankings (recall) ---------------------------------------
recall_pd <-
  predictions_df %>%
  filter(!feedback) %>%
  eval_recall(risk_col = "score", outcome_col = "outcome",
              groupby_vars = c("method", "features", "exp_name")) %>%
  mutate(
    exp_name = fct_relevel(
      exp_name,
      "COMPAS balanced BR",
      "COMPAS low BR",
      "LSI-R balanced BR"
    ),
    prop = round(prop, 5) # round to make sure the join works
  ) %>%
  left_join(posterior_draw_recalls %>% 
              mutate(prop = round(prop, 5)),
            by = c("method", "features", "exp_name", "prop")) %>%
  left_join(human_response_model_recall %>% 
              mutate(prop = round(prop, 5)),
            by = c("method", "features", "exp_name", "prop")) %>%
  mutate(
    recall_sd = coalesce(human_recall_sd, model_recall_sd),
    features = factor(features, 
                      levels=c('short', 'long'), 
                      labels=c("streamlined", "enriched"))
  )

message(rep("-", 80), "\nRecall at 50%:")
recall_at50 <- recall_pd %>%
  filter(prop == .5) %>%
  select(features, exp_name, method, recall, recall_sd) 
print(
  recall_at50 %>% 
    select(-recall_sd) %>% 
    spread(exp_name, recall)
)

p_recall <-
  ggplot(recall_pd, aes(y = recall, x = prop)) +
  geom_line(aes(color = method, linetype = method)) +
  # geom_ribbon(aes(ymin=recall - recall_sd * 2,
  #                 ymax=recall + recall_sd * 2,
  #                 fill = method), alpha=0.2) +
  scale_linetype_manual(
    element_blank(),
    limits = c("predicted_decision", "model_choice", "tool"),
    values = c("solid", "solid", "solid"),
    breaks = c("predicted_decision", "model_choice", "tool"),
    labels = c("Humans", "Logistic regression", "Existing tool")
  ) +
  scale_color_manual(
    element_blank(),
    limits = c("predicted_decision", "model_choice", "tool"),
    values = c("black", "red", "#0099e8"),
    breaks = c("predicted_decision", "model_choice", "tool"),
    labels = c("Humans", "Logistic regression", "Existing tool")
  ) +
  scale_fill_manual(
    element_blank(),
    limits = c("predicted_decision", "model_choice", "tool"),
    values = c("black", "red", "#0099e8"),
    breaks = c("predicted_decision", "model_choice", "tool"),
    labels = c("Humans", "Logistic regression", "Existing tool")
  ) +
  scale_x_continuous(
    "\nProportion of cases (ranked by estimated risk)",
    labels = scales::percent_format(1),
    expand = c(0, 0)
  ) +
  scale_y_continuous(
    "Proportion of recidivists (recall)\n",
    labels = scales::percent_format(1),
    expand = c(0, 0)
  ) +
  facet_grid(features ~ exp_name,
             scales = "free_y") +
  theme(
    legend.position = "top",
    axis.title = element_text(size = 16),
    axis.text = element_text(size = 10),
    legend.text = element_text(size = 14),
    legend.box.margin = margin(0, 0, 0, 0, "mm"),
    panel.spacing.x = unit(5, "mm"),
    panel.spacing.y = unit(5, "mm")
  ) +
  guides(fill = FALSE)
paper_results$p$p_recall <- p_recall

ggsave(
  file.path(PLOT_DIR, "perf_recall.pdf"),
  p_recall,
  width = 8,
  height = 5
)


# PLOT: Calibration plots ------------------------------------------------------
calib_pd <-
  predictions_df %>%
  filter(method == "predicted_decision") %>%
  group_by(method, score, features, feedback, exp_name) %>%
  summarize(
    N = n(),
    mean_pred = mean(score),
    p_recid = mean(outcome)
  ) %>%
  ungroup() %>%
  mutate(
    exp_name = fct_relevel(
      exp_name,
      "COMPAS balanced BR",
      "COMPAS low BR",
      "LSI-R balanced BR"
    ),
    features = fct_relevel(features, "short")
  )

target_preds_df <-
  bind_rows(
    predictions_df %>%
      filter(method == "predicted_decision",!feedback),
    predictions_df %>%
      filter(method == "predicted_decision", feedback) %>%
      group_by(user_id) %>%
      top_n(N_LAST, leave_time)
  )

p_calib <-
  ggplot(target_preds_df, aes(x = score, y = outcome)) +
  geom_abline(intercept = 0,
              slope = 1,
              linetype = "23") +
  geom_point(
    aes(
      x = mean_pred,
      y = p_recid,
      color = feedback,
      size = N
    ),
    data = calib_pd,
    shape = 1,
    alpha = .6
  ) +
  binomial_smooth(aes(color = feedback), size = .5, alpha = .4) +
  scale_size_area(guide = FALSE) +
  scale_color_hue(
    element_blank(),
    breaks = c(FALSE, TRUE),
    labels = c("No feedback", "Feedback")
  ) +
  scale_y_continuous(
    "Proportion recidivated\n",
    limits = c(0, 1),
    labels = scales::percent_format(1)
  ) +
  scale_x_continuous(
    "\nPredicted probability",
    limits = c(0, 1),
    labels = scales::percent_format(1)
  ) +
  facet_grid(features ~ exp_name,
             scales = "free_y",
             labeller = labeller(features = c(short = "streamlined",
                                              long = "enriched"))) +
  guides(color = guide_legend(title = element_blank(), 
                              override.aes = list(fill = NA))) +
  theme(
    legend.position = "top",
    axis.title = element_text(size = 16),
    axis.text = element_text(size = 11),
    legend.text = element_text(size = 14),
    panel.spacing.x = unit(8, "mm")
  ) +
  labs(caption = element_blank())
paper_results$p$p_calib <- p_calib
ggsave(
  file.path(PLOT_DIR, "human_calibration.pdf"),
  p_calib,
  width = 9,
  height = 5
)

# PLOT accuracy overtime -------------------------------------------------------
# sliding window size
sldg_wsize <- 10
step_size <- 1
sldg_wdw_perf <- results_df %>%
  group_by(user_group, user_id, exp_name) %>%
  mutate(user_qnum = 1:n()) %>%
  do(
    data.frame(
      window_id = .$user_qnum,
      slide_acc = sapply(.$user_qnum, function(x)
        ifelse((x >= sldg_wsize) && (x %% step_size == 0),
               mean(.[(x - sldg_wsize + 1):x, ]$bin_pred == 
                      .[(x - sldg_wsize + 1):x, ]$outcome),
               NA
        ))
    )
  )
sldg_wdw_perf_plot <- sldg_wdw_perf %>%
  gather("metric",
         "perf",
         slide_acc) %>%
  mutate(metric = factor(
    metric,
    levels = c("slide_acc"),
    labels = c("ACC")
  )) %>%
  filter(!is.na(perf))

sldg_wdw_perf_plot <-
  sldg_wdw_perf_plot[!rowSums(is.na(sldg_wdw_perf_plot)),] %>%
  group_by(exp_name, user_group, window_id, metric) %>%
  summarize(
    sd = sd(perf),
    se = sd(perf) / sqrt(n()),
    perf = mean(perf)
  )

# ACC increse overtime
p_acc_overtime <- ggplot(
  sldg_wdw_perf_plot %>%
    filter(metric == "ACC" &
             grepl("feedback", user_group)),
  aes(x = window_id, y = perf, color = user_group)
) +
  geom_line(size = 0.75) +
  geom_ribbon(
    aes(
      ymin = perf - qnorm(0.975) * se,
      ymax = perf + qnorm(0.975) * se
    ),
    linetype = 0,
    alpha = 0.1
  ) +
  facet_grid( ~ exp_name) +
  scale_y_continuous(labels = scales::percent_format(1)) +
  scale_x_continuous(limits = c(sldg_wsize, 50), expand = c(0, 0)) +
  scale_color_discrete(
    name = c("feedback streamlined", "feedback enriched"),
    labels = c("Streamlined", "Enriched")
  ) +
  labs(x = "Window ID", y = "Classification accuracy") +
  guides(color = guide_legend(title = element_blank(), 
                              override.aes = list(fill = NA))) +
  theme(
    legend.position = "top",
    panel.spacing = unit(1, "lines"),
    legend.text = element_text(size = 14)
  )
paper_results$p$p_acc_overtime <- p_acc_overtime
ggsave(
  p_acc_overtime,
  filename = file.path(PLOT_DIR, "acc_over_time.pdf"),
  width = 10,
  height = 4
)

# PRINT: Human machine performance for table -----------------------------------
# human vs. machine accuracy table
acc_perf_table <- acc_pd %>%
  full_join(
    # human vs. feedback human
    acc_pd %>%
      filter(feedback == "fb") %>%
      select(features, exp_name, fb_acc = acc, fb_se = se),
    by = c("features", "exp_name")
  ) %>%
  full_join(
    # no feedback human under long condition
    acc_pd %>%
      filter(feedback == "nofb", features == "long") %>%
      select(exp_name, nofb_long_acc = acc, nofb_long_se = se),
    by = c("exp_name")
  ) %>%
  full_join(# human vs. existing tool
    score_acc_pd %>%
      rename("et" = "acc"),
    by = c("exp_name")) %>%
  full_join(
    # human vs. model
    model_perfs %>%
      filter(measure == "Accuracy") %>%
      rename("model_acc" = "perf", "model_se" = "sd") %>%
      select(-feedback),
    by = c("features", "exp_name")
  ) %>%
  full_join(
    # human vs. model under long condition
    model_perfs %>%
      filter(measure == "Accuracy", features == "long") %>%
      select(
        exp_name,
        model_long_acc = perf,
        model_long_se = sd
      ),
    by = c("exp_name")
  ) %>%
  select(
    feedback,
    features,
    exp_name,
    acc,
    se,
    fb_acc,
    fb_se,
    model_acc,
    model_se,
    et,
    nofb_long_acc,
    nofb_long_se,
    model_long_acc,
    model_long_se
  )

# acc human vs. machine table
acc_hvm_perf_table <- acc_perf_table %>%
  mutate(
    et_h_diff = et - acc,
    et_h_diff_se = se,
    m_h_diff = model_acc - acc,
    m_h_diff_se = sqrt(se ^ 2 + model_se ^ 2)
  ) %>% 
  mutate(
    zprob_et_h_diff = pnorm(et_h_diff, 0, et_h_diff_se),
    et_h_diff = paste0(format(round(et_h_diff, 2), nsmall=2), 
                       " (", format(round(et_h_diff_se, 2), nsmall=2), ")"),
    zprob_m_h_diff = pnorm(m_h_diff, 0, m_h_diff_se),
    m_h_diff = paste0(format(round(m_h_diff, 2), nsmall=2), 
                      " (", format(round(m_h_diff_se, 2), nsmall=2), ")")
  ) %>%
  mutate(
    et_h_diff = bold_latex_stats_sig(et_h_diff, z=zprob_et_h_diff),
    m_h_diff = bold_latex_stats_sig(m_h_diff, z=zprob_m_h_diff)
  ) %>%
  select(
    feedback,
    features,
    expName = exp_name,
    "Existing tool - human diff" = et_h_diff,
    "Model - human diff" = m_h_diff
  )

paper_results$t$acc_hvm_perf_table_nofb <- acc_hvm_perf_table %>%
  filter(feedback == "nofb") %>%
  select(-feedback)
print_latex_table(paper_results$t$acc_hvm_perf_table_nofb)

paper_results$t$acc_hvm_perf_table_fb <- acc_hvm_perf_table %>%
  filter(feedback == "fb") %>%
  select(-feedback)
print_latex_table(paper_results$t$acc_hvm_perf_table_fb)

# human vs. machine AUC table
auc_perf_table <- auc_pd %>%
  full_join(
    # human vs. feedback human
    auc_pd %>%
      filter(feedback == "fb") %>%
      select(features, exp_name, fb_auc = auc, fb_se = se),
    by = c("features", "exp_name")
  ) %>%
  full_join(
    # no feedback human under long condition
    auc_pd %>%
      filter(feedback == "nofb", features == "long") %>%
      select(exp_name, nofb_long_auc = auc, nofb_long_se = se),
    by = c("exp_name")
  ) %>%
  full_join(# human vs. existing tool
    score_auc_pd %>%
      rename("et" = "auc"),
    by = c("exp_name")) %>%
  full_join(
    # human vs. model
    model_perfs %>%
      filter(measure == "AUC") %>%
      rename("model_auc" = "perf", "model_se" = "sd") %>%
      select(-feedback),
    by = c("features", "exp_name")
  ) %>%
  full_join(
    # human vs. model under long condition
    model_perfs %>%
      filter(measure == "AUC", features == "long") %>%
      select(
        exp_name,
        model_long_auc = perf,
        model_long_se = sd
      ),
    by = c("exp_name")
  ) %>%
  select(
    feedback,
    features,
    exp_name,
    auc,
    se,
    fb_auc,
    fb_se,
    model_auc,
    model_se,
    et,
    nofb_long_auc,
    nofb_long_se,
    model_long_auc,
    model_long_se
  )


# auc human vs. machine table
auc_hvm_perf_table <- auc_perf_table %>%
  mutate(
    et_h_diff = et - auc,
    et_h_diff_se = se,
    m_h_diff = model_auc - auc,
    m_h_diff_se = sqrt(se ^ 2 + model_se ^ 2)
  ) %>% 
  mutate(
    zprob_et_h_diff = pnorm(et_h_diff, 0, et_h_diff_se),
    et_h_diff = paste0(format(round(et_h_diff, 2), nsmall=2), 
                       " (", format(round(et_h_diff_se, 2), nsmall=2), ")"),
    zprob_m_h_diff = pnorm(m_h_diff, 0, m_h_diff_se),
    m_h_diff = paste0(format(round(m_h_diff, 2), nsmall=2), 
                      " (", format(round(m_h_diff_se, 2), nsmall=2), ")")
  ) %>%
  mutate(
    et_h_diff = bold_latex_stats_sig(et_h_diff, z=zprob_et_h_diff),
    m_h_diff = bold_latex_stats_sig(m_h_diff, z=zprob_m_h_diff)
  ) %>%
  select(
    feedback,
    features,
    expName = exp_name,
    "Existing tool - human diff" = et_h_diff,
    "Model - human diff" = m_h_diff
  )

paper_results$t$auc_hvm_perf_table_nofb <- auc_hvm_perf_table %>%
  filter(feedback == "nofb") %>%
  select(-feedback)

print_latex_table(paper_results$t$auc_hvm_perf_table_nofb)

# human VS human feedback
acc_hvh_perf_table <- acc_perf_table %>%
  filter(feedback == "nofb") %>%
  mutate(
    fbh_h_diff = fb_acc - acc,
    fbh_h_diff_se = sqrt(se ^ 2 + fb_se ^ 2)
  ) %>% 
  mutate(
    zprob_fbh_h_diff = pnorm(fbh_h_diff, 0, fbh_h_diff_se),
    fbh_h_diff = paste0(format(round(fbh_h_diff, 2), nsmall=2), 
                        " (", format(round(fbh_h_diff_se, 2), nsmall=2), ")")
  ) %>%
  mutate(
    fbh_h_diff = bold_latex_stats_sig(fbh_h_diff, z=zprob_fbh_h_diff)
  ) %>%
  select(features,
         expName = exp_name,
         "Feedback human - human diff" = fbh_h_diff) %>%
  arrange(desc(features))

paper_results$t$acc_hvh_perf_table <- acc_hvh_perf_table
print_latex_table(acc_hvh_perf_table)

# short vs. long
auc_short_long_perf_table <- auc_perf_table %>%
  filter(features == "short", feedback == "nofb") %>%
  drop_na() %>%
  mutate(
    m_ls_diff = model_long_auc - model_auc,
    m_ls_diff_se = sqrt(model_long_se ^ 2 + model_se ^ 2),
    h_ls_diff = nofb_long_auc - auc,
    h_ls_diff_se = sqrt(nofb_long_se ^ 2 + se ^ 2)
  ) %>% 
  mutate(
    zprob_m_ls_diff = pnorm(m_ls_diff, 0, m_ls_diff_se),
    m_ls_diff = paste0(format(round(m_ls_diff, 2), nsmall=2), 
                       " (", format(round(m_ls_diff_se, 2), nsmall=2), ")"),
    zprob_h_ls_diff = pnorm(h_ls_diff, 0, h_ls_diff_se),
    h_ls_diff = paste0(format(round(h_ls_diff, 2), nsmall=2), 
                       " (", format(round(h_ls_diff_se, 2), nsmall=2), ")")
  ) %>%
  mutate(
    m_ls_diff = bold_latex_stats_sig(m_ls_diff, z=zprob_m_ls_diff),
    h_ls_diff = bold_latex_stats_sig(h_ls_diff, z=zprob_h_ls_diff)
  ) %>%
  select(expName = exp_name,
         model = m_ls_diff,
         human = h_ls_diff) %>%
  gather(predictor, difference, model, human) %>%
  mutate(measure = "AUC")

acc_short_long_perf_table <- acc_perf_table %>%
  filter(features == "short", feedback == "nofb") %>%
  drop_na() %>%
  mutate(
    m_ls_diff = model_long_acc - model_acc,
    m_ls_diff_se = sqrt(model_long_se ^ 2 + model_se ^ 2),
    h_ls_diff = nofb_long_acc - acc,
    h_ls_diff_se = sqrt(nofb_long_se ^ 2 + se ^ 2)
  ) %>% 
  mutate(
    zprob_m_ls_diff = pnorm(m_ls_diff, 0, m_ls_diff_se),
    m_ls_diff = paste0(format(round(m_ls_diff, 2), nsmall=2), 
                       " (", format(round(m_ls_diff_se, 2), nsmall=2), ")"),
    zprob_h_ls_diff = pnorm(h_ls_diff, 0, h_ls_diff_se),
    h_ls_diff = paste0(format(round(h_ls_diff, 2), nsmall=2), 
                       " (", format(round(h_ls_diff_se, 2), nsmall=2), ")")
  ) %>%
  mutate(
    m_ls_diff = bold_latex_stats_sig(m_ls_diff, z=zprob_m_ls_diff),
    h_ls_diff = bold_latex_stats_sig(h_ls_diff, z=zprob_h_ls_diff)
  ) %>%
  select(expName = exp_name,
         model = m_ls_diff,
         human = h_ls_diff) %>%
  gather(predictor, difference, model, human) %>%
  mutate(measure = "Accuracy")

short_long_perf_table <-
  bind_rows(auc_short_long_perf_table, acc_short_long_perf_table) %>%
  spread(measure, difference) %>%
  select(predictor, expName, Accuracy, AUC) %>%
  arrange(predictor)

paper_results$t$short_long_perf_table <- short_long_perf_table
print_latex_table(short_long_perf_table)

# PRINT: recall at 50% test ----------------------------------------------------
recall_at50_mean <- recall_at50 %>%
  mutate(method_recall = paste0(method, "_recall")) %>%
  select(-method, -recall_sd) %>%
  spread(method_recall, recall)
recall_at50_sd <- recall_at50 %>%
  mutate(method_sd = paste0(method, "_recall_sd")) %>%
  select(-method, -recall) %>%
  spread(method_sd, recall_sd)

recall_at50_table <- recall_at50_mean %>%
  inner_join(recall_at50_sd, by = c("features", "exp_name")) %>%
  mutate(
    m_h_recall50_diff = model_choice_recall - predicted_decision_recall,
    m_h_recall50_diff_se = 
      sqrt(model_choice_recall_sd ^ 2 + predicted_decision_recall_sd ^ 2),
    et_h_recall50_diff = tool_recall - predicted_decision_recall,
    et_h_recall50_diff_se = predicted_decision_recall_sd
  ) %>% 
  mutate(
    zprob_m_h_recall50_diff = 
      pnorm(m_h_recall50_diff, 0, m_h_recall50_diff_se),
    m_h_recall50_diff = paste0(
      format(round(m_h_recall50_diff, 2), nsmall=2), 
      " (",format(round(m_h_recall50_diff_se, 2), nsmall=2),")"
    ),
    zprob_et_h_recall50_diff = 
      pnorm(et_h_recall50_diff, 0, et_h_recall50_diff_se),
    et_h_recall50_diff = paste0(
      format(round(et_h_recall50_diff, 2), nsmall=2), 
      " (",format(round(et_h_recall50_diff_se, 2), nsmall=2),")"
    )
  ) %>%
  mutate(
    m_h_recall50_diff = bold_latex_stats_sig(m_h_recall50_diff, 
                                             z=zprob_m_h_recall50_diff),
    et_h_recall50_diff = bold_latex_stats_sig(et_h_recall50_diff, 
                                              z=zprob_et_h_recall50_diff)
  ) %>%
  select(
    features,
    expName = exp_name,
    "Existing tool - human recall at 50% difference" = et_h_recall50_diff,
    "Model - human recall at 50% difference" = m_h_recall50_diff
  ) %>%
  arrange(desc(features))

paper_results$t$recall_at50_table <- recall_at50_table
print_latex_table(recall_at50_table)


# save paper results
write_rds(paper_results, PAPER_RESULTS_RDS)

# Turn warnings back on
options(warn = 0)
