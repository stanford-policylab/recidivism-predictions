source("eval_func.R")

clean_data <- function(user_df, resp_df, vign_df, is_compas) {
  # User comments --------------------------------------------------------------
  user_df %>%
    filter(!is.na(comment)) %>%
    pull(comment)
  
  # Clean and join -------------------------------------------------------------
  message(sprintf("Collected %d responses", nrow(user_df)))
  
  message(sprintf(
    "Removing users who did not submit (%d)",
    sum(user_df$exit_status != "submitted")
  ))
  user_df %>%
    group_by(exit_status) %>%
    summarize(N = n())
  
  # keep only one unique (user_id, assignment_id) pair in user_df
  message(sprintf(
    "De-duping assignments that were recorded twice (%d)",
    nrow(user_df) - nrow(user_df %>% distinct(user_id, assignment_id))
  ))
  
  user_df <- user_df %>%
    distinct(user_id, assignment_id, .keep_all = TRUE)
  
  user_df <- user_df %>%
    filter(exit_status == "submitted") %>%
    group_by(user_id) %>%
    mutate(n_resp = n()) %>%
    ungroup()
  
  # remove users who are still have > 1 submissions after de-dup
  message(
    sprintf(
      "Removing users who completed more than one assignment (%d)",
      user_df %>%
        filter(n_resp != 1) %>%
        pull(user_id) %>%
        unique() %>%
        length()
    )
  )
  user_df %>%
    filter(n_resp > 1) %>%
    group_by(user_id) %>%
    summarize(n_resp = max(n_resp)) %>%
    arrange(desc(n_resp))
  user_df <- user_df %>%
    filter(n_resp == 1)
  
  valid_resp <- user_df %>%
    left_join(resp_df, by = c("user_id", "user_block", "user_group")) %>%
    left_join(vign_df %>% select(individual_id, outcome),
              by = "individual_id")
  
  # Compute performance for compas ---------------------------------------------
  if (is_compas) {
    message("Baseline COMPAS performance")
    vign_df %>%
      filter(!grepl("dummy", individual_id)) %>%
      summarize(auc = compute_auc(compas_decile_score, outcome),
                acc = mean((compas_decile_score > 5) == outcome))
  }
  
  message("Evaluating all ", length(unique(valid_resp$user_id)), " responses")
  eval_responses(valid_resp %>% filter(!grepl("dummy", individual_id)))
  
  message("Evaluating responses that passed attn check")
  attn_check <- valid_resp %>%
    filter(grepl("dummy", individual_id)) %>%
    mutate(answer = if_else(outcome == 0, 2, 98),
           correct = answer == predicted_decision) %>%
    group_by(user_id) %>%
    summarize(pass = mean(correct == 1))
  message(sprintf(
    "%d/%d submitted users passed all attention checks",
    sum(attn_check$pass == 1),
    nrow(attn_check)
  ))
  
  clean_df <- valid_resp %>%
    filter(!grepl("dummy", individual_id)) %>%
    left_join(attn_check, by = c("user_id")) %>%
    filter(pass == 1)
  
  # remove duplicated repsonses ----------------------
  # remove responses with > 50 rows in clean_df or
  # only keep the first 50 if all the same
  is_bad_dup_resp <- function(df, nq = REAL_N_CASES) {
    group_df <- df %>%
      mutate(qnum = (0:(n() - 1)) %/% nq)
    qnum_group <- unique(group_df$qnum)
    for (i in 0:(length(qnum_group) - 2)) {
      for (j in (i + 1):(length(qnum_group) - 1)) {
        g1 <- group_df %>% filter(qnum == i) %>% select(-qnum)
        g2 <- group_df %>% filter(qnum == j) %>% select(-qnum)
        if (!all(g1 == g2, na.rm = T)) {
          return(TRUE)
        }
      }
    }
    return(FALSE)
  }
  
  good_dup_resp <- clean_df %>%
    group_by(user_id) %>%
    do(data.frame(
      is_bad_dup_resp = ifelse(
        nrow(.) > REAL_N_CASES,
        is_bad_dup_resp(., nq = REAL_N_CASES),
        FALSE
      )
    )) %>%
    filter(is_bad_dup_resp == FALSE) %>%
    select(user_id)
  
  clean_df <- clean_df %>%
    inner_join(good_dup_resp, by = c("user_id")) %>%
    group_by(user_id) %>%
    do(head(., n = REAL_N_CASES))
  
  
  # set up user group factor labels
  clean_df <- clean_df %>%
    mutate(user_group = factor(
      user_group,
      levels = c("vignette", "feedback_vignette", "full", "feedback_full"),
      labels = c("short", "feedback short", "long", "feedback long")
    ))
  
  list(clean_df = clean_df,
       valid_resp = valid_resp,
       attn_check = attn_check)
}
