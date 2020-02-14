#!/usr/bin/env Rscript
# Preprocess COMPAS data for experiments

# Preprocess Compas data ----------------------------------------------------------
compas_df <- read_csv(COMPAS_SCORE_PATH)
test_df <- read_csv(VIGN_PATH) %>%
  rename(degree = `charge_degree (misd/fel)`)

# Write violent recidivism data
test_df %>%
  select(-outcome) %>%
  left_join(compas_df %>% select(id, outcome = is_violent_recid), by = "id") %>%
  write_csv(VIOLENT_VIGN_PATH)

# Write fulldata rds
read_csv(BROWARD_CLEAN_PATH) %>%
  rename(degree = `charge_degree (misd/fel)`) %>%
  left_join(
    compas_df %>%
      select(
        -race,
        -sex,
        -age,-juv_fel_count,
        -juv_misd_count,
        -priors_count
      ),
    by = "id"
  ) %>%
  select(-decile_score_1) %>%
  mutate(fold = if_else(id %in% test_df$id, "test", "train")) %>%
  write_rds(DF_FULL_RDS)

# Preprocess LSI data ----------------------------------------------------------
# Check if raw LSI files exists
all_raw_lsi_files_exist <- all(unlist(map(TARGET_SITES,
                                          ~ file.exists(
                                            file.path(
                                              PRIVATE_DATA_DIR,
                                              paste0("individuals/lsi_vignettes_site_",
                                                     .x, ".csv")
                                            )
                                          ))))

# if raw LSI files exists, pre-process the data
if (all_raw_lsi_files_exist) {
  # Read data
  lsi_full_df <- map_dfr(
    TARGET_SITES,
    ~
      read_csv(file.path(
        PRIVATE_DATA_DIR,
        paste0("individuals/lsi_vignettes_site_", .x, ".csv")
      )) %>%
      mutate(
        ch_cat = cut(ch, c(-Inf, 1, 3, Inf), LMH_LABELS),
        ee_cat = cut(ee, c(-Inf, 1, 3, Inf), LMH_LABELS),
        fin_cat = cut(fin, c(-Inf, 0, 1, Inf), LMH_LABELS),
        fam_cat = cut(fam, c(-Inf, 0, 1, Inf), LMH_LABELS),
        acc_cat = cut(acc, c(-Inf, 0, 1, Inf), LMH_LABELS),
        leisure_cat = cut(leisure, c(-Inf, 0, 1, Inf), LMH_LABELS),
        peers_cat = cut(peers, c(-Inf, 1, 2, Inf), LMH_LABELS),
        drugs_cat = cut(drugs, c(-Inf, 0, 2, Inf), LMH_LABELS),
        mh_cat = cut(mh, c(-Inf, 0, 1, Inf), LMH_LABELS),
        cog_cat = cut(cog, c(-Inf, 0, 1, Inf), LMH_LABELS)
      ) %>%
      select(-hispanic) # un-used missing column
  )
  
  # Clean data and compute estimates
  complete_rows <- lsi_full_df %>%
    select(recid, age, male, ends_with("_cat")) %>%
    complete.cases
  lsi_full_df <- lsi_full_df %>%
    filter(complete_rows)
  
  write_rds(lsi_full_df, LSI_ALL_RDS)
} else {
  message("No LSI raw data, skipping creating LSI_ALL_RDS...")
}
