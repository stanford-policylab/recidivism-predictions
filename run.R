#!/usr/bin/env Rscript

setwd('src')

# Imports, consts, and helpers
source("pkgs.R")
source("consts.R")
source("helpers.R")
source("modeling.R")
source("eval_func.R")
source("prepare_data.R")

# Settings for plots and table displays
options(width = 200)
theme_set(theme_bw())

# Analysis scripts
source("preprocess.R")
source("preds_compas.R")
source("preds_lsi_all.R")
source("process_data.R")
source("postprocess_preds.R")
source("plot.R")
