#!/usr/bin/env Rscript

# Load packages and setup environment
#   Changes made to this script should not invalidate build cache!
PKGS <- c(
  "scales",
  "matrixStats",
  "boot",
  "arm",
  "ROCR",
  "assertthat",
  "doMC",
  "forcats",
  "furrr",
  "lubridate",
  "kableExtra",
  "tidyverse"
)

suppressMessages(PKGS <-
                   sapply(
                     PKGS,
                     library,
                     character.only = TRUE,
                     logical.return = TRUE
                   ))

if (any(PKGS == FALSE)) {
  print(PKGS)
  stop("Failed loading packages.")
}


message("Loaded packages:", paste(names(PKGS), collapse = ", "))

N_CORES <- min(20, availableCores() / 2)

message(sprintf("Futures planned for %s cores", N_CORES))

registerDoMC(cores = 10)  # This is only used in parallel CV for cv.glmnet
plan(multiprocess(workers = eval(N_CORES)))  # Plan for futures
