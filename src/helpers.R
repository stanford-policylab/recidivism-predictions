if (!exists("HELPERS_R__")) {
  source("consts.R")

  # Printing/debuging functions ------------------------------------------------
  print_sep <- function(msg = "") {
    if (nchar(msg) < 1) {
      message(rep("-", 80))
    } else {
      spaces <- max(4, 80 - 1 - nchar(msg))
      message(msg, " ", rep("-", spaces))
    }
  }
  
  print_latex_table <- function(df) {
    kable(df, "latex", escape = F) %>%
      kable_styling() %>%
      collapse_rows()
  }
  
  bold_latex_stats_sig <-function(s, z) {
    ifelse(z < 0.025 | z > 0.975, paste0("\\textbf{", s, "}"), s )
  }
  
  HELPERS_R__ <- TRUE
} else {
  message("helpers.R already loaded")
}
