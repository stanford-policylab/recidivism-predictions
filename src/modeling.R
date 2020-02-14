# Packages and helpers specific to model fitting and prediction
if (!exists("MODELING_R__")) {
  library("glmnet")
  library("gbm")
  
  .get_mm <- function (d, f, discrete_y = TRUE) {
    # Given a data.frame and formula, return the model matrix X and y as a list
    
    # Args:
    #   d: data frame used for training
    #   f: model formula
    #   discrete_y: boolean indicating whether y is discrete or continuous
    
    # Reutrns:
    #   List with elements X (the model matrix) and y (labels, extracted from the
    #   LHS of formula f)
    X <- model.matrix(f, d)
    
    # Extract target column from LHS of f
    y <- d[[as.character(rlang::f_lhs(f))]]
    
    # Heuristic to determine whether y should be discrete or continuous
    if (discrete_y) {
      y <- as.factor(y)
    }
    
    list(X = X, y = y)
  }
  
  pred_with_mm <- function(d, f, m, use_column, as = NULL, ...) {
    # Generate predictions and appends to the data frame as a new column, for a
    # model by first converting the data to a model matrix based on the formula
    # provided
    
    # Args:
    #   d: data frame to predict for
    #   f: model formula
    #   m: prediction model
    #   use_column: the column from prediction results to keep in the data frame
    #       e.g., for 1 for glmnet (single column), and 2 for randomForest
    #       (assuming the positive class prediction is of interest)
    #   as: character name of prediction column to add
    #   ...: other arguments passed to the general predict function
    
    # Reutrns:
    #   data frame with single column of name `as` containing predictions
    
    mm <- .get_mm(d, f)  # Don't worry about y --- not used
    
    if (is.null(as)) {
      predict(m, mm$X, ...)[, use_column]
    } else {
      d %>%
        transmute(!!as := predict(m, mm$X, ...)[, use_column])
    }
  }
  
  fit_glmnet <-
    function(d,
             f,
             family = "binomial",
             alpha = 1,
             ...) {
      # Fits a glmnet model, taking care of model matrix and other junk
      #
      # Args:
      #   d: data frame used for training
      #   f: model formula
      #   family: model type, passed as the family argument to cv.glmnet
      #   alpha: 1 for Lasso, 0 for Ridge; passed to cv.glmnet
      #   ...: arguments passed to cv.glmnet
      #
      # Reutrns:
      #   cv.glmnet object
      
      # Extract model matrix given data and formula
      mm <-
        .get_mm(d, f, discrete_y = ifelse(family == "binomial", TRUE, FALSE))
      X <- mm$X
      y <- mm$y
      
      cv.glmnet(X,
                y,
                family = family,
                alpha = alpha,
                parallel = TRUE,
                ...)
    }
  
  
  mm_lr <- function(df, f, beta) {
    # manually calculate logistic regression given a feature matrix and a beta
    # coef matrix representing a series of beta coefs
    #
    # Args:
    #   mm: matrix including the intercept of size n x m
    #   beta: beta coefs of size k x m
    #
    # Return:
    #   df: modified df with pos_sample_id and pos_sample_preds
    #       with n * n_pos_samples rows
    
    prob_mm <- inv.logit(.get_mm(df, f)$X %*% t(beta))
    n_pos_samples <- ncol(prob_mm)
    prob_mm_vec <- as.vector(t(prob_mm))
    df %>%
      mutate(pos_sample_id = map(1:nrow(df), ~ 1:n_pos_samples)) %>%
      unnest(pos_sample_id) %>%
      mutate(pos_sample_preds = prob_mm_vec)
    
  }
  
  recall_from_pos_samples <-
    function(posterior_preds,
             site = F,
             risk_col = "pos_sample_choice",
             outcome_col = "outcome") {
      # Obtain recall mean and SD from posterior samples
      #
      # Args:
      #   posterior_preds: matrix including the intercept of size n x m
      #   site: if aggregate by site (for LSI-R data)
      #   risk_col: risk column name
      #   outcome_col: outcome column name
      #
      # Return:
      #   posterior recalls for each proportion of individuals detained
      if (site) {
        return(
          posterior_preds %>%
            eval_recall(risk_col = risk_col, outcome_col = outcome_col, 
                        groupby_vars = c("model", "features", "target", "user_group", "pos_sample_id", "site")) %>%
            group_by(model, features, target, user_group, prop, site) %>%
            summarize(
              recall_mean = mean(recall),
              recall_sd = sd(recall)
            ) %>%
            ungroup()
        )
      } else {
        return(
          posterior_preds %>%
            eval_recall(risk_col = risk_col, outcome_col = outcome_col, 
                        groupby_vars = c("model", "features", "target", "user_group", "pos_sample_id")) %>%
            group_by(model, features, target, user_group, prop) %>%
            summarize(
              recall_mean = mean(recall),
              recall_sd = sd(recall)
            ) %>%
            ungroup()
        )
      }
      
    }
  
  pred_loo <- function(d, fs, ms, models = names(ms)) {
    # Given some data frame d, compute LOO predictions for given formulas
    future_map_dfr(1:nrow(d), .progress = TRUE, function(i) {
      # Fit models leaving the i-th row out
      train <- d[-i, ]
      test <- d[i, ]
      
      models_df <- expand.grid(
        features = names(fs),
        model = models,
        stringsAsFactors = FALSE
      ) %>%
        as_tibble() %>%
        mutate(f = fs[features],
               m = map2(f, model, ~ ms[[.y]](.x, train)))
      
      models_df %>%
        mutate(index = i,
               preds = pmap(list(model, m, f, list(test)), get_preds)) %>%
        unnest(preds) %>%
        select(-f, -m)
    }) %>%
      mutate(choice =
               SURVEY_SCALE[vapply(pred,
                                   function(x)
                                     which.min(abs(x - SURVEY_SCALE)),
                                   integer(1))])
  }
  
  
  # Compas covariates ------------------------------------------------------
  df_covars <- c("sex",
                 "age",
                 "juv_fel_count",
                 "juv_misd_count",
                 "priors_count",
                 "degree")
  df_race <- c("race")
  df_recid_target <- "two_year_recid"
  df_violent_target <- "is_violent_recid"
  
  compas_recid_score <- "compas_decile_score"
  compas_violent_score <- "v_decile_score"
  
  # Setup for modeling
  df_covariates <- list(
    full = c(
      df_race,
      df_covars,
      compas_recid_score,
      compas_violent_score
    ),
    long = c(df_covars, compas_recid_score, compas_violent_score),
    short = df_covars
  )
  
  df_targets <- c(recid = df_recid_target,
                  violent = df_violent_target)
  
  
  # LSI covariates ----------------------------------------------------------
  lsi_target <- "outcome"
  lsi_covars_full <-
    c("ch",
      "ee",
      "fin",
      "fam",
      "acc",
      "leisure",
      "peers",
      "drugs",
      "mh",
      "cog")
  lsi_covars_long <-
    c(
      "ch_cat",
      "ee_cat",
      "fin_cat",
      "fam_cat",
      "acc_cat",
      "leisure_cat",
      "peers_cat",
      "drugs_cat",
      "mh_cat",
      "cog_cat"
    )
  lsi_covars_short <- "ch_cat"
  lsi_covars_demo <- c("age", "male")
  lsi_formulas <- c(
    full = reformulate(
      c(lsi_covars_demo, lsi_covars_full, lsi_covars_long),
      lsi_target
    ),
    long = reformulate(c(lsi_covars_demo, lsi_covars_long), lsi_target),
    short = reformulate(c(lsi_covars_demo, lsi_covars_short), lsi_target)
  )
  
  # Model specifications ----------------------------------------------------
  model_specs <- c(
    logit = function(f, x)
      glm(f, x, family = binomial),
    gbm = function(f, x)
      gbm(
        f,
        data = x,
        distribution = "adaboost",
        n.trees = 1000,
        shrinkage = 0.01,
        train.fraction = .8,
        interaction.depth = 5,
        n.cores = 1
      ),
    l1 = function(f, x)
      fit_glmnet(x, f),
    l2 = function(f, x)
      fit_glmnet(x, f, alpha = 0)
  )
  
  get_preds <- function(typ, mod, fun, dat) {
    pred_specs <- c(
      logit = function(f, m, d)
        d %>%
        mutate(pred = predict(m, d, type = "response")),
      gbm = function(f, m, d)
        d %>%
        mutate(pred = predict(
          m,
          d,
          type = "response",
          n.trees = gbm.perf(m, method = "test",
                             plot.it = FALSE)
        )),
      l1 = function(f, m, d)
        d %>%
        mutate(pred = pred_with_mm(
          d, f, m, 1,
          s = "lambda.min", type = "response"
        )),
      l2 = function(f, m, d)
        d %>%
        mutate(pred = pred_with_mm(
          d, f, m, 1,
          s = "lambda.min", type = "response"
        ))
    )
    
    pred_fun <- pred_specs[[typ]]
    pred_fun(fun, mod, dat)
  }
  
  MODELING_R__ <- TRUE
} else {
  message("modeling.R already loaded")
}
