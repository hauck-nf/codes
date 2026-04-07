#PERFORM BASIC ANALYSIS ON PSYCHOMETRIC SCALES
#The function analyze_psychometrics uses the psych function scoreItems to produce: 
#  1) a list called "psychometrics" containing descriptive analysis and internal consistency estimates (alpha and G6); and
#  2) a data frame containing the original data plus the calculated scores for the target scales.

#The function required two r objects:
#  1) a data frame called data containing persons x variables
#  2) a dictionary containing the following metadata columns: 
#    a) "itemcode": with the names of the items to be analyzed (matching the names in the data frame);
#    b) "key": with each item coded as 1 when it is positively correlated with its corresponding factor (positively-keyed), and -1 when it is negatively correlated with the factor (positively-keyed);
#    c) "higher_order": with the name of the higher order factor that explains the item (or its first order factor) and that should be used as a guide to form broader scale scores
#    d) "scale": with the name of the scale where the item should be allocated in the calculation of scores and internal consistency estimates
#ANALYSE_PSYCHOMETRICS FUNCTION
analyze_psychometrics_hierarchical <- function(data, dictionary,
                                               missing = TRUE,
                                               impute = "none") {
  
  # -----------------------------
  # 1) Basic validation
  # -----------------------------
  if (!is.data.frame(data)) {
    stop("'data' must be a data frame.")
  }
  
  if (!is.data.frame(dictionary)) {
    stop("'dictionary' must be a data frame.")
  }
  
  required_cols <- c("itemcode", "instrument", "scale", "higher_order", "key")
  missing_cols <- setdiff(required_cols, names(dictionary))
  
  if (length(missing_cols) > 0) {
    stop("The dictionary is missing required columns: ",
         paste(missing_cols, collapse = ", "))
  }
  
  if (!all(dictionary$key %in% c(1, -1))) {
    stop("Column 'key' must contain only 1 and -1.")
  }
  
  if (anyDuplicated(dictionary$itemcode) > 0) {
    duplicated_items <- unique(dictionary$itemcode[duplicated(dictionary$itemcode)])
    warning("Duplicated itemcodes detected in dictionary: ",
            paste(duplicated_items, collapse = ", "),
            ". This is acceptable only if duplication is intentional for higher-order scoring.")
  }
  
  missing_items <- setdiff(unique(dictionary$itemcode), names(data))
  if (length(missing_items) > 0) {
    stop("These itemcodes are not present in data: ",
         paste(missing_items, collapse = ", "))
  }
  
  # -----------------------------
  # 2) Helper: extract alpha/G6 safely
  # -----------------------------
  extract_reliability <- function(scoreitems_obj) {
    
    alpha_val <- NA_real_
    g6_val <- NA_real_
    
    if (!is.null(scoreitems_obj$alpha)) {
      if (is.numeric(scoreitems_obj$alpha)) {
        alpha_val <- unname(scoreitems_obj$alpha[1])
      } else if (is.matrix(scoreitems_obj$alpha) || is.data.frame(scoreitems_obj$alpha)) {
        if ("raw_alpha" %in% colnames(scoreitems_obj$alpha)) {
          alpha_val <- as.numeric(scoreitems_obj$alpha[1, "raw_alpha"])
        } else {
          alpha_val <- as.numeric(scoreitems_obj$alpha[1, 1])
        }
      }
    }
    
    if (!is.null(scoreitems_obj$G6)) {
      if (is.numeric(scoreitems_obj$G6)) {
        g6_val <- unname(scoreitems_obj$G6[1])
      } else if (is.matrix(scoreitems_obj$G6) || is.data.frame(scoreitems_obj$G6)) {
        g6_val <- as.numeric(scoreitems_obj$G6[1, 1])
      }
    } else if (!is.null(scoreitems_obj$alpha)) {
      if ((is.matrix(scoreitems_obj$alpha) || is.data.frame(scoreitems_obj$alpha)) &&
          "G6(smc)" %in% colnames(scoreitems_obj$alpha)) {
        g6_val <- as.numeric(scoreitems_obj$alpha[1, "G6(smc)"])
      }
    }
    
    list(alpha = alpha_val, G6 = g6_val)
  }
  
  # -----------------------------
  # 3) Helper: run one construct
  # -----------------------------
  run_one_construct <- function(data, dict_subset, construct_name,
                              instrument_name, level_name,
                              missing = TRUE, impute = "none") {
  
  dict_subset <- dict_subset[!duplicated(dict_subset$itemcode), , drop = FALSE]
  
  items_df <- data[, dict_subset$itemcode, drop = FALSE]
  
  keyed_items <- ifelse(
    dict_subset$key == 1,
    dict_subset$itemcode,
    paste0("-", dict_subset$itemcode)
  )
  
  keys_list <- stats::setNames(list(keyed_items), construct_name)
  
  keys_matrix <- psych::make.keys(
    nvars = ncol(items_df),
    keys.list = keys_list,
    item.labels = colnames(items_df)
  )
  
  sc_result <- psych::scoreItems(
    keys = keys_matrix,
    items = items_df,
    missing = missing,
    impute = impute
  )
  
  rel <- extract_reliability(sc_result)
  
  score_df <- as.data.frame(sc_result$scores)
  names(score_df) <- paste(instrument_name, construct_name, sep = "__")
  score_vector <- score_df[[1]]
  
  descriptives_df <- data.frame(
    instrument = instrument_name,
    level = level_name,
    scale = construct_name,
    n = sum(!is.na(score_vector)),
    mean = mean(score_vector, na.rm = TRUE),
    sd = stats::sd(score_vector, na.rm = TRUE),
    min = min(score_vector, na.rm = TRUE),
    max = max(score_vector, na.rm = TRUE),
    stringsAsFactors = FALSE
  )
  
  reliability_df <- data.frame(
    instrument = instrument_name,
    level = level_name,
    scale = construct_name,
    n_items = ncol(items_df),
    alpha = rel$alpha,
    G6 = rel$G6,
    stringsAsFactors = FALSE
  )
  
  audit_df <- data.frame(
    instrument = instrument_name,
    level = level_name,
    scale = construct_name,
    itemcode = dict_subset$itemcode,
    key = dict_subset$key,
    stringsAsFactors = FALSE
  )
  
  list(
    scoreitems = sc_result,
    scores = score_df,
    reliability = reliability_df,
    descriptives = descriptives_df,
    audit = audit_df
  )
}
  
  # -----------------------------
  # 4) Main loop by instrument
  # -----------------------------
  instruments <- unique(dictionary$instrument)
  
  by_instrument <- list()
  all_scores <- list()
  all_reliability <- list()
  all_descriptives <- list()
  all_audit <- list()
  
  for (inst in instruments) {
    
    dict_inst <- dictionary[dictionary$instrument == inst, , drop = FALSE]
    
    instrument_results <- list(
      first_order = list(),
      higher_order = list()
    )
    
    instrument_scores <- list()
    instrument_reliability <- list()
    instrument_descriptives <- list()
    instrument_audit <- list()
    
    # First-order
    first_order_constructs <- unique(dict_inst$scale)
    first_order_constructs <- first_order_constructs[!is.na(first_order_constructs) & first_order_constructs != ""]
    
    for (sc in first_order_constructs) {
      dict_sc <- dict_inst[dict_inst$scale == sc, , drop = FALSE]
      
      res_sc <- run_one_construct(
        data = data,
        dict_subset = dict_sc,
        construct_name = sc,
        instrument_name = inst,
        level_name = "first_order",
        missing = missing,
        impute = impute
      )
      
      instrument_results$first_order[[sc]] <- res_sc$scoreitems
      instrument_scores[[paste(inst, sc, sep = "__")]] <- res_sc$scores
      instrument_reliability[[paste("first", sc, sep = "__")]] <- res_sc$reliability
      instrument_descriptives[[paste("first", sc, sep = "__")]] <- res_sc$descriptives
      instrument_audit[[paste("first", sc, sep = "__")]] <- res_sc$audit
    }
    
    # Higher-order
    higher_order_constructs <- unique(dict_inst$higher_order)
    higher_order_constructs <- higher_order_constructs[!is.na(higher_order_constructs) & higher_order_constructs != ""]
    
    if (length(higher_order_constructs) > 0) {
      for (ho in higher_order_constructs) {
        dict_ho <- dict_inst[dict_inst$higher_order == ho, , drop = FALSE]
        
        res_ho <- run_one_construct(
          data = data,
          dict_subset = dict_ho,
          construct_name = ho,
          instrument_name = inst,
          level_name = "higher_order",
          missing = missing,
          impute = impute
        )
        
        instrument_results$higher_order[[ho]] <- res_ho$scoreitems
        instrument_scores[[paste(inst, ho, sep = "__")]] <- res_ho$scores
        instrument_reliability[[paste("higher", ho, sep = "__")]] <- res_ho$reliability
        instrument_descriptives[[paste("higher", ho, sep = "__")]] <- res_ho$descriptives
        instrument_audit[[paste("higher", ho, sep = "__")]] <- res_ho$audit
      }
    }
    
    instrument_scores_df <- dplyr::bind_cols(instrument_scores)
    instrument_reliability_df <- dplyr::bind_rows(instrument_reliability)
    instrument_descriptives_df <- dplyr::bind_rows(instrument_descriptives)
    instrument_audit_df <- dplyr::bind_rows(instrument_audit)
    
    by_instrument[[inst]] <- list(
      scoreitems = instrument_results,
      scores = instrument_scores_df,
      reliability = instrument_reliability_df,
      descriptives = instrument_descriptives_df,
      audit = instrument_audit_df
    )
    
    all_scores[[inst]] <- instrument_scores_df
    all_reliability[[inst]] <- instrument_reliability_df
    all_descriptives[[inst]] <- instrument_descriptives_df
    all_audit[[inst]] <- instrument_audit_df
  }
  
  scores_df <- dplyr::bind_cols(all_scores)
  data_with_scores <- dplyr::bind_cols(data, scores_df)
  reliability_df <- dplyr::bind_rows(all_reliability)
  descriptives_df <- dplyr::bind_rows(all_descriptives)
  audit_df <- dplyr::bind_rows(all_audit)
  
  list(
    by_instrument = by_instrument,
    scores = scores_df,
    data_with_scores = data_with_scores,
    reliability = reliability_df,
    descriptives = descriptives_df,
    audit = audit_df
  )
}
