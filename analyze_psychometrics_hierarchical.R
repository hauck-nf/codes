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
# PERFORM BASIC ANALYSIS ON PSYCHOMETRIC SCALES
# The function analyze_psychometrics_hierarchical uses psych::scoreItems to produce:
# 1) descriptive analysis and internal consistency estimates (alpha and G6)
# 2) a data frame containing the original data plus the calculated scores
#
# Required inputs:
# 1) a data frame called data containing persons x variables
# 2) a dictionary containing the following metadata columns:
#    a) "itemcode": names of the items to be analyzed (matching names in data)
#    b) "key": 1 for positively keyed items, -1 for negatively keyed items
#    c) "higher_order": name of the higher-order factor
#    d) "scale": name of the first-order scale
#    e) "instrument": name of the instrument

analyze_psychometrics_hierarchical <- function(
  data,
  dictionary,
  min_items_per_scale = 2,
  missing = TRUE,
  impute = "none"
) {
  
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
    stop(
      "The dictionary is missing required columns: ",
      paste(missing_cols, collapse = ", ")
    )
  }
  
  if (!all(dictionary$key %in% c(1, -1))) {
    stop("Column 'key' must contain only 1 and -1.")
  }
  
  if (anyDuplicated(dictionary$itemcode) > 0) {
    duplicated_items <- unique(dictionary$itemcode[duplicated(dictionary$itemcode)])
    warning(
      "Duplicated itemcodes detected in dictionary: ",
      paste(duplicated_items, collapse = ", "),
      ". This is acceptable only if duplication is intentional for higher-order scoring."
    )
  }
  
  missing_items <- setdiff(unique(dictionary$itemcode), names(data))
  if (length(missing_items) > 0) {
    stop(
      "These itemcodes are not present in data: ",
      paste(missing_items, collapse = ", ")
    )
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
  # 3) Helper: reverse keyed items temporarily for alpha()
  # -----------------------------
  build_alpha_input <- function(items_df, key_vector) {
    items_df <- as.data.frame(items_df)
    
    non_numeric <- names(items_df)[!vapply(items_df, is.numeric, logical(1))]
    if (length(non_numeric) > 0) {
      stop(
        "The following items are not numeric and cannot be analyzed with psych::alpha(): ",
        paste(non_numeric, collapse = ", ")
      )
    }
    
    psych::reverse.code(
      keys = key_vector,
      items = items_df
    ) %>%
      as.data.frame()
  }
  
  # -----------------------------
  # 4) Helper: run one construct
  # -----------------------------
  run_one_construct <- function(
    data,
    dict_subset,
    construct_name,
    instrument_name,
    level_name,
    missing = TRUE,
    impute = "none"
  ) {
    
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
    
    # Detailed alpha() object
    items_for_alpha <- build_alpha_input(
      items_df = items_df,
      key_vector = dict_subset$key
    )
    
    alpha_result <- psych::alpha(
      x = items_for_alpha,
      check.keys = FALSE
    )
    
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
      alpha_raw = if (!is.null(alpha_result$total$raw_alpha)) alpha_result$total$raw_alpha else NA_real_,
      alpha_std = if (!is.null(alpha_result$total$std.alpha)) alpha_result$total$std.alpha else NA_real_,
      average_r = if (!is.null(alpha_result$total$average_r)) alpha_result$total$average_r else NA_real_,
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
      alpha_details = alpha_result,
      scores = score_df,
      reliability = reliability_df,
      descriptives = descriptives_df,
      audit = audit_df
    )
  }
  
  # -----------------------------
  # 5) Main loop by instrument
  # -----------------------------
  instruments <- unique(dictionary$instrument)
  
  by_instrument <- list()
  all_scores <- list()
  all_reliability <- list()
  all_descriptives <- list()
  all_audit <- list()
  all_skipped <- list()
  
  Classical_Test_Theory_Analysis <- list(
    scale = list(),
    higher_order = list()
  )
  
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
    
    # -----------------------------
    # 5a) Check first-order scales
    # -----------------------------
    scale_counts <- dict_inst %>%
      dplyr::filter(!is.na(scale), scale != "") %>%
      dplyr::count(scale, name = "n_items")
    
    valid_scales <- scale_counts %>%
      dplyr::filter(n_items >= min_items_per_scale) %>%
      dplyr::pull(scale)
    
    skipped_scales <- scale_counts %>%
      dplyr::filter(n_items < min_items_per_scale) %>%
      dplyr::mutate(
        instrument = inst,
        level = "first_order",
        reason = paste0("fewer than ", min_items_per_scale, " items")
      ) %>%
      dplyr::rename(name = scale)
    
    if (nrow(skipped_scales) > 0) {
      warning(
        paste0(
          "In instrument '", inst, "', the following first-order scales have fewer than ",
          min_items_per_scale,
          " items and were not scored: ",
          paste(skipped_scales$name, collapse = ", ")
        ),
        call. = FALSE
      )
    }
    
    # -----------------------------
    # 5b) Run first-order scales
    # -----------------------------
    for (sc in valid_scales) {
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
      
      Classical_Test_Theory_Analysis$scale[[paste(inst, sc, sep = "__")]] <- res_sc$alpha_details
    }
    
    # -----------------------------
    # 5c) Check higher-order scales
    # -----------------------------
    higher_counts <- dict_inst %>%
      dplyr::filter(!is.na(higher_order), higher_order != "") %>%
      dplyr::count(higher_order, name = "n_items")
    
    valid_higher <- higher_counts %>%
      dplyr::filter(n_items >= min_items_per_scale) %>%
      dplyr::pull(higher_order)
    
    skipped_higher <- higher_counts %>%
      dplyr::filter(n_items < min_items_per_scale) %>%
      dplyr::mutate(
        instrument = inst,
        level = "higher_order",
        reason = paste0("fewer than ", min_items_per_scale, " items")
      ) %>%
      dplyr::rename(name = higher_order)
    
    if (nrow(skipped_higher) > 0) {
      warning(
        paste0(
          "In instrument '", inst, "', the following higher-order scales have fewer than ",
          min_items_per_scale,
          " items and were not scored: ",
          paste(skipped_higher$name, collapse = ", ")
        ),
        call. = FALSE
      )
    }
    
    # -----------------------------
    # 5d) Run higher-order scales
    # -----------------------------
    for (ho in valid_higher) {
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
      
      Classical_Test_Theory_Analysis$higher_order[[paste(inst, ho, sep = "__")]] <- res_ho$alpha_details
    }
    
    # -----------------------------
    # 5e) Combine outputs
    # -----------------------------
    instrument_scores_df <- if (length(instrument_scores) > 0) {
      dplyr::bind_cols(instrument_scores)
    } else {
      data.frame()
    }
    
    instrument_reliability_df <- if (length(instrument_reliability) > 0) {
      dplyr::bind_rows(instrument_reliability)
    } else {
      data.frame()
    }
    
    instrument_descriptives_df <- if (length(instrument_descriptives) > 0) {
      dplyr::bind_rows(instrument_descriptives)
    } else {
      data.frame()
    }
    
    instrument_audit_df <- if (length(instrument_audit) > 0) {
      dplyr::bind_rows(instrument_audit)
    } else {
      data.frame()
    }
    
    instrument_skipped_df <- dplyr::bind_rows(skipped_scales, skipped_higher)
    
    by_instrument[[inst]] <- list(
      scoreitems = instrument_results,
      scores = instrument_scores_df,
      reliability = instrument_reliability_df,
      descriptives = instrument_descriptives_df,
      audit = instrument_audit_df,
      skipped_constructs = instrument_skipped_df
    )
    
    all_scores[[inst]] <- instrument_scores_df
    all_reliability[[inst]] <- instrument_reliability_df
    all_descriptives[[inst]] <- instrument_descriptives_df
    all_audit[[inst]] <- instrument_audit_df
    all_skipped[[inst]] <- instrument_skipped_df
  }
  
  scores_df <- if (length(all_scores) > 0) dplyr::bind_cols(all_scores) else data.frame()
  data_with_scores <- dplyr::bind_cols(data, scores_df)
  reliability_df <- dplyr::bind_rows(all_reliability)
  descriptives_df <- dplyr::bind_rows(all_descriptives)
  audit_df <- dplyr::bind_rows(all_audit)
  skipped_constructs_df <- dplyr::bind_rows(all_skipped)
  
  list(
    by_instrument = by_instrument,
    scores = scores_df,
    data_with_scores = data_with_scores,
    reliability = reliability_df,
    descriptives = descriptives_df,
    audit = audit_df,
    skipped_constructs = skipped_constructs_df,
    Classical_Test_Theory_Analysis = Classical_Test_Theory_Analysis
  )
}
