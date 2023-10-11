fit_table <- function(result) {
  # Initialize an empty data frame for fit indices
  fit_table <- data.frame(Model = character(0),
                          npar = numeric(0),
                          chisq = numeric(0),
                          df = numeric(0),
                          pvalue = numeric(0),
                          RMSEA = numeric(0),
                          CFI = numeric(0),
                          TLI = numeric(0))
  
  # Check if there's only one model in the list
  if (length(result$results) == 1) {
    model_name <- names(result$results)[1]
    model_fit <- result$results[[model_name]]
    
    # Extract fit indices directly from the "fit" object
    model_fit_indices <- data.frame(
      Model = model_name,
      npar = model_fit$fit['npar'],
      chisq = model_fit$fit['chisq'],
      df = model_fit$fit['df'],
      pvalue = model_fit$fit['pvalue'],
      RMSEA = model_fit$fit['rmsea'],
      CFI = model_fit$fit['cfi'],
      TLI = model_fit$fit['tli']
    )
    
    # Add the fit indices to the data frame
    fit_table <- bind_rows(fit_table, model_fit_indices)
  } else {
    # Loop through the list of CFA results
    for (i in 1:length(result$results)) {
      model_name <- paste0("model_", i)
      model_fit <- result$results[[model_name]]
      
      # Extract fit indices directly from the "fit" object
      model_fit_indices <- data.frame(
        Model = model_name,
        npar = model_fit$fit['npar'],
        chisq = model_fit$fit['chisq'],
        df = model_fit$fit['df'],
        pvalue = model_fit$fit['pvalue'],
        RMSEA = model_fit$fit['rmsea'],
        CFI = model_fit$fit['cfi'],
        TLI = model_fit$fit['tli']
      )
      
      # Add the fit indices to the data frame
      fit_table <- bind_rows(fit_table, model_fit_indices)
    }
  }
  
  return(fit_table)
}
