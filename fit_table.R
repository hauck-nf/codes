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
  
  # Loop through the list of CFA results
  for (i in 1:length(result$results)) {
    model_name <- paste0("model_", i)
    model_fit <- result$results[[model_name]]$fit
    
    # Extract fit indices directly from the lavaan.vector object
    model_fit_indices <- data.frame(
      Model = model_name,
      npar = model_fit["npar"],
      chisq = model_fit["chisq"],
      df = model_fit["df"],
      pvalue = model_fit["pvalue"],
      RMSEA = model_fit["rmsea"],
      CFI = model_fit["cfi"],
      TLI = model_fit["tli"]
    )
    
    # Add the fit indices to the data frame
    fit_table <- bind_rows(fit_table, model_fit_indices)
  }
  
  return(fit_table)
}
