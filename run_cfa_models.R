run_cfa_models <- function(models, data, ...) {
  results <- list()
  fit_summaries <- list()
  
  for (model_name in names(models)) {
    cfa_model <- models[[model_name]]
    
    # Rodar a análise CFA com todos os argumentos adicionais
    fit <- cfa(cfa_model, data = data, ...)
    
    # Calcular as estatísticas de ajuste
    fit_summary <- summary(fit, fit.measures = TRUE,,standardized = TRUE)  # Defina fit.measures = TRUE
    
    results[[model_name]] <- fit_summary
    fit_summaries[[model_name]] <- fit_summary$fit.measures
  }
  
  # Criar uma tabela comparativa de índices de ajuste
  return(list(results = results))
}
