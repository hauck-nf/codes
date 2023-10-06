create_cfa_models <- function(item_dic, models) {
  cfa_models <- list()
  for (model_name in models) {
    factors <- unique(item_dic[, model_name])
    model <- character()
    for (factor in factors) {
      # Filtrar itens associados a cada fator
      items <- item_dic %>%
        filter(.data[[model_name]] == factor) %>%
        pull(itemcode)
      model <- append(model, paste(factor, " =~", paste(items, collapse = " + ")))
    }
    cfa_models[[model_name]] <- paste(model, collapse = "\n")
  }
  return(cfa_models)
}
