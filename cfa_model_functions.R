#FUNCTION TO PRODUCE LAVAAN MODELS FROM AN ITEM DICTIONARY

#FIRST LET US SIMULATE SOME DATA
library(lavaan)
library(dplyr)
#500 cases, 10 items with 5 response options
#1 general factor, two specific factors
#items 4, 5, 9, and 10 are semantically reversed
n <- 500
model <- '
    f_general =~ .5*Item1 + .5*Item2 + .5*Item3 + (-.5)*Item4 + (-.5)*Item5 + .5*Item6 + .5*Item7 + .5*Item8 + (-.5)*Item9 + (-.5)*Item10
    f_specific1 =~ .5*Item1 + .5*Item2 + .5*Item3 + (-.5)*Item4 + (-.5)*Item5
    f_specific2 =~ .5*Item6 + .5*Item7 + .5*Item8 + (-.5)*Item9 + (-.5)*Item10
    f_general~~1*f_general
    f_specific1~~1*f_specific1
    f_specific2~~1*f_specific2
    f_general~~0*f_specific1
    f_general~~0*f_specific2
    f_specific1~~0*f_specific2
'
set.seed(1234)
simulated_data<-simulateData(model = model, model.type = "sem", standardized=TRUE,sample.nobs = 500)
simulated_data<-apply(simulated_data,2,function (x) cut(x,breaks=c(5), labels=c("1","2","3","4","5")))
simulated_data<-as.data.frame(apply(simulated_data,2,as.numeric))
head(simulated_data)

#ITEM DIC
item_dic <- data.frame(
  itemcode = c("Item1", "Item2", "Item3", "Item4", "Item5", "Item6", "Item7", "Item8", "Item9","Item10"),
  order=c(1,2,3,4,5,6,7,8,9,10),
  key=c(1,1,1,-1,-1,1,1,1,-1,-1),
  scale=c("Factor1", "Factor1", "Factor1", "Factor1", "Factor1", "Factor2", "Factor2", "Factor2", "Factor2","Factor2"),
  model_1 = c("Unidimensional", "Unidimensional", "Unidimensional", "Unidimensional", "Unidimensional", "Unidimensional", "Unidimensional", "Unidimensional", "Unidimensional","Unidimensional"),
  model_2 = c("Factor1", "Factor1", "Factor1", "Factor1", "Factor1", "Factor2", "Factor2", "Factor2", "Factor2","Factor2"),
  model_3 = c("Factor1", "Factor1", "Factor1", "Factor2", "Factor2", "Factor2", "Factor2", "Factor3", "Factor3","Factor3")
    )


#FUNCTION FOR CREATING CFA MODELS FROM THE ITEM DICTIONARY SPECIFICATIONS
# Função ajustada para criar modelos CFA a partir do item_dic
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

# Exemplo
cfa_models <- create_cfa_models(item_dic = item_dic, models = c("model_1", "model_2", "model_3"))
cfa_models


# FUNCTION FOR RUNNING THE CFA MODELS GENERATED FROM THE ITEM DICTIONARY
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


#EXAMPLE (admits all the lavaan cfa function arguments)
result=run_cfa_models(models=cfa_models,data=simulated_data,ordered=TRUE,estimator="WLSMV")
result[["results"]][["model_1"]]
result[["results"]][["model_2"]]
result[["results"]][["model_3"]]



# FUNCTION TO EXTRACT FIT INDICES FROM CFA RESULTS
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

# EXAMPLE USAGE with your "result" list of CFA outputs
fit_table <- fit_table(result)

# View the fit table
print(fit_table)
remove(cfa_model,result,fit_table)