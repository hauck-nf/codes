#PERFORM BASIC ANALYSIS ON PSYCHOMETRIC SCALES
#The function analyze_psychometrics uses the psych function scoreItems to produce: 
#  1) a list called "psychometrics" containing descriptive analysis and internal consistency estimates (alpha and G6); and
#  2) a data frame containing the original data plus the calculated scores for the target scales.

#The function required two r objects:
#  1) a data frame called data containing persons x variables
#  2) a dictionary containing the following metadata columns: 
#    a) "itemcode": with the names of the items to be analyzed (matching the names in the data frame);
#    b) "key": with each item coded as 1 when it is positively correlated with its corresponding factor (positively-keyed), and -1 when it is negatively correlated with the factor (positively-keyed);
#    c) "scale": with the name of the scale where the item should be allocated in the calculation of scores and internal consistency estimates
#    d) "order": with the position of each item relative to the complete instrument item set; the order should restart from 1 to n for each instrument
#ANALYSE_PSYCHOMETRICS FUNCTION
analyze_psychometrics <- function(data, dictionary) {
  library(dplyr)
  library(psych)
  
  # Ajustar a codificação dos itens de acordo com a chave positiva/negativa
  dictionary <- dictionary %>%
    mutate(order2 = ifelse(key == 1, itemcode, paste0("-", itemcode)))
  
  # Cria a lista de chaves para cada escala usando nest_by
  keys.list <- dictionary %>%
    group_by(scale) %>%
    nest_by() %>%
    mutate(keys = list(data$order2)) %>%
    pull(keys)
  
  # Define as chaves para scoreItems
  keys <- make.keys(nvars = ncol(data), keys.list = keys.list, item.labels = dictionary$itemcode)
  colnames(keys) <- unique(dictionary$scale)
  
  # Executa a análise psicométrica com scoreItems
  psychometrics <- scoreItems(
    keys = keys,
    items = data[, dictionary$itemcode],
    missing = TRUE,
    impute = "none"
  )
  
  # Combina os escores calculados com os dados originais
  result <- list(
    psychometrics = psychometrics,
    scores = cbind(data, as.data.frame(psychometrics$scores))
  )
  
  return(result)
}

