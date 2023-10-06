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

#ANALYSE_PSYCHOMETRICS FUNCTION
analyze_psychometrics <- function(data, dictionary) {
  library(dplyr)
  library(psych)
  dictionary <- dictionary %>%
    mutate(order2 = ifelse(key == 1, order, order * (-1)))
  
  keys.list <- dictionary %>%
    select(scale, order2) %>%
    group_by(scale) %>%
    summarize(order2 = list(unique(order2))) %>%
    pull(order2)
  
  keys <- make.keys(nvars = nrow(dictionary), keys.list = keys.list, item.labels = dictionary$itemcode)
  colnames(keys) <- unique(dictionary$scale)
  
  psychometrics <- scoreItems(
    keys = keys,
    items = data[, rownames(keys)],
    missing = TRUE,
    impute = "none",
    totals = FALSE,
  )
  
  result <- list(
    psychometrics = psychometrics,
    scores = cbind(data,
                   as.data.frame(psychometrics$scores)
    )
  )
  
  return(result)
}

# Exemplo de uso da função
scale_psychometrics <- analyze_psychometrics(data = simulated_data, dictionary = item_dic)
scale_psychometrics[["psychometrics"]][["alpha"]]
remove(scale_psychometrics)
