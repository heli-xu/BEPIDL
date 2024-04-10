#' Calculate reliability metrics from a table
#'
#' This function calculates reliability metrics including sensitivity, specificity, positive predictive value (PPV),
#' negative predictive value (NPV), Cohen's kappa, and area under the curve (AUC) from a given data table and a reference variable.
#'
#' @param var A character vector specifying the names of variables of interest in the data.
#' @param var_ref A character vector specifying the names of reference variables in the data. Usually GIS data.
#' @param data A data frame containing the variables of interest and reference variables, with joining GIS data with training/prediction/annotator data at street level.
#'
#' @return A data frame containing reliability metrics including sensitivity, specificity, PPV, NPV, kappa, and AUC.
#' 
#' @importFrome pROC auc roc
#' @importFrom caret sensitivity specificity posPredValue negPredValue 
#' @importFrom epiR epi.kappa
#' @importFrom purrr map2_dbl map2_dfr pluck
#' @importFrom magrittr %>%
#' 
#' @export
#' 
#' 

reliability_table <- function(var, var_ref, data){
  auc <- map2_dbl(var,var_ref, 
                  \(x, y) auc(data[[y]], as.numeric(data[[x]])))
  
  sensitivity <- map2_dbl(
    var, var_ref,
    \(x, y) sensitivity(data = data[[x]], reference = data[[y]], 
                        positive = "1")
  )
  
  specificity <- map2_dbl(
    var, var_ref,
    \(x, y) specificity(data = data[[x]], reference = data[[y]], 
                        negative = "0")
  )
  
  ppv <- map2_dbl(
    var, var_ref,
    \(x, y) posPredValue(data = data[[x]], reference = data[[y]], 
                         positive = "1")
  )
  
  npv <- map2_dbl(
    var, var_ref,
    \(x, y) negPredValue(data = data[[x]], reference = data[[y]], 
                         negative = "0")
  )
  
  kappa <- map2_dfr(
    var, var_ref,
    \(x, y) epi.kappa(table(data[[x]], data[[y]]), 
                      method = "cohen") %>% pluck("kappa")
  )
  
  df <- bind_cols("var" = var, 
                  "var_reference"= var_ref, 
                  "sensitivity" = sensitivity,
                  "specificity"=specificity, 
                  "ppv"=ppv, 
                  "npv"=npv,
                  kappa,
                  "auc" = auc) %>% 
    rename_with(~ paste0("kappa_", .x), est:upper)
  
  return(df)
}
