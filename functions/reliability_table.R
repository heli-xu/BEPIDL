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
