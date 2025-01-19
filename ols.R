source("volume/etl/util_loadPackages.R")

fct_ols = function(ols_form, data){
  ols_model = lm(as.formula(ols_form), data = data)
  ols_summary <- summary(ols_model)
  
  return(
    list(
      ols_model=ols_model,
      ols_summary=ols_summary
    )
  )
}
