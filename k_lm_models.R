library(dplyr)
library(tidyr)

exported_table3 <- read_csv("volume/causal_models/exported-table3.csv")
df <- exported_table3 %>% filter(cd_micro != 26019)
df$log_immigrants_pc = log(df$immigrants_pc)
df$log_labor_total_pc = log(df$labor_total_pc)
df$log_labor_total_pkm = log(df$labor_total_pkm)
df$log_mean_salary_total = log(df$mean_salary_total)
df$log_emig_total_pc = log(df$emig_total_pc)

# Define the dependent and independent variables
dependent_vars <- c(
  "emig_total_pc",
  "immigrants_pc",
  "nao_naturais_do_municipio",
  "nao_naturais_da_unidade_da_federacao"
)

independent_vars <- c(
  "labor_total_pc",
  "labor_total_pkm",
  "mean_salary_total",
  "dist_coast",
  "is_coastal_80km",
  "is_capital",
  "inhabitants_pkm",
  "nao_naturais_da_unidade_da_federacao",
  "immigrants_pc",
  "labor_total_pc",
  "labor_total_pkm",
  "mean_salary_total"
)

ll=0
ll = list()
# dep_var="nao_naturais_da_unidade_da_federacao"
# dep_var="emig_total_pc"
# dep_var="immigrants_pc"
# dep_var="log_immigrants_pc"
dep_var="log_emig_total_pc"
gc()
for(i in 1:length(independent_vars)) {
  comb = combn(independent_vars, i, simplify = FALSE)
  for(j in 1:length(comb)) {
    vars = comb[[j]]
    form = paste(dep_var, " ~ ", paste(vars, collapse = " + "))
    formula <- as.formula(form)
    
    ll[[form]]["model"] <- lm(formula, data = df)
    ll[[form]]["summary_model"] <- summary(lm(formula, data = df))
    ll[[form]]["adj_r2"] <- summary(lm(formula, data = df))$adj.r.squared
    
  }
}

adj_r2_values <- sapply(ll, function(x) x$adj_r2)
ranked_list <- ll[order(-adj_r2_values)]

ranked_list[[1]]
# Filter the dataset if needed

# Function to get all combinations of independent variables
get_all_models <- function(dep_var, indep_vars, data) {
  all_combinations <- lapply(1:length(indep_vars), function(n) combn(indep_vars, n, simplify = FALSE))
  all_combinations <- unlist(all_combinations, recursive = FALSE)
  
  # Store results
  results <- lapply(all_combinations, function(vars) {
    formula <- as.formula(paste(dep_var, "~", paste(vars, collapse = " + ")))
    model <- lm(formula, data = data)
    summary_model <- summary(model)
    adj_r2 <- summary_model$adj.r.squared
    aic <- AIC(model)
    
    list(
      formula = formula,
      adj_r2 = adj_r2,
      aic = aic,
      model = model
    )
  })
  
  # Convert to a data frame for easy ranking
  results_df <- do.call(rbind, lapply(results, function(res) {
    data.frame(
      formula = as.character(res$formula),
      adj_r2 = res$adj_r2,
      aic = res$aic,
      stringsAsFactors = FALSE
    )
  }))
  
  return(results_df)
}

# Iterate through each dependent variable
all_results <- lapply(dependent_vars, function(dep_var) {
  models <- get_all_models(dep_var, independent_vars, df)
  models <- models %>% arrange(desc(adj_r2)) # Rank by adj R^2
  models
})

# Combine results into a single data frame for all dependent variables
final_results <- bind_rows(all_results, .id = "dependent_var")

# View top-ranked models
print(final_results)




final_results$dependent_var[2]
final_results$formula[1]
final_results$adj_r2
