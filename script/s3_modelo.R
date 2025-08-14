##### PACOTES #####
library(cutpointr)
library(tidyverse)
library(tidymodels)
library(glmnet)
library(yardstick)

##### SPLIT TREINO/TESTE #####
set.seed(2814)
split <- initial_split(dt, strata = reocorrencia)
dt.train <- training(split)
dt.test  <- testing(split)

##### RECIPE #####
rec <- recipe(reocorrencia ~ ., data = dt.train) %>%
  step_impute_mode(all_nominal_predictors()) %>%   # preenche NAs nas categóricas
  step_dummy(all_nominal_predictors()) %>%         # transforma categóricas em dummies
  step_zv(all_predictors())                        # remove variáveis sem variação

##### MODEL SPECIFICATIONS #####
# Lasso (L1)
lasso_spec <- logistic_reg(penalty = tune(), mixture = 1) %>%
  set_engine("glmnet")

# Ridge (L2)
ridge_spec <- logistic_reg(penalty = tune(), mixture = 0) %>%
  set_engine("glmnet")

##### WORKFLOWS #####
wf_lasso <- workflow() %>%
  add_recipe(rec) %>%
  add_model(lasso_spec)

wf_ridge <- workflow() %>%
  add_recipe(rec) %>%
  add_model(ridge_spec)

##### VALIDAÇÃO CRUZADA #####
set.seed(2814)
folds <- vfold_cv(dt.train, v = 5, strata = reocorrencia)

# Grid de penalidades
# Penalidade maior para Lasso
grid <- tibble(penalty = 10^seq(-1, 1, length.out = 20))

##### TUNING LASSO #####
lasso_res <- tune_grid(
  wf_lasso,
  resamples = folds,
  grid = grid,
  metrics = yardstick::metric_set(yardstick::roc_auc, yardstick::accuracy)
)

best_lasso <- lasso_res %>% select_best(metric = "roc_auc")

# Finalizar workflow
final_lasso <- wf_lasso %>% finalize_workflow(best_lasso) %>% fit(dt.train)

##### TUNING RIDGE #####
ridge_res <- tune_grid(
  wf_ridge,
  resamples = folds,
  grid = grid,
  metrics = yardstick::metric_set(yardstick::roc_auc, yardstick::accuracy)
)

best_ridge <- ridge_res %>% select_best(metric = "roc_auc")
final_ridge <- wf_ridge %>% finalize_workflow(best_ridge) %>% fit(dt.train)

##### PREDIÇÕES #####
pred_lasso <- predict(final_lasso, dt.test, type="prob") %>%
  bind_cols(dt.test %>% select(reocorrencia))
pred_ridge <- predict(final_ridge, dt.test, type="prob") %>%
  bind_cols(dt.test %>% select(reocorrencia))

##### ==============================
##### PONTO DE CORTE OTIMIZADO
##### ==============================

# --- LASSO ---
# Gerar probabilidades
pred_lasso <- predict(final_lasso, dt.test, type = "prob") %>%
  bind_cols(dt.test %>% select(reocorrencia))

# Encontrar ponto de corte ótimo (soma sensibilidade + especificidade)
cut_lasso <- pred_lasso %>%
  cutpointr(
    x = .pred_Sim,
    class = reocorrencia,
    method = maximize_metric,
    metric = sum_sens_spec,
    pos_class = "Sim"
  ) %>%
  filter(specificity >= 0.5) %>%          # mínimo de especificidade
  slice_max(sensitivity, n = 1)          # maximiza sensibilidade

# Criar coluna de predição binária usando o corte otimizado
pred_lasso <- pred_lasso %>%
  mutate(pred_class = if_else(.pred_Sim >= cut_lasso$optimal_cutpoint, "Sim", "Não")) %>%
  mutate(pred_class = factor(pred_class, levels = levels(reocorrencia)))

# Matriz de confusão
conf_mat(pred_lasso, truth = reocorrencia, estimate = pred_class) %>%
  autoplot(type = "heatmap")

# --- RIDGE ---
# Gerar probabilidades
pred_ridge <- predict(final_ridge, dt.test, type = "prob") %>%
  bind_cols(dt.test %>% select(reocorrencia))

# Encontrar ponto de corte ótimo
cut_ridge <- pred_ridge %>%
  cutpointr(
    x = .pred_Sim,
    class = reocorrencia,
    method = maximize_metric,
    metric = sum_sens_spec,
    pos_class = "Sim"
  ) %>%
  filter(specificity >= 0.5) %>%
  slice_max(sensitivity, n = 1)

# Criar coluna de predição binária usando o corte otimizado
pred_ridge <- pred_ridge %>%
  mutate(pred_class = if_else(.pred_Sim >= cut_ridge$optimal_cutpoint, "Sim", "Não")) %>%
  mutate(pred_class = factor(pred_class, levels = levels(reocorrencia)))

# Matriz de confusão
conf_mat(pred_ridge, truth = reocorrencia, estimate = pred_class) %>%
  autoplot(type = "heatmap")

##### ==============================
##### COMPARAÇÃO E MELHOR MODELO
##### ==============================

# Calcular ROC AUC para cada modelo
roc_lasso <- roc_auc(pred_lasso, truth = reocorrencia, .pred_Sim)
roc_ridge <- roc_auc(pred_ridge, truth = reocorrencia, .pred_Sim)

roc_lasso
roc_ridge

# Escolher o melhor modelo
if (roc_lasso$.estimate >= roc_ridge$.estimate) {
  melhor_modelo <- "Lasso"
  pred_melhor <- pred_lasso
  corte_otimo <- cut_lasso$optimal_cutpoint
} else {
  melhor_modelo <- "Ridge"
  pred_melhor <- pred_ridge
  corte_otimo <- cut_ridge$optimal_cutpoint
}

cat("Melhor modelo:", melhor_modelo, "\n")
cat("Ponto de corte ótimo usado:", corte_otimo, "\n")

# Calcular medidas de classificação no ponto de corte ótimo
conf_mat(pred_melhor, truth = reocorrencia, estimate = pred_class) %>% summary()


##### Interpretando o modelo ######

# Extrair ajuste final do workflow
lasso_fit_final <- extract_fit_parsnip(final_lasso)$fit

# Extrair coeficientes no lambda usado pelo modelo final
coefs_lasso <- coef(lasso_fit_final, s = lasso_fit_final$lambda)

# Transformar em data frame
coefs_lasso_df <- data.frame(
  variavel = rownames(coefs_lasso),
  coef = as.numeric(coefs_lasso)
) %>%
  mutate(odds_ratio = exp(coef)) %>%
  filter(coef != 0) %>%
  arrange(desc(abs(coef)))

coefs_lasso_df

lasso_fit_final <- extract_fit_parsnip(final_lasso)$fit
coefs_lasso <- coef(lasso_fit_final, s = lasso_fit_final$lambda)
coefs_lasso_df <- data.frame(
  variavel = rownames(coefs_lasso),
  coef = as.numeric(coefs_lasso)
) %>%
  filter(coef != 0) %>%
  mutate(odds_ratio = exp(coef)) %>%
  arrange(desc(abs(coef)))
coefs_lasso_df


