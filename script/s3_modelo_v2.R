##### PACOTES #####
library(tidyverse)
library(tidymodels)
library(cutpointr)
library(glmnet)
library(vip)

##### SPLIT TREINO/TESTE #####
set.seed(2814)
split <- initial_split(dt, strata = reocorrencia)
dt.train <- training(split)
dt.test  <- testing(split)

##### RECIPE ENXUTA #####
rec <- recipe(reocorrencia ~ ., data = dt.train) %>%
  step_impute_mode(all_nominal_predictors()) %>%
  step_dummy(all_nominal_predictors()) %>%
  step_zv(all_predictors()) %>%
  step_corr(all_numeric_predictors(), threshold = 0.9)  # remove multicolinearidade alta

##### MODEL SPECIFICATIONS - ELASTIC NET #####
elastic_spec <- logistic_reg(
  penalty = tune(),
  mixture = tune()
) %>% set_engine("glmnet")

##### WORKFLOW #####
wf_elastic <- workflow() %>%
  add_recipe(rec) %>%
  add_model(elastic_spec)

##### VALIDAÇÃO CRUZADA #####
folds <- vfold_cv(dt.train, v = 5, strata = reocorrencia)

##### GRID DE TUNING #####
grid <- expand.grid(
  penalty = 10^seq(-4, 0, length.out = 20),
  mixture = seq(0, 1, by = 0.1)
)

##### TUNING ELASTIC NET #####
elastic_res <- tune_grid(
  wf_elastic,
  resamples = folds,
  grid = grid,
  metrics = metric_set(roc_auc)
)

best_elastic <- elastic_res %>% select_best(metric = "roc_auc")

##### AJUSTE FINAL #####
final_elastic <- wf_elastic %>%
  finalize_workflow(best_elastic) %>%
  fit(dt.train)

##### PREDIÇÕES E PONTO DE CORTE #####
pred_elastic <- predict(final_elastic, dt.test, type = "prob") %>%
  bind_cols(dt.test %>% select(reocorrencia))

cut_elastic <- pred_elastic %>%
  cutpointr(
    x = .pred_Sim,
    class = reocorrencia,
    method = maximize_metric,
    metric = sum_sens_spec,
    pos_class = "Sim"
  ) %>%
  filter(specificity >= 0.5) %>%
  slice_max(sensitivity, n = 1)

pred_elastic <- pred_elastic %>%
  mutate(pred_class = if_else(.pred_Sim >= cut_elastic$optimal_cutpoint, "Sim", "Não")) %>%
  mutate(pred_class = factor(pred_class, levels = levels(reocorrencia)))

conf_mat(pred_elastic, truth = reocorrencia, estimate = pred_class) %>%
  autoplot(type = "heatmap")

##### COEFICIENTES, ODDS E IMPORTÂNCIA #####
elastic_fit_final <- extract_fit_parsnip(final_elastic)$fit
coefs_elastic <- coef(elastic_fit_final, s = best_elastic$penalty)
coefs_elastic_df <- data.frame(
  variavel = rownames(coefs_elastic),
  coef = as.numeric(coefs_elastic)
) %>%
  mutate(odds_ratio = exp(coef)) %>%
  filter(coef != 0) %>%
  arrange(desc(abs(coef)))

coefs_elastic_df

# Visualizar importância das variáveis
vip(elastic_fit_final, num_features = 20) + ggtitle("Top 20 Variáveis Elastic Net")
