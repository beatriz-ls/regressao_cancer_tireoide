
library(tidymodels)

dt_clean <- na.omit(dt)
# dt_clean <- dt |> dplyr::select(-c(estagio, exame_fisico))

## Dividindo amostra

set.seed(2814)
split <- initial_split(dt_clean, strata = reocorrencia)
dt.train <- training(split)
dt.test  <- testing(split)

## Modelo completo (sem interação)

modelo <- glm(reocorrencia ~ .,
               data = dt.train, family = binomial)
summary(modelo)

car::vif(modelo)

## Seleção de variaveis usando stepAIC

step_modelo <- MASS::stepAIC(modelo,
                    direction = "both",
                    trace = 0)
summary(step_modelo)

## Firth logistic regression (dealing with complete separation or quasi-complete separation)

modelo_firth <- logistf::logistf(reocorrencia ~ resposta_tratamento,
                                 data = dt.train)
summary(modelo_firth)

## Testando o modelo

probabilidades <- predict(modelo_firth, newdata = dt.test, type = "response")
predicoes <- ifelse(probabilidades > 0.5, "Sim", "Não")

## Matriz de confusão

conf_matrix <- caret::confusionMatrix(as.factor(predicoes),
                                      dt.test$reocorrencia,
                                      positive = "Sim")
conf_matrix

## Curva ROC

library(pROC)

roc_curve <- roc(dt.test$reocorrencia, probabilidades)

plot(roc_curve, main = "Curva ROC", col = "#1c61b6", lwd = 2)
legend("bottomright", legend = paste("AUC =", round(auc(roc_curve), 3)),
       bty = "n", cex = 1.2)

## Ponto de corte (focado na sensibilidade)

coords_obj <- coords(roc_curve,
                     x = "all",
                     ret = c("threshold", "sensitivity", "specificity")) |>
  as_tibble() |>
  filter(sensitivity >= 0.95) |>  # Exigência mínima de sensibilidade (95%)
  arrange(desc(specificity)) |>   # Dentro disso, escolhe a melhor especificidade
  slice(1)                        # Pega o melhor caso

best_threshold <- coords_obj$threshold

predicoes_otimizadas <- ifelse(probabilidades > best_threshold, "Sim", "Não")

# Nova matriz de confusão
conf_matrix_otimizada <- caret::confusionMatrix(
  as.factor(predicoes_otimizadas),
  dt.test$reocorrencia,
  positive = "Sim"
)

conf_matrix_otimizada

## Gráfico de barras para os coeficientes

## Forest plot para as odds ratio


