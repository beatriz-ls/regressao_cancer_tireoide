library(dplyr)
library(glmnet)
library(logistf)
library(caret)

set.seed(123)

# --- 1. Separar treino e teste (70%-30%) ---
train_index <- createDataPartition(dt1$reocorrencia, p = 0.7, list = FALSE)
train_data <- dt1[train_index, ]
test_data  <- dt1[-train_index, ]

# --- 2. Preparar matrizes para glmnet ---
# glmnet precisa de matriz numérica; usamos model.matrix para fatores
X_train <- model.matrix(reocorrencia ~ ., data = train_data)[, -1]
y_train <- ifelse(train_data$reocorrencia == "Sim", 1, 0)

X_test <- model.matrix(reocorrencia ~ ., data = test_data)[, -1]
y_test <- ifelse(test_data$reocorrencia == "Sim", 1, 0)

# --- 3. Ajustar LASSO logistic ---
cv_lasso <- cv.glmnet(
  X_train, y_train, family = "binomial",
  alpha = 1, nfolds = 5
)

# Coeficientes selecionados pelo lambda ótimo
coef_lasso <- coef(cv_lasso, s = "lambda.min")
vars_selected <- rownames(coef_lasso)[coef_lasso[,1] != 0]
vars_selected <- vars_selected[vars_selected != "(Intercept)"]
vars_selected

# --- 4. Ajustar modelo Firth apenas com variáveis selecionadas ---
formula_firth <- as.formula(
  paste("reocorrencia ~", paste(vars_selected, collapse = " + "))
)

modelo_firth_sel <- logistf(
  reocorrencia ~ N + genero + risco + resposta_tratamento + estagio,
  data = train_data,
  control = logistf.control(maxit = 200),
  plcontrol = logistpl.control(maxit = 500)
)

summary(modelo_firth_sel)

# --- 5. Avaliar desempenho em treino ---
train_pred_prob <- predict(modelo_firth_sel, type = "response")
train_pred <- ifelse(train_pred_prob > 0.5, "Sim", "Não")
confusionMatrix(factor(train_pred, levels=c("Não","Sim")), train_data$reocorrencia)

# --- 6. Avaliar desempenho em teste ---
# Precisamos usar a mesma codificação que model.matrix gerou
test_pred_prob <- predict(modelo_firth_sel, newdata = test_data, type = "response")
test_pred <- ifelse(test_pred_prob > 0.5, "Sim", "Não")
confusionMatrix(factor(test_pred, levels=c("Não","Sim")), test_data$reocorrencia)


