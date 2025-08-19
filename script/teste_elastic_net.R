library(dplyr)
library(glmnet)
library(logistf)
library(caret)


# --- 1. Separar treino e teste ---
set.seed(123)
train_index <- createDataPartition(dt1$reocorrencia, p = 0.7, list = FALSE)
train_data <- dt1[train_index, ]
test_data  <- dt1[-train_index, ]

# --- 2. Preparar matriz para glmnet ---
x_train <- model.matrix(reocorrencia ~ ., train_data)[,-1]  # remove intercepto
y_train <- ifelse(train_data$reocorrencia == "Sim", 1, 0)

x_test <- model.matrix(reocorrencia ~ ., test_data)[,-1]
y_test <- ifelse(test_data$reocorrencia == "Sim", 1, 0)

# --- 3. Rodar Elastic Net com validação cruzada ---
set.seed(123)
cv_en <- cv.glmnet(
  x_train, y_train,
  alpha = 0.5,         # Elastic Net (0.5 = mistura Lasso + Ridge)
  family = "binomial",
  nfolds = 5
)

# Melhor lambda
lambda_min <- cv_en$lambda.min

# --- 4. Extrair variáveis selecionadas ---
coef_en <- coef(cv_en, s = "lambda.min")
selected_vars <- rownames(coef_en)[coef_en[,1] != 0]
selected_vars <- selected_vars[selected_vars != "(Intercept)"]
print("Variáveis selecionadas pelo Elastic Net:")
print(selected_vars)


modelo_firth_en <- logistf(
  formula = reocorrencia ~ T + N + genero + quadro_tireoide + adenopatia +
    patologia + risco + resposta_tratamento + estagio,
  data = train_data,
  control = logistf.control(maxit = 200),
  plcontrol = logistpl.control(maxit = 500))


summary(modelo_firth_en)

# --- 6. Avaliação no treino e teste ---
# Probabilidades
train_pred_prob <- predict(modelo_firth_en, type = "response")
test_pred_prob  <- predict(modelo_firth_en, newdata = test_data, type = "response")

# Predições binárias
train_pred <- ifelse(train_pred_prob > 0.5, "Sim", "Não")
test_pred  <- ifelse(test_pred_prob  > 0.5, "Sim", "Não")

# Matriz de confusão
conf_train <- confusionMatrix(factor(train_pred, levels = c("Não","Sim")), train_data$reocorrencia)
conf_test  <- confusionMatrix(factor(test_pred, levels = c("Não","Sim")), test_data$reocorrencia)

print(conf_train)
print(conf_test)

# Curva ROC
library(pROC)
roc_train <- roc(train_data$reocorrencia, train_pred_prob, levels = c("Não","Sim"))
roc_test  <- roc(test_data$reocorrencia, test_pred_prob, levels = c("Não","Sim"))

print(paste("AUC Treino:", round(auc(roc_train),3)))
print(paste("AUC Teste:", round(auc(roc_test),3)))

