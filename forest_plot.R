resultados <- summary(modelo_elastic)

# Preparar os dados para o forest plot
forest_data <- data.frame(
  variavel = resultados$terms,
  coef = resultados$coefficients,
  coef_05 = confint(modelo_elastic)[,1],
  coef_95 = confint(modelo_elastic)[,2],
  p_value = resultados$prob
)

# Calcular intervalos de confiança (95%)
forest_data$lower <- forest_data$coef_05
forest_data$upper <- forest_data$coef_95

# Converter odds ratios
forest_data$or <- exp(forest_data$coef)
forest_data$or_lower <- exp(forest_data$lower)
forest_data$or_upper <- exp(forest_data$upper)

# Remover o intercepto se desejar
forest_data <- forest_data[forest_data$variavel != "(Intercept)", ]

# Criar forest plot básico com ggplot2
ggplot(forest_data, aes(x = or, y = reorder(variavel, or))) +
  labs(
    title = "Forest Plot - Modelo de Regressão Logística",
    subtitle = "Odds Ratios com Intervalos de Confiança de 95%",
    x = "Odds Ratio (log10)",
    y = "Variáveis Predictoras",
    caption = "Linha vermelha: OR = 1 (referência)"
  ) +
  geom_point(size = 3, shape = 15) +
  # geom_errorbarh(aes(xmin = or_lower, xmax = or_upper), height = 0.2) +
  geom_vline(xintercept = 1, linetype = "dashed", color = "red") +
  scale_x_continuous(
    breaks = c(0.01, 0.1, 0.5, 1, 2, 5, 10, 20, 50, 100),
    labels = c("0.01", "0.1", "0.5", "1", "2", "5", "10", "20", "50", "100")
  )
  scale_x_log10() +
  theme_minimal() +
  theme(axis.text.y = element_text(size = 10))
