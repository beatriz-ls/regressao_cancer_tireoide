library(ggplot2)
library(broom)

set.seed(123)

# ANOVA comparativa
resultado_anova <- anova(modelo_lasso, modelo_elastic)
print(resultado_anova)

resultados <- broom::tidy(modelo_elastic)

grafico <- ggplot(resultados, aes(x = term, y = estimate, fill = p.value < 0.05)) +
  geom_bar(stat = "identity", color = "black") +
  geom_errorbar(aes(ymin = estimate - std.error, ymax = estimate + std.error), width = 0.2) +
  labs(
    title = "Coeficientes do Modelo Logistico",
    x = "Termos do Modelo",
    y = "Estimativa",
    fill = "Significativo (p < 0.05)"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

