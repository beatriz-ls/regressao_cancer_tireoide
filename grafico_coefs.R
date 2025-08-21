# Extrair coeficientes e estatísticas
coeficientes <- modelo_elastic$coefficients
valores_p <- modelo_elastic$prob
coef_05 <- confint(modelo_elastic)[,1]
coef_95 <- confint(modelo_elastic)[,2]

summary(modelo_elastic)

# Criar um data frame manualmente
resultados <- data.frame(
  term = names(coeficientes),
  estimate = coeficientes,
  coef_05,
  coef_95,
  p.value = valores_p
)

print(resultados)

grafico <- ggplot(resultados, aes(x = term, y = estimate, fill = p.value < 0.05)) +
  geom_bar(stat = "identity", color = "black") +
  geom_errorbar(aes(ymin = coef_05, ymax = coef_95), width = 0.2) +
  scale_fill_viridis_d(option = "viridis", name = "Significativo (p < 0.05)") +
  labs(
    title = "Coeficientes do Modelo Logistico",
    x = "Termos do Modelo",
    y = "Estimativa"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

grafico
