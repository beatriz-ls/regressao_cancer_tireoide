# Libs

library(summarytools)
library(gtsummary)
library(ggplot2)

# Descriptive analysis ########################################################

# Coorte description

coorte_descr <- dfSummary(dt %>% select(faixa_etaria, genero, fumante,
                                        historico_fumante, historico_radioterapia,
                                        quadro_tireoide))

coorte_descr1 <- dt %>% tbl_summary(include = c(faixa_etaria, genero, fumante,
                                                historico_fumante,
                                                historico_radioterapia,
                                                quadro_tireoide))

# Criar os gráficos
p1 <- ggplot(dt, aes(x = faixa_etaria)) +
  geom_bar(fill = "skyblue") +
  labs(title = "Distribuição de Faixa Etária",
       x = "Faixa Etária",
       y = "Frequência") +
  theme_minimal()

p2 <- ggplot(dt, aes(x = genero)) +
  geom_bar(fill = "pink") +
  labs(title = "Distribuição de Gênero",
       x = "Gênero",
       y = "Frequência") +
  theme_minimal()

p3 <- ggplot(dt, aes(x = fumante)) +
  geom_bar(fill = "lightgreen") +
  labs(title = "Distribuição de Fumante",
       x = "Fumante",
       y = "Frequência") +
  theme_minimal()

p4 <- ggplot(dt, aes(x = historico_fumante)) +
  geom_bar(fill = "purple") +
  labs(title = "Distribuição de Histórico de Fumo",
       x = "Histórico de Fumante",
       y = "Frequência") +
  theme_minimal()

p5 <- ggplot(dt, aes(x = historico_radioterapia)) +
  geom_bar(fill = "yellow") +
  labs(title = "Distribuição de Histórico de Radioterapia",
       x = "Histórico de Radioterapia",
       y = "Frequência") +
  theme_minimal()

dt1 <- dt %>%
  mutate(quadro_tireoide_num = case_when(`Thyroid Function` == "Euthyroid" ~ 1,
                   `Thyroid Function` == "Clinical Hyperthyroidism" ~ 2,
                   `Thyroid Function` == "Clinical Hypothyroidism" ~ 3,
                   `Thyroid Function` == "Subclinical Hyperthyroidism" ~ 4,
                   `Thyroid Function` == "Subclinical Hypothyroidism" ~ 5))

legend_text <- c(
  "1 = Eutireoide",
  "2 = Hipertireoidismo Clínico",
  "3 = Hipotireoidismo Clínico",
  "4 = Hipertireoidismo Subclínico",
  "5 = Hipotireoidismo Subclínico")

p6 <- ggplot(dt1, aes(x = quadro_tireoide_num)) +
  geom_bar(fill = "coral") +
  labs(title = "Distribuição de Função da Tireoide",
       x = "Função da Tireoide",
       y = "Frequência") +
  theme_minimal()
p6 <- p6 + annotate("text", x = Inf, y = Inf, label = paste(legend_text, collapse = "\n"),
                    hjust = 1.1, vjust = 1.1, size = 3)

# Dispor os gráficos juntos
grid.arrange(p1, p2, p3, p4, p5,p6, ncol = 2)

# Cancer symptons

cancer_descr <- dfSummary(dt %>% select(exame_fisico,adenopatia,
                                        patologia, foco, risco, resposta_tratamento,
                                        reocorrencia))

# Bivariated analysis #########################################################

# profile x risk
tab_perfil_risco <- dt %>%
  tbl_summary(include = c(faixa_etaria, genero, fumante, historico_fumante,
                          historico_radioterapia, quadro_tireoide),
              by = risco) %>%
  add_p(test = list(faixa_etaria ~ "fisher.test",
                    genero ~ "chisq.test",
                    fumante ~ "chisq.test",
                    historico_fumante ~ "chisq.test",
                    historico_radioterapia ~ "fisher.test",
                    quadro_tireoide ~ "fisher.test"),
        pvalue_fun = ~ style_pvalue(.x, digits = 2))

# cancer symptons

tab_sintomas_tratamentos <- dt %>%
  tbl_summary(include = c(exame_fisico, adenopatia, patologia, foco, risco),
              by = resposta_tratamento) %>%
  add_p(test = list(exame_fisico ~ "fisher.test",
                    adenopatia ~ "fisher.test",
                    patologia ~ "fisher.test",
                    foco ~ "chisq.test",
                    risco ~ "chisq.test"))
