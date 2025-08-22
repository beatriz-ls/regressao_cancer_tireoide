# Libs
library(vcd)
library(summarytools)
library(gtsummary)
library(ggplot2)
library(lmtest)
library(car)
library(tidyverse)
library(janitor)    # para limpeza de nomes
library(patchwork)  # para organizar gráficos
library(ggtext)     # para formatação de títulos
library(reshape2)   # para manipulação de dados longos
library(ggmosaic)

# Descriptive analysis ########################################################

# Coorte description

coorte_descr <- dfSummary(dt %>% select(faixa_etaria, genero, fumante,
                                        historico_fumante, historico_radioterapia,
                                        quadro_tireoide, risco))

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

cancer_descr <- dfSummary(dt %>% select(exame_fisico,adenopatia,estagio,
                                        patologia, foco, risco, resposta_tratamento,
                                        reocorrencia, T, N, M))

# Análises Bivariadas #########################################################

# Coorte perfil x Risco #

# Tabela qui quadrado e fisher
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
# Calculo medidas de associação
vars <- c("faixa_etaria", "genero", "fumante", "historico_fumante",
          "historico_radioterapia", "quadro_tireoide")
odds <- list()

for (var in vars) {
  aux <- table(dt[[var]], dt$reocorrencia)
  odds <- assocstats(aux)
  print(paste0("Variável:",var))
  print(odds)
}

# perfil x reocorrencia

tab_perfil_reocorrencia<- dt %>%
  tbl_summary(include = c(faixa_etaria, genero, fumante, historico_fumante,
                          historico_radioterapia, quadro_tireoide),
              by = reocorrencia) %>%
  add_p(test = list(faixa_etaria ~ "fisher.test",
                    genero ~ "chisq.test",
                    fumante ~ "chisq.test",
                    historico_fumante ~ "chisq.test",
                    historico_radioterapia ~ "fisher.test",
                    quadro_tireoide ~ "fisher.test"),
        pvalue_fun = ~ style_pvalue(.x, digits = 2))

# cancer perfil x reocorrência


tab_sintomas_reocorrencia <- dt %>%
  tbl_summary(include = c(exame_fisico,adenopatia,estagio,
                          patologia, foco, risco, resposta_tratamento, T, N, M),
              by = reocorrencia) %>%
  add_p(test = list(exame_fisico ~ "fisher.test",
                    adenopatia ~ "fisher.test",
                    patologia ~ "chisq.test",
                    foco ~ "chisq.test",
                    risco ~ "chisq.test",
                    estagio ~ "fisher.test",
                    resposta_tratamento ~ "fisher.test",
                    T ~ "fisher.test",
                    N ~ "chisq.test",
                    M ~ "fisher.test"))

# Calculo medidas de associação
vars <- c("exame_fisico","adenopatia","estagio",
          "patologia", "foco", "risco", "resposta_tratamento", "T", "N", "M")
odds <- list()

for (var in vars) {
  aux <- table(dt[[var]], dt$reocorrencia)
  odds <- assocstats(aux)
  print(paste0("Variável:",var))
  print(odds)
}

### Análise de concordancia de risco avaliado e reocorrencia

tab_risco_reocorrencia <- table(dt$risco, dt$reocorrencia)

kappa <- kappa(tab_risco_reocorrencia)

# Gráficos de frequencia -------------------------------------------------------

# =========================================
# BLOCO 1 – Perfil Demográfico (barras proporcionais)
# =========================================
p1 <- ggplot(dt, aes(x = faixa_etaria, fill = reocorrencia)) +
  geom_bar(position = "fill") +
  scale_y_continuous(labels = scales::percent) +
  labs(title = "Faixa Etária por reocorrencia", y = "Proporção", x = NULL) +
  scale_fill_viridis_d(option = "D", direction = -1) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 30, hjust = 1))

p2 <- ggplot(dt, aes(x = genero, fill = reocorrencia)) +
  geom_bar(position = "fill") +
  scale_y_continuous(labels = scales::percent) +
  labs(title = "Gênero por reocorrencia", y = "Proporção", x = NULL) +
  scale_fill_viridis_d(option = "D", direction = -1) +
  theme_minimal()

p3 <- ggplot(dt, aes(x = fumante, fill = reocorrencia)) +
  geom_bar(position = "fill") +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_viridis_d(option = "D", direction = -1) +
  labs(title = "Tabagismo Atual por reocorrencia", y = "Proporção", x = NULL) +
  theme_minimal()

p4 <- ggplot(dt, aes(x = historico_fumante, fill = reocorrencia)) +
  geom_bar(position = "fill") +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_viridis_d(option = "D", direction = -1) +
  labs(title = "Histórico de Tabagismo por reocorrencia", y = "Proporção", x = NULL) +
  theme_minimal()

(p1 | p2) / (p3 | p4)
ggsave("plot/bloco1_barras.png", width = 10, height = 8)

# =========================================
# BLOCO 2 – Características Clínicas (barras proporcionais)
# =========================================
p_qt <- ggplot(dt, aes(x = quadro_tireoide, fill = reocorrencia)) +
  geom_bar(position = "fill") +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_viridis_d(option = "D", direction = -1) +
  labs(title = "Quadro Tireóide por reocorrencia", y = "Proporção", x = NULL) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 30, hjust = 1))

p_exame <- ggplot(dt, aes(x = exame_fisico, fill = reocorrencia)) +
  geom_bar(position = "fill") +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_viridis_d(option = "D", direction = -1) +
  labs(title = "Exame Físico por reocorrencia", y = "Proporção", x = NULL) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 30, hjust = 1))

p_adeno <- ggplot(dt, aes(x = adenopatia, fill = reocorrencia)) +
  geom_bar(position = "fill") +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_viridis_d(option = "D", direction = -1) +
  labs(title = "Adenopatia por reocorrencia", y = "Proporção", x = NULL) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 30, hjust = 1))

# Salva painel
(p_qt | p_exame) / p_adeno
ggsave("plot/bloco2_barras.png", width = 12, height = 8)

# =========================================
# BLOCO 3 – Patologia (mosaic plot)
# =========================================
p_mosaico_patologia <- ggplot(data = dt) +
  geom_mosaic(aes(weight = 1,
                  x = product(patologia),
                  fill = reocorrencia),
              na.rm = TRUE) +
  scale_fill_viridis_d(option = "D", direction = -1) +
  labs(title = "Distribuição da Patologia por Recorrência",
       x = "Patologia",
       y = "Proporção") +
  theme_minimal()


ggsave("plot/bloco3_patologia_mosaico.png", p_mosaico_patologia, width = 6, height = 4)


p_mosaico_radio <- ggplot(data = dt) +
  geom_mosaic(aes(weight = 1,
                  x = product(historico_radioterapia),
                  fill = reocorrencia),
              na.rm = TRUE) +
  scale_fill_viridis_d(option = "D", direction = -1) +
  labs(title = "Distribuição da Histórico de Radioterapia por Recorrência",
       x = "Histórico de Radioterapia",
       y = "Proporção") +
  theme_minimal()


ggsave("plot/bloco3_radioterapia_mosaico.png", p_mosaico_radio, width = 6, height = 4)

p_mosaico_estagio <- ggplot(data = dt) +
  geom_mosaic(aes(weight = 1,
                  x = product(estagio),
                  fill = reocorrencia),
              na.rm = TRUE) +
  scale_fill_viridis_d(option = "D", direction = -1) +
  labs(title = "Distribuição do Estagio por Recorrência",
       x = "Estagio",
       y = "Proporção") +
  theme_minimal()


ggsave("plot/bloco3_estagio_mosaico.png", p_mosaico_estagio, width = 6, height = 4)

# =========================================
# BLOCO 4 – TNM, Risco e Resposta (barras)
# =========================================
p_t <- ggplot(dt, aes(x = T, fill = reocorrencia)) +
  geom_bar(position = "fill") +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_viridis_d(option = "D", direction = -1) +
  labs(title = "T por reocorrencia", y = "Proporção", x = NULL) +
  theme_minimal()

p_n <- ggplot(dt, aes(x = N, fill = reocorrencia)) +
  geom_bar(position = "fill") +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_viridis_d(option = "D", direction = -1) +
  labs(title = "N por reocorrencia", y = "Proporção", x = NULL) +
  theme_minimal()

p_m <- ggplot(dt, aes(x = M, fill = reocorrencia)) +
  geom_bar(position = "fill") +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_viridis_d(option = "D", direction = -1) +
  labs(title = "M por reocorrencia", y = "Proporção", x = NULL) +
  theme_minimal()

p_risco <- ggplot(dt, aes(x = risco, fill = reocorrencia)) +
  geom_bar(position = "fill") +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_viridis_d(option = "D", direction = -1) +
  labs(title = "Risco por reocorrencia", y = "Proporção", x = NULL) +
  theme_minimal()

p_resposta <- ggplot(dt, aes(x = resposta_tratamento, fill = reocorrencia)) +
  geom_bar(position = "fill") +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_viridis_d(option = "D", direction = -1) +
  labs(title = "Resposta ao Tratamento por reocorrencia", y = "Proporção", x = NULL) +
  theme_minimal()

(p_t | p_n | p_m) / (p_risco | p_resposta)
ggsave("plot/bloco4_barras.png", width = 12, height = 8)


