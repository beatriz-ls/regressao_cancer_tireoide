# Libs
library(vcd)
library(summarytools)
library(gtsummary)
library(ggplot2)
library(lmtest)
library(car)

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

##### Modelos Log Lineares #####

# adenopatia x reocorrencia

tab_adenopatia <- table(dt$adenopatia, dt$reocorrencia)

tab_adenopatia <- as.data.frame(tab_adenopatia)

colnames(tab_adenopatia) <- c("adenopatia", "reocorrencia", "Freq")


mod_sat_adenopatia <- glm(Freq ~ adenopatia*reocorrencia, family = poisson(), data = tab_adenopatia)

mod_ind_adenopatia <- glm(Freq ~ adenopatia+reocorrencia, family = poisson(), data = tab_adenopatia)

anova(mod_ind_adenopatia,mod_sat_adenopatia)

# patologia

tab_patologia <- table(dt$patologia, dt$reocorrencia)

tab_patologia <- as.data.frame(tab_patologia)

colnames(tab_patologia) <- c("patologia", "reocorrencia", "Freq")


mod_sat_patologia <- glm(Freq ~ patologia*reocorrencia, family = poisson(), data = tab_patologia)

mod_ind_patologia <- glm(Freq ~ patologia+reocorrencia, family = poisson(), data = tab_patologia)


# foco

tab_foco <- table(dt$foco, dt$reocorrencia)

tab_foco <- as.data.frame(tab_foco)

colnames(tab_foco) <- c("foco", "reocorrencia", "Freq")


mod_sat_foco <- glm(Freq ~ foco*reocorrencia, family = poisson(), data = tab_foco)

mod_ind_foco <- glm(Freq ~ foco+reocorrencia, family = poisson(), data = tab_foco)

# resposta_tratamento

tab_resposta_tratamento <- table(dt$resposta_tratamento, dt$reocorrencia)

tab_resposta_tratamento <- as.data.frame(tab_resposta_tratamento)

colnames(tab_resposta_tratamento) <- c("resposta_tratamento", "reocorrencia", "Freq")


mod_sat_resposta_tratamento <- glm(Freq ~ resposta_tratamento*reocorrencia, family = poisson(), data = tab_resposta_tratamento)

mod_ind_resposta_tratamento <- glm(Freq ~ resposta_tratamento+reocorrencia, family = poisson(), data = tab_resposta_tratamento)

# T

tab_T <- table(dt$T, dt$reocorrencia)

tab_T <- as.data.frame(tab_T)

colnames(tab_T) <- c("T", "reocorrencia", "Freq")


mod_sat_T <- glm(Freq ~ T*reocorrencia, family = poisson(), data = tab_T)

mod_ind_T <- glm(Freq ~ T+reocorrencia, family = poisson(), data = tab_T)


# N

# M

####### Modelo Logistico #####


dt <- dt %>% select(-c(estagio, exame_fisico, risco))
dt1 <- dt1 %>% select(-c(estagio, exame_fisico,risco))

model <- glm(reocorrencia ~., data = as.data.frame(dt), family = binomial)
model1 <- glm(reocorrencia ~., data = as.data.frame(dt1), family = binomial)

vif_values <- vif(model)
print(vif_values)

vif_values1 <- vif(model1)
print(vif_values1)

# Aplicar o método stepwise para seleção de variáveis
step_model <- step(model, direction = "both", trace = 0)

# Teste de razão de verossimilhança (likelihood ratio test)

lr_test <- anova(model, test = "LRT")
lr_test
lr_test1 <- anova(model1, test = "LRT")
lr_test1

# teste de Wald para cada coeficiente
wald_test <- summary(model)$coefficients[, "Pr(>|z|)"]
wald_test







