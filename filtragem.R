# Libs

library(dplyr)
library(readr)

# Load data

dt0 <- read_csv("Thyroid_Diff.csv")

# Pre processig data

dt <- dt0 |>
  mutate(faixa_etaria = case_when(#Age < 14 ~ "Infantil",
                                  Age >= 14 & Age < 20 ~ "Adolescente",
                                  Age >= 20 & Age < 35 ~ "Jovem Adulto",
                                  Age >= 35 & Age < 60 ~ "Adulto",
                                  TRUE ~ "Idoso"),
         genero = as.factor(ifelse(Gender == "F", "Mulher", "Homem")),
         fumante = as.factor(ifelse(Smoking == "No", "Não", "Sim")),
         historico_fumante = as.factor(ifelse(`Hx Smoking` == "No" ,"Não","Sim")),
         historico_radioterapia = as.factor(ifelse(`Hx Radiothreapy` == "No","Não","Sim")),
         quadro_tireoide = as.factor(case_when(`Thyroid Function` == "Euthyroid" ~ "Eutireoide",
                                               `Thyroid Function` == "Clinical Hyperthyroidism" ~ "Hipertireoidismo Clínico",
                                               `Thyroid Function` == "Clinical Hypothyroidism" ~ "Hipotireoidismo Clínico",
                                               `Thyroid Function` == "Subclinical Hyperthyroidism" ~ "Hipertireoidismo Subclínico",
                                               `Thyroid Function` == "Subclinical Hypothyroidism" ~ "Hipotireoidismo Subclínico")),
         exame_fisico = as.factor(case_when(`Physical Examination` == "Single nodular goiter-left" ~ "Bócio nodular único à esquerda",
                                            `Physical Examination` == "Single nodular goiter-right" ~ "Bócio nodular único à direita",
                                            `Physical Examination` == "Multinodular goiter" ~ "Bócio multinodular",
                                            `Physical Examination` == "Diffuse goiter" ~ "Bócio difuso")),
         adenopatia = as.factor(case_when(Adenopathy == "No" ~ "Não",
                                          Adenopathy == "Right" ~ "Direita",
                                          Adenopathy == "Extensive" ~ "Extensivo",
                                          Adenopathy == "Left" ~ "Esquerda",
                                          Adenopathy == "Bilateral" ~ "Bilateral",
                                          Adenopathy == "Posterior" ~"Posterior")),
         patologia = as.factor(case_when(Pathology == "Micropapillary" ~ "Micropapilar",
                                         Pathology == "Papillary" ~ "Papilar",
                                         Pathology == "Follicular" ~ "Folicular",
                                         Pathology == "Hurthel cell" ~ "Células de Hurthle")),
         foco = as.factor(ifelse(Focality == "Uni-Focal","Unifocal","Multifocal")),
         risco = as.factor(case_when(Risk  == "Low" ~ "Baixo",
                                     #Risk == "Itermediate" ~ "Intermediato",
                                     TRUE ~ "Alto")),
         resposta_tratamento = as.factor(case_when(Response == "Indeterminate" ~ "Indeterminado",
                                                   Response == "Excellent" ~ "Excelente",
                                                   Response == "Structural Incomplete" ~ "Estrutura incompleta",
                                                   TRUE ~ "Bioquimica incompleta")),
         reocorrencia = as.factor(ifelse(Recurred == "No", "Não", "Sim")),
         T = factor(case_when(T == "T1a" ~ "T1",
                              T == "T1b" ~ "T1",
                              T == "T2" ~ "T2",
                              T == "T3a" ~ "T3",
                              T == "T3b" ~ "T3",
                              T =="T4a" ~ "T4",
                              T == "T4b" ~ "T4"),
                    levels = c("T1", "T2", "T3", "T4")),
         N = factor(case_when(N == "N0" ~ "N0",
                              N == "N1a" ~ "N1",
                              N == "N1b" ~ "N1"),
                    levels = c("N0", "N1"))) %>%
  mutate(faixa_etaria = factor(faixa_etaria,
                               levels = c("Adolescente", "Jovem Adulto", "Adulto", "Idoso")),
         risco = factor(risco, levels = c("Baixo", "Alto")),
         estagio = factor(Stage, levels = c("I", "II", "III", "IV"))) %>%
  select(-c(Age, Gender, Smoking, `Hx Smoking`, `Thyroid Function`,
            `Physical Examination`, `Adenopathy`, `Pathology`, `Focality`, `Risk`,
            `Stage`,`Response`, `Recurred`, `Hx Radiothreapy`))

