# Libs
library(vcd)
library(Desctools)

# Data

dados<-matrix(c(221, 200, 208, 160, 291, 106, 360, 160, 316, 140, 311, 97),
              nrow = 3, ncol = 4,
              dimnames = list(Partido=c("Democrata", "Independente", "Republicano"),
                              Moradia=c("A", "B", "C", "D")))

# teste qui quadrado

chisq <- chisq.test(dados)

# assocstats

assocstats(dados)

# modelo saturado

mod.sat <- glm(Freq ~ Partido*Moradia, family = poisson(), data = dados)

exp(mod.sat$coefficients)

exp(confint(mod.sat))

# modelo independente

mod.ind <- glm(Freq ~ Partido + Moradia, family = poisson(), data = dados)

exp(mod.ind$coefficients)

exp(confint(mod.ind))

anova(mod.ind, mod.sat)

############# ANÁLISE MAIS NÍVEIS ##############

admit <- UCBAdmissions

dimnames(admit) <- list(
  Admit = c("Admited", "Rejected"), # a referencia é na ordem
  Sex = c("Male", "Female"),
  Dept = c("A", "B", "C", "D", "E", "F")
)

ftable(admit,row.vars = c("Dept", "Sex"), col.vars = "Admit")

SA.D <- oddsratio(admit) # oods ratio de sexo e admição por departamento

exp(SA.D$coefficients)

mantelhaen.test(admit) # associação marginal

# associação homogenia
# test brewslow day

SA <- margin.table(admit, c(2,1))

oddsratio(SA)

BreslowDayTest(admit)
