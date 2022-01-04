# Pacotes -------------------------------------------------------------------------------------

library(tidyverse)
library(lme4)
library(emmeans)

# Criando grupos ------------------------------------------------------------------------------

set.seed(1234)

x <- data.frame(
  sujeito = as.numeric(c(1:20)),
  grupo = as.factor(c("ctrl", "trt")),
  tempo = as.factor(c("Baseline", "pos")),
  imc = as.numeric(c(runif(n = 20, min = 18, max = 60)))
)

set.seed(123)

y <- data.frame(
  sujeito = as.numeric(c(1:20)),
  grupo = as.factor(c("ctrl", "trt")),
  tempo = as.factor(c("pos","Baseline")),
  imc = as.numeric(c(runif(n = 20, min = 40, max = 80)))
)

data <- rbind(x,y)

skimr::skim(data)

# Criando um csv para testar no SAS -----------------------------------------------------------

write_excel_csv(x = data,file = "data.csv",col_names = FALSE,quote = "all")

# Visualização exploratória -------------------------------------------------------------------

# média e SD <- grupo*tempo

data |>
  group_by(grupo, tempo) |>
  summarise(mean = mean(imc),
            DP = sd(imc))

# obs -> igual ao proc means do SAS

# Gráfico

data |>
  ggplot(mapping = aes(x=grupo,
                       y=imc)) +
  geom_boxplot(mapping = aes(fill = tempo),
               width=0.2,
               position = position_dodge(width=0.5)) +
  labs(title = "Indice de Massa corporal após período experimental",
       x = "Grupo", y = "IMC (peso/estatura*2)")

# Mixed model analyis -------------------------------------------------------------------------

datalemr <- lmer(formula = imc ~ grupo*tempo + (1|sujeito),
                 data = data,
                 REML = FALSE)

# Extraindo coeficientes

coefs <- data.frame(coef(summary(datalemr)))

# P-value baseado em uma distribuição normal

coefs$p.value <- 2 * (1 - pnorm(abs(coefs$t.value)))

round(coefs,digits = 8)

# Coeficientes utilizando a função summary

summary(datalemr)

# Comparações multiplas -----------------------------------------------------------------------

# Ajuste de Bonferroni

emmeans::emmeans(datalemr,
                 ddf = "Kenward_Roger",
                 specs = pairwise ~ grupo*tempo,
                 adjust = "Bonferroni")

# obs. igual ao Differences of Least Squares Means so SAS

# Ajuste de Tukey

emmeans::emmeans(datalemr,
                 ddf = "Kenward_Roger",
                 specs = pairwise ~ grupo*tempo,
                 adjust = "Tukey")


