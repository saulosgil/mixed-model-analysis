# Pacotes -------------------------------------------------------------------------------------

library(tidyverse)
library(lme4)
library(lmerTest)
library(emmeans)

# Para não aparecer notação cientifica

options(scipen = 999)

# Criando grupos ------------------------------------------------------------------------------

set.seed(1234)

x <- data.frame(
  sujeito = as.numeric(c(1:20)),
  grupo = as.factor(c("ctrl", "trt")),
  tempo = as.factor(c("Baseline", "pos")),
  vardep = as.numeric(c(runif(n = 20, min = 18, max = 60)))
)

set.seed(123)

y <- data.frame(
  sujeito = as.numeric(c(1:20)),
  grupo = as.factor(c("ctrl", "trt")),
  tempo = as.factor(c("pos","Baseline")),
  vardep = as.numeric(c(runif(n = 20, min = 40, max = 80)))
)

data <- rbind(x,y)

glimpse(data)

skimr::skim(data)

# Criando um csv para testar no SAS -----------------------------------------------------------

# write_excel_csv(x = data,file = "data.csv",col_names = FALSE,quote = "all")

# Visualização exploratória -------------------------------------------------------------------

# média e SD <- grupo*tempo

data |>
  group_by(grupo, tempo) |>
  summarise(mean = round(mean(vardep),digits = 2),
            DP = round(sd(vardep),digits = 2))

# obs -> igual ao proc means do SAS

# Gráfico

data |>
  ggplot(mapping = aes(x=grupo,
                       y=vardep)) +
  geom_boxplot(mapping = aes(fill = tempo),
               width=0.2,
               position = position_dodge(width=0.5)) +
  labs(title = "Indice de Massa corporal após período experimental",
       x = "Grupo", y = "vardep")

# Mixed model analyis -------------------------------------------------------------------------

datalemr <- lmer(formula = vardep ~ grupo*tempo + (1|sujeito),
                 data = data,
                 REML = TRUE)

# resumo descritivo do modelo

summary(datalemr)

# 06 de agosto das 18 as 22h e 07 das 8h as 18h

anova(datalemr, ddf = "Kenward-Roger")

# Obs. igual ao Type 3 Tests of Fixed Effects do SAS

# Comparações multiplas -----------------------------------------------------------------------

# Ajuste de Bonferroni

pairwise_bonferroni <- emmeans::emmeans(datalemr,
                 ddf = "Kenward_Roger",
                 specs = pairwise ~ grupo*tempo,
                 adjust = "Bonferroni")

pairwise_bonferroni

# 95%CI

confint_bonferroni <- confint(pairwise_bonferroni)

CL_bonferroni <- data.frame(lower.CL = confint_bonferroni$contrasts$lower.CL,
                 upper.CL = confint_bonferroni$contrasts$upper.CL)

# tabela com comparações, EMD, SE, DF, t.ratio, P.value e 95%CI

cbind(
  list(pairwise_bonferroni)[[1]]$contrasts,
  CL_bonferroni)

# Ajuste de Tukey

pairwise_tukey <- emmeans::emmeans(datalemr,
                 ddf = "Kenward_Roger",
                 specs = pairwise ~ grupo*tempo,
                 adjust = "Tukey")

# 95%CI

confint_tukey <- confint(pairwise_tukey)

CL <- data.frame(lower.CL = confint_tukey$contrasts$lower.CL,
                 upper.CL = confint_tukey$contrasts$upper.CL)

# tabela com comparações, EMD, SE, DF, t.ratio, P.value e 95%CI

cbind(
  list(pairwise_tukey)[[1]]$contrasts,
  CL
)

# obs. resultado igual ao Differences of Least Squares Means so SAS


