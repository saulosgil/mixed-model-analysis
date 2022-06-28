# Função para fazer ANOVA para medidas repetidas com ajuste de Tukey
# Para rodar o teste basta rodar uma data.frame com o as colunas:
#   sujeito;
#   grupo;
#   tempo;
#   vardep;

anova_tukey <- function(x){
  # Para não aparecer conotação cientifica
  options(scipen = 999)

  # Ajustando as colunas

  x <-
    x |>
    dplyr::mutate(
      sujeito = as.numeric(sujeito),
      grupo = as.factor(grupo),
      tempo = as.factor(tempo),
    vardep = as.numeric(vardep)
  )

  # Mixed model analyis -------------------------------------------------------------------------

  datalemr <- lmer(formula = vardep ~ grupo*tempo + (1|sujeito),
                   data = x,
                   REML = TRUE)

  # Type III Analysis of Variance Table with Kenward-Roger's method

  type3effects <- anova(datalemr, ddf = "Kenward-Roger")

  # Comparações multiplas -----------------------------------------------------------------------

  # Ajuste de Tukey

  pairwise_tukey <- emmeans::emmeans(datalemr,
                                          ddf = "Kenward_Roger",
                                          specs = pairwise ~ grupo*tempo,
                                          adjust = "Tukey")

  # 95%CI

  confint_tukey <- confint(pairwise_tukey)

  CL_tukey <- data.frame(lower.CL = confint_tukey$contrasts$lower.CL,
                              upper.CL = confint_tukey$contrasts$upper.CL)

  # tabela com comparações, EMD, SE, DF, t.ratio, P.value e 95%CI

  multiplas_comparacoes <-
    cbind(
    list(pairwise_tukey)[[1]]$contrasts,
    CL_tukey)

  # Imprimindo os resultados

  if (type3effects$`Pr(>F)`[3] < 0.05){
    print(multiplas_comparacoes)
    }
  else
    print(type3effects)
}
