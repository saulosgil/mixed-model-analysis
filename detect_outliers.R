# Pacotes -------------------------------------------------------------------------------------
library(tidyverse)

# Pegando a base mtcars -----------------------------------------------------------------------

mtcars <- mtcars

head(mtcars)

# Inserindo valores extremos ------------------------------------------------------------------

mtcars_com_outliers <- mtcars |>
  bind_rows(
  data.frame(
      mpg = c(0.1, 1039, 481, 1402),
      wt = c(3.21, 1.8230, 2.6740, 3.6720)
    )
  )

# Plot com todos os valores -------------------------------------------------------------------

mtcars_com_outliers |>
  ggplot(mapping = aes(x = wt,
                       y = mpg)) +
  geom_point(size = 2) +
  ggplot2::theme_minimal() +
  ggplot2::labs(x = "Peso do carro",
                y = "Consumo de combustível (milhas/galão)",
                color = "")

# Verificando outliers - z-score --------------------------------------------------------------

# Verificando zscore > 2

mtcars_com_outliers |>
  mutate(
    zscore = (mpg-mean(mpg))/sd(mpg),
    e_outlier = if_else(
      condition = abs(zscore) > 2, true = "É outlier",false = "Não é outlier" )
  ) |>
  ggplot(mapping = aes(x = wt,
                       y = mpg,
                       color = e_outlier)) +
  geom_point(size = 2) +
  ggplot2::theme_minimal() +
  ggplot2::labs(x = "Peso do carro",
                y = "Consumo de combustível (milhas/galão)",
                color = "")

# Verificando zscore > 1

mtcars_com_outliers |>
  mutate(
    zscore = (mpg-mean(mpg))/sd(mpg),
    e_outlier = if_else(
      condition = abs(zscore) > 1, true = "É outlier",false = "Não é outlier" )
  ) |>
  ggplot(mapping = aes(x = wt,
                       y = mpg,
                       color = e_outlier)) +
  geom_point(size = 2) +
  ggplot2::theme_minimal() +
  ggplot2::labs(x = "Peso do carro",
                y = "Consumo de combustível (milhas/galão)",
                color = "")

# Agora vamos ver como fica os dados sem os outliers (zscore > 1)

mtcars_com_outliers |>
  mutate(
    zscore = (mpg-mean(mpg))/sd(mpg),
    e_outlier = if_else(
      condition = abs(zscore) > 1, true = "É outlier",false = "Não é outlier")
  ) |>
  filter(e_outlier == "Não é outlier") |>
  ggplot(mapping = aes(x = wt,
                       y = mpg)) +
  geom_point(size = 2) +
  ggplot2::theme_minimal() +
  ggplot2::labs(x = "Peso do carro",
                y = "Consumo de combustível (milhas/galão)",
                color = "")

# Mais bonitinho
#
# =D

# Verificando outliers - z-score robusto ------------------------------------------------------

mtcars_com_outliers |>
  mutate(
    zscore = (mpg-median(mpg))/mad(mpg),
    e_outlier = if_else(
      condition = abs(zscore) > 2, true = "É outlier",false = "Não é outlier" )
  ) |>
  ggplot(mapping = aes(x = wt,
                       y = mpg,
                       color = e_outlier)) +
  geom_point(size = 2) +
  ggplot2::theme_minimal() +
  ggplot2::labs(x = "Peso do carro",
                y = "Consumo de combustível (milhas/galão)",
                color = "")

# Repare que há mais outliers do que no quando utilizando o zscore

# Verificando zscore robusto > 2

mtcars_com_outliers |>
  mutate(
    zscore = (mpg-median(mpg))/mad(mpg),
    e_outlier = if_else(
      condition = abs(zscore) > 1, true = "É outlier",false = "Não é outlier" )
  ) |>
  ggplot(mapping = aes(x = wt,
                       y = mpg,
                       color = e_outlier)) +
  geom_point(size = 2) +
  ggplot2::theme_minimal() +
  ggplot2::labs(x = "Peso do carro",
                y = "Consumo de combustível (milhas/galão)",
                color = "")

# Agora vamos ver como fica os dados sem os outliers (zscore > 1)

mtcars_com_outliers |>
  mutate(
    zscore = (mpg-median(mpg))/mad(mpg),
    e_outlier = if_else(
      condition = abs(zscore) > 1, true = "É outlier",false = "Não é outlier")
  ) |>
  filter(e_outlier == "Não é outlier") |>
  ggplot(mapping = aes(x = wt,
                       y = mpg)) +
  geom_point(size = 2) +
  ggplot2::theme_minimal() +
  ggplot2::labs(x = "Peso do carro",
                y = "Consumo de combustível (milhas/galão)",
                color = "")

# Mais bonitinho, mas ainda tem um no canto inferior direito!
# Vamos testar zscore robusto > 3!

mtcars_com_outliers |>
  mutate(
    zscore = (mpg-median(mpg))/mad(mpg),
    e_outlier = if_else(
      condition = abs(zscore) > 3, true = "É outlier",false = "Não é outlier")
  ) |>
  filter(e_outlier == "Não é outlier") |>
  ggplot(mapping = aes(x = wt,
                       y = mpg)) +
  geom_point(size = 2) +
  ggplot2::theme_minimal() +
  ggplot2::labs(x = "Peso do carro",
                y = "Consumo de combustível (milhas/galão)",
                color = "")

# AGORA SIM, DADOS BONITINHOS S2 !!!


# FONTE -> [Curso-R](https://blog.curso-r.com/posts/2021-04-15-como-encontrar-outliers/)

