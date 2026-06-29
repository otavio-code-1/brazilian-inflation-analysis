# =====================================================
# Projeto: Análise do IPCA
# Autor: Otávio Oliveira
# Linguagem: R
# Objetivo: Analisar a evolução da inflação brasileira
# =====================================================

rm(list = ls())
install.packages(c(
  "tidyverse",
  "ggplot2",
  "readr",
  "dplyr",
  "lubridate",
  "rbcb"
))

library(tidyverse)
library(lubridate)
library(rbcb)
library(ggplot2)
library(readr)
library(dplyr)

# =====================================================
# Agora iremos importar os dados do IPCA(%) mensal
#                 (2000-2024)
# =====================================================

ipca <- get_series(
  code = 433,
  start_date = "2000-01-01"
) %>%
  rename(ipca = `433`) %>%
  mutate(year = year(date)) %>%
  dplyr::filter(year <= 2024)
head(ipca)

# ==================================================
# Explorando a base de dados
# ==================================================

# Estrutura da base
str(ipca)

# Resumo estatístico
summary(ipca)

# Número de linhas e colunas
dim(ipca)

# Primeiras observações
head(ipca)

# Últimas observações
tail(ipca)

# ==================================================
# Gráfico da evolução do IPCA
# ==================================================

ggplot(ipca, aes(x = date, y = ipca)) +
  geom_line(linewidth = 1) +
  labs(
    title = "Evolução do IPCA Mensal no Brasil (2000–2024)",
    subtitle = "Dados do Banco Central do Brasil - Série 433",
    x = "Ano",
    y = "IPCA Mensal (%)"
  ) +
  scale_x_date(
    date_breaks = "2 years",
    date_labels = "%Y"
  ) +
  theme_minimal(base_size = 12)

# ==================================================
# Estatísticas Descritivas
# ==================================================

summary(ipca)

mean(ipca$ipca)

median(ipca$ipca)

sd(ipca$ipca)

min(ipca$ipca)

max(ipca$ipca)

ipca <- ipca %>%
  mutate(
    month = month(date, label = TRUE, abbr = TRUE)
  )

ipca_mes <- ipca %>%
  group_by(month) %>%
  summarise(
    media = mean(ipca)
  )

# ==================================================
# Agora vamos gerar o gráfico da média do IPCA
#             por mês (2000-2024)
# ==================================================

ggplot(ipca_mes,
       aes(x = month,
           y = media)) +
  geom_col() +
  labs(
    title = "Média do IPCA por mês (2000–2024)",
    x = "Mês",
    y = "IPCA médio (%)"
  ) +
  theme_minimal()

ggplot(ipca,
       aes(x = month,
           y = ipca)) +
  geom_boxplot() +
  labs(
    title = "Distribuição do IPCA por mês",
    x = "Mês",
    y = "IPCA (%)"
  ) +
  theme_minimal()

# ==================================================
# Agora vamos gerar uma tabela com os dados dos
# top 10 meses com maior IPCA (%) (2000-2024)
# ==================================================

top10 <- ipca %>%
  arrange(desc(ipca)) %>%
  select(date, ipca) %>%
  head(10)

top10

# ==================================================
# Agora vamos gerar uma tabela com os dados dos
# top 10 meses com menor IPCA (%) (2000-2024)
# ==================================================

bottom10 <- ipca %>%
  arrange(ipca) %>%
  select(date, ipca) %>%
  head(10)

bottom10