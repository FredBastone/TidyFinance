#### Exercicios Utilizando o S&P500 ####

library(tidyverse)
library(tidyquant)
library(ggplot2)

#### Baixar a Base de Dados ####

ticker <- tq_index("SP500")

SNP500 <- tq_get(ticker,
       get = "stock.prices",
       from = "2001-01-01",
       to = "2022-11-18"
)

SNP500$dailyspread <- SNP500$high - SNP500$low

#### Plotando as cotações ajustadas e não ajustadas ####

plotSNP500 <- SNP500 %>%
  ggplot(aes(x = date, y = adjusted, color = symbol)) +
  geom_line() +
  labs(
    x = NULL,
    y = "Cotação Ajustada $",
    title = "Cotação das Ações dos Constituintes da S&P500 desde 2001-01-02")+
  theme_bw() +
  theme(legend.position = "none")

plotSNP500un <- SNP500 %>%
  ggplot(aes(x = date, y = close, color = symbol)) +
  geom_line() +
  labs(
    x = NULL,
    y = "Cotação Ajustada $",
    title = "Cotação das Ações dos Constituintes da S&P500 desde 2001-01-02")+
  theme_bw() +
  theme(legend.position = "none")

#### Calculando os Retornos ####

retornosSNP500 <- SNP500 %>%
  group_by(symbol) %>%
  arrange(date) %>%
  mutate(ret = (adjusted / lag(adjusted)) -1) %>%
  select(symbol, date, ret) %>%
  drop_na(ret)

#### Histograma dos Retornos ####

quintil_05 <- quantile(retornosSNP500 %>% pull(ret)*100, probs = 0.05)

histretSNP500 <- retornosSNP500 %>%
  ggplot(aes(x = ret*100)) +
  geom_histogram(bins = 1000) +
  geom_vline(aes(xintercept = quintil_05),
             linetype = "dashed") +
  labs(
    x = NULL,
    y = NULL,
    title = "Distribuição dos retornos percentuais díarios de constituintes do SNP500"
  ) +
  theme_bw()

#### Sumarizando os Retornos ####

sumretSNP500 <- retornosSNP500 %>%
  mutate(ret = ret*100) %>%
  summarize(across(ret, list(media_diaria = mean, sd_diaria = sd, minima_diaria = min, maxima_diaria = max)
  ))

sumretSNP500ano <- retornosSNP500 %>%
  mutate(ret = ret*100) %>%
  group_by(year = year(date)) %>%
  summarize(across(ret, list(media_diaria = mean, sd_diaria = sd, minima_diaria = min, maxima_diaria = max),
                   .names = "{.fn}"
  )) %>%
  print(n = Inf)

#### Volume S&P500 ####

volumeSNP500 <- SNP500 %>%
  group_by(date) %>%
  summarize(volume = sum(volume*close/1e9))

plotvolumeSNP500 <- volumeSNP500 %>%
  ggplot(aes(x = date, y = volume)) +
  geom_line() +
  labs(
    x = NULL,
    y = NULL,
    title = "Volume agregado de trading Diário - S&P500") + 
  theme_bw()

####

SNP500NA <- drop_na(SNP500)

SNP500mod <- SNP500NA %>%
  group_by(symbol) %>%
  mutate(n = n()) %>%
  ungroup() %>%
  filter(n == max(n)) %>%
  select(-n)

retornosSNP500mod <- SNP500NA %>%
  mutate(month = floor_date(date, "month")) %>%
  group_by(symbol, month) %>%
  summarize(price = last(adjusted), .groups = "drop_last") %>%
  mutate(ret = price/lag(price) - 1) %>%
  drop_na(ret) %>%
  select(-price)

matriz_retornos <- retornosSNP500mod %>%
  pivot_wider(
    names_from = symbol,
    values_from = ret) %>%
  select(-month)

View(matriz_retornos)

sigmaSNP500 <- cov(matriz_retornos)
sigmaSNP500 <- drop_na(sigmaSNP500)
muSNP500 <- colMeans(matriz_retornos)

#### Achar o protifolio de mínima variância ####

N <- ncol(matriz_retornos)
iota <- rep(1,N)
mvp_weightsSNP500 <- solve(sigmaSNP500) %*% iota
mvp_weightsSNP500 <- mvp_weightsSNP500/sum(mvp_weightsSNP500)

mvpsummarySNP500 <- tibble(
  retorno_medio = as.numeric(t(mvp_weightsSNP500) %*% muSNP500),
  volatilidade = as.numeric(sqrt(t(mvp_weightsSNP500) %*% sigmaSNP500 %*% mvp_weightsSNP500))
)

View(mvpsummarySNP500)