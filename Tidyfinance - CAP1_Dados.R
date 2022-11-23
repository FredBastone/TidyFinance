#### Instalar e executar os pacotes necessários para Tidyverse ####

install.packages('tidyverse')
install.packages('tidyquant')
install.packages('ggplot2')

library(tidyverse)
library(tidyquant)
library(ggplot2)

#### Baixar a cotação da Apple do Yahoo Finance ####

precos <- tq_get(c("AAPL"),
                 get = "stock.prices",
                 from = "2001-01-01",
                 to = "2022-11-18"
)

View(Precos)

#### Plotando a série obtida ####

plotprecos <- precos %>%
  ggplot(aes(x = date, y = adjusted)) +
  geom_line() +
  labs(
    x = NULL,
    y = "Cotação Ajustada $",
    title = "Cotação das Ações da Apple do início de 2000 até novembro de 2022")+
  theme_bw()

#### Obter o retorno diário das ações ####

retornos <- precos %>%
  arrange(date) %>%
  mutate(ret = (adjusted / lag(adjusted)) -1) %>%
  select(symbol, date, ret)

View(retornos)

retornos <- retornos %>%
  drop_na(ret)

#### Obter a distribuição dos retornos percentuais em um histograma ####

quintil_05 <- quantile(retornos %>% pull(ret)*100, probs = 0.05)

histret <- retornos %>%
  ggplot(aes(x = ret*100)) +
  geom_histogram(bins = 250) +
  geom_vline(aes(xintercept = quintil_05),
             linetype = "dashed") +
  labs(
    x = NULL,
    y = NULL,
    title = "Distribuição dos retornos percentuais díarios da Apple"
  ) +
  theme_bw()

#### Sumarizar os dados ####

sumret <- retornos %>%
  mutate(ret = ret*100) %>%
  summarize(across(ret, list(media_diaria = mean, sd_diaria = sd, minima_diaria = min, maxima_diaria = max)
  ))
  
sumretano <- retornos %>%
  mutate(ret = ret*100) %>%
  group_by(year = year(date)) %>%
  summarize(across(ret, list(media_diaria = mean, sd_diaria = sd, minima_diaria = min, maxima_diaria = max),
                   .names = "{.fn}"
  )) %>%
  print(n = Inf)

#### Trabalhando com Indices ####

ticker <- tq_index("DOW")

precosind <- tq_get(ticker,
                    get = "stock.prices",
                    from = "2001-01-01",
                    to = "2022-11-18"
                  )

plotprecosind <- precosind %>%
  ggplot(aes(x = date, y = adjusted, color = symbol)) +
  geom_line() +
  labs(
    x = NULL,
    y = "Cotação Ajustada $",
    title = "Cotação de 30 empresas listadas no Dow Jones do início de 2000 até novembro de 2022")+
  theme_bw()

#### Retornos do ìndice ####

retornosind <- precosind %>%
  group_by(symbol) %>%
  arrange(date) %>%
  mutate(ret = (adjusted / lag(adjusted)) -1) %>%
  select(symbol, date, ret) %>%
  drop_na(ret)

sumretindsym <- retornosind %>%
  mutate(ret = ret*100) %>%
  group_by(symbol) %>%
  summarize(across(ret, list(media_diaria = mean, sd_diaria = sd, minima_diaria = min, maxima_diaria = max),
                   .names = "{.fn}"
  )) %>%
  print(n = Inf)

#### Outros métodos de Agregação ####

volume <- precosind %>%
  group_by(date) %>%
  summarize(volume = sum(volume*close/1e9))

plotvolume <- volume %>%
  ggplot(aes(x = date, y = volume)) +
  geom_line() +
  labs(
    x = NULL,
    y = NULL,
    title = "Volume agregado de trading Diário - DOW Jones") + 
  theme_bw()
