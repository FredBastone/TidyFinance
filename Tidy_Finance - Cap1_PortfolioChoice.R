#### Instalar e executar os pacotes necessários para Tidyverse ####

install.packages('tidyverse')
install.packages('tidyquant')
install.packages('ggplot2')

library(tidyverse)
library(tidyquant)
library(ggplot2)

#### Baixando os Dados de empresas Listadas no DOW ####

ticker <- tq_index("DOW")

precosind <- tq_get(ticker,
                    get = "stock.prices",
                    from = "2001-01-01",
                    to = "2022-11-18"
)

#### Ajustando os Dados ####

precosind2 <- precosind %>%
  group_by(symbol) %>%
  mutate(n = n()) %>%
  ungroup() %>%
  filter(n == max(n)) %>%
  select(-n)

retornosind2 <- precosind2 %>%
  mutate(month = floor_date(date, "month")) %>%
  group_by(symbol, month) %>%
  summarize(preco = last(adjusted), .groups = "drop_last") %>%
  mutate(ret = preco/lag(preco) - 1) %>%
  drop_na(ret) %>%
  select(-preco)

matriz_retornos2 <- retornosind2 %>%
  pivot_wider(
    names_from = symbol,
    values_from = ret) %>%
  select(-month)

View(matriz_retornos2)

#### Obtendo o portifólio de variância mínima ####

sigma <- cov(matriz_retornos2)
mu <- colMeans(matriz_retornos2)

N <- ncol(matriz_retornos2)
iota <- rep(1,N)
mvp_weights <- solve(sigma) %*% iota
mvp_weights <- mvp_weights/sum(mvp_weights)

mvpsummary <- tibble(
  retorno_medio = as.numeric(t(mvp_weights) %*% mu),
  volatilidade = as.numeric(sqrt(t(mvp_weights) %*% sigma %*% mvp_weights))
)

#### Obtendo o portifólio de menor variância dando um retorno 3x o MVP ####

mu_bar <- 3 * t(mvp_weights) %*% mu

C <- as.numeric(t(iota) %*% solve(sigma) %*% iota)
D <- as.numeric(t(iota) %*% solve(sigma) %*% mu)
E <- as.numeric(t(mu) %*% solve(sigma) %*% mu)

lambda_tilde <- as.numeric(2*(mu_bar - D/C)/(E - D^2/C))
efp_weights <- mvp_weights + lambda_tilde/2*(solve(sigma) %*% mu - D * mvp_weights)

c <- seq(from = -0.4, to = 1.9, by = 0.01)

res <- tibble(
  c = c,
  mu = NA,
  sd = NA
)

for (j in seq_along(c)) {
  w <- (1 - c[j]) * mvp_weights + (c[j]) * efp_weights
  res$mu[j] <- 12 * 100 * t(w) %*% mu
  res$sd[j] <- 12 * sqrt(100) * sqrt(t(w) %*% sigma %*% w)
}

plotef <- res %>%
  ggplot(aes(x = sd, y = mu)) +
  geom_point() +
  geom_point(
    data = res %>% filter(c %in% c(0,1)),
    size = 2
  ) +
  geom_point(
    data = tibble(
      mu = 12 * 100 * mu,
      sd = 12 * 10 * sqrt(diag(sigma))
    ),
    aes(y = mu, x = sd), size = 1
  ) +
  labs(
    x = "Desvio padrão anualizado em %",
    y = "Retorno esperado anualizado em %",
    title = "Fronteira Eficiente para constituintes do índice DOW") +
  theme_bw()