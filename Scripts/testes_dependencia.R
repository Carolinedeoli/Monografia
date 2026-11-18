#===============================================================================
#Objetivo desse código é realisar testes de hipótese para investigar se 
#características sociais impactam no desempenho dos participantes e, se sim,
#descobrir o valor do efeito e qual categoria difere.

# BIBLIOTECAS
# 1. Instalação dos pacotes (execute apenas uma vez se não os tiver instados)
install.packages(c("rstatix", "effectsize", "car"))

# 2. Carregamento das bibliotecas
library(tidyverse)   # Manipulação e visualização de dados
library(rstatix)     # Testes estatísticos (Levene, ANOVA, Tukey, Games-Howell)
library(effectsize)  # Tamanho do efeito (Omega-quadrado)
library(car)         # Suporte para ANOVA de fatores não balanceados
library(moments)
library(dplyr)
library(emmeans)
library(purrr)
#============================================================================
#  BASE DE DADOS COM CATEGORIAS
#============================================================================

enem_2023 <- read_csv("DOCUMENTOS_ENEM/DADOS/ENEM_2023_categorias.csv")


#TRANSFORMANDO EM FATOR
enem_2023$dep_adm <- as.factor(enem_2023$dep_adm)
enem_2023$tipo_escola <- as.factor(enem_2023$tipo_escola)
enem_2023$escolaridade_mae <- as.factor(enem_2023$escolaridade_mae)
enem_2023$TP_FAIXA_ETARIA <- as.factor(enem_2023$TP_FAIXA_ETARIA)
enem_2023$desempenho <- as.numeric(enem_2023$desempenho)


# ----------------------------------------------------------------------
# 3. EXEMPLO DE FLUXO DE ANÁLISE COMPLETO
# Supondo que seu dataset se chame 'dados', com as colunas 'nota' e 'rede'
# ----------------------------------------------------------------------

# A. Teste de Homocedasticidade (Igualdade de Variâncias de Levene)
# Serve para saber se seguimos com ANOVA tradicional ou ANOVA de Welch
teste_levene <- enem_2023 %>% 
  levene_test(desempenho ~ dep_adm)
print(teste_levene)


# B. Cálculo de Assimetria e Curtose por Dependência Administrativa
estatisticas_forma <- enem_2023 %>%
  group_by(dep_adm) %>%
  summarise(
    N = n(),
    Assimetria = skewness(desempenho, na.rm = TRUE),
    Curtose = kurtosis(desempenho, na.rm = TRUE)
  )

print(estatisticas_forma)
# Interpretação: 
# Assimetria próxima de 0 (entre -1 e 1) = distribuição simétrica.
# Curtose próxima de 3 (entre 2 e 4) = curva normal (mesocúrtica).

# ----------------------------------------------------------------------
# OPÇÃO 1: Se o Levene der p >= 0.05 (Variâncias iguais)
# ----------------------------------------------------------------------

# ANOVA Tradicional
res_anova <- dados %>% 
  anova_test(nota ~ rede)
print(res_anova)

# Teste Post-Hoc de Tukey (Comparações Par a Par)
res_tukey <- dados %>% 
  tukey_hsd(nota ~ rede)
print(res_tukey)

# Tamanho do Efeito: Omega-quadrado (w^2)
modelo_lm <- lm(desempenho ~ dep_adm, data = enem_2023)
omega_squared(modelo_lm)


# ----------------------------------------------------------------------
# OPÇÃO 2: Se o Levene der p < 0.05 (Variâncias diferentes - Mais provável)
# ----------------------------------------------------------------------

# ANOVA de Welch (Ajustada para heterocedasticidade)
res_welch <- enem_2023 %>% 
  welch_anova_test(desempenho ~ dep_adm)
print(res_welch)

# Teste Post-Hoc de Games-Howell (Substituto do Tukey para variâncias desiguais)
res_gh <- enem_2023 %>% 
  games_howell_test(desempenho ~ dep_adm)
print(res_gh)


# Tamanho do Efeito ajustado
modelo_lm <- lm(desempenho ~ dep_adm, data = enem_2023)
omega_squared(modelo_lm)

library(effectsize)
modelo_welch <- oneway.test(desempenho ~ escolaridade_mae, data = enem_2023, var.equal = FALSE)
omega_squared(modelo_welch)


library(effectsize)

# Modelo base ajustado para variâncias desiguais
modelo_welch <- oneway.test(desempenho ~ dep_adm, data = enem_2023, var.equal = FALSE)

# Cálculo correto do Omega Quadrado para Welch
omega_squared(modelo_welch)

# 1. Extração dos valores da ANOVA de Welch
F_welch <- res_welch$statistic
k       <- res_welch$DFn + 1
N_total <- nrow(enem_2023)

# 2. Ômega Quadrado Ajustado pelo N real
omega2_corrigido <- ((k - 1) * (F_welch - 1)) / (((k - 1) * (F_welch - 1)) + N_total)

# Formatação em porcentagem
print(paste0("Omega2 Ajustado: ", round(omega2_corrigido * 100, 2), "%"))

#EFEITO

# Cálculo do d de Cohen para todas as combinações pares
grupos <- levels(enem_2023$dep_adm)
resultado <- do.call(
  rbind,
  lapply(combn(grupos, 2, simplify = FALSE), function(g) {
    
    dados_sub <- subset(enem_2023, dep_adm %in% g)
    
    d <- cohens_d(
      desempenho ~ dep_adm,
      data = dados_sub
    )
    
    d
  })
)
resultado


modelo <- lm(desempenho ~ dep_adm, data = enem_2023)
emm <- emmeans(modelo, ~ dep_adm)
eff_size(
  emm,
  sigma = sigma(modelo),
  edf = df.residual(modelo)
)

#=====================
#Efeito considerando denominador diferente devido a diferença de variancia



grupos <- levels(enem_2023$dep_adm)

resultado_dav <- map_dfr(
  combn(grupos, 2, simplify = FALSE),
  function(g) {
    
    dados <- enem_2023 %>%
      filter(dep_adm %in% g) %>%
      group_by(dep_adm) %>%
      summarise(
        n = n(),
        media = mean(desempenho, na.rm = TRUE),
        dp = sd(desempenho, na.rm = TRUE),
        .groups = "drop"
      )
    
    d_av <- (dados$media[1] - dados$media[2]) /
      sqrt((dados$dp[1]^2 + dados$dp[2]^2) / 2)
    
    tibble(
      grupo1 = g[1],
      grupo2 = g[2],
      diferenca = dados$media[1] - dados$media[2],
      d_av = d_av
    )
  }
)

resultado_dav

resultado_dav <- resultado_dav %>%
  mutate(
    magnitude = case_when(
      abs(d_av) < 0.20 ~ "Desprezível",
      abs(d_av) < 0.50 ~ "Pequeno",
      abs(d_av) < 0.80 ~ "Médio",
      TRUE ~ "Grande"
    ),
    d_av = round(d_av, 3)
  )


#Como estao usando o desvio-padrão médio dos dois grupos:
  
#  $$ d_{av} = \frac{\bar X_1-\bar X_2} {\sqrt{(s_1^2+s_2^2)/2}} $$
  
#  o mais correto é apresentar como \(d_{av}\) ou diferença padronizada de médias com desvio-padrão médio.

#Isso deixa claro para o leitor que não foi utilizado o Cohen's d clássico com desvio-padrão pooled.
