#===============================================================================
#                 BIBLIOTECAS
#===============================================================================

library(readr)
library(dplyr)
library(tidyr)
library(mirt)
library(knitr)
library(kableExtra)
library(nortest)
library(scales)
library(ggplot2)
library(patchwork)



#===============================================================================
#                 IMPORTANDO BASES
#===============================================================================
#Microdados 2023
# dados_2023 <- read_delim("DOCUMENTOS_ENEM/DADOS/MICRODADOS_ENEM_2023.csv", 
#                          delim = ";", escape_double = FALSE, trim_ws = TRUE)
# 
# itens_2023 <- read_delim("DOCUMENTOS_ENEM/DADOS/ITENS_PROVA_2023.csv", 
#                          delim = ";", escape_double = FALSE, trim_ws = TRUE)

#===============================================================================
#                SELECIONAR UM ALUNO
#===============================================================================

set.seed(0725)

participante_teste <- dados_2023 %>%
  filter(
    TP_ST_CONCLUSAO == 2,
    TP_PRESENCA_CN == 1,
    TP_PRESENCA_CH == 1,
    TP_PRESENCA_LC == 1,
    TP_PRESENCA_MT == 1,
    TP_STATUS_REDACAO == 1,
    !is.na(NU_NOTA_MT)
  ) %>%
  sample_n(1)

#===============================================================================
#              INFORMAÇÃOES
#===============================================================================
participante_teste <- participante_teste %>%
  mutate(
    lingua = case_when(
      TP_LINGUA == 0 ~ "Inglês",
      TP_LINGUA == 1 ~ "Espanhol"
    ),
    
    escola = case_when(
      TP_ESCOLA == 1 ~ "Não respondeu",
      TP_ESCOLA == 2 ~ "Pública",
      TP_ESCOLA == 3 ~ "Privada"
    ),
    
    sexo = case_when(
      TP_SEXO == "M" ~ "Masculino",
      TP_SEXO == "F" ~ "Feminino"
    )
  )

participante_teste %>%
  select(
    sexo,
    TP_FAIXA_ETARIA,
    lingua,
    escola,
    NU_NOTA_CN,
    NU_NOTA_CH,
    NU_NOTA_LC,
    NU_NOTA_MT,
    NU_NOTA_REDACAO,
    CO_PROVA_MT,
    CO_PROVA_LC,
    CO_PROVA_CN,
    CO_PROVA_CH
  )
mean(
  c(
    participante_teste$NU_NOTA_CN,
    participante_teste$NU_NOTA_CH,
    participante_teste$NU_NOTA_LC,
    participante_teste$NU_NOTA_MT,
    participante_teste$NU_NOTA_REDACAO
  )
)

#===============================================================================
#              GABARITOS
#===============================================================================


corrigir_prova <- function(respostas, gabarito) {
  
  resp <- strsplit(respostas, "")[[1]]
  gab  <- strsplit(gabarito, "")[[1]]
  
  as.numeric(resp == gab)
  
}

resp_mt <- corrigir_prova(
  participante_teste$TX_RESPOSTAS_MT,
  participante_teste$TX_GABARITO_MT
)

resp_cn <- corrigir_prova(
  participante_teste$TX_RESPOSTAS_CN,
  participante_teste$TX_GABARITO_CN
)

resp_ch <- corrigir_prova(
  participante_teste$TX_RESPOSTAS_CH,
  participante_teste$TX_GABARITO_CH
)

# Linguagens 
corrigir_lc <- function(respostas, gabarito, lingua){
  
  resp <- strsplit(respostas, "")[[1]]
  gab  <- strsplit(gabarito, "")[[1]]
  
  gab_valido <- if(lingua == 0){
    c(gab[1:5], gab[11:50])   # Inglês
  } else {
    c(gab[6:10], gab[11:50])  # Espanhol
  }
  
  ifelse(
    resp %in% c(".", "*"),
    0,
    as.numeric(resp == gab_valido)
  )
}

resp_lc_bin <- corrigir_lc(
  participante_teste$TX_RESPOSTAS_LC,
  participante_teste$TX_GABARITO_LC,
  participante_teste$TP_LINGUA
)

#Número de acertos

acertos_cn <- sum(resp_cn)
acertos_ch <- sum(resp_ch)
acertos_lc <- sum(resp_lc_bin)
acertos_mt <- sum(resp_mt)


acertos_area <- data.frame(
  Area = c(
    "Ciências da Natureza",
    "Ciências Humanas",
    "Linguagens e Códigos",
    "Matemática"
  ),
  Acertos = c(
    acertos_cn,
    acertos_ch,
    acertos_lc,
    acertos_mt
  ),
  Total_Itens = c(45, 45, 45, 45)
)

total_acertos <- acertos_cn +
  acertos_ch +
  acertos_lc +
  acertos_mt

total_acertos

acertos_area$Percentual <- round(
  100 * acertos_area$Acertos / acertos_area$Total_Itens,
  1
)

acertos_area

#===============================================================================
#              GABARITOS
#===============================================================================

gabaritos_mt <- itens_2023 %>%
  filter(SG_AREA == "MT") %>%
  select(CO_PROVA, CO_POSICAO, TX_GABARITO)

participante_mt <- data.frame(
  CO_POSICAO = 1:length(resp_mt),
  resposta = resp_mt
)

comparacao <- gabaritos_mt %>%
  left_join(participante_mt, by = "CO_POSICAO") %>%
  mutate(acerto = TX_GABARITO == resposta)

score_provas <- comparacao %>%
  group_by(CO_PROVA) %>%
  summarise(
    acertos = sum(acerto, na.rm = TRUE),
    total = n(),
    .groups = "drop"
  ) %>%
  arrange(desc(acertos))

prova_certa <- score_provas$CO_PROVA[1]
#------------------

prova_certa <- participante_teste$CO_PROVA_MT
itens_mt <- itens_2023 %>%
  filter(SG_AREA == "MT",
         CO_PROVA == prova_certa) %>%
  arrange(CO_POSICAO)

#matriz para TRI
dados <- itens_2023 %>%
  filter(
    SG_AREA == "MT",
    CO_PROVA == prova_certa
  ) %>%
  arrange(CO_POSICAO) %>%
  mutate(
    item_id = CO_POSICAO,
    acerto = resp_mt
  ) %>%
  filter(TX_GABARITO != "X")

dados <- dados %>%
  filter(
    !is.na(acerto),
    !is.na(NU_PARAM_A),
    !is.na(NU_PARAM_B),
    !is.na(NU_PARAM_C)
  )


#-------------------------------------------------------------------------------

theta_data <- dados %>%
  select(item_id, acerto, NU_PARAM_A, NU_PARAM_B,NU_PARAM_C ) %>%
  arrange(item_id)

u <- theta_data$acerto   
a <- theta_data$NU_PARAM_A
b <- theta_data$NU_PARAM_B
c <- theta_data$NU_PARAM_C

#==================~===========================================================
##          TESTANDO TRI PARA UM ÚNICO ALUNO MATEMÁTICA 
#==============================================================================

# --- PASSO 1: Criar os 40 pontos de quadratura na distribuição a priori ---
# Conforme o edital do ENEM: 40 pontos distribuídos em uma Normal Padrão

 G <- 40
 quad_nodes <- seq(-4, 4, length.out = G)              # Pontos de -4 a 4 (escala theta original)
 quad_weights <- dnorm(quad_nodes)                     # Pesos da densidade normal padrão
quad_weights <- quad_weights / sum(quad_weights)       # Normaliza os pesos para somarem 1

# --- PASSO 2: Garantir que vetores de parâmetros estão limpos ---

u_respostas <- as.numeric(theta_data$acerto)
a_params    <- as.numeric(theta_data$NU_PARAM_A)
b_params    <- as.numeric(theta_data$NU_PARAM_B)
c_params    <- as.numeric(theta_data$NU_PARAM_C)

# --- PASSO 3: Calcular a Verossimilhança para cada ponto de quadratura ---
# Criamos uma matriz para armazenar a probabilidade do padrão de resposta em cada nó

L_theta <- numeric(G)

for (g_idx in 1:G) {
  theta_g <- quad_nodes[g_idx]
  
  # Equação do Modelo Logístico de 3 Parâmetros (3PL)
  # P(theta) = c + (1 - c) / (1 + exp(-a * (theta - b)))
  D <- 1
  
 
  P <- c_params + (1 - c_params) / (D + exp(-a_params * (theta_g - b_params)))
  
  # Evita que probabilidades fiquem estritamente em 0 ou 1 (estabilidade numérica)
  P <- pmax(pmin(P, 1 - 1e-15), 1e-15)
  
  # Probabilidade do padrão de respostas observado (Verossimilhança)
  # L = Produtório de [P^u * (1-P)^(1-u)]
  L_theta[g_idx] <- exp(sum(u_respostas * log(P) + (1 - u_respostas) * log(1 - P)))
}

# --- PASSO 4: Aplicar o cálculo do Expected A Posteriori (EAP) ---
# Conforme a fórmula do edital: Integral(theta * L * f) / Integral(L * f)
numerador   <- sum(quad_nodes * L_theta * quad_weights)
denominador <- sum(L_theta * quad_weights)

theta_original <- numerador / denominador

# --- PASSO 5: Aplicar a Transformação Linear para a Escala do ENEM ---
nota_final_enem <- 100 * theta_original + 500

# --- RESULTADO FINAL ---
cat("\n============================================\n")
cat("Nota simulada na escala do ENEM:", round(nota_final_enem, 2), "\n")
cat("Nota real registrada (para comparar):", participante_teste$NU_NOTA_MT, "\n")
cat("============================================\n")

