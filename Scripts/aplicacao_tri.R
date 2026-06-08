#===============================================================================
#                 BIBLIOTECAS
#===============================================================================

library(readr)
library(dplyr)
library(tidyr)
library(nortest)
library(scales)
library(ggplot2)
library(knitr)
library(kableExtra)
library(patchwork)

#===============================================================================
#                 IMPORTANDO BASES
#===============================================================================
#Microdados 2023
dados_2023 <- read_delim("DOCUMENTOS_ENEM/DADOS/MICRODADOS_ENEM_2023.csv", 
                         delim = ";", escape_double = FALSE, trim_ws = TRUE)

itens_2023 <- read_delim("DOCUMENTOS_ENEM/DADOS/ITENS_PROVA_2023.csv", 
                         delim = ";", escape_double = FALSE, trim_ws = TRUE)

#===============================================================================
#                SELECIONAR UM ALUNO
#===============================================================================

set.seed(08)

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
    NU_NOTA_REDACAO
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

itens_mt <- itens_2023 %>%
  filter(SG_AREA == "MT",
         CO_PROVA == prova_certa) %>%
  arrange(CO_POSICAO)

#metriz para TRI
dados <- itens_2023 %>%
  filter(SG_AREA == "MT",
         CO_PROVA == prova_certa) %>%
  arrange(CO_POSICAO) %>%
  mutate(
    item_id = CO_POSICAO,
    acerto = resp_mt
  )

theta_data <- dados %>%
  select(item_id, acerto, CO_HABILIDADE, NU_PARAM_A, NU_PARAM_B,NU_PARAM_C ) %>%
  arrange(item_id)

u <- theta_data$acerto   # <- isso é o que TRI usa (0/1)
a <- theta_data$NU_PARAM_A
b <- theta_data$NU_PARAM_B
c <- theta_data$NU_PARAM_C


###########################3
library(mirt)

mod <- mirt(theta_data[, c("acerto")], 1, itemtype = "2PL")

#https://cran.r-project.org/web/packages/irtoys/index.html
