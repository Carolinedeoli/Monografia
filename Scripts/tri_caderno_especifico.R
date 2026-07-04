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
dados_2023 <- read_delim("DOCUMENTOS_ENEM/DADOS/MICRODADOS_ENEM_2023.csv", 
                         delim = ";", escape_double = FALSE, trim_ws = TRUE)

itens_2023 <- read_delim("DOCUMENTOS_ENEM/DADOS/ITENS_PROVA_2023.csv", 
                         delim = ";", escape_double = FALSE, trim_ws = TRUE)



#===============================================================================
# 1. IDENTIFICAÇÃO DA PROVA CERTA (manualmente)
#===============================================================================

table(dados_2023$CO_PROVA_MT)
prova_certa <- 1212   

#===============================================================================
# 2. SELEÇÃO DADOS DO CADERNO ESCOLHIDO
#===============================================================================


participantes_processar <- dados_2023 %>%
  filter(
    CO_PROVA_MT == prova_certa,
    TP_PRESENCA_MT == 1,
    !is.na(TX_RESPOSTAS_MT),
    !is.na(NU_NOTA_MT)
  )

#===============================================================================
# 3. FILTRAGEM E MAPEAMENTO DOS ITENS VÁLIDOS DA TRI
#===============================================================================
itens_validos <- itens_2023 %>%
  filter(
    SG_AREA == "MT",
    CO_PROVA == prova_certa,
    TX_GABARITO != "X",
    !is.na(NU_PARAM_A),
    !is.na(NU_PARAM_B),
    !is.na(NU_PARAM_C)
  ) %>%
  arrange(CO_POSICAO) %>%
  mutate(
    indice_resposta = CO_POSICAO - min(CO_POSICAO) + 1
  )

# Guardar os parâmetros limpos em vetores
a_params         <- as.numeric(itens_validos$NU_PARAM_A)
b_params         <- as.numeric(itens_validos$NU_PARAM_B)
c_params         <- as.numeric(itens_validos$NU_PARAM_C)
posicoes_validas <- as.numeric(itens_validos$CO_POSICAO)
gabarito_oficial <- itens_validos$TX_GABARITO 

#===============================================================================
# 5. MATRIZ DE RESPOSTAS (0/1)
#===============================================================================

cat("Montando matriz de respostas...\n")

mat_respostas <- matrix(
  NA,
  nrow = nrow(participantes_processar),
  ncol = nrow(itens_validos)
)

for(i in seq_len(nrow(participantes_processar))){
  
  resp_vetor <- strsplit(
    participantes_processar$TX_RESPOSTAS_MT[i],
    ""
  )[[1]]
  
  resp_filtrada <- resp_vetor[itens_validos$indice_resposta]
  
  acertos <- as.numeric(
    resp_filtrada == itens_validos$TX_GABARITO
  )
  
  acertos[is.na(acertos)] <- 0
  
  mat_respostas[i, ] <- acertos
}

total_acertos_validos <- rowSums(mat_respostas)

colnames(mat_respostas) <- paste0(
  "V",
  sprintf("%02d", 1:ncol(mat_respostas))
)

#===============================================================================
# 6. MODELO MIRT COM PARÂMETROS FIXOS
#===============================================================================

cat("Criando tabela de parâmetros...\n")

pars <- mirt(
  as.data.frame(mat_respostas),
  1,
  itemtype = "3PL",
  pars = "values"
)

#===============================================================================
# 7. FIXANDO PARÂMETROS OFICIAIS DO ENEM
#===============================================================================

# Discriminação
pars$value[pars$name == "a1"] <- a_params

# Dificuldade
pars$value[pars$name == "d"] <- -a_params * b_params

# Acerto casual
pars$value[pars$name == "g"] <- c_params

# Fixa os parâmetros
pars$est[pars$name %in% c("a1","d","g")] <- FALSE

#===============================================================================
# 8. AJUSTE COM ITENS FIXOS
#===============================================================================

cat("Estimando proficiências...\n")

modelo_enem <- mirt(
  as.data.frame(mat_respostas),
  1,
  itemtype = "3PL",
  pars = pars,
  quadpts = 40,
  verbose = TRUE
)
#===============================================================================
# 9. ESTIMAÇÃO EAP
#===============================================================================

custom_prior <- function(theta, ...){
  dnorm(theta, 0, 1)
}

theta_eap <- fscores(
  modelo_enem,
  method = "EAP",
  quadpts = 40,
  full.scores = TRUE
)
theta_eap <- as.numeric(theta_eap)

#===============================================================================
# 10. ESCALA ENEM
#===============================================================================

notas_simuladas_tri <- 500 + 100 * theta_eap

#===============================================================================
# 11. BASE FINAL
#===============================================================================

base_tri_comparativa <- participantes_processar %>%
  select(
    NU_INSCRICAO,
    NU_NOTA_MT
  ) %>%
  mutate(
    CO_PROVA = prova_certa,
    ACERTOS_VALIDOS = total_acertos_validos,
    NOTA_SIMULADA_TRI = round(notas_simuladas_tri,2),
    DIFERENCA = round(
      NOTA_SIMULADA_TRI - NU_NOTA_MT,
      2
    )
  )


#===============================================================================
# 6. BASE CONSOLIDADA E MÉTRICAS DE ERRO
#===============================================================================
base_tri_comparativa <- participantes_processar %>%
  select(NU_INSCRICAO, NU_NOTA_MT) %>%
  mutate(
    CO_PROVA = prova_certa,
    ACERTOS_VALIDOS = total_acertos_validos,
    NOTA_SIMULADA_TRI = round(notas_simuladas_tri, 2),
    DIFERENCA = round(NOTA_SIMULADA_TRI - NU_NOTA_MT, 2)
  )

cat("\n--- PRIMEIRAS LINHAS DA BASE FINAL ---\n")
print(head(base_tri_comparativa))

ERROS <- base_tri_comparativa$DIFERENCA
cat("\n======================================================\n")
cat("      ESTATÍSTICAS DE VALIDAÇÃO DA AMOSTRA            \n")
cat("======================================================\n")
cat("Erro Médio Absoluto (MAE)  :", round(mean(abs(ERROS)), 3), "pontos\n")
cat("Raiz do Erro Médio (RMSE)  :", round(sqrt(mean(ERROS^2)), 3), "pontos\n")
cat("Desvio Padrão do Erro      :", round(sd(ERROS), 3), "pontos\n")
cat("======================================================\n")


#===============================================================================
# 7. VISUALIZAÇÃO GRÁFICA 
#===============================================================================

#======================================
#DENSIDADE COM COEFICIENTES MODELO
#======================================
cor_real <- "#1f4e5b"      
cor_estimada <- "#e06d53"  

ggplot(base_temp_teste) +
  # Curva da Nota Real
  geom_density(aes(x = NU_NOTA_MT, color = "Nota Real"), linewidth = 1.2) +
  # Curva da Nota Estimada
  geom_density(aes(x = NOTA_SIMULADA_TRI_TESTE, color = "Nota Estimada"), linewidth = 1.2) +
  # Configuração manual de cores e mapeamento para as linhas da legenda
  scale_color_manual(
    values = c("Nota Real" = cor_real, "Nota Estimada" = cor_estimada),
    name = NULL
  ) +
  labs(
    title = "Comparativo de Densidade: Notas Reais vs. Estimadas TESTE",
    subtitle = "Evidência visual do efeito de encolhimento do estimador EAP",
    x = "Nota na Prova de Matemática",
    y = "Densidade Populacional"
  ) +
  scale_x_continuous(breaks = seq(200, 1000, by = 100)) +
  theme_minimal(base_size = 11) +
  theme(
    panel.grid.minor = element_blank(),
    plot.title = element_text(face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 9, color = "grey40", hjust = 0.5),
    legend.position = c(0.85, 0.85),
    legend.background = element_rect(fill = "white", color = "grey90")
  )

#DENSIDADE COM ESCALA 500, 100
#======================================
ggplot(base_temp_teste) +
  # Curva da Nota Real
  geom_density(aes(x = NU_NOTA_MT, color = "Nota Real"), linewidth = 1.2) +
  # Curva da Nota Estimada
  geom_density(aes(x = NOTA_SIMULADA_TRI, color = "Nota Estimada"), linewidth = 1.2) +
  # Configuração manual de cores e mapeamento para as linhas da legenda
  scale_color_manual(
    values = c("Nota Real" = cor_real, "Nota Estimada" = cor_estimada),
    name = NULL
  ) +
  labs(
    title = "Comparativo de Densidade: Notas Reais vs. Estimadas ",
    subtitle = "Evidência visual do efeito de encolhimento do estimador EAP",
    x = "Nota na Prova de Matemática",
    y = "Densidade Populacional"
  ) +
  scale_x_continuous(breaks = seq(200, 1000, by = 100)) +
  theme_minimal(base_size = 11) +
  theme(
    panel.grid.minor = element_blank(),
    plot.title = element_text(face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 9, color = "grey40", hjust = 0.5),
    legend.position = c(0.85, 0.85),
    legend.background = element_rect(fill = "white", color = "grey90") 
  )
