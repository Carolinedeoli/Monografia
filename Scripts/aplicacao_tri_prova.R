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
# 
 itens_2023 <- read_delim("DOCUMENTOS_ENEM/DADOS/ITENS_PROVA_2023.csv", 
                          delim = ";", escape_double = FALSE, trim_ws = TRUE)



#===============================================================================
# 1. IDENTIFICAÇÃO DA PROVA CERTA (manualmente)
#===============================================================================

prova_certa <- 1211

#===============================================================================
# 2. SELEÇÃO DA AMOSTRA DE 300 PARTICIPANTES REAIS
#===============================================================================


participantes_processar <- dados_2023 %>%
  filter(
    CO_PROVA_MT == prova_certa,
    TP_PRESENCA_MT == 1,
    !is.na(TX_RESPOSTAS_MT),
    !is.na(NU_NOTA_MT)
  )

#===============================================================================
# 3. FILTRAGEM E MAPEAMENTO DOS ITENS VÁLIDOS DA TRI (Seu processo de limpeza)
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

# Guardamos os parâmetros limpos em vetores
a_params         <- as.numeric(itens_validos$NU_PARAM_A)
b_params         <- as.numeric(itens_validos$NU_PARAM_B)
c_params         <- as.numeric(itens_validos$NU_PARAM_C)
posicoes_validas <- as.numeric(itens_validos$CO_POSICAO)
gabarito_oficial <- itens_validos$TX_GABARITO # 🌟 Gabarito real para o confronto de acertos

#===============================================================================
# 4. CONFIGURAÇÃO DA QUADRATURA DE GAUSS
#===============================================================================
G <- 40
quad_nodes <- seq(-4, 4, length.out = G)
quad_weights <- dnorm(quad_nodes)
quad_weights <- quad_weights / sum(quad_weights)

#===============================================================================
# 5. LOOP EM MASSA DA TRI (Mapeando acertos primeiro, sem NAs)
#===============================================================================
notas_simuladas_tri <- numeric(nrow(participantes_processar))
total_acertos_validos <- numeric(nrow(participantes_processar))

cat("Processando a TRI para os participantes da prova...\n")

for (i in 1:nrow(participantes_processar)) {
  
  # Captura a string de respostas do aluno atual (45 caracteres)
  resp_string <- participantes_processar$TX_RESPOSTAS_MT[i]
  resp_vetor  <- strsplit(resp_string, "")[[1]]
  
  
  # Extrai apenas as respostas das posições que possuem parâmetros válidos
  resp_filtrada <- resp_vetor[itens_validos$indice_resposta]
  
  # 🌟 CONFRONTO DIRETO: Compara a resposta do aluno com o gabarito oficial dos itens
  u_respostas <- as.numeric(
    resp_filtrada == itens_validos$TX_GABARITO
  )
  
  
  
  cat(
    "Acertos:",
    sum(u_respostas),
    "de",
    length(u_respostas),
    "\n"
  )
  
  
  
  
  # Remove nulos e marcações inválidas (* ou .) transformando em erro (0)
  u_respostas[is.na(u_respostas)] <- 0
  
  
  if(i <= 5){
    
    cat("\n========================\n")
    
    cat("Aluno:", i, "\n")
    
    cat("Acertos:", sum(u_respostas), "\n")
    
    cat("Primeiras respostas:\n")
    print(head(resp_filtrada,10))
    
    cat("Primeiros gabaritos:\n")
    print(head(itens_validos$TX_GABARITO,10))
    
  }
  
  # Salva a contagem de acertos reais dele
  total_acertos_validos[i] <- sum(u_respostas)
  
  # --- CÁLCULO DA VEROSSIMILHANÇA (Fórmula idêntica à sua) ---
  L_theta <- numeric(G)
  for (g_idx in 1:G) {
    theta_g <- quad_nodes[g_idx]
    
    P <- c_params + (1 - c_params) / (1 + exp(-a_params * (theta_g - b_params)))
    P <- pmax(pmin(P, 1 - 1e-15), 1e-15)
    
    L_theta[g_idx] <- exp(sum(u_respostas * log(P) + (1 - u_respostas) * log(1 - P)))
  }
  
  # --- CÁLCULO DO EAP ---
  numerador   <- sum(quad_nodes * L_theta * quad_weights)
  denominador <- sum(L_theta * quad_weights)
  theta_original <- numerador / denominador
  
  # --- ESCALA DO ENEM ---
  notas_simuladas_tri[i] <- 100 * theta_original + 500
}

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
# 7. VISUALIZAÇÃO GRÁFICA ATUALIZADA (Sem warnings)
#===============================================================================
library(ggplot2)
library(patchwork)

g1 <- ggplot(base_tri_comparativa, aes(x = DIFERENCA)) +
  geom_histogram(aes(y = .data$..density..), bins = 30, fill = "#2b5c8f", color = "white", alpha = 0.8) +
  geom_density(color = "#d95f02", linewidth = 1) +
  geom_vline(xintercept = 0, color = "black", linetype = "dashed", linewidth = 0.8) +
  labs(title = "Distribuição do Erro de Estimação", x = "Diferença (Simulada - Real)", y = "Densidade") +
  theme_minimal()

g2 <- ggplot(base_tri_comparativa, aes(x = NU_NOTA_MT, y = NOTA_SIMULADA_TRI)) +
  geom_point(alpha = 0.4, color = "#4f9a74") +
  geom_abline(intercept = 0, slope = 1, color = "red", linetype = "solid", linewidth = 1) +
  labs(title = "Correlação de Notas", x = "Nota Real (INEP)", y = "Nota Simulada (TRI)") +
  theme_minimal()

g1 + g2


#================================

head(itens_validos$indice_resposta)

length(itens_validos$indice_resposta)

class(itens_validos$indice_resposta)

