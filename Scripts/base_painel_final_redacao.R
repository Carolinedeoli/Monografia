#===============================================================================

#                BASES PAINEL: REDAÇÃO

#===============================================================================

#========  BIBLIOTECAS =========================================================

library(data.table)
library(tidyverse)
library(jsonlite)

#===============================================================================
processar_redacao_enem <- function(ano_prova) {
  cat("Processando redação do ano:", ano_prova, "...\n")
  arquivo_input <- paste0("DOCUMENTOS_ENEM/DADOS/MICRODADOS_ENEM_", ano_prova, ".csv")
  
  dados <- fread(arquivo_input, sep = ";", dec = ",", select = c(
   "SG_UF_PROVA", "TP_ESCOLA", "TP_DEPENDENCIA_ADM_ESC", "TP_STATUS_REDACAO"
  ))
  
  # 1. Primeiro, descobrimos o total absoluto de registros do ano (Denominador)
  total_geral_ano <- nrow(dados)
  
  # 2. Processamos e calculamos a porcentagem sobre esse total geral
  base_redacao_ano <- dados %>%
    mutate(
      ANO = as.integer(ano_prova),
      estado_prova = as.character(SG_UF_PROVA),
      tipo_escola = case_when(
        TP_ESCOLA == 2 ~ "Pública",
        TP_ESCOLA == 3 ~ "Privada",
        TRUE ~ "Não Informado"
      ),
      
      dep_adm = case_when(
        TP_DEPENDENCIA_ADM_ESC == 1 ~ "Federal",
        TP_DEPENDENCIA_ADM_ESC == 2 ~ "Estadual",
        TP_DEPENDENCIA_ADM_ESC == 3 ~ "Municipal",
        TP_DEPENDENCIA_ADM_ESC == 4 ~ "Privada",
        TRUE ~ "Não Informado"
      ),
      
      motivo_status = case_when(
        TP_STATUS_REDACAO == 1 ~ "Válida",
        TP_STATUS_REDACAO == 2 ~ "Anulada",
        TP_STATUS_REDACAO == 3 ~ "Cópia Texto Motivador",
        TP_STATUS_REDACAO == 4 ~ "Em Branco",
        TP_STATUS_REDACAO == 6 ~ "Fuga ao Tema",
        TP_STATUS_REDACAO == 7 ~ "Não Atendimento ao Tipo",
        TP_STATUS_REDACAO == 8 ~ "Texto Insuficiente",
        TP_STATUS_REDACAO == 9 ~ "Parte Desconectada",
        TRUE ~ "Não Compareceu/Outros"
      )
    ) %>%
    group_by(ANO, estado_prova, tipo_escola, dep_adm, motivo_status) %>%
    summarise(
      numero_pessoas = as.integer(n()),
      .groups = 'drop'
    ) %>%
    # A porcentagem é baseada no TOTAL DO ANO
    mutate(
      porcentagem = round((numero_pessoas / total_geral_ano) * 100, 5)
    )%>%
    mutate(
      
      tipo_escola =
        as.character(tipo_escola),
      
      dep_adm =
        as.character(dep_adm),
      
      motivo_status =  
        as.character(motivo_status),
      
      porcentagem =
        as.numeric(porcentagem))
  
  return(base_redacao_ano)
}

# Unificação
base_redacao_completa <- bind_rows(processar_redacao_enem(2019),processar_redacao_enem(2020),processar_redacao_enem(2021),processar_redacao_enem(2022), processar_redacao_enem(2023))

#write_json(redacao_completa, "DADOS/painel_redacao_completo.json", dataframe = "rows")
#write.csv(base_redacao_completa, "DOCUMENTOS_ENEM/DADOS/REDAÇÃO.csv", row.names = FALSE)
