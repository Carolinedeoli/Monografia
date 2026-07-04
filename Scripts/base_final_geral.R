#===============================================================================

#                BASES PAINEL: GERAL

#===============================================================================

#========  BIBLIOTECAS =========================================================

library(data.table)
library(tidyverse)
library(jsonlite)

#===============================================================================

# --- FUNÇÃO PARA PROCESSAR CADA ANO ---
processar_ano_enem <- function(ano_prova) {
  cat("Processando o ano:", ano_prova, "...\n")
  
  arquivo_input <- paste0("DOCUMENTOS_ENEM/DADOS/MICRODADOS_ENEM_", ano_prova, ".csv")
  
  # 1. Leitura seletiva das colunas
  dados <- fread(arquivo_input, sep = ";", dec = ",", select = c(
   "SG_UF_PROVA","TP_ESCOLA", "TP_DEPENDENCIA_ADM_ESC", 
    "TP_PRESENCA_CN", "TP_PRESENCA_CH", "TP_PRESENCA_LC", "TP_PRESENCA_MT",
    "TP_STATUS_REDACAO"
  ))
  total_geral_ano <- nrow(dados)
  total_redacoes_feitas <- sum(ifelse(dados$TP_STATUS_REDACAO %in% c(1, 2, 3, 4, 6, 7, 8, 9), 1, 0), na.rm = TRUE)
  # 2. Processamento e Categorização (Apenas alterando para "Não Informado")
  base_ano <- dados %>%
    mutate(
      ANO = as.integer(ano_prova),
      estado_prova = as.character(SG_UF_PROVA),
      
      # Categoria 1: Tipo de Escola (Mantém original, apenas unifica o termo nulo)
      tipo_escola = case_when(
        TP_ESCOLA == 2 ~ "Pública",
        TP_ESCOLA == 3 ~ "Privada",
        TP_ESCOLA == 4 ~ "Exterior",
        TRUE ~ "Não Informado"
      ),
      
      # Categoria 2: Dependência Administrativa (Mantém original)
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
      ),
      # Lógica de Presença e Participação
      presenca_dia_1 = ifelse(TP_PRESENCA_CH == 1 & TP_PRESENCA_LC == 1, 1, 0),
      presenca_dia_2 = ifelse(TP_PRESENCA_CN == 1 & TP_PRESENCA_MT == 1, 1, 0),
      participantes = ifelse(presenca_dia_1 == 1 & presenca_dia_2 == 1 & TP_STATUS_REDACAO %in% c(1, 2, 3, 4, 6, 7, 8, 9), 1, 0)
    ) %>%
    # 3. Agrupamento pelas variáveis de perfil
    group_by(ANO,estado_prova, tipo_escola, dep_adm, motivo_status) %>%
    summarise(
      total_inscritos = as.integer(n()),
      qtd_presenca_dia_1 = as.integer(sum(presenca_dia_1, na.rm = TRUE)),
      qtd_presenca_dia_2 = as.integer(sum(presenca_dia_2, na.rm = TRUE)),
      qtd_participantes = as.integer(sum(participantes, na.rm = TRUE)),
      qtd_redacoes_validas = as.integer(sum(TP_STATUS_REDACAO == 1, na.rm = TRUE)),
      
      .groups = 'drop'
    ) %>%
  # 4. Tipagem final
  mutate(
    
    tipo_escola =
      as.character(tipo_escola),
    
    dep_adm =
      as.character(dep_adm),
    
    motivo_status =
      as.character(motivo_status)
  )
  
  return(base_ano)
  
  
  
}

# --- EXECUÇÃO E UNIFICAÇÃO DOS ANOS ---
base_painel_completa <- bind_rows(processar_ano_enem(2019),processar_ano_enem(2020),processar_ano_enem(2021),processar_ano_enem(2022), processar_ano_enem(2023))


# 4. Exportação Única para o seu Painel JS
#write_json(base_painel_completa, "DADOS/painel_categorias_completo.json", dataframe = "rows")
#write.csv(base_painel_completa, "DOCUMENTOS_ENEM/DADOS/GERAL.csv", row.names = FALSE)
cat("Processo finalizado! Arquivo com dados originais unificados criado.")

