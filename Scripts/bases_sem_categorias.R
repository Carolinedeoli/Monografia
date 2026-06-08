################################################################################

###          LIMPEZA SEM TROCAR NOMES DE CATEGORIAS

################################################################################
library(readr)
library(dplyr)
library(tidyr)
library(arrow)

# ===============================================================================
#                FUNÇÃO DE LIMPEZA E TRATAMENTO DO ENEM (INDIVIDUAL)
# ===============================================================================

limpar_base_enem <- function(dados_raw, ano_corrente) {
  
  message(paste("Iniciando a filtragem da base do ENEM de", ano_corrente, "..."))
  
  # =========================================================
  # 1. FILTRAGEM DE PRESENÇA E REDAÇÃO EM DIA
  # =========================================================
  dados_tratados <- dados_raw %>%
    filter(
      TP_STATUS_REDACAO == 1,
      TP_PRESENCA_CN == 1,
      TP_PRESENCA_CH == 1,
      TP_PRESENCA_LC == 1,
      TP_PRESENCA_MT == 1
    )
  
  # =========================================================
  # 2. REMOVER NOTAS NULAS/ZERADAS (USANDO OS NOMES ORIGINAIS)
  # =========================================================
  col_notas_originais <- c(
    "NU_NOTA_CN",
    "NU_NOTA_CH",
    "NU_NOTA_LC",
    "NU_NOTA_MT",
    "NU_NOTA_REDACAO"
  )
  
  dados_tratados <- dados_tratados %>%
    filter(
      if_all(
        all_of(col_notas_originais),
        ~ !is.na(.x) & .x > 0
      )
    )
  
  # =========================================================
  # 3. CÁLCULO DO DESEMPENHO MÉDIO GERAL
  # =========================================================
  dados_tratados <- dados_tratados %>%
    mutate(
      desempenho = round(
        rowSums(across(all_of(col_notas_originais))) / 5,
        1
      )
    )
  
  # =========================================================
  # 4. RENOMEAR APENAS AS COLUNAS SOCIOECONÔMICAS (MANTÉM OS CÓDIGOS)
  # =========================================================
  dados_tratados <- dados_tratados %>%
    rename(
      ANO              = NU_ANO,
      estado_prova     = SG_UF_PROVA,
      tipo_escola      = TP_ESCOLA,
      dep_adm          = TP_DEPENDENCIA_ADM_ESC,
      escolaridade_mae = Q002,
      moradores        = Q005,
      renda_familiar   = Q006,
      celulares        = Q022,
      computadores     = Q024,
      internet         = Q025
    )
  
  # =========================================================
  # 5. SELEÇÃO FINAL DAS COLUNAS NECESSÁRIAS
  # =========================================================
  dados_final <- dados_tratados %>%
    select(
      NU_INSCRICAO,
      ANO,
      estado_prova,
      TP_FAIXA_ETARIA,
      TP_SEXO,
      TP_ESTADO_CIVIL,
      TP_COR_RACA,
      TP_ST_CONCLUSAO,
      TP_ANO_CONCLUIU,
      tipo_escola,
      IN_TREINEIRO,
      dep_adm,
      NU_NOTA_MT,
      NU_NOTA_CN,
      NU_NOTA_CH,
      NU_NOTA_LC,
      NU_NOTA_REDACAO,
      desempenho,
      NU_NOTA_COMP1,
      NU_NOTA_COMP2,
      NU_NOTA_COMP3,
      NU_NOTA_COMP4,
      NU_NOTA_COMP5,
      escolaridade_mae,
      renda_familiar,
      internet,
      celulares,
      computadores,
      TP_LINGUA
    )
  
  message(paste("Base de", ano_corrente, "filtrada com sucesso!"))
  return(dados_final)
}

# ===============================================================================
# EXECUÇÃO DO PROCESSO
# ===============================================================================

#===========  2023 =============================================================
# 1. Importa a base bruta (usando read_delim do readr que é mais rápida)
dados_2023 <- read_delim(
  "DOCUMENTOS_ENEM/DADOS/MICRODADOS_ENEM_2023.csv", 
  delim = ";", 
  escape_double = FALSE, 
  trim_ws = TRUE,
  show_col_types = FALSE
)

# 2. Executa a função
dados_filtrado_2023 <- limpar_base_enem(dados_2023, ano_corrente = 2023)

# 3. Salva o resultado de forma otimizada (sem aspas desnecessárias nas strings)
#write_csv(dados_filtrado_2023, "DOCUMENTOS_ENEM/DADOS/ENEM_2023.csv")

write_parquet(dados_filtrado_2023, "DOCUMENTOS_ENEM/DADOS/ENEM_2023.parquet")
#===========  2022 =============================================================
# 1. Importa a base bruta (usando read_delim do readr que é mais rápida)
dados_2022 <- read_delim(
  "DOCUMENTOS_ENEM/DADOS/MICRODADOS_ENEM_2022.csv", 
  delim = ";", 
  escape_double = FALSE, 
  trim_ws = TRUE,
  show_col_types = FALSE
)

# 2. Executa a função
dados_filtrado_2022 <- limpar_base_enem(dados_2022, ano_corrente = 2022)

# 3. Salva o resultado de forma otimizada (sem aspas desnecessárias nas strings)
#write_csv(dados_filtrado_2022, "DOCUMENTOS_ENEM/DADOS/ENEM_2022.csv")

write_parquet(dados_filtrado_2022, "DOCUMENTOS_ENEM/DADOS/ENEM_2022.parquet")

#===========  2021 =============================================================
# 1. Importa a base bruta (usando read_delim do readr que é mais rápida)
dados_2021 <- read_delim(
  "DOCUMENTOS_ENEM/DADOS/MICRODADOS_ENEM_2021.csv", 
  delim = ";", 
  escape_double = FALSE, 
  trim_ws = TRUE,
  show_col_types = FALSE
)

# 2. Executa a função
dados_filtrado_2021 <- limpar_base_enem(dados_2021, ano_corrente = 2021)

# 3. Salva o resultado de forma otimizada (sem aspas desnecessárias nas strings)
#write_csv(dados_filtrado_2021, "DOCUMENTOS_ENEM/DADOS/ENEM_2021.csv")

write_parquet(dados_filtrado_2021, "DOCUMENTOS_ENEM/DADOS/ENEM_2021.parquet")

#===========  2020 =============================================================
# 1. Importa a base bruta (usando read_delim do readr que é mais rápida)
dados_2020 <- read_delim(
  "DOCUMENTOS_ENEM/DADOS/MICRODADOS_ENEM_2020.csv", 
  delim = ";", 
  escape_double = FALSE, 
  trim_ws = TRUE,
  show_col_types = FALSE
)

# 2. Executa a função
dados_filtrado_2020 <- limpar_base_enem(dados_2020, ano_corrente = 2020)

# 3. Salva o resultado de forma otimizada (sem aspas desnecessárias nas strings)
#write_csv(dados_filtrado_2020, "DOCUMENTOS_ENEM/DADOS/ENEM_2020.csv")

write_parquet(dados_filtrado_2020, "DOCUMENTOS_ENEM/DADOS/ENEM_2020.parquet")

#===========  2019 =============================================================
# 1. Importa a base bruta (usando read_delim do readr que é mais rápida)
dados_2019 <- read_delim(
  "DOCUMENTOS_ENEM/DADOS/MICRODADOS_ENEM_2019.csv", 
  delim = ";", 
  escape_double = FALSE, 
  trim_ws = TRUE,
  show_col_types = FALSE
)

# 2. Executa a função
dados_filtrado_2019 <- limpar_base_enem(dados_2019, ano_corrente = 2019)

# 3. Salva o resultado de forma otimizada (sem aspas desnecessárias nas strings)
#write_csv(dados_filtrado_2019, "DOCUMENTOS_ENEM/DADOS/ENEM_2019.csv")

write_parquet(dados_filtrado_2019, "DOCUMENTOS_ENEM/DADOS/ENEM_2019.parquet")
