################################################################################

###          LIMPEZA TROCAR NOMES DE CATEGORIAS

################################################################################
library(readr)
library(dplyr)
library(tidyr)
library(arrow)

# ===============================================================================
#                DICIONÁRIOS DE RÓTULOS DAS VARIÁVEIS CATEGÓRICAS
# ===============================================================================
# Cada vetor é nomeado pelo código original (como texto) e aponta para o rótulo.
# Usar sempre como.character(coluna) na indexação para não haver erro de match
# entre inteiro e string, e para que categorias ausentes virem NA (e não "NA").

LABELS_SEXO <- c(
  "M" = "Masculino",
  "F" = "Feminino"
)

LABELS_COR_RACA <- c(
  "0" = "Não Informado",
  "1" = "Branca",
  "2" = "Preta",
  "3" = "Parda",
  "4" = "Amarela",
  "5" = "Indígena",
  "6" = "Não Informado"
)

LABELS_ESCOLA <- c(
  "1" = "Não Informado",
  "2" = "Pública",
  "3" = "Privada"
)

LABELS_DEP_ADM <- c(
  "1" = "Federal",
  "2" = "Estadual",
  "3" = "Municipal",
  "4" = "Privada"
)

LABELS_INTERNET <- c(
  "A" = "Não possui Internet",
  "B" = "Possui Internet"
)

LABELS_TREINEIRO <- c(
  "0" = "Não",
  "1" = "Sim"
)

LABELS_CONCLUSAO <- c(
  "1" = "Já concluiu o Ensino Médio",
  "2" = "Concluirá o Ensino Médio nesse ano",
  "3" = "Concluirá após este ano",
  "4" = "Não concluiu/não cursa"
)

LABELS_ESCOLARIDADE_MAE <- c(
  "A" = "Nunca estudou",
  "B" = "Não completou EF (até 5º ano)",
  "C" = "Não completou EF (até 9º ano)",
  "D" = "EF completo",
  "E" = "Médio completo",
  "F" = "Superior completo",
  "G" = "Pós-graduação",
  "H" = "Não sabe"
)

LABELS_ESCOLARIDADE_PAI <- c(
  "A" = "Nunca estudou",
  "B" = "Não completou EF (até 5º ano)",
  "C" = "Não completou EF (até 9º ano)",
  "D" = "EF completo",
  "E" = "Médio completo",
  "F" = "Superior completo",
  "G" = "Pós-graduação",
  "H" = "Não sabe"
)

LABELS_RENDA <- c(
  "A" = "Sem renda",
  "B" = "Até 1 SM",
  "C" = "De 1 a 1.5 SM",
  "D" = "De 1.5 a 2 SM",
  "E" = "De 2 a 2.5 SM",
  "F" = "De 2.5 a 3 SM",
  "G" = "De 3 a 4 SM",
  "H" = "De 4 a 5 SM",
  "I" = "De 5 a 6 SM",
  "J" = "De 6 a 7 SM",
  "K" = "De 7 a 8 SM",
  "L" = "De 8 a 9 SM",
  "M" = "De 9 a 10 SM",
  "N" = "De 10 a 12 SM",
  "O" = "De 12 a 15 SM",
  "P" = "De 15 a 20 SM",
  "Q" = "Acima de 20 SM"
)

LABELS_FAIXA_ETARIA <- c(
  "1"  = "Menor de 17",
  "2"  = "17 anos",
  "3"  = "18 anos",
  "4"  = "19 anos",
  "5"  = "20 anos",
  "6"  = "21 anos",
  "7"  = "22 anos",
  "8"  = "23 anos",
  "9"  = "24 anos",
  "10" = "25 anos",
  "11" = "26 a 30",
  "12" = "31 a 35",
  "13" = "36 a 40",
  "14" = "41 a 45",
  "15" = "46 a 50",
  "16" = "51 a 55",
  "17" = "56 a 60",
  "18" = "61 a 65",
  "19" = "66 a 70",
  "20" = "Maior de 70"
)

LABELS_ESTADO_CIVIL <- c(
  "0" = "Não Informado",
  "1" = "Solteiro(a)",
  "2" = "Casado(a)",
  "3" = "Divorciado(a)",
  "4" = "Viúvo(a)"
)

LABELS_CELULAR <- c(
  "A" = "Não",
  "B" = "Um",
  "C" = "Dois",
  "D" = "Três",
  "E" = "Quatro ou mais"
)

LABELS_COMPUTADOR <- c(
  "A" = "Não",
  "B" = "Um",
  "C" = "Dois",
  "D" = "Três",
  "E" = "Quatro ou mais"
)

LABELS_LINGUA <- c(
  "0" = "Inglês",
  "1" = "Espanhol"
)

# Função auxiliar: aplica o dicionário de rótulos a uma coluna, preservando NA
# para qualquer código que não conste no dicionário (em vez de virar "NA").
rotular <- function(coluna, dicionario) {
  unname(dicionario[as.character(coluna)])
}

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
      escolaridade_mae = Q001,
      escolaridade_pai = Q002,
      moradores        = Q005,
      renda_familiar   = Q006,
      celulares        = Q022,
      computadores     = Q024,
      internet         = Q025
    )
  
  # =========================================================
  # 5. APLICAR RÓTULOS ÀS VARIÁVEIS CATEGÓRICAS
  #    E PADRONIZAR O TIPO DE CADA COLUNA
  # =========================================================
  # Padronizar tipos aqui é essencial: cada ano do microdado pode trazer a
  # mesma coluna como integer, double ou character. Fixando o tipo final de
  # cada coluna, o bind_rows()/write_parquet() das bases não quebra depois.
  # TP_ANO_CONCLUIU é relativo ao ano da prova (ano_corrente), então o
  # dicionário precisa ser montado a cada chamada da função, e não fixo.
  # Conferido com o dicionário oficial do Enem 2023 (ano_corrente = 2023):
  #   0  -> "Não informado"
  #   1  -> 2022 (ano_corrente - 1)
  #   2  -> 2021 (ano_corrente - 2)
  #   ...
  #   16 -> 2007 (ano_corrente - 16)
  #   17 -> "Antes de 2007" (ano_corrente - 16)
  LABELS_ANO_CONCLUIU <- c(
    "0" = "Não informado",
    setNames(as.character(ano_corrente - (1:16)), as.character(1:16)),
    "17" = paste("Antes de", ano_corrente - 16)
  )

  dados_tratados <- dados_tratados %>%
    mutate(
      # --- identificadores e numéricas "puras" ---
      NU_INSCRICAO     = as.character(NU_INSCRICAO),
      ANO              = as.integer(ANO),
      estado_prova     = as.character(estado_prova),
      
      # --- notas: sempre double, com 1 casa decimal como em "desempenho" ---
      NU_NOTA_MT       = as.numeric(NU_NOTA_MT),
      NU_NOTA_CN       = as.numeric(NU_NOTA_CN),
      NU_NOTA_CH       = as.numeric(NU_NOTA_CH),
      NU_NOTA_LC       = as.numeric(NU_NOTA_LC),
      NU_NOTA_REDACAO  = as.numeric(NU_NOTA_REDACAO),
      NU_NOTA_COMP1    = as.numeric(NU_NOTA_COMP1),
      NU_NOTA_COMP2    = as.numeric(NU_NOTA_COMP2),
      NU_NOTA_COMP3    = as.numeric(NU_NOTA_COMP3),
      NU_NOTA_COMP4    = as.numeric(NU_NOTA_COMP4),
      NU_NOTA_COMP5    = as.numeric(NU_NOTA_COMP5),
      desempenho       = as.numeric(desempenho),

      # --- categóricas: recodificadas para rótulo (texto) e fixadas como character ---
      TP_SEXO          = rotular(TP_SEXO, LABELS_SEXO),
      TP_COR_RACA      = rotular(TP_COR_RACA, LABELS_COR_RACA),
      TP_ESTADO_CIVIL  = rotular(TP_ESTADO_CIVIL, LABELS_ESTADO_CIVIL),
      TP_FAIXA_ETARIA  = rotular(TP_FAIXA_ETARIA, LABELS_FAIXA_ETARIA),
      TP_ST_CONCLUSAO  = rotular(TP_ST_CONCLUSAO, LABELS_CONCLUSAO),
      TP_ANO_CONCLUIU  = rotular(TP_ANO_CONCLUIU, LABELS_ANO_CONCLUIU),
      tipo_escola      = rotular(tipo_escola, LABELS_ESCOLA),
      dep_adm          = rotular(dep_adm, LABELS_DEP_ADM),
      IN_TREINEIRO     = rotular(IN_TREINEIRO, LABELS_TREINEIRO),
      escolaridade_mae = rotular(escolaridade_mae, LABELS_ESCOLARIDADE_MAE),
      escolaridade_pai = rotular(escolaridade_pai, LABELS_ESCOLARIDADE_PAI),
      renda_familiar   = rotular(renda_familiar, LABELS_RENDA),
      internet         = rotular(internet, LABELS_INTERNET),
      TP_LINGUA        = rotular(TP_LINGUA, LABELS_LINGUA),
      celulares        = rotular(celulares, LABELS_CELULAR), # Q022 (A-E) - sem rótulo solicitado
      computadores     = rotular(computadores, LABELS_COMPUTADOR), # Q024 (A-E) - sem rótulo solicitado
      
    )

  # =========================================================
  # 6. SELEÇÃO FINAL DAS COLUNAS NECESSÁRIAS
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
      escolaridade_pai,
      renda_familiar,
      moradores,
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
write_csv(dados_filtrado_2023, "DOCUMENTOS_ENEM/DADOS/ENEM_2023_categorias.csv")
#write_parquet(dados_filtrado_2023, "DOCUMENTOS_ENEM/DADOS/ENEM_2023.parquet")
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
write_csv(dados_filtrado_2022, "DOCUMENTOS_ENEM/DADOS/ENEM_2022_categorias.csv")
#write_parquet(dados_filtrado_2022, "DOCUMENTOS_ENEM/DADOS/ENEM_2022.parquet")

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
write_csv(dados_filtrado_2021, "DOCUMENTOS_ENEM/DADOS/ENEM_2021_categorias.csv")
#write_parquet(dados_filtrado_2021, "DOCUMENTOS_ENEM/DADOS/ENEM_2021.parquet")

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
write_csv(dados_filtrado_2020, "DOCUMENTOS_ENEM/DADOS/ENEM_2020_categorias.csv")

#write_parquet(dados_filtrado_2020, "DOCUMENTOS_ENEM/DADOS/ENEM_2020.parquet")

#===========  2019 =============================================================
# 1. Importa a base bruta (usando read_delim do readr que é mais rápida)
dados_2019 <- read_delim(
  "DOCUMENTOS_ENEM/DADOS/MICRODADOS_ENEM_2019.csv", 
  delim = ";", 
  escape_double = FALSE, 
  trim_ws = TRUE,
  show_col_types = FALSE
)

#0 2. Executa a função
dados_filtrado_2019 <- limpar_base_enem(dados_2019, ano_corrente = 2019)

# 3. Salva o resultado de forma otimizada (sem aspas desnecessárias nas strings)
write_csv(dados_filtrado_2019, "DOCUMENTOS_ENEM/DADOS/ENEM_2019_categorias.csv")
#write_parquet(dados_filtrado_2019, "DOCUMENTOS_ENEM/DADOS/ENEM_2019.parquet")
