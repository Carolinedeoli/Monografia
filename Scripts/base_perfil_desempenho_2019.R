#===============================================================================
#                     FUNÇÃO DE LIMPEZA E TRATAMENTO DO ENEM
#===============================================================================

limpar_base_enem <- function(dados_raw, ano_corrente) {
  
  message(paste(
    "Iniciando a limpeza da base do ENEM de",
    ano_corrente,
    "..."
  ))
  
  # =========================================================
  # LABELS
  # =========================================================
  
  labels_escolaridade <- c(
    "A" = "Nunca estudou",
    "B" = "Não completou EF (até 5º ano)",
    "C" = "EF incompleto (até 9º ano)",
    "D" = "EF completo",
    "E" = "Médio completo",
    "F" = "Superior completo",
    "G" = "Pós-graduação"
  )
  
  labels_renda <- c(
    "A" = "Nenhuma renda",
    "B" = "Até 1 salário mínimo",
    "C" = "De 1 a 1,5 salários mínimos",
    "D" = "De 1,5 a 2 salários mínimos",
    "E" = "De 2 a 2,5 salários mínimos",
    "F" = "De 2,5 a 3 salários mínimos",
    "G" = "De 3 a 4 salários mínimos",
    "H" = "De 4 a 5 salários mínimos",
    "I" = "De 5 a 6 salários mínimos",
    "J" = "De 6 a 7 salários mínimos",
    "K" = "De 7 a 8 salários mínimos",
    "L" = "De 8 a 9 salários mínimos",
    "M" = "De 9 a 10 salários mínimos",
    "N" = "De 10 a 12 salários mínimos",
    "O" = "De 12 a 15 salários mínimos",
    "P" = "De 15 a 20 salários mínimos",
    "Q" = "Acima de 20 salários mínimos"
  )
  
  labels_celular <- c(
    "A" = "Não possui",
    "B" = "1 celular",
    "C" = "2 celulares",
    "D" = "3 celulares",
    "E" = "4 ou mais celulares"
  )
  
  labels_computador <- c(
    "A" = "Não possui",
    "B" = "1 computador",
    "C" = "2 computadores",
    "D" = "3 computadores",
    "E" = "4 ou mais computadores"
  )
  
  labels_internet <- c(
    "A" = "Não",
    "B" = "Sim"
  )
  
  # =========================================================
  # 1. FILTRAGEM INICIAL
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
  # 2. REMOVER NOTAS NULAS/ZERADAS
  # =========================================================
  
  col_notas <- c(
    "NU_NOTA_CN",
    "NU_NOTA_CH",
    "NU_NOTA_LC",
    "NU_NOTA_MT",
    "NU_NOTA_REDACAO"
  )
  
  dados_tratados <- dados_tratados %>%
    filter(
      if_all(
        all_of(col_notas),
        ~ !is.na(.x) & .x > 0
      )
    )
  
  # =========================================================
  # 3. DESEMPENHO MÉDIO
  # =========================================================
  
  dados_tratados <- dados_tratados %>%
    mutate(
      desempenho = round(
        rowSums(across(all_of(col_notas))) / 5,
        1
      )
    )
  
  # =========================================================
  # 4. TRATAMENTO DAS VARIÁVEIS
  # =========================================================
  
  dados_tratados <- dados_tratados %>%
    
    rename(
      ANO = NU_ANO,
      estado_prova = SG_UF_PROVA,
      tipo_escola = TP_ESCOLA,
      dep_adm = TP_DEPENDENCIA_ADM_ESC,
      escolaridade_mae = Q002,
      moradores = Q005,
      renda_familiar = Q006,
      celulares = Q022,
      computadores = Q024,
      internet = Q025
    ) %>%
    
    mutate(
      
      # =====================================================
      # TIPAGEM
      # =====================================================
      
      NU_INSCRICAO = as.character(NU_INSCRICAO),
      
      ANO = as.integer(ANO),
      
      moradores = as.numeric(moradores),
      
      across(
        c(
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
          NU_NOTA_COMP5
        ),
        as.numeric
      ),
      
      # =====================================================
      # CATEGÓRICAS
      # =====================================================
      
      TP_FAIXA_ETARIA = case_when(
        TP_FAIXA_ETARIA == 1  ~ "Menor de 17 anos",
        TP_FAIXA_ETARIA == 2  ~ "17 anos",
        TP_FAIXA_ETARIA == 3  ~ "18 anos",
        TP_FAIXA_ETARIA == 4  ~ "19 anos",
        TP_FAIXA_ETARIA == 5  ~ "20 anos",
        TP_FAIXA_ETARIA == 6  ~ "21 anos",
        TP_FAIXA_ETARIA == 7  ~ "22 anos",
        TP_FAIXA_ETARIA == 8  ~ "23 anos",
        TP_FAIXA_ETARIA == 9  ~ "24 anos",
        TP_FAIXA_ETARIA == 10 ~ "25 anos",
        TP_FAIXA_ETARIA == 11 ~ "Entre 26 e 30 anos",
        TP_FAIXA_ETARIA == 12 ~ "Entre 31 e 35 anos",
        TP_FAIXA_ETARIA == 13 ~ "Entre 36 e 40 anos",
        TP_FAIXA_ETARIA == 14 ~ "Entre 41 e 45 anos",
        TP_FAIXA_ETARIA == 15 ~ "Entre 46 e 50 anos",
        TP_FAIXA_ETARIA == 16 ~ "Entre 51 e 55 anos",
        TP_FAIXA_ETARIA == 17 ~ "Entre 56 e 60 anos",
        TP_FAIXA_ETARIA == 18 ~ "Entre 61 e 65 anos",
        TP_FAIXA_ETARIA == 19 ~ "Entre 66 e 70 anos",
        TP_FAIXA_ETARIA == 20 ~ "Maior de 70 anos",
        TRUE ~ "Não Informado"
      ),
      
      TP_SEXO = case_when(
        TP_SEXO == "M" ~ "Masculino",
        TP_SEXO == "F" ~ "Feminino",
        TRUE ~ "Não Informado"
      ),
      
      TP_ESTADO_CIVIL = case_when(
        TP_ESTADO_CIVIL == 0 ~ "Não Informado",
        TP_ESTADO_CIVIL == 1 ~ "Solteiro(a)",
        TP_ESTADO_CIVIL == 2 ~ "Casado(a)/Companheiro(a)",
        TP_ESTADO_CIVIL == 3 ~ "Divorciado(a)/Separado(a)",
        TP_ESTADO_CIVIL == 4 ~ "Viúvo(a)",
        TRUE ~ "Não Informado"
      ),
      
      TP_COR_RACA = case_when(
        TP_COR_RACA == 0 ~ "Não Informado",
        TP_COR_RACA == 1 ~ "Branca",
        TP_COR_RACA == 2 ~ "Preta",
        TP_COR_RACA == 3 ~ "Parda",
        TP_COR_RACA == 4 ~ "Amarela",
        TP_COR_RACA == 5 ~ "Indígena",
        TP_COR_RACA == 6 ~ "Não Informado",
        TRUE ~ "Não Informado"
      ),
      
      TP_NACIONALIDADE = case_when(
        TP_NACIONALIDADE == 0 ~ "Não Informado",
        TP_NACIONALIDADE == 1 ~ "Brasileiro(a)",
        TP_NACIONALIDADE == 2 ~ "Naturalizado(a)",
        TP_NACIONALIDADE == 3 ~ "Estrangeiro(a)",
        TP_NACIONALIDADE == 4 ~ "Brasileiro(a) nascido(a) no exterior",
        TRUE ~ "Não Informado"
      ),
      
      tipo_escola = case_when(
        tipo_escola == 1 ~ "Não Informado",
        tipo_escola == 2 ~ "Pública",
        tipo_escola == 3 ~ "Privada",
        tipo_escola == 4 ~ "Exterior",
        TRUE ~ "Não Informado"
      ),
      
      dep_adm = case_when(
        dep_adm == 1 ~ "Federal",
        dep_adm == 2 ~ "Estadual",
        dep_adm == 3 ~ "Municipal",
        dep_adm == 4 ~ "Privada",
        TRUE ~ "Não Informado"
      ),
      
      TP_ST_CONCLUSAO = case_when(
        TP_ST_CONCLUSAO == 1 ~
          "Já concluí o Ensino Médio",
        
        TP_ST_CONCLUSAO == 2 ~ paste(
          "Estou cursando e concluirei o Ensino Médio em",
          ano_corrente
        ),
        
        TP_ST_CONCLUSAO == 3 ~ paste(
          "Estou cursando e concluirei o Ensino Médio após",
          ano_corrente
        ),
        
        TP_ST_CONCLUSAO == 4 ~
          "Não concluí e não estou cursando o Ensino Médio",
        
        TRUE ~ "Não Informado"
      ),
      
      IN_TREINEIRO = case_when(
        IN_TREINEIRO == 1 ~ "Sim",
        IN_TREINEIRO == 0 ~ "Não",
        TRUE ~ "Não Informado"
      ),
      
      TP_ANO_CONCLUIU = case_when(
        TP_ANO_CONCLUIU == 0  ~ "Não Informado",
        TP_ANO_CONCLUIU == 1  ~ as.character(2018),
        TP_ANO_CONCLUIU == 2  ~ as.character(2017),
        TP_ANO_CONCLUIU == 3  ~ as.character(2016),
        TP_ANO_CONCLUIU == 4  ~ as.character(2015),
        TP_ANO_CONCLUIU == 5  ~ as.character(2014),
        TP_ANO_CONCLUIU == 6 ~ as.character(2013),
        TP_ANO_CONCLUIU == 7 ~ as.character(2012),
        TP_ANO_CONCLUIU == 8 ~ as.character(2011),
        TP_ANO_CONCLUIU == 9 ~ as.character(2010),
        TP_ANO_CONCLUIU == 10 ~ as.character(2009),
        TP_ANO_CONCLUIU == 11 ~ as.character(2008),
        TP_ANO_CONCLUIU == 12 ~ as.character(2007),
        
        TP_ANO_CONCLUIU == 13 ~ paste(
          "Antes de 2007"
        ),
        
        TRUE ~ "Não Informado"
      ),
      
      escolaridade_mae = case_when(
        escolaridade_mae %in% names(labels_escolaridade) ~
          labels_escolaridade[escolaridade_mae],
        TRUE ~ "Não Informado"
      ),
      
      renda_familiar = case_when(
        renda_familiar %in% names(labels_renda) ~
          labels_renda[renda_familiar],
        TRUE ~ "Não Informado"
      ),
      
      celulares = case_when(
        celulares %in% names(labels_celular) ~
          labels_celular[celulares],
        TRUE ~ "Não Informado"
      ),
      
      computadores = case_when(
        computadores %in% names(labels_computador) ~
          labels_computador[computadores],
        TRUE ~ "Não Informado"
      ),
      
      internet = case_when(
        internet %in% names(labels_internet) ~
          labels_internet[internet],
        TRUE ~ "Não Informado"
      )
    )
  
  # =========================================================
  # 5. BASE FINAL
  # =========================================================
  
  dados_final <- dados_tratados %>%
    select(
      NU_INSCRICAO,
      ANO,
      estado_prova,
      TP_FAIXA_ETARIA,
      TP_SEXO,
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
      computadores
    )
  
  message(paste(
    "Base de",
    ano_corrente,
    "limpa com sucesso!"
  ))
  
  return(dados_final)
}



# 1. Importa a base bruta
dados_2019 <- read_delim("DOCUMENTOS_ENEM/DADOS/MICRODADOS_ENEM_2019.csv", delim = ";", escape_double = FALSE, trim_ws = TRUE)

# 2. Executa a função passando o ano correto
dados_filtrado_2019<- limpar_base_enem(dados_2019, ano_corrente = 2019)

# 3. Salva o resultado
write.csv(dados_filtrado_2019, "DOCUMENTOS_ENEM/DADOS/ENEM_2019.csv", row.names = FALSE)
