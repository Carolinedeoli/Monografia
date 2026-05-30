# ==============================================================================
# SCRIPT EM R: MODELAGEM COM ANO INCLUÍDO NA DIMENSÃO DE PERFIL
# ==============================================================================

library(dplyr)
library(readr)

caminho_dados <- "DOCUMENTOS_ENEM/DADOS/"
anos <- c(2022, 2023)

lista_bases_tratadas <- list()
lista_perfis          <- list()

# ==============================================================================
# PASSO 1: LER AS BASES TRATADAS E ISOLAR PERFIS + ANO
# ==============================================================================
cat("=== PASSO 1: Lendo bases tratadas e isolando perfis com Ano ===\n")

for (ano in anos) {
  arquivo_tratado <- paste0(caminho_dados, "ENEM_", ano, ".csv")
  
  if (file.exists(arquivo_tratado)) {
    cat("Lendo arquivo já tratado de:", ano, "...\n")
    
    # Lendo o arquivo (ajuste para delim = ";" se o seu arquivo usar ponto e vírgula)
    base_ano <- read_csv(arquivo_tratado, show_col_types = FALSE)
    
    lista_bases_tratadas[[as.character(ano)]] <- base_ano
    
    # 🌟 O ANO entra aqui junto com as colunas categóricas de perfil
    perfil_ano <- base_ano |>
      select(
        ANO,
        estado_prova, TP_FAIXA_ETARIA, TP_SEXO, TP_COR_RACA, TP_ST_CONCLUSAO,
        tipo_escola, IN_TREINEIRO, dep_adm, escolaridade_mae, renda_familiar,
        internet, celulares, computadores
      ) |>
      mutate(across(everything(), as.character)) 
    
    lista_perfis[[as.character(ano)]] <- perfil_ano
    cat("✅ Perfis e Ano mapeados para", ano, ".\n")
  } else {
    cat("⚠️ Atenção: Arquivo tratado não encontrado:", arquivo_tratado, "\n")
  }
}

# ==============================================================================
# PASSO 2: UNIFICAR, AGRUPAR POR ANO E PERFIL, E GERAR O IDENTIFICADOR (ID)
# ==============================================================================
cat("\n=== PASSO 2: Criando a base unificada DIM_PERFIL_UNICO com coluna ANO ===\n")

df_perfil_unico <- bind_rows(lista_perfis) |>
  # 🌟 O ANO foi adicionado ao group_by para que a contagem seja separada por ano
  group_by(
    ANO,
    estado_prova, TP_FAIXA_ETARIA, TP_SEXO, TP_COR_RACA, TP_ST_CONCLUSAO,
    tipo_escola, IN_TREINEIRO, dep_adm, escolaridade_mae, renda_familiar,
    internet, celulares, computadores
  ) |>
  summarise(TOTAL_PEOPLE = n(), .groups = "drop") |>
  # Gera a chave primária única para cada combinação de Perfil + Ano
  mutate(ID_PERFIL = row_number()) |>
  # Organiza a ordem visual das colunas na planilha
  select(ID_PERFIL, ANO, TOTAL_PEOPLE, everything())

# Salva o arquivo de perfis com a coluna de Ano (Alimentará a Página de Perfil)
write_csv(df_perfil_unico, paste0(caminho_dados, "DIM_PERFIL_UNICO.csv"))
cat("✅ Tabela DIM_PERFIL_UNICO.csv exportada com sucesso!\n")


# ==============================================================================
# PASSO 3: GERAR AS BASES FATO DE NOTAS INDIVIDUAIS COM O ID VINCULADO
# ==============================================================================
cat("\n=== PASSO 3: Vinculando identificadores às notas individuais ===\n")

for (ano in anos) {
  base_completa <- lista_bases_tratadas[[as.character(ano)]]
  
  if (!is.null(base_completa)) {
    cat("Cruzando notas do ano:", ano, "...\n")
    
    df_fato_desempenho <- base_completa |>
      mutate(across(c(ANO, estado_prova, TP_FAIXA_ETARIA, TP_SEXO, TP_COR_RACA, TP_ST_CONCLUSAO,
                      tipo_escola, IN_TREINEIRO, dep_adm, escolaridade_mae, renda_familiar,
                      internet, celulares, computadores), as.character)) |>
      # O join agora cruza usando o ANO e as colunas categóricas juntas
      inner_join(df_perfil_unico, by = c(
        "ANO", "estado_prova", "TP_FAIXA_ETARIA", "TP_SEXO", "TP_COR_RACA", "TP_ST_CONCLUSAO",
        "tipo_escola", "IN_TREINEIRO", "dep_adm", "escolaridade_mae", "renda_familiar",
        "internet", "celulares", "computadores"
      )) |>
      select(
        ID_PERFIL,
        ANO,
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
      )
    
    write_csv(df_fato_desempenho, paste0(caminho_dados, "FATO_DESEMPENHO_", ano, ".csv"))
    cat("✅ Arquivo FATO_DESEMPENHO_", ano, ".csv gerado com sucesso!\n", sep="")
  }
}

cat("\n=== MODELAGEM DIMENSIONAL CONCLUÍDA! ===")