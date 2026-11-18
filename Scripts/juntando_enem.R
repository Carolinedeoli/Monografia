

#-------------------------------------------------------------------------------

#      CRUZAMENTO SISU (UFES) x ENEM POR NOTAS + BASE UNIFICADA (TODOS OS ANOS)

#-------------------------------------------------------------------------------

library(readr)
library(dplyr)
library(purrr)
library(tidyr)

#-------------------------------------------------------------------------------
base <- "DOCUMENTOS_ENEM/DADOS"

# Anos de ENEM disponíveis (arquivo ENEM_<ano>.csv já tratado/categorizado).
# O SISU correspondente é sempre ano_enem + 1 (quem fez o Enem 2019 entrou
# pelo Sisu em 2020, e assim por diante).
ANOS_ENEM <- c(2019, 2020, 2021, 2022, 2023)

num       <- function(x) as.numeric(sub(",", ".", as.character(x)))
norm_matr <- function(x) toupper(gsub("[^0-9A-Za-z]", "", x))
# Chave das 5 notas: arredonda a 1 casa e vira inteiro
k <- function(cn, ch, lc, mt, red) paste(round(num(cn) * 10), round(num(ch) * 10), round(num(lc) * 10),
                                          round(num(mt) * 10), round(num(red) * 10), sep = "_")

# Reverte "dupla decodificação": um texto que já era UTF-8 e foi lido como
# se fosse latin1 (gera "NÃ£o", ou caracteres de controle no cabeçalho).
corrigir_mojibake <- function(x) {
  x <- iconv(x, from = "UTF-8", to = "latin1")
  Encoding(x) <- "UTF-8"
  x
}

# ---- SISU: UFES inteira, todos os anos (lido uma única vez) ----
# encoding = "latin1": os DADOS desse CSV vêm de um sistema legado em Latin-1
# (sem isso, CURSO/FORMAÇÃO viram "�"). Só o CABEÇALHO do arquivo já é
# UTF-8 — por isso, logo após ler, revertemos a mojibake apenas nos nomes
# das colunas, senão "MATRÍCULA" não é mais encontrada pelo nome.
sisu_completo <- read_delim(file.path(base, "INGRESSANTES_SISU_2018_2026.csv"),
                             delim = ";", show_col_types = FALSE, trim_ws = TRUE,
                             locale = locale(decimal_mark = ",", encoding = "latin1"))
names(sisu_completo) <- corrigir_mojibake(names(sisu_completo))

# Mesma correção, agora nos VALORES de texto (CURSO, FORMAÇÃO, TURNO,
# MODALIDADE...) — eles vieram com a mesma mojibake que o cabeçalho tinha.
sisu_completo <- sisu_completo %>%
  mutate(across(where(is.character), corrigir_mojibake))

sisu_completo <- sisu_completo %>%
  mutate(matricula     = norm_matr(MATRÍCULA),
         nota_faltando = if_any(c(`CIÊNCIAS DA NATUREZA`, `CIÊNCIAS HUMANAS`, LINGUAGENS,
                                   `MATEMÁTICA`, `REDAÇÃO`), ~ is.na(num(.x))),
         key           = k(`CIÊNCIAS DA NATUREZA`, `CIÊNCIAS HUMANAS`, LINGUAGENS, `MATEMÁTICA`, `REDAÇÃO`))

# ---- Função: processa o cruzamento de UM ano de Enem com o Sisu do ano seguinte ----
processar_ano <- function(ano_enem, sisu_completo, base) {

  ano_sisu <- ano_enem + 1
  arq_enem <- paste0("ENEM_", ano_enem, "_categorias.csv")

  sisu <- sisu_completo %>% filter(`ANO INGRESSO` == ano_sisu)

  # Colunas que, no script de categorização, viram texto (rótulo) — se algum
  # ano não tiver passado por essa etapa, elas ainda podem chegar aqui como
  # número. Forçamos todas para character para o bind_rows nunca quebrar.
  cols_texto_enem <- c("TP_SEXO", "TP_COR_RACA", "TP_ESTADO_CIVIL", "TP_FAIXA_ETARIA",
                        "TP_ST_CONCLUSAO", "TP_ANO_CONCLUIU", "tipo_escola", "dep_adm",
                        "IN_TREINEIRO", "escolaridade_mae", "escolaridade_pai", "renda_familiar", "internet",
                        "NU_INSCRICAO", "estado_prova", "celulares", "computadores")

  # Cada pessoa só é buscada na base do Enem do ano correspondente ao seu Sisu
  # O arquivo ENEM_<ano>.csv aqui já é a SAÍDA do script de categorização,
  # escrita em UTF-8 — diferente do microdado bruto do INEP (que é Latin-1).
  # Ler com encoding = "latin1" duplica o encoding e gera "NÃ£o", "PÃºblica" etc.
  enem <- read_delim(file.path(base, arq_enem), delim = ",", show_col_types = FALSE,
                      locale = locale(encoding = "UTF-8"), progress = FALSE,
                      col_types = cols(TP_FAIXA_ETARIA = col_character(),
                                        .default = col_guess())) %>%
    filter(if_all(starts_with("NU_NOTA"), ~ !is.na(.x) & .x > 0)) %>%
    mutate(key = k(NU_NOTA_CN, NU_NOTA_CH, NU_NOTA_LC, NU_NOTA_MT, NU_NOTA_REDACAO),
           across(any_of(cols_texto_enem), as.character))

  # Verifica chaves únicas preservando as colunas do Enem
  enem_unicos <- enem %>%
    add_count(key, name = "n_enem") %>%
    filter(n_enem == 1) # mantém só combinações únicas das 5 notas

  # Cruzamento (mantém UFES + todas as colunas do Enem)
  res <- sisu %>%
    left_join(enem_unicos, by = "key") %>%
    mutate(n_enem   = if_else(nota_faltando, NA_integer_, coalesce(n_enem, 0L)),
           ano_enem = ano_enem,
           ano_sisu = ano_sisu)

  com_nota <- res %>% filter(!nota_faltando)
  dups     <- sisu %>% count(matricula) %>% filter(n > 1)

  metricas <- tibble(
    ano_sisu               = ano_sisu,
    ano_enem               = ano_enem,
    ingressantes_sisu      = nrow(res),
    matriculas_duplicadas  = nrow(dups),
    sem_nota_completa      = sum(res$nota_faltando),
    com_5_notas            = nrow(com_nota),
    match_sucesso          = sum(com_nota$n_enem == 1),
    match_falhou           = sum(com_nota$n_enem != 1)
  )

  list(res = res, metricas = metricas)
}

# ---- Roda o cruzamento para todos os anos de uma vez ----
resultados <- map(ANOS_ENEM, processar_ano, sisu_completo = sisu_completo, base = base)

# ---- Base unificada (todos os anos, colunas do Enem já homogeneizadas) ----
df_ufes_completo <- map_dfr(resultados, "res")

# ---- Verifica duplicação silenciosa pós-join ----
# Se duas pessoas da UFES (mesmo ano) tiverem as 5 notas idênticas, o
# left_join multiplica a linha do lado do Enem para as duas. Como o filtro
# de chave única (enem_unicos) só olha o lado do Enem, essa checagem pega
# duplicação vinda do lado do Sisu, que o restante do script não detecta.
dups_pos_join <- df_ufes_completo %>%
  count(ano_sisu, matricula, name = "n_linhas") %>%
  filter(n_linhas > 1)

if (nrow(dups_pos_join) > 0) {
  cat("\n⚠ ATENÇÃO: matrícula(s) com mais de uma linha após o join (possível chave de notas repetida):\n")
  print(dups_pos_join)
} else {
  cat("\nNenhuma matrícula duplicada após o join — cada pessoa aparece em uma única linha.\n")
}

# ---- Remove duplicatas EXATAS (mesma linha repetida) ----
# Ex.: a mesma pessoa aparecendo 4x com todas as colunas idênticas.
n_antes <- nrow(df_ufes_completo)
df_ufes_completo <- df_ufes_completo %>% distinct()
cat("\nLinhas exatamente duplicadas removidas:", n_antes - nrow(df_ufes_completo), "\n")

# ---- Verifica duplicação silenciosa pós-join (o que sobrar aqui NÃO é
# linha idêntica repetida — é a mesma matrícula com alguma coluna diferente,
# ex.: dois CURSOs distintos para a mesma pessoa/ano — precisa decidir à mão) ----
dups_pos_join <- df_ufes_completo %>%
  count(ano_sisu, matricula, name = "n_linhas") %>%
  filter(n_linhas > 1)

if (nrow(dups_pos_join) > 0) {
  cat("\n⚠ ATENÇÃO: matrícula(s) com mais de uma linha DIFERENTE após o join (decidir manualmente):\n")
  print(
    df_ufes_completo %>%
      semi_join(dups_pos_join, by = c("ano_sisu", "matricula")) %>%
      select(ano_sisu, matricula, CURSO, FORMAÇÃO, TURNO)
  )
} else {
  cat("\nNenhuma matrícula duplicada após o join — cada pessoa aparece em uma única linha.\n")
}

# ---- Resolução manual de conflitos específicos (mesma matrícula, curso
# diferente) — ajuste/repita essa linha para cada caso que aparecer acima.
# Ex.: matrícula 2020102447X tinha 2 cursos; mantendo só a Licenciatura.
df_ufes_completo <- df_ufes_completo %>%
  filter(!(matricula == norm_matr("2020102447X") &
             CURSO != "Ciências Sociais - Licenciatura - Vespertino"))

# Recontagem pós-ajuste manual, para o resumo final refletir a base já corrigida
dups_pos_join <- df_ufes_completo %>%
  count(ano_sisu, matricula, name = "n_linhas") %>%
  filter(n_linhas > 1)

# ---- Tabela de métricas de verificação, ano a ano + total geral ----
# Calculada AQUI, depois do distinct() e do ajuste manual de conflitos —
# reflete a base já limpa, não o resultado bruto do join.
metricas_por_ano <- df_ufes_completo %>%
  group_by(ano_sisu, ano_enem) %>%
  summarise(
    ingressantes_sisu     = n(),
    matriculas_duplicadas = sum(duplicated(matricula) | duplicated(matricula, fromLast = TRUE)),
    sem_nota_completa     = sum(nota_faltando),
    com_5_notas           = sum(!nota_faltando),
    match_sucesso         = sum(!nota_faltando & n_enem == 1),
    match_falhou          = sum(!nota_faltando & n_enem != 1),
    .groups = "drop"
  ) %>%
  mutate(taxa_aproveitamento = round(100 * match_sucesso / ingressantes_sisu, 1))

metricas_total <- metricas_por_ano %>%
  summarise(
    ano_sisu              = NA_integer_,
    ano_enem              = NA_integer_,
    ingressantes_sisu     = sum(ingressantes_sisu),
    matriculas_duplicadas = sum(matriculas_duplicadas),
    sem_nota_completa     = sum(sem_nota_completa),
    com_5_notas           = sum(com_5_notas),
    match_sucesso         = sum(match_sucesso),
    match_falhou          = sum(match_falhou)
  ) %>%
  mutate(taxa_aproveitamento = round(100 * match_sucesso / ingressantes_sisu, 1))

metricas_final <- bind_rows(metricas_por_ano, metricas_total)
print(metricas_final)

#====================================================================================

#      MARCANDO QUEM ENTROU PELO SISU (MODALIDADE) NA BASE UNIFICADA

#====================================================================================

modalidades_sisu <- c(
  "AC", "BRO", "BRP", "BROd", "BRPd", "LB_EP", "LB_PCD", "LB_PPI", "LB_Q",
  "LI_EP", "LI_PCD", "LI_PPI", "NOPT", "RNO", "RNOd", "RNP", "RNPd"
)

df_ufes_limpo <- df_ufes_completo %>%
  mutate(entrada_sisu = if_else(MODALIDADE %in% modalidades_sisu, "Sim", "Não"))


# ---- Resumo geral no console ----
cat("\n==== RESUMO GERAL (todos os anos) ====\n")
cat("Ingressantes SISU (total)        :", metricas_total$ingressantes_sisu, "\n")
cat("  matrículas duplicadas          :", metricas_total$matriculas_duplicadas, "\n")
cat("  com nota faltando (sem casar)  :", metricas_total$sem_nota_completa, "\n")
cat("  com as 5 notas                 :", metricas_total$com_5_notas, "\n")
cat("    - match COM sucesso (ENEM)   :", metricas_total$match_sucesso, "\n")
cat("    - match FALHOU (0 ou >1)     :", metricas_total$match_falhou, "\n")
cat("  matrículas duplicadas pós-join :", nrow(dups_pos_join), "\n")
cat("  matrículas duplicadas pós-join :", nrow(dups_pos_join), "\n")
cat("\nTaxa de aproveitamento (linhas com Sisu + 5 notas + match único no Enem):\n")
cat(" ", metricas_total$taxa_aproveitamento, "% (", metricas_total$match_sucesso, "de", metricas_total$ingressantes_sisu, "linhas)\n")


#=====================================================================================
#            BASE APENAS COM QUEM ENTROU PELO SISU
#=====================================================================================

# ---- Base só com quem deu match nas 5 notas (Sisu + Enem confirmados) ----
# n_enem == 1 já cobre isso sozinho: é NA quando faltava nota no Sisu, e só
# vira 1 quando a chave das 5 notas casou com exatamente 1 registro do Enem.
df_ufes_match <- df_ufes_limpo %>%
  filter(n_enem == 1, entrada_sisu == "Sim")
cat("\nBase filtrada (só match nas 5 notas):", nrow(df_ufes_match), "de", nrow(df_ufes_limpo), "linhas\n")

# ---- Exporta a base unificada final ----
write_csv(df_ufes_limpo, file.path(base, "base_ufes_enem_TODOS_ANOS.csv"))

write_csv(df_ufes_match, file.path(base, "base_consolidada_ufes_enem.csv"))



#======================================================================================

#                  CRIANDO VARIÁVEIS DE CURSO E CAMPI PARA O MODELO FUTURO

#======================================================================================


df_ufes_match <- df_ufes_match %>%
  mutate(
    # ============================================================
    # CAMPUS
    # ============================================================
    CAMPUS = case_when(
      grepl("Alegre", CURSO) ~ "Alegre",
      grepl("São Mateus", CURSO) ~ "São Mateus",
      
      # Cursos do campus de Maruípe
      CURSO %in% c(
        "Enfermagem e Obstetrícia",
        "Farmácia",
        "Fisioterapia",
        "Fonoaudiologia",
        "Medicina",
        "Nutrição",
        "Odontologia",
        "Terapia Ocupacional - Integral"
      ) ~ "Maruípe",
      
      # Demais cursos de Vitória
      TRUE ~ "Goiabeiras"
    ),
    
    # ============================================================
    # CENTRO
    # ============================================================
    CENTRO = case_when(
      
      # ----------------------------------------------------------
      # CAr - Centro de Artes
      # ----------------------------------------------------------
      CURSO %in% c(
        "Arquitetura e Urbanismo",
        "Artes Plásticas",
        "Artes Visuais - Licenciatura - Diurno",
        "Artes Visuais - Licenciatura - Noturno",
        "Cinema e Audiovisual",
        "Comunicação Social - Habilitação em Jornalismo",
        "Comunicação Social - Habilitação em Publicidade e Propaganda",
        "Design",
        "Jornalismo",
        "Música - Bacharelado",
        "Música - Licenciatura"
      ) ~ "CAr",
      
      # ----------------------------------------------------------
      # CCAE - Centro de Ciências Agrárias e Engenharias
      # Alegre
      # ----------------------------------------------------------
      CURSO %in% c(
        "Agronomia - Alegre",
        "Engenharia de Alimentos - Alegre",
        "Engenharia Florestal - Alegre",
        "Engenharia Industrial Madeireira - Alegre",
        "Engenharia Química - Bacharelado - Alegre",
        "Medicina Veterinária - Alegre",
        "Zootecnia - Alegre"
      ) ~ "CCAE",
      
      # ----------------------------------------------------------
      # CCENS - Centro de Ciências Exatas, Naturais e da Saúde
      # Alegre
      # ----------------------------------------------------------
      CURSO %in% c(
        "Ciência da Computação - Bacharelado - Alegre",
        "Ciências Biológicas - Bacharelado - Alegre",
        "Ciências Biológicas - Licenciatura - Alegre",
        "Farmácia - Bacharelado - Alegre",
        "Física - Licenciatura - Alegre",
        "Geologia - Alegre",
        "Matemática - Licenciatura - Alegre",
        "Nutrição - Alegre",
        "Química - Licenciatura - Alegre",
        "Sistemas de Informação - Bacharelado - Alegre"
      ) ~ "CCENS",
      
      # ----------------------------------------------------------
      # CCE - Centro de Ciências Exatas
      # Goiabeiras
      # ----------------------------------------------------------
      CURSO %in% c(
        "Estatística",
        "Estatística e Ciência de Dados",
        "Física - Bacharelado",
        "Física - Licenciatura",
        "Matemática - Bacharelado",
        "Matemática - Licenciatura",
        "Química - Bacharelado",
        "Química - Licenciatura"
      ) ~ "CCE",
      
      # ----------------------------------------------------------
      # CCHN - Centro de Ciências Humanas e Naturais
      # Goiabeiras
      # ----------------------------------------------------------
      CURSO %in% c(
        "Ciências Biológicas",
        "Ciências Biológicas - Bacharelado",
        "Ciências Biológicas - Licenciatura",
        "Ciências Sociais - Bacharelado - Vespertino",
        "Ciências Sociais - Licenciatura - Vespertino",
        "Ciências Sociais - Vespertino",
        "Ciências Sociais - Bacharelado - Noturno",
        "Ciências Sociais - Licenciatura - Noturno",
        "Ciências Sociais - Noturno",
        "Filosofia - Bacharelado - Noturno",
        "Filosofia - Licenciatura - Noturno",
        "Geografia - Bacharelado - Matutino",
        "Geografia - Bacharelado - Noturno",
        "Geografia - Licenciatura - Matutino",
        "Geografia - Licenciatura - Noturno",
        "História - Licenciatura - Vespertino",
        "História - Licenciatura - Noturno",
        "Lic. em Língua Port. e Literat. de Língua Port. - Matutino",
        "Lic. em Língua Port. e Literat. de Língua Port. - Noturno",
        "Licenciatura Dupla em Português e Espanhol",
        "Língua e Literatura Inglesa - Licenciatura",
        "Oceanografia",
        "Psicologia"
      ) ~ "CCHN",
      
      # ----------------------------------------------------------
      # CCJE - Centro de Ciências Jurídicas e Econômicas
      # Goiabeiras
      # ----------------------------------------------------------
      CURSO %in% c(
        "Administração - Diurno",
        "Administração - Noturno",
        "Arquivologia - Noturno",
        "Biblioteconomia",
        "Ciências Contábeis",
        "Ciências Contábeis - Vespertino",
        "Ciências Econômicas - Bacharelado",
        "Direito",
        "Gemologia",
        "Serviço Social"
      ) ~ "CCJE",
      
      # ----------------------------------------------------------
      # CCS - Centro de Ciências da Saúde
      # Maruípe
      # ----------------------------------------------------------
      CURSO %in% c(
        "Enfermagem e Obstetrícia",
        "Farmácia",
        "Fisioterapia",
        "Fonoaudiologia",
        "Medicina",
        "Nutrição",
        "Odontologia",
        "Terapia Ocupacional - Integral"
      ) ~ "CCS",
      
      # ----------------------------------------------------------
      # CE - Centro de Educação
      # Goiabeiras
      # ----------------------------------------------------------
      CURSO %in% c(
        "Pedagogia - Licenciatura - Matutino",
        "Pedagogia - Licenciatura - Noturno"
      ) ~ "CE",
      
      # ----------------------------------------------------------
      # CEFD - Centro de Educação Física e Desportos
      # Goiabeiras
      # ----------------------------------------------------------
      CURSO %in% c(
        "Educação Física - Bacharelado",
        "Educação Física - Licenciatura"
      ) ~ "CEFD",
      
      # ----------------------------------------------------------
      # CT - Centro Tecnológico
      # Goiabeiras
      # ----------------------------------------------------------
      CURSO %in% c(
        "Ciência da Computação",
        "Engenharia Ambiental",
        "Engenharia Civil",
        "Engenharia de Computação",
        "Engenharia de Produção (Noturno)",
        "Engenharia de Produção - Vespertino",
        "Engenharia Elétrica",
        "Engenharia Mecânica"
      ) ~ "CT",
      
      # ----------------------------------------------------------
      # CEUNES - Centro Universitário Norte do Espírito Santo
      # São Mateus
      # ----------------------------------------------------------
      CURSO %in% c(
        "Agronomia - São Mateus",
        "Ciência da Computação - São Mateus",
        "Ciências Biológicas - Bacharelado - São Mateus",
        "Ciências Biológicas - Licenciatura - São Mateus",
        "Enfermagem - São Mateus",
        "Engenharia da Computação - São Mateus",
        "Engenharia de Petróleo - São Mateus",
        "Engenharia de Produção - São Mateus",
        "Engenharia Química - São Mateus",
        "Farmácia - São Mateus",
        "Física - Licenciatura - São Mateus",
        "Matemática - Licenciatura - São Mateus",
        "Matemática Industrial - São Mateus",
        "Pedagogia - São Mateus",
        "Química - Licenciatura - São Mateus"
      ) ~ "CEUNES",
      
      TRUE ~ "Não identificado"
    )
  )

df_ufes_match <- df_ufes_match %>%
  mutate(
    DEP_ADM_AGRUPADA = case_when(
      dep_adm %in% c("Estadual", "Municipal") ~ "Estadual/Municipal",
      dep_adm == "Federal"                   ~ "Federal",
      dep_adm == "Privada"                   ~ "Privada",
      TRUE                                   ~ "Não informado"
    )
  )

df_ufes_match <- df_ufes_match %>%
  mutate(
    MODALIDADE_PADRONIZADA = case_when(
      
      # Ampla concorrência
      MODALIDADE %in% c("NOPT", "AC") ~ "AC",
      
      # Escola pública - independente de renda
      MODALIDADE %in% c("RNP", "LI_EP") ~ "LI_EP",
      
      # Escola pública - baixa renda
      MODALIDADE %in% c("BRP", "LB_EP") ~ "LB_EP",
      
      # Escola pública + PPI - independente de renda
      MODALIDADE %in% c("RNO", "LI_PPI") ~ "LI_PPI",
      
      # Escola pública + PPI - baixa renda
      MODALIDADE %in% c("BRO", "LB_PPI") ~ "LB_PPI",
      
      # Escola pública + PcD - independente de renda
      MODALIDADE %in% c("RNPd", "LI_PCD") ~ "LI_PCD",
      
      # Escola pública + PcD - baixa renda
      MODALIDADE %in% c("BRPd", "LB_PCD") ~ "LB_PCD",
      
      # Modalidades sem correspondência direta na nova estrutura
      MODALIDADE == "BROd" ~ "BROd",
      
      # Quilombola - modalidade nova
      MODALIDADE == "LB_Q" ~ "LB_Q",
      
      TRUE ~ NA_character_
    )
  )

ORDEM_FAIXA_ETARIA_AGRUPADA <- c(
  "Menor de idade",
  "18 anos",
  "19 anos",
  "20 a 25 anos",
  "26 a 50 anos",
  "Acima de 50 anos"
)

df_ufes_match <- df_ufes_match %>%
  mutate(
    FAIXA_ETARIA_AGRUPADA = case_when(
      TP_FAIXA_ETARIA %in% c("Menor de 17", "17 anos") ~ "Menor de idade",
      
      TP_FAIXA_ETARIA == "18 anos" ~ "18 anos",
      
      TP_FAIXA_ETARIA == "19 anos" ~ "19 anos",
      
      TP_FAIXA_ETARIA %in% paste0(20:25, " anos") ~ "20 a 25 anos",
      
      TP_FAIXA_ETARIA %in% c(
        "26 a 30", "31 a 35", "36 a 40",
        "41 a 45", "46 a 50"
      ) ~ "26 a 50 anos",
      
      TP_FAIXA_ETARIA %in% c(
        "51 a 55", "56 a 60", "61 a 65",
        "66 a 70", "Maior de 70"
      ) ~ "Acima de 50 anos",
      
      TRUE ~ NA_character_
    ),
    
    FAIXA_ETARIA_AGRUPADA = factor(
      FAIXA_ETARIA_AGRUPADA,
      levels = ORDEM_FAIXA_ETARIA_AGRUPADA
    )
  )

# ---- Exporta a base unificada final ----
write_csv(df_ufes_match, file.path(base, "base_consolidada_ufes_enem.csv"))


#----- Eliminando duplicadas ----------------
df_x <- df_ufes_match %>%
  filter(
    str_detect(
      str_trim(as.character(`MATRÍCULA`)),
      regex("X$", ignore_case = TRUE)
    )
  )
nrow(df_x)
nrow(df_ufes_match)

df_limpo <- df_ufes_match %>%
  filter(
    !str_detect(
      str_trim(as.character(`MATRÍCULA`)),
      regex("X$", ignore_case = TRUE)
    )
  )
nrow(df_limpo)

df_0 <- df_limpo %>%
  filter(
    str_detect(
      str_trim(as.character(`MATRÍCULA`)),
      "^0"
    )
  )

nrow(df_0)


df_limpo <- df_limpo %>%
  filter(
    is.na(`MATRÍCULA`) |
      !str_detect(
        str_trim(as.character(`MATRÍCULA`)),
        "^0"
      )
  )
nrow(df_limpo)




#==========
#Tirando duplicadas iniciadas em 0

df_limpo <- df_limpo %>%
  group_by(NOME, `ANO INGRESSO`, SEMESTRE) %>%
  mutate(
    duplicado_0 = !is.na(NOME) &
      n() > 1 &
      n_distinct(`MATRÍCULA`, na.rm = TRUE) > 1 &
      any(
        str_detect(
          str_trim(as.character(`MATRÍCULA`)),
          "^0"
        ),
        na.rm = TRUE
      ),
    
    matricula_0 = str_detect(
      str_trim(as.character(`MATRÍCULA`)),
      "^0"
    )
  ) %>%
  filter(
    !duplicado_0 | coalesce(matricula_0, FALSE)
  ) %>%
  ungroup() %>%
  select(-duplicado_0, -matricula_0)


cat("Registros originais:", nrow(df_ufes_match), "\n")
cat("Registros após limpeza:", nrow(df_limpo), "\n")
cat("Registros removidos:", nrow(df_ufes_match)- nrow(df_limpo), "\n")
cat("Registros duplicados:", nrow(df_x)+ nrow(df_0), "\n")

df_limpo <- df_limpo %>%
  select(-`CIÊNCIAS HUMANAS`, -`CIÊNCIAS DA NATUREZA`, -`MATEMÁTICA`, -`LINGUAGENS`, -`REDAÇÃO`,-`matricula`)


#===============================================================================

#criando renda mensal agrupada de acordo com o ibge
#https://www.ibge.gov.br/estatisticas/sociais/populacao/17374-indica-dores-sociais-minimos.html?utm_source=chatgpt.com&t=resultados

df_limpo <- read_csv(file.path(base, "base_LIMPA_ufes_sisu.csv"), show_col_types = FALSE)


df_limpo <- df_limpo %>%
  mutate(
    renda_agrupada = case_when(
      renda_familiar == "Sem renda" ~ "Sem renda",
      
      renda_familiar %in% c(
        "Até 1 SM",
        "De 1 a 1.5 SM",
        "De 1.5 a 2 SM"
      ) ~ "Até 2 SM",
      
      renda_familiar %in% c(
        "De 2 a 2.5 SM",
        "De 2.5 a 3 SM",
        "De 3 a 4 SM",
        "De 4 a 5 SM"
      ) ~ "Mais de 2 a 5 SM",
      
      renda_familiar %in% c(
        "De 5 a 6 SM",
        "De 6 a 7 SM",
        "De 7 a 8 SM",
        "De 8 a 9 SM",
        "De 9 a 10 SM"
      ) ~ "Mais de 5 a 10 SM",
      
      renda_familiar %in% c(
        "De 10 a 12 SM",
        "De 12 a 15 SM",
        "De 15 a 20 SM"
      ) ~ "Mais de 10 a 20 SM",
      
      renda_familiar == "Acima de 20 SM" ~ "Mais de 20 SM",
      
      TRUE ~ NA_character_
    ),
    
    renda_agrupada = factor(
      renda_agrupada,
      levels = c(
        "Sem renda",
        "Até 2 SM",
        "Mais de 2 a 5 SM",
        "Mais de 5 a 10 SM",
        "Mais de 10 a 20 SM",
        "Mais de 20 SM"
      ),
      ordered = TRUE
    )
  )

# Conferir correspondência entre categorias
table(
  df_limpo$renda_familiar,
  df_limpo$renda_agrupada,
  useNA = "ifany"
)

# Distribuição percentual
df_limpo %>%
  count(renda_agrupada) %>%
  mutate(
    percentual = round(100 * n / sum(n), 2)
  )


#===============================================================================
#                MODALIDADE AGRUPADA

df_limpo <- df_limpo %>%
  mutate(
    modalidade_agrupada = case_when(
      MODALIDADE_PADRONIZADA == "AC" ~ "AC",
      
      MODALIDADE_PADRONIZADA == "LI_EP" ~ "LI_EP",
      
      MODALIDADE_PADRONIZADA == "LB_EP" ~ "LB_EP",
      
      MODALIDADE_PADRONIZADA %in% c(
        "LI_PPI", "LI_PCD"
      ) ~ "LI_SOCIAL",
      
      MODALIDADE_PADRONIZADA %in% c(
        "LB_PPI", "LB_PCD", "LB_Q", "BROd"
      ) ~ "LB_SOCIAL",
      
      TRUE ~ NA_character_
    ),
    
    modalidade_agrupada = factor(
      modalidade_agrupada,
      levels = c(
        "AC",
        "LI_EP",
        "LB_EP",
        "LI_SOCIAL",
        "LB_SOCIAL"
      )
    )
  )

# Conferir a recodificação
df_limpo %>%
  count(MODALIDADE_PADRONIZADA, modalidade_agrupada)

# Distribuição das cinco categorias
df_limpo %>%
  count(modalidade_agrupada) %>%
  mutate(
    percentual = round(100 * n / sum(n), 2)
  )
# ---- Exporta a base unificada final ----
write_csv(df_limpo, file.path(base, "base_LIMPA_ufes_sisu.csv"))
