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
#                 IMPORTANDO BASE
#===============================================================================
#Microdados 2023
dados_2023 <- read_delim("DOCUMENTOS_ENEM/DADOS/MICRODADOS_ENEM_2023.csv", 
                         delim = ";", escape_double = FALSE, trim_ws = TRUE)

#itens_2023 <- read_delim("DADOS/ITENS_PROVA_2023.csv", 
#                         delim = ";", escape_double = FALSE, trim_ws = TRUE)

#======================================================================================
# DESCRITIVAS COM A BASE COMPLETA
#======================================================================================
#dimensão_completa <- dim(dados_2023)

# #RESUMO BASE -------------------------------------------------------------------
# tabela_resumo <- dados_2023 %>%
#   summarise(
#     `Total de Inscritos (Linhas)` = n(),
#     `Presentes: Ciências da Natureza (CN)` = sum(TP_PRESENCA_CN == 1, na.rm = TRUE),
#     `Presentes: Ciências Humanas (CH)` = sum(TP_PRESENCA_CH == 1, na.rm = TRUE),
#     `Presentes: Linguagens e Códigos (LC)` = sum(TP_PRESENCA_LC == 1, na.rm = TRUE),
#     `Presentes: Matemática (MT)` = sum(TP_PRESENCA_MT == 1, na.rm = TRUE),
#     `Redações válidas` = sum(TP_STATUS_REDACAO == 1, na.rm = TRUE),
#     `Presentes no Dia 1 (CH e LC)` = sum(TP_PRESENCA_CH == 1 & TP_PRESENCA_LC == 1, na.rm = TRUE),
#     `Presentes no Dia 2 (CN e MT)` = sum(TP_PRESENCA_CN == 1 & TP_PRESENCA_MT == 1, na.rm = TRUE),
#     `Redações feitas` = sum(!is.na(TP_STATUS_REDACAO)),
#     `Presentes em Todas as Provas` = sum(
#       TP_PRESENCA_CN == 1 &
#         TP_PRESENCA_CH == 1 &
#         TP_PRESENCA_LC == 1 &
#         TP_PRESENCA_MT == 1,
#       na.rm = TRUE
#     )
#   ) %>%
#   pivot_longer(
#     cols = everything(),
#     names_to = "Métrica",
#     values_to = "Quantidade"
#   ) %>%
#   mutate(
#     Porcentagem = scales::percent(Quantidade / Quantidade[Métrica == "Total de Inscritos (Linhas)"],
#                                   accuracy = 0.1,
#                                   decimal.mark = ",")
#   ) 
# 
# # 2. Chamada da tabela (Em uma nova linha ou bloco)
# knitr::kable(
#   tabela_resumo,
#   align = c("l", "r", "r"),
#   caption = "Resumo de Participação dos Inscritos no ENEM",
#   format.args = list(big.mark = ".", decimal.mark = ","),
#   booktabs = TRUE
# )
# #REDAÇÃO-------------------------------------------------------------------------
# 
# # 1. Criar a contagem por categoria
# tabela_redacao <- dados_2023 %>%
#   group_by(TP_STATUS_REDACAO) %>%
#   summarise(Quantidade = n()) %>%
#   mutate(
#     Descricao = case_when(
#       TP_STATUS_REDACAO == 1 ~ "Sem problemas",
#       TP_STATUS_REDACAO == 2 ~ "Anulada",
#       TP_STATUS_REDACAO == 3 ~ "Cópia Texto Motivador",
#       TP_STATUS_REDACAO == 4 ~ "Em Branco",
#       TP_STATUS_REDACAO == 6 ~ "Fuga ao tema",
#       TP_STATUS_REDACAO == 7 ~ "Não atendimento ao tipo",
#       TP_STATUS_REDACAO == 8 ~ "Texto insuficiente",
#       TP_STATUS_REDACAO == 9 ~ "Parte desconectada",
#       TRUE ~ "Não compareceu / Outros"
#     )
#   ) %>%
#   select(TP_STATUS_REDACAO, Descricao, Quantidade) %>%
#   arrange(TP_STATUS_REDACAO)
# 
# # 2. Adicionar porcentagem para facilitar a análise
# total_com_redacao <- sum(tabela_redacao$Quantidade)
# tabela_redacao <- tabela_redacao %>%
#   mutate(Porcentagem = paste0(round((Quantidade / total_com_redacao) * 100, 2), "%"))
# 
# #NOTAS--------------------------------------------------------------------------
# # 1. Calculando todas as estatísticas 
# tabela_notas_1 <- dados_2023 %>%
#   summarise(across(
#     starts_with("NU_NOTA_"), 
#     list(
#       Mín = ~min(.x, na.rm = TRUE),
#       Máx = ~max(.x, na.rm = TRUE),
#       Média = ~mean(.x, na.rm = TRUE),
#       Mediana = ~median(.x, na.rm = TRUE),
#       DP = ~sd(.x, na.rm = TRUE)
#     ),
#     .names = "{.col}###{.fn}"
#   )) %>%
#   pivot_longer(
#     cols = everything(),
#     names_to = c("Materia", "Estatistica"),
#     names_sep = "###"
#   ) %>%
#   pivot_wider(
#     names_from = Estatistica,
#     values_from = value
#   ) %>%
#   mutate(
#     Materia = gsub("NU_NOTA_", "", Materia),
#     Materia = recode(Materia,
#                      "CN" = "Ciências da Natureza",
#                      "CH" = "Ciências Humanas",
#                      "LC" = "Linguagens e Códigos",
#                      "MT" = "Matemática",
#                      "REDACAO" = "Redação") # Adicionei Redação caso exista na base
#   )

#===============================================================================
#LIMPEZA
#===============================================================================
# Retirando todos os alunos que não estiveram presentes em 
#ambos os dias e tiveram as redações aprovadas

a <- dados_2023 %>%
  filter(
    TP_PRESENCA_CN == 1,
    TP_PRESENCA_CH == 1,
    TP_PRESENCA_LC == 1,
    TP_PRESENCA_MT == 1,
    TP_STATUS_REDACAO == 1
  )

#testandos se eliminou todos
#----
# auxiliar <- b %>%
#   summarise(across(
#     starts_with("NU_NOTA_"),
#     list(
#       Min = ~min(.x, na.rm = TRUE),
#       Max = ~max(.x, na.rm = TRUE),
#       Media = ~mean(.x, na.rm = TRUE),
#       Mediana = ~median(.x, na.rm = TRUE),
#       SD = ~sd(.x, na.rm = TRUE)
#     ),
#     .names = "{.col}###{.fn}" # Usamos um separador único para facilitar o pivot
#   )) %>%
#   # 2. Transformar de "largo" para "longo" (formato de tabela)
#   pivot_longer(
#     cols = everything(),
#     names_to = c("Materia", "Estatistica"),
#     names_sep = "###"
#   ) %>%
#   # 3. Organizar para que as estatísticas virem colunas
#   pivot_wider(
#     names_from = Estatistica,
#     values_from = value
#   ) %>%
#   # Limpar o nome da matéria (remover o prefixo NU_NOTA_)
#   mutate(Materia = gsub("NU_NOTA_", "", Materia))
# # Exibir o resultado
# print(auxiliar)

#Retirando os que possuem nota 0 em alguma prova objetiva ou zerou todas as competências da redação
#----
objetivas <- c("NU_NOTA_CN", "NU_NOTA_CH", "NU_NOTA_LC", "NU_NOTA_MT")
col_notas <- c("NU_NOTA_CN", "NU_NOTA_CH", "NU_NOTA_LC", "NU_NOTA_MT", "NU_NOTA_REDACAO")
b <- a %>%
  filter(if_all(all_of(col_notas), ~ !is.na(.x) & .x > 0))
b <- b %>%
  mutate(desempenho = rowSums(across(all_of(col_notas))) / 5)

dados_enem_presentes <- b
#===============================================================================
#     MANTENDO APENAS COLUNAS ÚTEIS
#===============================================================================

dados_enem_presentes <- dados_enem_presentes %>%
  select(NU_INSCRICAO, NU_ANO, SG_UF_PROVA, TP_FAIXA_ETARIA, TP_SEXO, TP_COR_RACA, TP_ST_CONCLUSAO, 
         TP_ANO_CONCLUIU, TP_ESCOLA, IN_TREINEIRO, TP_DEPENDENCIA_ADM_ESC, 
         NU_NOTA_MT, NU_NOTA_CN, NU_NOTA_CH, NU_NOTA_LC, NU_NOTA_REDACAO, desempenho,
         NU_NOTA_COMP1, NU_NOTA_COMP2, NU_NOTA_COMP3, NU_NOTA_COMP4, NU_NOTA_COMP5,
         Q002,Q006, Q025, Q022, Q024)

#===============================================================================
#Criação de variável desempenho (média simples de todas as notas)

#dados_enem_presentes <- dados_enem_presentes %>%
#  mutate(desempenho = rowSums(across(all_of(col_notas))) / 5)
#===============================================================================

# =====================================================================================
#   AJUSTAR FATORES
# =====================================================================================

dados_enem_presentes$TP_FAIXA_ETARIA <- factor(
  dados_enem_presentes$TP_FAIXA_ETARIA,
  levels = 1:20,
  labels = c(
    'Menor de 17 anos','17 anos','18 anos','19 anos','20 anos','21 anos','22 anos',
    '23 anos','24 anos','25 anos','Entre 26 e 30 anos','Entre 31 e 35 anos',
    'Entre 36 e 40 anos','Entre 41 e 45 anos','Entre 46 e 50 anos','Entre 51 e 55 anos',
    'Entre 56 e 60 anos','Entre 61 e 65 anos','Entre 66 e 70 anos','Maior de 70 anos'
  )
)

dados_enem_presentes$TP_SEXO <- factor(dados_enem_presentes$TP_SEXO, 
                                       levels = c('M','F'), labels=c('Masculino','Feminino'))

dados_enem_presentes$TP_COR_RACA <- factor(
  dados_enem_presentes$TP_COR_RACA,
  levels = c(0,1,2,3,4,5,6),
  labels=c('Não declarado','Branca','Preta','Parda','Amarela','Indígena','Não dispõe')
)

dados_enem_presentes$TP_ESCOLA <- factor(
  dados_enem_presentes$TP_ESCOLA,
  levels = c(1,2,3),
  labels=c('Não respondeu','Pública', 'Privada')
)


dados_enem_presentes$TP_DEPENDENCIA_ADM_ESC <-factor(
  dados_enem_presentes$TP_DEPENDENCIA_ADM_ESC,
  levels = c(1,2,3,4),
  labels = c('Federal','Estadual','Municipal','Privada')
)

dados_enem_presentes$TP_ST_CONCLUSAO <-factor(
  dados_enem_presentes$TP_ST_CONCLUSAO,
  levels = c(1,2,3,4),
  labels = c('Já concluí o Ensino Médio',
             'Estou cursando e concluirei o Ensino Médio em 2023',
             'Estou cursando e concluirei o Ensino Médio após 2023',
             'Não concluí e não estou cursando o Ensino Médio'))

dados_enem_presentes$TP_ANO_CONCLUIU <- factor(
  dados_enem_presentes$TP_ANO_CONCLUIU,
  levels = 0:17,
  labels = c("Não informado","2023","2021","2020","2019","2018","2017","2016",
             "2015","2014","2013","2012","2011","2010","2009","2008","2007",
             "Antes de 2007")
)

dados_enem_presentes$IN_TREINEIRO <- factor(
  dados_enem_presentes$IN_TREINEIRO,
  levels = c(1,0),
  labels=c('Sim','Não')
)

valores_na <- c('Não respondeu', "Não informado", 'Não declarado')
dados_enem_presentes[] <- lapply(dados_enem_presentes, function(x){
  if(is.factor(x)) x <- as.character(x)
  if(is.character(x)) x[x %in% valores_na] <- NA
  x
})


#==========QUESTIONÁRIO 

labels_escolaridade <- c(
  "A" = "Nunca estudou",
  "B" = "Não completou EF (até 5º ano)",
  "C" = "EF incompleto (até 9º ano)",
  "D" = "EF completo",
  "E" = "Médio completo",
  "F" = "Superior completo",
  "G" = "Pós-graduação"
)

dados_enem_presentes <- dados_enem_presentes %>%
  rename(
    escolaridade_mae = Q002
  ) %>%
  mutate(
    escolaridade_mae = na_if(escolaridade_mae, "H"),
    
    
    escolaridade_mae = factor(escolaridade_mae,
                              levels = names(labels_escolaridade),
                              labels = labels_escolaridade)
  )


labels_renda <- c(
  "A" = "Nenhuma renda",
  "B" = "Até R$ 1.320",
  "C" = "R$ 1.320,01 – R$ 1.980",
  "D" = "R$ 1.980,01 – R$ 2.640",
  "E" = "R$ 2.640,01 – R$ 3.300",
  "F" = "R$ 3.300,01 – R$ 3.960",
  "G" = "R$ 3.960,01 – R$ 5.280",
  "H" = "R$ 5.280,01 – R$ 6.600",
  "I" = "R$ 6.600,01 – R$ 7.920",
  "J" = "R$ 7.920,01 – R$ 9.240",
  "K" = "R$ 9.240,01 – R$ 10.560",
  "L" = "R$ 10.560,01 – R$ 11.880",
  "M" = "R$ 11.880,01 – R$ 13.200",
  "N" = "R$ 13.200,01 – R$ 15.840",
  "O" = "R$ 15.840,01 – R$ 19.800",
  "P" = "R$ 19.800,01 – R$ 26.400",
  "Q" = "Acima de R$ 26.400"
)

dados_enem_presentes <- dados_enem_presentes %>%
  rename(renda_familiar = Q006) %>%
  mutate(
    renda_familiar = factor(renda_familiar,
                            levels = names(labels_renda),
                            labels = labels_renda,
                            ordered = TRUE)
  )



labels_celular <- c(
  "A" = "Não possui",
  "B" = "1 celular",
  "C" = "2 celulares",
  "D" = "3 celulares",
  "E" = "4 ou mais celulares"
)

dados_enem_presentes <- dados_enem_presentes %>%
  rename(celulares = Q022) %>%
  mutate(
    celulares = factor(celulares,
                       levels = names(labels_celular),
                       labels = labels_celular,
                       ordered = TRUE)
  )

# Labels computador
labels_computador <- c(
  "A" = "Não possui",
  "B" = "1 computador",
  "C" = "2 computadores",
  "D" = "3 computadores",
  "E" = "4 ou mais computadores"
)

# Labels internet
labels_internet <- c(
  "A" = "Não",
  "B" = "Sim"
)

dados_enem_presentes <- dados_enem_presentes %>%
  rename(
    computadores = Q024,
    internet = Q025
  ) %>%
  mutate(
    computadores = factor(computadores,
                          levels = names(labels_computador),
                          labels = labels_computador,
                          ordered = TRUE),
    
    internet = factor(internet,
                      levels = names(labels_internet),
                      labels = labels_internet)
  )


#===============================================================================
#     MANTENDO APENAS COLUNAS ÚTEIS
#===============================================================================

write.csv(dados_enem_presentes, "ENEM_2023.csv", row.names = FALSE)


enem_2023 <- read_csv("DADOS/ENEM_2023.csv")