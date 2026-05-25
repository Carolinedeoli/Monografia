#==========================================================
#DESCRITIVAS COM BASE LIMPA
#==========================================================

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
#REFAZENDO AS ANÁLISES COM BASE DE PARTICIPANTES LIMPA
#===============================================================================
enem_2023 <- read_csv("DADOS/ENEM_2023.csv")

dim(enem_2023)

library(dplyr)

tabela <- enem_2023 %>%
  group_by(TP_ESCOLA, TP_DEPENDENCIA_ADM_ESC) %>%
  summarise(n = n(), .groups = "drop")


tabela

#Total participantes: 2.569.190
#notas----


#-----
hist(enem_2023$desempenho)
ad.test(enem_2023$desempenho)
#================================
#   DESCRITIVAS
#================================

table(enem_2023$TP_DEPENDENCIA_ADM_ESC)
table(enem_2023$TP_SEXO)


summary(enem_2023)

tab_frequencia <- function(dados, variavel, titulo = "Distribuição da Variável") {
  
  dados %>%
    count(.data[[variavel]]) %>%
    mutate(
      Proporção = n / sum(n),
      Proporção_Txt = scales::percent(Proporção, accuracy = 0.1, decimal.mark = ",")
    ) %>%
    rename(Categoria = all_of(variavel), Frequência = n) %>%
    select(Categoria, Frequência, Proporção = Proporção_Txt) %>%
    kable(
      caption = titulo,
      align = "lrr",
      booktabs = TRUE,
      format.args = list(big.mark = ".", decimal.mark = ",")
    ) %>%
    kable_styling(
      latex_options = c("striped", "hold_position"),
      full_width = FALSE
    )
}

table(enem_2023$TP_SEXO)
# Tabelas de Frequência formatadas para o PDF
tab_frequencia(enem_2023, "TP_SEXO", "Distribuição por Sexo")
tab_frequencia(enem_2023, "TP_FAIXA_ETARIA", "Distribuição por Faixa Etária")
tab_frequencia(enem_2023, "TP_COR_RACA", "Distribuição por Cor/Raça")
tab_frequencia(enem_2023, "TP_ESCOLA", "Distribuição por Tipo de Escola")
tab_frequencia(enem_2023, "TP_DEPENDENCIA_ADM_ESC", "Distribuição por Dependência Administrativa")
tab_frequencia(enem_2023, "IN_TREINEIRO", "Distribuição de Treineiros")
tab_frequencia(enem_2023, "TP_ANO_CONCLUIU", "Distribuição por Ano de Conclusão")
tab_frequencia(enem_2023, "TP_ST_CONCLUSAO", "Situação de Conclusão do Ensino Médio")

#------------------------------
#DATAFRAME DE DADOS FALTANTES
#------------------------------

data.frame(
  Variável = names(enem_2023),
  NAs = colSums(is.na(enem_2023)),
  Percentual = colMeans(is.na(enem_2023))
) %>%
  mutate(
    Percentual = percent(Percentual, accuracy = 0.1, decimal.mark = ",")
  ) %>%
  arrange(desc(NAs)) %>%  # 🔥 ordena pelas mais problemáticas
  kable(
    align = c("l", "r", "r"),
    col.names = c("Variável", "Quantidade de NAs", "Percentual"),
    caption = "Valores Ausentes por Variável",
    format.args = list(big.mark = ".", decimal.mark = ","),
    booktabs = TRUE
  ) %>%
  kable_styling(
    latex_options = c("striped", "hold_position", "scale_down"),
    full_width = FALSE,
    font_size = 9
  ) %>%
  column_spec(1, width = "6cm")


#---------------------------
#Gráficos de barra do perfil
#---------------------------
{grafico_barra <- function(data, var, titulo){
  
  data %>%
    count(!!sym(var)) %>%
    mutate(pct = n / sum(n) * 100) %>%
    ggplot(aes(x = !!sym(var), n), y = n) +
    geom_col(fill = "#4C72B0") +
    geom_text(aes(label = sprintf("%.1f%%", pct)), 
              hjust = -0.1, size = 3.5) +
    coord_flip() +
    labs(title = titulo, x = "", y = "Frequência") +
    theme_minimal(base_size = 12)
}}
  

  
  grafico_barra(enem_2023, "TP_FAIXA_ETARIA", "Faixa Etária dos Participantes")
  grafico_barra(enem_2023, "TP_ANO_CONCLUIU", "Ano que concluiu o Ensino Médio")
  grafico_barra(enem_2023, "TP_COR_RACA", "Cor/Raça Declarada")
  grafico_barra(enem_2023, "TP_DEPENDENCIA_ADM_ESC", "Tipo de Dependência da Escola")
  grafico_barra(enem_2023, "TP_ST_CONCLUSAO", "Tipo de situação de Conclusão do EM")


# -------------------------------
#   HISTOGRAMAS DAS NOTAS
# -------------------------------
{grafico_histograma <- function(data, var, titulo){
    ggplot(data, aes(x = !!sym(var))) +
      geom_histogram(bins = 30, fill = "#4C72B0", color = "white") +
      labs(title = titulo, x = "Nota", y = "Frequência") +
      theme_minimal(base_size = 12)
}}
  
  grafico_histograma(enem_2023, "NU_NOTA_CN", "Distribuição da Nota de Ciências da Natureza")
  grafico_histograma(enem_2023, "NU_NOTA_CH", "Distribuição da Nota de Ciências Humanas")
  grafico_histograma(enem_2023, "NU_NOTA_LC", "Distribuição da Nota de Linguagens")
  grafico_histograma(enem_2023, "NU_NOTA_MT", "Distribuição da Nota de Matemática")
  grafico_histograma(enem_2023, "desempenho", "Desempenho Médio dos Participantes")

# -------------------------------
#   GRAFICO DE BARRAS COMP REDAÇÃO 
# -------------------------------
{
  grafico_comp_redacao <- function(data, var, titulo){
    
    df <- data %>%
      count(!!sym(var)) %>%
      mutate(pct = n / sum(n) * 100)
    
    ggplot(df, aes(x = factor(!!sym(var)), y = pct)) +
      geom_col(fill = "#4C72B0") +
      geom_text(aes(label = sprintf("%.1f%%", pct)),
                vjust = -0.3, size = 3.5) +
      labs(title = titulo, 
           x = "Nota", 
           y = "Porcentagem") +
      theme_minimal(base_size = 12)
  }}
  
  grafico_comp_redacao(enem_2023, "NU_NOTA_COMP1", "Distribuição — Redação Competência 1")
  grafico_comp_redacao(enem_2023, "NU_NOTA_COMP2", "Distribuição – Redação Competência 2")
  grafico_comp_redacao(enem_2023, "NU_NOTA_COMP3", "Distribuição – Redação Competência 3")
  grafico_comp_redacao(enem_2023, "NU_NOTA_COMP4", "Distribuição – Redação Competência 4")
  grafico_comp_redacao(enem_2023, "NU_NOTA_COMP5", "Distribuição – Redação Competência 5")

#============================================================================
#  BASE ESTADUAL
#============================================================================
dados_limpos_es <- enem_2023 %>% filter(SG_UF_ESC == 'ES')
View(dados_limpos_es)
dados_aux <- enem_2023 %>% filter(SG_UF_PROVA == 'ES')
dim(dados_limpos_es)
dim(dados_aux)
data.frame(
  NAs = colSums(is.na(dados_limpos_es)),
  Percentual = round(100 * colMeans(is.na(dados_limpos_es)), 2)
)




#===============================================================================
#     SOCIAIS E DESEMPENHO 
#===============================================================================

#Desempenho por sexo
#----


#Desempenho por tipo de escola
#desempenho por dependencia
#desempenho se for treineiro
#desempenho por ano que concluiu o ensino médio

tabela_resumo <- function(enem_2023, var_cat, var_num){
  
  tapply(enem_2023[[var_num]], enem_2023[[var_cat]], function(x){
    c(
      N = length(x),
      Media = mean(x, na.rm = TRUE),
      Desvio = sd(x, na.rm = TRUE),
      Min = min(x, na.rm = TRUE),
      Q1 = quantile(x, 0.25, na.rm = TRUE),
      Mediana = median(x, na.rm = TRUE),
      Q3 = quantile(x, 0.75, na.rm = TRUE),
      Max = max(x, na.rm = TRUE)
    )
  })
}
tabela_resumo(enem_2023, "TP_SEXO", "desempenho")
tabela_resumo(enem_2023, "TP_DEPENDENCIA_ADM_ESC", "desempenho")
tabela_resumo(enem_2023, "TP_ESCOLA", "desempenho")


library(dplyr)
library(knitr)
library(kableExtra)

tabela_resumo_profissional <- function(dados, var_cat, var_num, titulo = "") {
  
  library(dplyr)
  library(knitr)
  library(kableExtra)
  
  resumo <- dados %>%
    group_by(categoria = .data[[var_cat]]) %>%   # 🔥 nome limpo
    summarise(
      N = n(),
      Média = mean(.data[[var_num]], na.rm = TRUE),
      DP = sd(.data[[var_num]], na.rm = TRUE),
      Mín = min(.data[[var_num]], na.rm = TRUE),
      Q1 = quantile(.data[[var_num]], 0.25, na.rm = TRUE),
      Mediana = median(.data[[var_num]], na.rm = TRUE),
      Q3 = quantile(.data[[var_num]], 0.75, na.rm = TRUE),
      Máx = max(.data[[var_num]], na.rm = TRUE),
      .groups = "drop"
    )
  
  resumo %>%
    kable(
      digits = 2, 
      format.args = list(big.mark = ".", decimal.mark = ","),
      caption = titulo,
      booktabs = TRUE,
      align = "lrrrrrrrr"   # 🔥 alinhamento correto
    ) %>%
    kable_styling(
      latex_options = c("striped", "hold_position", "scale_down"),
      full_width = FALSE,
      font_size = 8
    ) %>%
    column_spec(1, width = "5cm")   # 🔥 controla texto
}

# Exemplo por Sexo
tabela_resumo_profissional(enem_2023, "TP_SEXO", "desempenho", "Resumo do Desempenho por Sexo")

# Exemplo por DEPENDENCIA de Escola
tabela_resumo_profissional(enem_2023, "TP_DEPENDENCIA_ADM_ESC", "desempenho", "Resumo do Desempenho por Tipo de Escola")

#TIPO DE ESCOLA
tabela_resumo_profissional(enem_2023, "TP_ESCOLA", "desempenho", "Resumo do Desempenho por Tipo de Escola")
tabela_resumo_profissional(enem_2023, "TP_FAIXA_ETARIA", "desempenho", "Resumo do Desempenho por Tipo de Escola")
tabela_resumo_profissional(enem_2023, "TP_ST_CONCLUSAO", "desempenho", "Resumo do Desempenho por Tipo de Escola")
tabela_resumo_profissional(enem_2023, "TP_COR_RACA", "desempenho", "Resumo do Desempenho por Tipo de Escola")

plot_hist_individual <- function(dados, var_cat, var_num,
                                 nome_cat = var_cat,
                                 nome_num = var_num){
  
  library(ggplot2)
  
  categorias <- na.omit(unique(dados[[var_cat]]))
  
  for(cat in categorias){
    
    dados_filtrados <- dados[dados[[var_cat]] == cat, ]
    
    p <- ggplot(dados_filtrados, aes(x = .data[[var_num]])) +
      geom_histogram(bins = 30, fill = "lightblue", color = "black") +
      labs(
        title = paste("Distribuição de", nome_num, "por", nome_cat),
        subtitle = paste("Categoria:", cat),
        x = nome_num,
        y = "Frequência"
      ) +
      theme_minimal()
    
    print(p)
  }
}

plot_hist_individual(enem_2023, "TP_SEXO", "desempenho")
plot_hist_individual(enem_2023, "TP_ESCOLA", "desempenho")
plot_hist_individual(enem_2023, "TP_ST_CONCLUSAO", "desempenho")
plot_hist_individual(enem_2023, "TP_DEPENDENCIA_ADM_ESC", "desempenho")
plot_hist_individual(enem_2023, "TP_COR_RACA", "desempenho")


plot_densidade <- function(dados, var_cat, var_num){
  
  library(ggplot2)
  
  ggplot(dados, aes(x = .data[[var_num]], color = .data[[var_cat]])) +
    geom_density(size = 1) +
    scale_color_discrete(na.translate = FALSE)+
    labs(
      title = paste("Distribuição de", var_num, "por", var_cat),
      x = "Nota",
      y = "Densidade"
    ) +
    
    theme_minimal()
}

plot_densidade(enem_2023, "TP_SEXO", "desempenho")
plot_densidade(enem_2023, "TP_ESCOLA", "desempenho")
plot_densidade(enem_2023, "IN_TREINEIRO", "desempenho")
plot_densidade(enem_2023, "TP_DEPENDENCIA_ADM_ESC", "desempenho")

#==============================================================================
#      ANÁLISE QUESTIONÁRIO SOCIAL
#==============================================================================
table(enem_2023$Q005)



#===============================================================================
tab_frequencia(enem_2023, "escolaridade_pai", "Distribuição por Escolaridade do Pai")
tab_frequencia(enem_2023, "escolaridade_mae", "Distribuição por Escolaridade da Mãe")
tab_frequencia(enem_2023, "renda_familiar", "Distribuição por Renda Familiar")
tab_frequencia(enem_2023, "celulares", "Distribuição por posse de celular na residência")
tab_frequencia(enem_2023, "computadores", "Distribuição por posse de computadores na residência")
tab_frequencia(enem_2023, "internet", "Distribuição por acesso a internet")


#======= DESEMPENHO E QUESTIONÁRIO ======================================
tabela_resumo_profissional(enem_2023, "escolaridade_pai", "desempenho", "Resumo do Desempenho por Escolaridade do Pai")
tabela_resumo_profissional(enem_2023, "escolaridade_mae", "desempenho", "Resumo do Desempenho por Escolaridade da Mãe")
tabela_resumo_profissional(enem_2023, "renda_familiar", "desempenho", "Resumo do Desempenho por Renda Familiar")
tabela_resumo_profissional(enem_2023, "celulares", "desempenho", "Resumo do Desempenho por posse de celular na residência")
tabela_resumo_profissional(enem_2023, "computadores", "desempenho", "Resumo do Desempenho por posse de computadores na residência")
tabela_resumo_profissional(enem_2023, "internet", "desempenho", "Resumo do Desempenho por acesso a internet")


#========== histograma
plot_hist_individual(enem_2023, "internet", "desempenho")
plot_hist_individual(enem_2023, "escolaridade_mae", "desempenho")
plot_hist_individual(enem_2023, "celulares", "desempenho")

#===================== Desempenho maiores de idade==========================================
# 1. Filtragem da base de dados conforme os critérios de idade e conclusão
df_analise <- enem_2023 %>%
  filter(
    TP_FAIXA_ETARIA != "17 anos", 
    TP_ST_CONCLUSAO %in% c(
      "Estou cursando e concluirei o Ensino Médio após 2023", 
      "Não concluí e não estou cursando o Ensino Médio"
    )
  )

# 2. Cálculo da porcentagem total de aprovação
# Definimos como aprovado apenas quem atingiu TODAS as notas mínimas
resultado_total <- df_analise %>%
  summarise(
    Total_Pessoas = n(),
    Total_Aprovados = sum(
      NU_NOTA_CN >= 450 & 
        NU_NOTA_CH >= 450 & 
        NU_NOTA_LC >= 450 & 
        NU_NOTA_MT >= 450 & 
        NU_NOTA_REDACAO >= 500, 
      na.rm = TRUE
    ),
    Porcentagem_Total = (Total_Aprovados / Total_Pessoas) * 100
  )

# 3. Exibição do resultado direto
cat("--- Resultado da Certificação Total ---\n")
cat("Total de candidatos analisados:", resultado_total$Total_Pessoas, "\n")
cat("Total que alcançou todas as notas:", resultado_total$Total_Aprovados, "\n")
cat("Porcentagem Total de Aprovação:", round(resultado_total$Porcentagem_Total, 2), "%\n")

# 1. Filtragem da base de dados conforme os critérios de idade e conclusão
df_conclusao <- enem_2023 %>%
  filter(TP_ST_CONCLUSAO ==
      "Já concluí o Ensino Médio"
    
  )

# 2. Cálculo da porcentagem total de aprovação
# Definimos como aprovado apenas quem atingiu TODAS as notas mínimas
resultado_conclusao <- df_conclusao %>%
  summarise(
    Total_Pessoas = n(),
    Total_Aprovados = sum(
      NU_NOTA_CN >= 450 & 
        NU_NOTA_CH >= 450 & 
        NU_NOTA_LC >= 450 & 
        NU_NOTA_MT >= 450 & 
        NU_NOTA_REDACAO >= 500, 
      na.rm = TRUE
    ),
    Porcentagem_Total = (Total_Aprovados / Total_Pessoas) * 100
  )

# 3. Exibição do resultado direto
cat("--- Resultado da Certificação de concluintes ---\n")
cat("Total de candidatos analisados:", resultado_conclusao$Total_Pessoas, "\n")
cat("Total que alcançou todas as notas:", resultado_conclusao$Total_Aprovados, "\n")
cat("Porcentagem Total de Aprovação:", round(resultado_conclusao$Porcentagem_Total, 2), "%\n")

