#===============================================================================
#DESCRITIVAS COM BASE LIMPA
#===============================================================================

#Ler inicialmente arquivo base_perfil_desempenho_2023.R

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
library(scales)

#===============================================================================
#REFAZENDO AS ANÁLISES COM BASE DE PARTICIPANTES LIMPA
#===============================================================================
enem_2023 <- read_csv("DOCUMENTOS_ENEM/DADOS/ENEM_2023_categorias.csv")

dim(enem_2023)
colnames(dados_2023)

#Total participantes: 2.569.190
#===============================================================================
#ORGANIZANDO FATORES
#===============================================================================
enem_2023 <- enem_2023 %>%
  mutate(
    TP_FAIXA_ETARIA = factor(
      TP_FAIXA_ETARIA,
      levels = c(
        "Menor de 17 anos",
        "17 anos",
        "18 anos",
        "19 anos",
        "20 anos",
        "21 anos",
        "22 anos",
        "23 anos",
        "24 anos",
        "25 anos",
        "Entre 26 e 30 anos",
        "Entre 31 e 35 anos",
        "Entre 36 e 40 anos",
        "Entre 41 e 45 anos",
        "Entre 46 e 50 anos",
        "Entre 51 e 55 anos",
        "Entre 56 e 60 anos",
        "Entre 61 e 65 anos",
        "Entre 66 e 70 anos",
        "Maior de 70 anos",
        "Não Informado"
      ),
      ordered = TRUE
    )
  )

enem_2023 <- enem_2023 %>%
  mutate(
    renda_familiar = factor(
      renda_familiar,
      levels = c(
        "Nenhuma renda",
        "Até 1 salário mínimo",
        "De 1 a 1,5 salários mínimos",
        "De 1,5 a 2 salários mínimos",
        "De 2 a 2,5 salários mínimos",
        "De 2,5 a 3 salários mínimos",
        "De 3 a 4 salários mínimos",
        "De 4 a 5 salários mínimos",
        "De 5 a 6 salários mínimos",
        "De 6 a 7 salários mínimos",
        "De 7 a 8 salários mínimos",
        "De 8 a 9 salários mínimos",
        "De 9 a 10 salários mínimos",
        "De 10 a 12 salários mínimos",
        "De 12 a 15 salários mínimos",
        "De 15 a 20 salários mínimos",
        "Acima de 20 salários mínimos"
      ),
      ordered = TRUE
    )
  )


enem_2023 <- enem_2023 %>%
  mutate(
escolaridade_mae = factor(
  escolaridade_mae,
  levels = c(
    "Nunca estudou",
    "Não completou EF (até 5º ano)",
    "EF incompleto (até 9º ano)",
    "EF completo",
    "Médio completo",
    "Superior completo",
    "Pós-graduação",
    "Não Informado"
  ),
  ordered = TRUE
))
#===============================================================================
#   DESCRITIVAS
#===============================================================================

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
  arrange(desc(NAs)) %>%
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
  
  
  grafico_histograma <- function(data, var, titulo){
    ggplot(data, aes(x = !!sym(var))) +
      geom_histogram(bins = 30, fill = "#4C72B0", color = "white") +
      scale_y_continuous(
        labels = label_number(
          big.mark = "",
          decimal.mark = ","
        )
      )+
      labs(
        title = titulo,
        x = "Nota",
        y = "Frequência"
      ) +
      theme_minimal(base_size = 12)
  }
  
 g1 <-  grafico_histograma(enem_2023, "NU_NOTA_CN", "Distribuição da Nota de Ciências da Natureza")
 g3 <-  grafico_histograma(enem_2023, "NU_NOTA_CH", "Distribuição da Nota de Ciências Humanas")
 g4 <-  grafico_histograma(enem_2023, "NU_NOTA_LC", "Distribuição da Nota de Linguagens")
 g2 <-  grafico_histograma(enem_2023, "NU_NOTA_MT", "Distribuição da Nota de Matemática")
  grafico_histograma(enem_2023, "desempenho", "Desempenho Médio dos Participantes")
  g1+g2
  g4+g3
  
  
 
# ------------------------------------------------------
#   GRAFICO DE BARRAS COMPETENCIA REDAÇÃO 
# ------------------------------------------------------
  
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
  
 g1 <-  grafico_comp_redacao(enem_2023, "NU_NOTA_COMP1", "Distribuição — Redação Competência 1")
 g2 <-  grafico_comp_redacao(enem_2023, "NU_NOTA_COMP2", "Distribuição – Redação Competência 2")
 g3 <-  grafico_comp_redacao(enem_2023, "NU_NOTA_COMP3", "Distribuição – Redação Competência 3")
 g4 <-  grafico_comp_redacao(enem_2023, "NU_NOTA_COMP4", "Distribuição – Redação Competência 4")
 g5 <-  grafico_comp_redacao(enem_2023, "NU_NOTA_COMP5", "Distribuição – Redação Competência 5")
 G6 <- grafico_histograma(enem_2023, "NU_NOTA_REDACAO", "Distribuição – Notas Redação")
 
 g1+g2
 g3+g4
 g5
 G6
 
 #===============================================================================
 # GERANDO A TABELA DE REDAÇÕES NOTA 1000 POR ESTADO
 #===============================================================================
 
 # 1. Filtra os dados e faz a contagem por estado (SG_UF_PROVA ou SG_UF_RESIDENCIA)
 tabela_redacao_1000 <- dados_2023 %>%
   filter(
     TP_PRESENCA_LC == 1,       # Presente no dia da redação
     NU_NOTA_REDACAO == 1000       # Nota máxima
   ) %>%
   group_by(SG_UF_PROVA) %>%   # Agrupa por estado de residência do aluno
   summarise(
     `Quantidade de Redacoes Nota 1000` = n(),
     .groups = "drop"
   ) %>%
   arrange(desc(`Quantidade de Redacoes Nota 1000`)) # Ordena do maior para o menor
 
 # 2. Adiciona a linha de Total Geral ao fim da tabela
 total_nota_1000 <- sum(tabela_redacao_1000$`Quantidade de Redacoes Nota 1000`)
 
 tabela_final <- tabela_redacao_1000 %>%
   bind_rows(
     tibble(
       SG_UF_PROVA = "Total Geral",
       `Quantidade de Redacoes Nota 1000` = total_nota_1000
     )
   )
 
 # 3. Exporta diretamente em formato LaTeX no padrão acadêmico
 tabela_final %>%
   kable(
     format = "latex",
     booktabs = TRUE,
     align = "lc",
     col.names = c("Estado (UF)", "Quantidade de Redacoes Nota 1000"),
     caption = "Distribuicao dos Participantes com Nota Maxima (1000) na Redacao por Unidade da Federacao"
   ) %>%
   row_spec(0, background = "#2B3E50", color = "white", bold = TRUE) %>%
   row_spec(nrow(tabela_final), bold = TRUE) 
 
#============================================================================
#  BASE TREINEIROS
#============================================================================
 
dados_trineiro <- enem_2023 %>% filter(IN_TREINEIRO == 'Sim')
View(dados_trineiro)

g1 <-  grafico_histograma(dados_trineiro, "NU_NOTA_CN", "Distribuição da Nota de Ciências da Natureza de Treineiros")
g3 <-  grafico_histograma(dados_trineiro, "NU_NOTA_CH", "Distribuição da Nota de Ciências Humanas de Treineiros")
g4 <-  grafico_histograma(dados_trineiro, "NU_NOTA_LC", "Distribuição da Nota de Linguagens de Treineiros")
g2 <-  grafico_histograma(dados_trineiro, "NU_NOTA_MT", "Distribuição da Nota de Matemática de Treineiros")
grafico_histograma(dados_trineiro, "desempenho", "Desempenho Médio dos Treineiros")
g1+g2
g4+g3

g1 <-  grafico_comp_redacao(dados_trineiro, "NU_NOTA_COMP1", "Distribuição — Redação Treineiros Competência 1")
g2 <-  grafico_comp_redacao(dados_trineiro, "NU_NOTA_COMP2", "Distribuição – Redação Treineiros Competência 2")
g3 <-  grafico_comp_redacao(dados_trineiro, "NU_NOTA_COMP3", "Distribuição – Redação Treineiros Competência 3")
g4 <-  grafico_comp_redacao(dados_trineiro, "NU_NOTA_COMP4", "Distribuição – Redação Treineiros Competência 4")
g5 <-  grafico_comp_redacao(dados_trineiro, "NU_NOTA_COMP5", "Distribuição – Redação Treineiros Competência 5")

g1+g2
g3+g4
g5

tabela_resumo_profissional(dados_trineiro, "IN_TREINEIRO", "desempenho", "Resumo do Desempenho por Estado Civil")
#=======================================================
# TABELA REDAÇÃO COM A BASE DO ENEM COMPLETA (FEZ AS PROVAS)

dados_presentes <- dados_2023 %>%
  filter(
    
    TP_PRESENCA_CN == 1,
    TP_PRESENCA_CH == 1,
    TP_PRESENCA_LC == 1,
    TP_PRESENCA_MT == 1
)
col_notas <- c(
  "NU_NOTA_CN",
  "NU_NOTA_CH",
  "NU_NOTA_LC",
  "NU_NOTA_MT"
)
col_notas2 <- c(
  "NU_NOTA_CN",
  "NU_NOTA_CH",
  "NU_NOTA_LC",
  "NU_NOTA_MT",
  "NU_NOTA_REDACAO"
)

dados_presentes <- dados_presentes %>%
  filter(
    if_all(
      all_of(col_notas),
      ~ !is.na(.x) & .x > 0
    )
  )

dados_presentes <- dados_presentes %>%
  mutate(
    desempenho = round(
      rowSums(across(all_of(col_notas))) / 4,
      1
    )
  )

tabela_resumo_profissional(dados_presentes, "TP_STATUS_REDACAO", "desempenho", "Resumo do Desempenho por Situação da Redação")
plot_densidade_grid(dados_presentes, "TP_STATUS_REDACAO", "desempenho", "Resumo do Desempenho por Situação da Redação")


colnames(dados_presentes)
#===============================================================================
#     SOCIAIS E DESEMPENHO 
#===============================================================================

tabela_resumo_profissional <- function(dados, var_cat, var_num, titulo = "") {
  
  library(dplyr)
  library(knitr)
  library(kableExtra)
  
  resumo <- dados %>%
    group_by(categoria = .data[[var_cat]]) %>%   
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
      align = "lrrrrrrrr"   
    ) %>%
    kable_styling(
      latex_options = c("striped", "hold_position", "scale_down"),
      full_width = FALSE,
      font_size = 8
    ) %>%
    column_spec(1, width = "5cm")  
}


tabela_resumo_profissional(enem_2023, "TP_SEXO", "desempenho", "Resumo do Desempenho por Sexo")
tabela_resumo_profissional(enem_2023, "dep_adm", "desempenho", "Resumo do Desempenho por Dependência Administrativa")
tabela_resumo_profissional(enem_2023, "tipo_escola", "desempenho", "Resumo do Desempenho por Tipo de Escola")
tabela_resumo_profissional(enem_2023, "TP_FAIXA_ETARIA", "desempenho", "Resumo do Desempenho por Faixa Etária")
tabela_resumo_profissional(enem_2023, "TP_ST_CONCLUSAO", "desempenho", "Resumo do Desempenho por Situação de Conclusão")
tabela_resumo_profissional(enem_2023, "TP_COR_RACA", "desempenho", "Resumo do Desempenho por Raça/Cor")
tabela_resumo_profissional(enem_2023, "escolaridade_mae", "desempenho", "Resumo do Desempenho por Escolaridade da Mãe")
tabela_resumo_profissional(enem_2023, "renda_familiar", "desempenho", "Resumo do Desempenho por Renda Familiar")
tabela_resumo_profissional(enem_2023, "celulares", "desempenho", "Resumo do Desempenho por Posse de Celulares")
tabela_resumo_profissional(enem_2023, "internet", "desempenho", "Resumo do Desempenho por Acesso a Internet")
tabela_resumo_profissional(enem_2023, "TP_ESTADO_CIVIL", "desempenho", "Resumo do Desempenho por Estado Civil")


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


plot_densidade <- function(dados, var_cat, var_num, titulo){
  
  ggplot(dados, aes(x = .data[[var_num]], color = .data[[var_cat]], fill = .data[[var_cat]])) +
    
    geom_density(aes(y = after_stat(density)), linewidth = 1, alpha = 0.05) + 
    
    # Transforma o eixo Y em porcentagem real (ex: 0.1 vira 10%)
    scale_y_continuous(labels = percent_format(accuracy = 0.1)) +
    
    scale_color_discrete(na.translate = FALSE) +
    scale_fill_discrete(na.translate = FALSE) +
    
    labs(
      title = titulo,
      x = "Nota (Desempenho Geral)",
      y = "Proporção de Alunos (%)",
      color = "Categorias",
      fill = "Categorias"
    ) +
    
    theme_minimal() +
    theme(
      plot.title = element_text(face = "plain", hjust = 0.5),
      plot.subtitle = element_text(size = 9, color = "gray30", hjust = 0.5),
      legend.position = "bottom",
      panel.grid.minor = element_blank()
    )
}

plot_densidade(enem_2023,  "internet", "desempenho", "Resumo do Desempenho por Acesso a Internet")
plot_densidade(enem_2023, "tipo_escola", "desempenho", "Resumo do Desempenho por Tipo de Escola")
plot_densidade(enem_2023, "TP_COR_RACA", "desempenho", "Resumo do Desempenho por Raça/Cor")
plot_densidade(enem_2023, "escolaridade_mae", "desempenho", "Resumo do Desempenho por Escolaridade da Mãe")
plot_densidade(enem_2023, "celulares", "desempenho", "Resumo do Desempenho por Posse de Celulares")
plot_densidade(enem_2023, "internet", "desempenho", "Resumo do Desempenho por Acesso a Internet")
plot_densidade(enem_2023, "dep_adm", "desempenho", "Resumo do Desempenho por Dependência Administrativa")
plot_densidade(enem_2023, "TP_SEXO", "desempenho", "Resumo do Desempenho por Sexo")

#plot densidade para conclusão com 2 linhas na legenda
plot_densidade2 <- function(dados, var_cat, var_num, titulo){
  
  ggplot(dados, aes(x = .data[[var_num]],
                    color = .data[[var_cat]],
                    fill = .data[[var_cat]])) +
    
    geom_density(aes(y = after_stat(density)),
                 linewidth = 1,
                 alpha = 0.05) +
    
    scale_y_continuous(labels = scales::percent_format(accuracy = 0.1)) +
    
    scale_color_discrete(na.translate = FALSE) +
    scale_fill_discrete(na.translate = FALSE) +
    
    guides(
      color = guide_legend(nrow = 2),
      fill  = guide_legend(nrow = 2)
    ) +
    
    labs(
      title = titulo,
      x = "Nota (Desempenho Geral)",
      y = "Proporção de Alunos (%)",
      color = "Escolaridade",
      fill = "Escolaridade"
    ) +
    
    theme_minimal() +
    theme(
      plot.title = element_text(face = "plain", hjust = 0.5),
      plot.subtitle = element_text(size = 9, color = "gray30", hjust = 0.5),
      legend.position = "bottom",
      panel.grid.minor = element_blank()
    )
}
plot_densidade2(enem_2023, "TP_ST_CONCLUSAO", "desempenho", "Resumo do Desempenho por Situação de Conclusão")
plot_densidade2(enem_2023, "TP_ESTADO_CIVIL", "desempenho", "Resumo do Desempenho por Estado Civil")

#plot densidade com grid
plot_densidade_grid <- function(dados, var_cat, var_num, titulo){
  
  ggplot(dados, aes(x = .data[[var_num]])) +
    
    geom_density(
      fill = "steelblue",
      color = "black",
      alpha = 0.3,
      linewidth = 0.7
    ) +
    
    facet_wrap(as.formula(paste("~", var_cat))) +
    
    scale_y_continuous(
      labels = scales::percent_format(accuracy = 0.1)
    ) +
    
    labs(
      title = titulo,
      x = "Nota (Desempenho Geral)",
      y = "Proporção de Alunos (%)"
    ) +
    
    theme_minimal() +
    
    theme(
      plot.title = element_text(hjust = 0.5),
      strip.text = element_text(face = "bold"),
      panel.grid.minor = element_blank(),
      legend.position = "none"
    )
}
plot_densidade_grid(enem_2023, "renda_familiar", "desempenho", "Resumo do Desempenho por Renda Familiar")
plot_densidade_grid(enem_2023, "TP_FAIXA_ETARIA", "desempenho", "Resumo do Desempenho por Faixa Etária")



plot_densidade(enem_2023, "TP_SEXO", "NU_NOTA_MT", "Resumo de Nota Matemática por Sexo")

ggplot(
  enem_2023,
  aes(
    x = NU_NOTA_MT,
    color = TP_ST_CONCLUSAO
  )
) +
  geom_histogram(
    aes(y = after_stat(density)),
    bins = 30,
    fill = NA,
    linewidth = 0.8,
    position = "identity"
  ) +
  theme_minimal()
#==============================================================================
#      ANÁLISE QUESTIONÁRIO SOCIAL
#==============================================================================

tab_frequencia(enem_2023, "escolaridade_pai", "Distribuição por Escolaridade do Pai")
tab_frequencia(enem_2023, "escolaridade_mae", "Distribuição por Escolaridade da Mãe")
tab_frequencia(enem_2023, "renda_familiar", "Distribuição por Renda Familiar")
tab_frequencia(enem_2023, "celulares", "Distribuição por posse de celular na residência")
tab_frequencia(enem_2023, "computadores", "Distribuição por posse de computadores na residência")
tab_frequencia(enem_2023, "internet", "Distribuição por acesso a internet")
#========== histograma==========================================================
plot_hist_individual(enem_2023, "internet", "desempenho")
plot_hist_individual(enem_2023, "escolaridade_mae", "desempenho")
plot_hist_individual(enem_2023, "celulares", "desempenho")
#===================== Desempenho maiores de idade==========================================
# 1. Filtragem da base de dados conforme os critérios de idade e conclusão
df_analise <- enem_2023 %>%
  filter(
    TP_FAIXA_ETARIA != c("17 anos", "Menor de 17 anos"), 
    TP_ST_CONCLUSAO %in% c(
      "Estou cursando e concluirei o Ensino Médio após esse ano", 
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

# 1. Processamento dos dados
tabela_final <- enem_2023 %>%
  # Filtrando categorias conforme sua base categórica
  filter(TP_FAIXA_ETARIA != c("17 anos", "Menor de 17 anos"), 
         TP_ST_CONCLUSAO %in% c("Estou cursando e concluirei o Ensino Médio após esse ano", 
                                "Não concluí e não estou cursando o Ensino Médio")) %>%
  
  # Criando critério de certificação
  mutate(conseguiu = ifelse(
    NU_NOTA_CN >= 450 & 
      NU_NOTA_CH >= 450 & 
      NU_NOTA_LC >= 450 & 
      NU_NOTA_MT >= 450 & 
      NU_NOTA_REDACAO >= 500, 
    1, 0)) %>%
  
  # Agrupamento e Sumário
  group_by(TP_FAIXA_ETARIA, TP_ST_CONCLUSAO) %>%
  summarise(
    Total = n(),
    Certificados = sum(conseguiu, na.rm = TRUE),
    Proporcao = (Certificados / Total) * 100,
    .groups = "drop"
  )


# 2. Gerando a tabela com kable
tabela_final %>%
  select(TP_FAIXA_ETARIA, TP_ST_CONCLUSAO, Total, Proporcao) %>%
  kable(
    col.names = c("Faixa Etária", "Status Conclusão", "Total Inscritos", "Proporção (%)"),
    digits = 2,
    caption = "Proporção de Candidatos aptos à Certificação por Faixa Etária e Conclusão",
    align = "llcc",
    escape = FALSE 
  ) %>%
  kable_styling(
    latex_options = c("striped", "hover", "hold_position", "scale_down"),
    full_width = FALSE
  )

#=== Quem já concluiu===========================================================

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

# 1. Processamento dos dados
tabela_concluiu <- enem_2023 %>%
  # Filtrando categorias conforme sua base categórica
  filter(TP_ST_CONCLUSAO == "Já concluí o Ensino Médio") %>%
  
  # Criando critério de certificação
  mutate(conseguiu = ifelse(
    NU_NOTA_CN >= 450 & 
      NU_NOTA_CH >= 450 & 
      NU_NOTA_LC >= 450 & 
      NU_NOTA_MT >= 450 & 
      NU_NOTA_REDACAO >= 500, 
    1, 0)) %>%
  
  # Agrupamento e Sumário
  group_by(TP_ANO_CONCLUIU) %>%
  summarise(
    Total = n(),
    Certificados = sum(conseguiu, na.rm = TRUE),
    Proporcao = (Certificados / Total) * 100,
    .groups = "drop"
  )


# 2. Gerando a tabela com kable
tabela_concluiu %>%
  kable(
    col.names = c("Ano de Conclusão", "Total Inscritos", "Qtd. Aprovados", "Proporção (%)"),
    digits = 2,
    caption = "Desempenho no ENEM 2023: Candidatos já Formados que atingiriam critérios de Certificação",
    align = "lccc"
  ) %>%
  kable_styling(
    bootstrap_options = c("striped", "hover", "condensed", "responsive"),
    full_width = F
  )
#===============================================================================
#Graficos por area de ensino
#===============================================================================
dados_long <- dados_trineiro %>%
  pivot_longer(
    cols = c(
      NU_NOTA_MT,
      NU_NOTA_LC,
      NU_NOTA_CH,
      NU_NOTA_CN
    ),
    names_to = "area",
    values_to = "Nota"
  ) %>%
  mutate(
    area = recode(
      area,
      "NU_NOTA_MT" = "Matemática",
      "NU_NOTA_LC" = "Linguagens",
      "NU_NOTA_CH" = "Ciências Humanas",
      "NU_NOTA_CN" = "Ciências da Natureza"
    )
  )

dados_long <- dados_long %>%
  mutate(
    escolaridade_mae = factor(
      escolaridade_mae,
      levels = c(
        "Nunca estudou",
        "Não completou EF (até 5º ano)",
        "EF incompleto (até 9º ano)",
        "EF completo",
        "Médio completo",
        "Superior completo",
        "Pós-graduação",
        "Não Informado"
      ),
      ordered = TRUE
    )
  )

ggplot(
  dados_long,
  aes(
    x = IN_TREINEIRO,
    y = Nota
  )
) +
  geom_violin(
    fill = "lightblue",
    alpha = 0.5,
    color = NA
  ) +
  geom_boxplot(
    width = 0.5,
    outlier.shape = NA,
    alpha = 0.8
  ) +
  facet_wrap(~ area, ncol = 2) +
  coord_flip() +
  labs(
    x = "Treineiro",
    y = "Nota"
  ) +
  theme_minimal() +
  theme(
    strip.text = element_text(face = "bold"),
    legend.position = "none"
  )


#-------

dados_long2 <- dados_presentes %>%
  pivot_longer(
    cols = c(
      NU_NOTA_MT,
      NU_NOTA_LC,
      NU_NOTA_CH,
      NU_NOTA_CN
    ),
    names_to = "area",
    values_to = "Nota"
  ) %>%
  mutate(
    area = recode(
      area,
      "NU_NOTA_MT" = "Matemática",
      "NU_NOTA_LC" = "Linguagens",
      "NU_NOTA_CH" = "Ciências Humanas",
      "NU_NOTA_CN" = "Ciências da Natureza"
    )
  )

dados_long2 <- dados_long2 %>%
  mutate(
    TP_STATUS_REDACAO = factor(
      TP_STATUS_REDACAO,
      levels = c(1,2,3,4,6,7,8,9),
      labels = c(
        "Sem problemas",
        "Anulada",
        "Cópia texto motivador",
        "Em branco",
        "Fuga ao tema",
        "Não atendimento tipo textual",
        "Texto insuficiente",
        "Parte desconectada"
      )
    )
  )

ggplot(
  dados_long2,
  aes(
    x = TP_STATUS_REDACAO,
    y = Nota
  )
) +
  geom_violin(
    fill = "lightblue",
    alpha = 0.5,
    color = NA
  ) +
  geom_boxplot(
    width = 0.5,
    outlier.shape = NA,
    alpha = 0.8
  ) +
  facet_wrap(~ area, ncol = 2) +
  coord_flip() +
  labs(
    x = "Situação de Redação",
    y = "Nota"
  ) +
  theme_minimal() +
  theme(
    strip.text = element_text(face = "bold"),
    legend.position = "none"
  )
