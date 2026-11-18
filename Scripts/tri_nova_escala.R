#==========================================================
# BIBLIOTECAS
#==========================================================

library(mirt)
library(readr)
library(dplyr)
library(tidyr)
library(mirt)
library(knitr)
library(kableExtra)
library(nortest)
library(scales)
library(ggplot2)
library(patchwork)

#==========================================================
# CHAMANDO DADOS
#==========================================================
dados_2023 <- read_delim("DOCUMENTOS_ENEM/DADOS/MICRODADOS_ENEM_2023.csv", 
                         delim = ";", escape_double = FALSE, trim_ws = TRUE)

itens_2023 <- read_delim("DOCUMENTOS_ENEM/DADOS/ITENS_PROVA_2023.csv", 
                         delim = ";", escape_double = FALSE, trim_ws = TRUE)

#----------------------------------------------------------
# Aplicações
#----------------------------------------------------------
aplicacoes <- list(
  Regular = c(1211:1219),
  Reaplicacao = c(1291:1294)
)


resultados_finais <- list()

#==========================================================
# LOOP DAS APLICAÇÕES
#==========================================================
for(nome_aplicacao in names(aplicacoes)){
  
  cat("\n==============================\n")
  cat(nome_aplicacao, "\n")
  cat("==============================\n")
  
  provas <- aplicacoes[[nome_aplicacao]]
  
  participantes_processar <- dados_2023 %>%
    filter(
      CO_PROVA_MT %in% provas,
      TP_PRESENCA_MT == 1,
      !is.na(TX_RESPOSTAS_MT),
      !is.na(NU_NOTA_MT)
    )
  
  itens_validos <- itens_2023 %>%
    filter(
      SG_AREA == "MT",
      CO_PROVA %in% provas,
      TX_GABARITO != "X",
      !is.na(NU_PARAM_A),
      !is.na(NU_PARAM_B),
      !is.na(NU_PARAM_C)
    ) %>%
    group_by(CO_PROVA) %>%
    arrange(CO_POSICAO,.by_group=TRUE) %>%
    mutate(
      indice_resposta = CO_POSICAO-min(CO_POSICAO)+1
    ) %>%
    ungroup()
  
  parametros <- itens_validos %>%
    distinct(
      CO_ITEM,
      .keep_all=TRUE
    ) %>%
    arrange(CO_ITEM)
  
  a_params <- parametros$NU_PARAM_A
  b_params <- parametros$NU_PARAM_B
  c_params <- parametros$NU_PARAM_C
  
  mat_respostas <- matrix(
    NA,
    nrow=nrow(participantes_processar),
    ncol=nrow(parametros)
  )
  
  colnames(mat_respostas) <- parametros$CO_ITEM
  
  #====================================================
  # PROCESSAMENTO POR PROVA (VETORIZADO)
  #====================================================
  
  for(prova in provas){
    
    cat("Prova",prova,"\n")
    
    idx <- which(
      participantes_processar$CO_PROVA_MT==prova
    )
    
    if(length(idx)==0)
      next
    
    itens_prova <- itens_validos %>%
      filter(CO_PROVA==prova) %>%
      arrange(indice_resposta)
    
    posicoes <- match(
      itens_prova$CO_ITEM,
      parametros$CO_ITEM
    )
    
    #------------------------------------------
    # Todas as respostas da prova
    #------------------------------------------
    
    respostas_char <-
      do.call(
        rbind,
        strsplit(
          participantes_processar$TX_RESPOSTAS_MT[idx],
          ""
        )
      )
    
    respostas_char <-
      respostas_char[
        ,
        itens_prova$indice_resposta,
        drop=FALSE
      ]
    
    gabarito <-
      matrix(
        itens_prova$TX_GABARITO,
        nrow=nrow(respostas_char),
        ncol=ncol(respostas_char),
        byrow=TRUE
      )
    
    acertos <-
      (respostas_char==gabarito)*1
    
    acertos[is.na(acertos)] <- 0
    
    mat_respostas[
      idx,
      posicoes
    ] <- acertos
    
  }
  
  total_acertos <-
    rowSums(
      mat_respostas,
      na.rm=TRUE
    )
  
  #====================================================
  # TRI
  #====================================================
  
  pars <- mirt(
    as.data.frame(mat_respostas),
    1,
    itemtype="3PL",
    pars="values"
  )
  
  pars$value[pars$name=="a1"] <- a_params
  pars$value[pars$name=="d"]  <- -a_params*b_params
  pars$value[pars$name=="g"]  <- c_params
  
  pars$est[
    pars$name %in% c("a1","d","g")
  ] <- FALSE
  
  modelo_enem <- mirt(
    as.data.frame(mat_respostas),
    1,
    itemtype="3PL",
    pars=pars,
    quadpts=40
  )
  
  theta <- fscores(
    modelo_enem,
    method="EAP",
    quadpts=40,
    full.scores=TRUE
  )
  
  theta_vec <- as.numeric(theta)
  
  ajuste_escala <- lm(participantes_processar$NU_NOTA_MT ~ theta_vec)
  
  B_intercepto <- coef(ajuste_escala)[1] # Equivalente ao "500" médio
  A_inclinacao <- coef(ajuste_escala)[2] # Equivalente ao "100" desvio
  
  cat(sprintf("Escala Calibrada para %s: Nota = %.2f * theta + %.2f\n", 
              nome_aplicacao, A_inclinacao, B_intercepto))
  
  # Aplica a fórmula com os coeficientes encontrados automaticamente
  nota_tri <- B_intercepto + A_inclinacao * theta_vec
  
  resultados_finais[[nome_aplicacao]] <-
    participantes_processar %>%
    transmute(
      NU_INSCRICAO,
      CO_PROVA_MT,
      APLICACAO=nome_aplicacao,
      NU_NOTA_MT,
      ACERTOS_VALIDOS=total_acertos,
      
      # Coeficientes guardados para conferência histórica se precisar
      COEF_A = round(A_inclinacao, 4),
      COEF_B = round(B_intercepto, 4),
      
      NOTA_SIMULADA_TRI=round(nota_tri, 2),
      DIFERENCA=round(nota_tri - NU_NOTA_MT, 2)
    )
  
}

#==========================================================
# VISUALIZAÇÃO DOS RESULTADOS
#==========================================================

base_tri_comparativa <-
  bind_rows(resultados_finais)

base_tri_comparativa_filtrada <- base_tri_comparativa %>%
  filter(NU_NOTA_MT > 0) %>%
  mutate(
    # Criando a coluna de proficiência na escala padrão
    THETA_ESTIMADO = (NOTA_SIMULADA_TRI - 500) / 100
  )

tabela_estatisticas <- base_tri_comparativa_filtrada %>%
  select(
    `Nota Real (INEP)` = NU_NOTA_MT,
    `Nota Estimada (TRI)` = NOTA_SIMULADA_TRI,
    `Proficiência` = THETA_ESTIMADO # Adicionado na tabela!
  ) %>%
  pivot_longer(
    everything(),
    names_to = "Variável",
    values_to = "Valor"
  ) %>%
  group_by(Variável) %>%
  summarise(
    N = n(),
    Média = mean(Valor, na.rm = TRUE),
    DP = sd(Valor, na.rm = TRUE),
    Mínimo = min(Valor, na.rm = TRUE),
    Q1 = quantile(Valor, 0.25, na.rm = TRUE),
    Mediana = median(Valor, na.rm = TRUE),
    Q3 = quantile(Valor, 0.75, na.rm = TRUE),
    Máximo = max(Valor, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    across(where(is.numeric), ~round(., 2))
  )

kable(
  tabela_estatisticas,
  caption = "Métricas descritivas dos resultados observados e simulados"
) %>%
  kable_styling(
    bootstrap_options = c("striped", "hover"),
    full_width = FALSE
  ) %>%
  row_spec(
    0,
    bold = TRUE,
    background = "#2c3e50",
    color = "white"
  )

cat("\n--- PRIMEIRAS LINHAS DA BASE FINAL ---\n")
print(head(base_tri_comparativa_filtrada))

ERROS <- base_tri_comparativa_filtrada$DIFERENCA
cat("\n======================================================\n")
cat("      ESTATÍSTICAS DE VALIDAÇÃO DA AMOSTRA            \n")
cat("======================================================\n")
cat("Erro Médio Absoluto (MAE)  :", round(mean(abs(ERROS)), 3), "pontos\n")
cat("Raiz do Erro Médio (RMSE)  :", round(sqrt(mean(ERROS^2)), 3), "pontos\n")
cat("Desvio Padrão do Erro      :", round(sd(ERROS), 3), "pontos\n")
cat("======================================================\n")


ERROS <- base_tri_comparativa_filtrada$DIFERENCA

tabela_validacao <- data.frame(
  Métrica = c(
    "Erro Médio",
    "Erro Médio Absoluto (MAE)",
    "Raiz do Erro Quadrático Médio (RMSE)",
    "Desvio Padrão do Erro",
    "Erro Mínimo",
    "Erro Máximo"
  ),
  Valor = c(
    round(mean(ERROS), 3),
    round(mean(abs(ERROS)), 3),
    round(sqrt(mean(ERROS^2)), 3),
    round(sd(ERROS), 3),
    round(min(ERROS), 3),
    round(max(ERROS), 3)
  )
)

kable(
  tabela_validacao,
  col.names = c("Métrica de Validação", "Valor (pontos)"),
  align = c("l", "c"),
  caption = "Estatísticas de validação das notas estimadas"
) %>%
  kable_styling(
    bootstrap_options = c("striped", "hover", "bordered"),
    full_width = FALSE
  ) %>%
  row_spec(
    0,
    bold = TRUE,
    background = "#34495e",
    color = "white"
  )

resultados_finais


#===============================================================================
# CÁLCULOS DE VALIDAÇÃO E CORRELAÇÃO
#===============================================================================
ERROS <- base_tri_comparativa_filtrada$DIFERENCA

# Realiza o teste de hipótese para a Correlação de Pearson
teste_cor <- cor.test(
  base_tri_comparativa_filtrada$NU_NOTA_MT, 
  base_tri_comparativa_filtrada$NOTA_SIMULADA_TRI, 
  method = "pearson"
)

# Extrai os resultados do teste
valor_cor <- teste_cor$estimate       # Coeficiente r de Pearson
p_valor   <- teste_cor$p.value        # P-valor do teste

# Formata o p-valor para exibição acadêmica (< 0.001 se for muito pequeno)
p_valor_fmt <- if(p_valor < 0.001) "< 0.001" else round(p_valor, 4)

#==========================================================
# GRÁFICOS
#==========================================================

cor_principal <- "#1f4e5b"  
cor_linha     <- "red"  
cor_pontos    <- "#5c9296"  

# # G1: Distribuição do Erro
# g1 <- ggplot(base_tri_comparativa_filtrada, aes(x = DIFERENCA)) +
#   geom_histogram(aes(y = after_stat(density)), bins = 35, 
#                  fill = cor_principal, color = "white", alpha = 0.85) +
#   geom_density(color = cor_linha, linewidth = 1.2) +
#   geom_vline(xintercept = 0, color = "grey30", linetype = "dashed", linewidth = 0.8) +
#   labs(
#     title = "Distribuição do Erro de Estimação",
#     subtitle = "Resíduo estatístico (Simulada - Real)",
#     x = "Diferença (Pontos)",
#     y = "Densidade"
#   ) +
#   theme_minimal(base_size = 12) +
#   theme(
#     panel.grid.minor = element_blank(),
#     plot.title = element_text(face = "bold", hjust = 0.5),
#     plot.subtitle = element_text(size = 10, color = "grey40", hjust = 0.5)
#   )

# G2: Correlação de Notas
ggplot(base_tri_comparativa_filtrada, aes(x = NU_NOTA_MT, y = NOTA_SIMULADA_TRI)) +
  # Reduzi a opacidade (alpha = 0.15) para destacar o acúmulo de dados (região central)
  geom_point(alpha = 0.15, color = cor_pontos, size = 1.2) +
  # Linha Ideal (Identidade)
  geom_abline(intercept = 0, slope = 1, color = cor_linha, linetype = "solid", linewidth = 1) +
  # Linha de tendência real dos seus dados estimados
  geom_smooth(method = "gam", color = cor_principal, linetype = "longdash", linewidth = 1, se = FALSE) +
  labs(
    title = "Dispersão Comparativa das Notas",
    subtitle = "Comparativo: Notas INEP vs. Nota Simulada",
    x = "Nota Real (INEP)",
    y = "Nota Simulada (TRI)"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid.minor = element_blank(),
    plot.title = element_text(face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 10, color = "grey40", hjust = 0.5)
  )

#===============================================================================
# Comparativo das distribuições: Notas Reais vs. Estimadas
#===============================================================================

# Cores do padrão visual do projeto
cor_real <- "#1f4e5b"      # Azul petróleo escuro
cor_estimada <- "#e06d53"  # Coral suave

ggplot(base_tri_comparativa_filtrada) +
  
  # Curva da Nota Real
  geom_density(
    aes(x = NU_NOTA_MT, color = "Nota Real"),
    linewidth = 1.2,
    trim = TRUE,
    cut = 0
  ) +
  
  # Curva da Nota Estimada
  geom_density(
    aes(x = NOTA_SIMULADA_TRI, color = "Nota Estimada"),
    linewidth = 1.2,
    trim = TRUE,
    cut = 0
  ) +
  
  # Cores das curvas
  scale_color_manual(
    values = c(
      "Nota Real" = cor_real,
      "Nota Estimada" = cor_estimada
    ),
    name = NULL
  ) +
  
  # Rótulos
  labs(
    title = "Comparativo de Densidade: Notas Reais vs. Estimadas",
    subtitle = "Evidência visual do efeito de encolhimento do estimador EAP",
    x = "Nota na Prova de Matemática",
    y = "Densidade Populacional"
  ) +
  
  # Escala do eixo X
  scale_x_continuous(
    breaks = seq(150, 1000, by = 100),
    expand = expansion(mult = c(0, 0))
  ) +
  
  # Tema
  theme_minimal(base_size = 11) +
  theme(
    panel.grid.minor = element_blank(),
    plot.title = element_text(face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 9, color = "grey40", hjust = 0.5),
    legend.position = c(0.85, 0.85),
    legend.background = element_rect(
      fill = "white",
      color = "grey90"
    )
  )