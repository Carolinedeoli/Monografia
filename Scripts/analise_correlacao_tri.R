
#===============================================================================
# VALIDAÇÃO DAS NOTAS TRI - CONCORDÂNCIA DE LIN E BLAND-ALTMAN
#===============================================================================

# Instalar apenas se necessário:
# install.packages(c("DescTools", "ggplot2"))

library(DescTools)
library(ggplot2)
library(arrow)
library(dplyr)
library(tidyr)
base_tri_comparativa <- read_parquet(
  "DOCUMENTOS_ENEM/DADOS/base_tri_comparativa.parquet"
)

base_tri_comparativa_filtrada <- base_tri_comparativa %>%
  filter(NU_NOTA_MT > 0) %>%
  mutate(
    # Criando a coluna de proficiência na escala padrão
    THETA_ESTIMADO = (NOTA_SIMULADA_TRI - 500) / 100
  )

# 1. Selecionar os mesmos participantes para todas as métricas
dados_validacao <- base_tri_comparativa_filtrada[
  , c("NU_NOTA_MT", "NOTA_SIMULADA_TRI")
]

dados_validacao <- dados_validacao[
  complete.cases(dados_validacao), 
]

stopifnot(
  is.numeric(dados_validacao$NU_NOTA_MT),
  is.numeric(dados_validacao$NOTA_SIMULADA_TRI)
)

dados_validacao <- dados_validacao[
  is.finite(dados_validacao$NU_NOTA_MT) &
    is.finite(dados_validacao$NOTA_SIMULADA_TRI),
]

nota_oficial <- dados_validacao$NU_NOTA_MT
nota_estimada <- dados_validacao$NOTA_SIMULADA_TRI

# 2. Correlação de Pearson (apenas para comparação)
r_pearson <- cor(nota_oficial, nota_estimada)

# 3. Coeficiente de Concordância de Lin (CCC)
resultado_ccc <- DescTools::CCC(
  x = nota_oficial,
  y = nota_estimada,
  ci = "z-transform",
  conf.level = 0.95
)

# Extração das estimativas
ccc <- resultado_ccc$rho.c[1, "est"]
ccc_ic_inf <- resultado_ccc$rho.c[1, "lwr.ci"]
ccc_ic_sup <- resultado_ccc$rho.c[1, "upr.ci"]

# 4. Diferenças para Bland-Altman
# Convenção: nota estimada - nota oficial
diferenca <- nota_estimada - nota_oficial
media_notas <- (nota_estimada + nota_oficial) / 2

vies <- mean(diferenca)
dp_diferenca <- sd(diferenca)

# Limites aproximados de concordância de 95%
limite_inferior <- vies - 1.96 * dp_diferenca
limite_superior <- vies + 1.96 * dp_diferenca

# 5. Manter métricas de erro usadas no TCC
mae <- mean(abs(diferenca))
rmse <- sqrt(mean(diferenca^2))

# 6. Resumo dos resultados
resumo_validacao <- data.frame(
  N = length(diferenca),
  Pearson = r_pearson,
  CCC_Lin = ccc,
  CCC_IC95_inferior = ccc_ic_inf,
  CCC_IC95_superior = ccc_ic_sup,
  Vies = vies,
  MAE = mae,
  RMSE = rmse,
  Bland_Altman_LI = limite_inferior,
  Bland_Altman_LS = limite_superior
)

print(round(resumo_validacao, 4))

# 7. Gráfico de Bland-Altman
# Usar amostra apenas para visualização, devido ao tamanho da base.
# Todos os indicadores acima são calculados com a base completa.

set.seed(2026)
indices <- sample.int(
  length(diferenca),
  size = min(50000L, length(diferenca))
)

dados_grafico <- data.frame(
  Media = media_notas[indices],
  Diferenca = diferenca[indices]
)

grafico_bland_altman <- ggplot(
  dados_grafico,
  aes(x = Media, y = Diferenca)
) +
  geom_point(alpha = 0.15, size = 0.5) +
  geom_hline(
    yintercept = vies,
    color = "blue",
    linewidth = 0.8
  ) +
  geom_hline(
    yintercept = c(limite_inferior, limite_superior),
    color = "red",
    linetype = "dashed",
    linewidth = 0.8
  ) +
  labs(
    title = "Concordância entre as notas estimadas e oficiais",
    subtitle = "Análise de Bland-Altman — ENEM 2023",
    x = "Média entre a nota estimada e a oficial",
    y = "Diferença (estimada - oficial)",
    caption = paste0(
      "Viés = ", round(vies, 2),
      " | Limites de concordância: [",
      round(limite_inferior, 2), "; ",
      round(limite_superior, 2), "]",
      "\nPontos exibidos: amostra aleatória; ",
      "limites calculados na base completa."
    )
  ) +
  theme_minimal(base_size = 12)

print(grafico_bland_altman)


# Investigar viés proporcional
modelo_vies <- lm(diferenca ~ media_notas)

# Coeficientes da regressão
coef(modelo_vies)

# Intervalos de confiança
confint(modelo_vies)

# Adicionar tendência ao gráfico já produzido
grafico_bland_altman_tendencia <-
  grafico_bland_altman +
  geom_abline(
    intercept = coef(modelo_vies)[1],
    slope = coef(modelo_vies)[2],
    color = "darkgreen",
    linewidth = 0.9
  )

print(grafico_bland_altman_tendencia)
#===============================================================================

# Nova escala 

table(base_tri_comparativa_filtrada$CO_PROVA, useNA = "ifany")


library(dplyr)


base_tri_comparativa_filtrada <- base_tri_comparativa_filtrada %>%
  mutate(
    NOTA_SIMULADA_TRI_CALIBRADA = case_when(
      CO_PROVA %in% 1211:1219 ~
        133.07 * ((NOTA_SIMULADA_TRI - 500) / 100) + 497.04,
      
      CO_PROVA %in% 1291:1294 ~
        130.02 * ((NOTA_SIMULADA_TRI - 500) / 100) + 499.73,
      
      TRUE ~ NA_real_
    )
  )

# Conferir quantas notas foram recalibradas
sum(!is.na(base_tri_comparativa_filtrada$NOTA_SIMULADA_TRI_CALIBRADA))
#-------------------------------------------------------------------------------
#Função de validação

library(dplyr)
library(DescTools)

# Função para calcular as métricas de validação
validar_notas <- function(oficial, estimada) {
  
  erros <- estimada - oficial
  
  # Correlação de Pearson
  pearson <- cor(oficial, estimada)
  
  # Coeficiente de Concordância de Lin
  resultado_ccc <- DescTools::CCC(
    x = oficial,
    y = estimada,
    ci = "z-transform",
    conf.level = 0.95
  )
  
  # Viés e limites de Bland-Altman
  vies <- mean(erros)
  dp_erros <- sd(erros)
  
  # Retornar as métricas
  data.frame(
    N = length(erros),
    Pearson = pearson,
    CCC_Lin = resultado_ccc$rho.c[1, "est"],
    CCC_IC95_inferior = resultado_ccc$rho.c[1, "lwr.ci"],
    CCC_IC95_superior = resultado_ccc$rho.c[1, "upr.ci"],
    Vies = vies,
    MAE = mean(abs(erros)),
    RMSE = sqrt(mean(erros^2)),
    Bland_Altman_LI = vies - 1.96 * dp_erros,
    Bland_Altman_LS = vies + 1.96 * dp_erros
  )
}
#-------------------------------------------------------------------------------


resultado_original <- validar_notas(
  base_tri_comparativa_filtrada$NU_NOTA_MT,
  base_tri_comparativa_filtrada$NOTA_SIMULADA_TRI
)

resultado_calibrado <- validar_notas(
  base_tri_comparativa_filtrada$NU_NOTA_MT,
  base_tri_comparativa_filtrada$NOTA_SIMULADA_TRI_CALIBRADA
)

comparacao_escalas <- rbind(
  Original = resultado_original,
  Calibrada = resultado_calibrado
)

print(round(comparacao_escalas, 4))


# saveRDS(
#   base_tri_comparativa_filtrada,
#   "base_tri_comparativa_calibrada.rds"
# )


library(ggplot2)

# ==========================================================
# BLAND-ALTMAN - NOVA ESCALA RECALIBRADA
# ==========================================================
library(dplyr)

dados_comparacao <- base_tri_comparativa_filtrada %>%
  filter(
    is.finite(NU_NOTA_MT),
    is.finite(NOTA_SIMULADA_TRI),
    is.finite(NOTA_SIMULADA_TRI_CALIBRADA)
  )


library(ggplot2)

# ==========================================================
# BLAND-ALTMAN - NOVA ESCALA
# ==========================================================

# Notas oficiais e estimadas com a nova escala
oficial <- base_tri_comparativa_filtrada$NU_NOTA_MT
estimada <- base_tri_comparativa_filtrada$NOTA_SIMULADA_TRI_CALIBRADA

# Remover valores ausentes
validos <- is.finite(oficial) & is.finite(estimada)
oficial <- oficial[validos]
estimada <- estimada[validos]

# Média e diferença
media_notas <- (oficial + estimada) / 2
diferenca <- estimada - oficial

# Viés e limites de concordância
vies <- mean(diferenca)
dp_diferenca <- sd(diferenca)

limite_inferior <- vies - 1.96 * dp_diferenca
limite_superior <- vies + 1.96 * dp_diferenca

# Amostra apenas para visualização
set.seed(2026)

indices <- sample.int(
  length(diferenca),
  min(50000, length(diferenca))
)

dados_grafico <- data.frame(
  Media = media_notas[indices],
  Diferenca = diferenca[indices]
)

# Gráfico igual ao da primeira escala
grafico_bland_altman_calibrado <- ggplot(
  dados_grafico,
  aes(x = Media, y = Diferenca)
) +
  geom_point(
    alpha = 0.15,
    size = 0.5,
    color = "black"
  ) +
  geom_hline(
    yintercept = vies,
    color = "blue",
    linewidth = 0.8
  ) +
  geom_hline(
    yintercept = c(limite_inferior, limite_superior),
    color = "red",
    linetype = "dashed",
    linewidth = 0.8
  ) +
  labs(
    title = "Concordância entre as notas estimadas e oficiais",
    subtitle = "Análise de Bland-Altman — Nova Escala — ENEM 2023",
    x = "Média entre a nota estimada e a oficial",
    y = "Diferença (estimada - oficial)",
    caption = paste0(
      "Viés = ", round(vies, 2),
      " | Limites de concordância: [",
      round(limite_inferior, 2), "; ",
      round(limite_superior, 2), "]",
      "\nPontos exibidos: amostra aleatória; ",
      "limites calculados na base completa."
    )
  ) +
  theme_minimal(base_size = 12)

print(grafico_bland_altman_calibrado)

# Salvar
dir.create("fig_tri", showWarnings = FALSE)

ggsave(
  "fig_tri/bland_altman_nova_escala.png",
  plot = grafico_bland_altman_calibrado,
  width = 8,
  height = 5,
  dpi = 300
)
