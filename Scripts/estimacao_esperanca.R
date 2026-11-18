# ============================================================
# MÉDIAS E DIFERENÇAS AJUSTADAS - GAMLSS SN1
# Intervalos de confiança de 95% pelo método delta
# ============================================================


library(gamlss)
library(gamlss.dist)
library(ggplot2)
library(dplyr)
library(tidyr)
library(patchwork)



#===============================================================================
# 1. MODELO, BASE E VARIÁVEIS
#===============================================================================

modelo <- modelo_formacao_todos

base <- dados_teste %>%
  dplyr::mutate(
    dplyr::across(
      dplyr::all_of(variaveis_analisar),
      as.factor
    )
  ) %>%
  droplevels()


#===============================================================================
#  VERIFICAÇÃO E PREPARAÇÃO PARA NOVA ABORDAGEM
#===============================================================================

# Matriz conjunta de variância-covariância
V <- tryCatch(
  vcov(modelo_formacao_todos, type = "all"),
  error = function(e) {
    message(conditionMessage(e))
    NULL
  }
)

if (!is.null(V)) {
  print(dim(V))
  print(all(is.finite(V)))
  print(isSymmetric(V, tol = 1e-6))
  print(length(coef(modelo_formacao_todos, what = "mu")))
  print(length(coef(modelo_formacao_todos, what = "sigma")))
  print(length(coef(modelo_formacao_todos, what = "nu")))
}

class(V)
names(V)
str(V, max.level = 1)



# Extrair os resultados
coeficientes <- V$coef
matriz_cov <- V$vcov

# Verificações
cat("Número de coeficientes:", length(coeficientes), "\n")
cat("Dimensão da matriz:", dim(matriz_cov), "\n")
cat("Todos os valores são finitos:",
    all(is.finite(matriz_cov)), "\n")
cat("Matriz simétrica:",
    isSymmetric(matriz_cov, tol = 1e-6), "\n")

# Autovalores
autovalores <- eigen(
  matriz_cov,
  symmetric = TRUE,
  only.values = TRUE
)$values

cat("Menor autovalor:", min(autovalores), "\n")
cat("Maior autovalor:", max(autovalores), "\n")

#===============================================================================
#  CONFERIR ORDEM DOS COEFICIENTES
#===============================================================================

# Quantidade de coeficientes por parâmetro
n_mu <- length(coef(modelo_formacao_todos, what = "mu"))
n_sigma <- length(coef(modelo_formacao_todos, what = "sigma"))
n_nu <- length(coef(modelo_formacao_todos, what = "nu"))

cat("Coeficientes de mu:", n_mu, "\n")
cat("Coeficientes de sigma:", n_sigma, "\n")
cat("Coeficientes de nu:", n_nu, "\n")
cat("Total:", n_mu + n_sigma + n_nu, "\n")

# Verificar correspondência com a matriz conjunta
stopifnot(
  n_mu + n_sigma + n_nu == length(V$coef),
  nrow(V$vcov) == length(V$coef)
)

# Conferir nomes e posições
head(names(V$coef), 10)
tail(names(V$coef), 10)


modelo <- modelo_formacao_todos

beta_mu <- coef(modelo, what = "mu")
beta_sigma <- coef(modelo, what = "sigma")
beta_nu <- coef(modelo, what = "nu")

beta_conjunto <- c(beta_mu, beta_sigma, beta_nu)

# Comparar os 129 coeficientes
cat(
  "Valores coincidem:",
  isTRUE(all.equal(
    unname(V$coef),
    unname(beta_conjunto),
    tolerance = 1e-8
  )),
  "\n"
)

cat(
  "Nomes coincidem:",
  identical(
    names(V$coef),
    names(beta_conjunto)
  ),
  "\n"
)

# Conferir os três blocos separadamente
c(
  mu = isTRUE(all.equal(
    unname(V$coef[1:52]),
    unname(beta_mu)
  )),
  sigma = isTRUE(all.equal(
    unname(V$coef[53:97]),
    unname(beta_sigma)
  )),
  nu = isTRUE(all.equal(
    unname(V$coef[98:129]),
    unname(beta_nu)
  ))
)

# MATRIZES

X_mu <- model.matrix(modelo, what = "mu")
X_sigma <- model.matrix(modelo, what = "sigma")
X_nu <- model.matrix(modelo, what = "nu")

# Dimensões esperadas:
# 21247 x 52
# 21247 x 45
# 21247 x 32

dim(X_mu)
dim(X_sigma)
dim(X_nu)

# Conferir a ordem das colunas
c(
  mu = identical(colnames(X_mu), names(beta_mu)),
  sigma = identical(colnames(X_sigma), names(beta_sigma)),
  nu = identical(colnames(X_nu), names(beta_nu))
)





modelo <- modelo_formacao_todos
base <- dados_teste

# Variáveis analisadas
variaveis <- c(
  "FAIXA_ETARIA_AGRUPADA",
  "TP_SEXO",
  "TP_COR_RACA",
  "escolaridade_pai",
  "escolaridade_mae",
  "renda_agrupada",
  "TP_LINGUA",
  "internet",
  "DEP_ADM_AGRUPADA",
  "TEMPO_ESPERA_AGRUPADO",
  "CENTRO",
  "modalidade_agrupada",
  "FORMAÇÃO"
)

# Coeficientes e covariância conjunta
beta <- as.numeric(V$coef)
Sigma <- V$vcov

n_mu <- 52
n_sigma <- 45
n_nu <- 32

b_mu <- beta[1:n_mu]
b_sigma <- beta[(n_mu + 1):(n_mu + n_sigma)]
b_nu <- beta[(n_mu + n_sigma + 1):length(beta)]

# Matrizes de desenho originais
X_mu <- model.matrix(modelo, what = "mu")
X_sigma <- model.matrix(modelo, what = "sigma")
X_nu <- model.matrix(modelo, what = "nu")

stopifnot(
  nrow(base) == nrow(X_mu),
  nrow(base) == nrow(X_sigma),
  nrow(base) == nrow(X_nu),
  all(variaveis %in% names(base))
)

# Garantir que todas as variáveis são fatores.
# Se alguma ainda for character, os níveis deverão coincidir
# com os utilizados no ajuste.
for (v in variaveis) {
  if (!is.factor(base[[v]])) {
    base[[v]] <- factor(base[[v]])
  }
}

# ============================================================
# 1. CONSTRUIR MATRIZES PARA NOVOS CENÁRIOS
# ============================================================

criar_matrizes <- function(novos_dados) {
  
  M_mu <- model.matrix(
    modelo$mu.terms,
    data = novos_dados,
    contrasts.arg = attr(X_mu, "contrasts")
  )
  
  M_sigma <- model.matrix(
    modelo$sigma.terms,
    data = novos_dados,
    contrasts.arg = attr(X_sigma, "contrasts")
  )
  
  M_nu <- model.matrix(
    modelo$nu.terms,
    data = novos_dados,
    contrasts.arg = attr(X_nu, "contrasts")
  )
  
  stopifnot(
    identical(colnames(M_mu), colnames(X_mu)),
    identical(colnames(M_sigma), colnames(X_sigma)),
    identical(colnames(M_nu), colnames(X_nu))
  )
  
  list(mu = M_mu, sigma = M_sigma, nu = M_nu)
}

# Verificar se reconstruímos as matrizes originais
M_teste <- criar_matrizes(base)

stopifnot(
  isTRUE(all.equal(
    unname(M_teste$mu),
    unname(X_mu),
    tolerance = 1e-8
  )),
  isTRUE(all.equal(
    unname(M_teste$sigma),
    unname(X_sigma),
    tolerance = 1e-8
  )),
  isTRUE(all.equal(
    unname(M_teste$nu),
    unname(X_nu),
    tolerance = 1e-8
  ))
)

rm(M_teste)

# ============================================================
# 2. MÉDIA AJUSTADA E GRADIENTE
# ============================================================

calcular_media_gradiente <- function(M) {
  
  n <- nrow(M$mu)
  
  # Preditores lineares
  eta_mu <- drop(M$mu %*% b_mu)
  eta_sigma <- drop(M$sigma %*% b_sigma)
  eta_nu <- drop(M$nu %*% b_nu)
  
  # Parâmetros da SN1
  mu <- eta_mu
  sigma <- exp(eta_sigma)
  nu <- eta_nu
  
  cte <- sqrt(2 / pi)
  
  # Média condicional
  media_individual <- mu +
    sigma * cte * nu / sqrt(1 + nu^2)
  
  media_ajustada <- mean(media_individual)
  
  # Derivadas da média em relação aos preditores
  d_mu <- rep(1, n)
  
  d_sigma <- sigma * cte *
    nu / sqrt(1 + nu^2)
  
  d_nu <- sigma * cte /
    (1 + nu^2)^(3/2)
  
  # Gradiente da média marginal em relação aos 129 betas
  gradiente <- c(
    colMeans(M$mu),
    drop(crossprod(M$sigma, d_sigma)) / n,
    drop(crossprod(M$nu, d_nu)) / n
  )
  
  stopifnot(
    length(gradiente) == length(beta),
    all(is.finite(gradiente)),
    is.finite(media_ajustada)
  )
  
  list(
    media = media_ajustada,
    gradiente = gradiente
  )
}

# ============================================================
# 3. ESTIMATIVAS PARA TODAS AS CATEGORIAS
# ============================================================

resultados <- list()
contador <- 0

for (v in variaveis) {
  
  niveis <- levels(base[[v]])
  referencia <- niveis[1]
  
  cat("\nAnalisando:", v, "\n")
  cat("Referência:", referencia, "\n")
  
  # Calcular a média e o gradiente na referência
  dados_ref <- base
  dados_ref[[v]] <- factor(
    rep(referencia, nrow(base)),
    levels = niveis
  )
  
  res_ref <- calcular_media_gradiente(
    criar_matrizes(dados_ref)
  )
  
  rm(dados_ref)
  
  for (nivel in niveis) {
    
    dados_cenario <- base
    
    dados_cenario[[v]] <- factor(
      rep(nivel, nrow(base)),
      levels = niveis
    )
    
    res <- calcular_media_gradiente(
      criar_matrizes(dados_cenario)
    )
    
    rm(dados_cenario)
    
    # IC da média ajustada
    var_media <- drop(
      crossprod(
        res$gradiente,
        Sigma %*% res$gradiente
      )
    )
    
    ep_media <- sqrt(max(0, var_media))
    
    # Diferença em relação à referência
    diferenca <- res$media - res_ref$media
    
    grad_dif <- res$gradiente - res_ref$gradiente
    
    var_dif <- drop(
      crossprod(
        grad_dif,
        Sigma %*% grad_dif
      )
    )
    
    ep_dif <- sqrt(max(0, var_dif))
    
    contador <- contador + 1
    
    resultados[[contador]] <- data.frame(
      Variavel = v,
      Categoria = nivel,
      Referencia = referencia,
      Media_ajustada = res$media,
      EP_media = ep_media,
      IC_media_inf = res$media - 1.96 * ep_media,
      IC_media_sup = res$media + 1.96 * ep_media,
      Diferenca = diferenca,
      EP_diferenca = ep_dif,
      IC_dif_inf = diferenca - 1.96 * ep_dif,
      IC_dif_sup = diferenca + 1.96 * ep_dif
    )
    
    cat("  Concluído:", nivel, "\n")
  }
}

tabela_ajustada <- bind_rows(resultados)

# Identificar categoria de referência
tabela_ajustada <- tabela_ajustada %>%
  mutate(
    Categoria_referencia = Categoria == Referencia,
    IC_exclui_zero = !Categoria_referencia &
      (IC_dif_inf > 0 | IC_dif_sup < 0)
  )

# ============================================================
# 4. SALVAR RESULTADOS
# ============================================================

dir.create(
  "resultados_gamlss/medias_ajustadas",
  recursive = TRUE,
  showWarnings = FALSE
)

write.csv2(
  tabela_ajustada,
  "resultados_gamlss/medias_ajustadas/
   medias_ajustadas_delta.csv" |>
    gsub("\\s+", "", x = _),
  row.names = FALSE
)

saveRDS(
  tabela_ajustada,
  "resultados_gamlss/medias_ajustadas/
   medias_ajustadas_delta.rds" |>
    gsub("\\s+", "", x = _)
)

print(tabela_ajustada)

# Exemplo: dependência administrativa
tabela_ajustada %>%
  filter(Variavel == "DEP_ADM_AGRUPADA") %>%
  print()
#===============================================================================

# GRÁFICO PARA AS 13 VARIÁVEIS

#===============================================================================

pasta <- "resultados_gamlss/medias_ajustadas/graficos"

dir.create(
  pasta,
  recursive = TRUE,
  showWarnings = FALSE
)

for (v in unique(tabela_ajustada$Variavel)) {
  
  dados_grafico <- tabela_ajustada %>%
    filter(
      Variavel == v,
      !Categoria_referencia
    )
  
  grafico <- ggplot(
    dados_grafico,
    aes(
      x = Diferenca,
      y = reorder(Categoria, Diferenca)
    )
  ) +
    geom_vline(
      xintercept = 0,
      linetype = "dashed",
      color = "gray50"
    ) +
    geom_pointrange(
      aes(
        xmin = IC_dif_inf,
        xmax = IC_dif_sup
      )
    ) +
    labs(
      title = paste("Diferenças ajustadas:", v),
      subtitle = paste(
        "Referência:",
        dados_grafico$Referencia[1]
      ),
      x = "Diferença ajustada nas notas (pontos)",
      y = NULL,
      caption = "IC aproximado de 95% — método delta"
    ) +
    theme_minimal(base_size = 12)
  
  arquivo <- paste0(
    gsub("[^[:alnum:]_]", "_", v),
    ".png"
  )
  
  ggsave(
    filename = file.path(pasta, arquivo),
    plot = grafico,
    width = 9,
    height = 6,
    dpi = 300
  )
}

#===============================================================================
#NOVOS GRAFICOS
#===============================================================================

moda_factor <- function(x) {
  names(sort(table(x), decreasing = TRUE))[1]
}

variaveis_modelo <- names(base)
variaveis_modelo <- variaveis_modelo[variaveis_modelo != "desempenho"]

perfil_base <- base[1, variaveis_modelo, drop = FALSE]

for (v in variaveis_modelo) {
  if (is.factor(base[[v]])) {
    perfil_base[[v]] <- factor(
      moda_factor(base[[v]]),
      levels = levels(base[[v]])
    )
  } else {
    perfil_base[[v]] <- median(base[[v]], na.rm = TRUE)
  }
}

perfil_base


gerar_curvas_variavel <- function(modelo, base, perfil_base, variavel,
                                  x_grid = seq(300, 900, length.out = 500)) {
  
  # níveis da variável
  niveis_var <- levels(base[[variavel]])
  
  # replicar perfil base
  novos_dados <- perfil_base[rep(1, length(niveis_var)), , drop = FALSE]
  
  # alterar só a variável de interesse
  novos_dados[[variavel]] <- factor(niveis_var, levels = levels(base[[variavel]]))
  
  # prever parâmetros
  mu_hat <- predict(
    modelo,
    newdata = novos_dados,
    what = "mu",
    type = "response"
  )
  
  sigma_hat <- predict(
    modelo,
    newdata = novos_dados,
    what = "sigma",
    type = "response"
  )
  
  nu_hat <- predict(
    modelo,
    newdata = novos_dados,
    what = "nu",
    type = "response"
  )
  
  # montar curva para cada nível
  curvas <- lapply(seq_along(niveis_var), function(i) {
    data.frame(
      desempenho = x_grid,
      densidade = gamlss.dist::dSN1(
        x_grid,
        mu = mu_hat[i],
        sigma = sigma_hat[i],
        nu = nu_hat[i]
      ),
      categoria = niveis_var[i],
      variavel = variavel
    )
  })
  
  bind_rows(curvas)
}

plotar_curvas_variavel <- function(curvas, titulo = NULL) {
  ggplot(curvas, aes(x = desempenho, y = densidade, color = categoria)) +
    geom_line(linewidth = 1) +
    labs(
      title = titulo,
      x = "Desempenho previsto",
      y = "Densidade prevista",
      color = NULL,
      subtitle = "Demais covariáveis fixadas no perfil de referência"
    ) +
    theme_minimal(base_size = 13) +
    theme(
      plot.title = element_text(face = "bold"),
      legend.position = "bottom"
    )
}
variaveis_importantes <- c(
  "DEP_ADM_AGRUPADA",
  "renda_agrupada",
  "escolaridade_mae",
  "escolaridade_pai",
  "modalidade_agrupada",
  "FORMAÇÃO"
)
lista_graficos <- list()

for (v in variaveis_importantes) {
  curvas_v <- gerar_curvas_variavel(
    modelo = modelo,
    base = base,
    perfil_base = perfil_base,
    variavel = v
  )
  
  lista_graficos[[v]] <- plotar_curvas_variavel(
    curvas_v,
    titulo = paste("Distribuições previstas por", v)
  )
}

lista_graficos[["DEP_ADM_AGRUPADA"]]
lista_graficos[["renda_agrupada"]]
lista_graficos[["modalidade_agrupada"]]
lista_graficos[["FORMAÇÃO"]]

painel_curvas <- patchwork::wrap_plots(
  lista_graficos[variaveis_importantes],
  ncol = 2
) +
  patchwork::plot_annotation(
    title = "Distribuições previstas das notas de ingresso na UFES",
    subtitle = "Modelo GAMLSS–SN1: características socioeconômicas, familiares e educacionais"
  )

painel_curvas


dir.create(
  "resultados_gamlss/modelo_final_TCC/graficos_distribuicoes",
  recursive = TRUE,
  showWarnings = FALSE
)

ggsave(
  "resultados_gamlss/modelo_final_TCC/graficos_distribuicoes/painel_distribuicoes_previstas.png",
  plot = painel_curvas,
  width = 15,
  height = 17,
  dpi = 300,
  bg = "white"
)

# ============================================================
# SALVAR GRÁFICOS INDIVIDUAIS E PAINEL CONJUNTO
# ============================================================

# Pasta de destino
pasta_graficos <- paste0(
  "resultados_gamlss/modelo_final_TCC/",
  "graficos_distribuicoes"
)

dir.create(
  pasta_graficos,
  recursive = TRUE,
  showWarnings = FALSE
)


# ============================================================
# GRÁFICOS DE DISTRIBUIÇÕES PREVISTAS - VERSÃO FINAL TCC
# ============================================================

# 1. Variáveis selecionadas
variaveis_importantes <- c(
  "DEP_ADM_AGRUPADA",
  "renda_agrupada",
  "escolaridade_mae",
  "escolaridade_pai",
  "modalidade_agrupada",
  "FORMAÇÃO"
)

# 2. Títulos para apresentação
titulos <- c(
  DEP_ADM_AGRUPADA = "Dependência administrativa da escola",
  renda_agrupada = "Renda familiar",
  escolaridade_mae = "Escolaridade materna",
  escolaridade_pai = "Escolaridade paterna",
  modalidade_agrupada = "Modalidade de ingresso",
  "FORMAÇÃO" = "Formação do curso"
)

# 3. Nomes dos arquivos
nomes_arquivos <- c(
  DEP_ADM_AGRUPADA = "distribuicao_dependencia_administrativa",
  renda_agrupada = "distribuicao_renda_familiar",
  escolaridade_mae = "distribuicao_escolaridade_mae",
  escolaridade_pai = "distribuicao_escolaridade_pai",
  modalidade_agrupada = "distribuicao_modalidade_ingresso",
  "FORMAÇÃO" = "distribuicao_formacao"
)

# ============================================================
# 4. PERFIL DE REFERÊNCIA UTILIZADO
# ============================================================

# Exibir o perfil efetivamente usado na previsão
print(perfil_base)

# Salvar as características do perfil
pasta <- paste0(
  "resultados_gamlss/modelo_final_TCC/",
  "graficos_distribuicoes"
)

dir.create(
  pasta,
  recursive = TRUE,
  showWarnings = FALSE
)

write.csv2(
  perfil_base,
  file.path(pasta, "perfil_referencia.csv"),
  row.names = FALSE
)

# Produzir uma descrição textual do perfil.
# Para cada gráfico, a variável apresentada é alterada;
# as demais permanecem nos valores de perfil_base.

descricao_perfil <- paste(
  paste0(
    names(perfil_base),
    ": ",
    as.character(perfil_base[1, ])
  ),
  collapse = "; "
)

writeLines(
  c(
    "PERFIL DE REFERÊNCIA DAS DISTRIBUIÇÕES PREVISTAS",
    "",
    "As covariáveis foram fixadas nos seguintes valores:",
    descricao_perfil,
    "",
    paste(
      "Em cada gráfico, somente a variável representada",
      "é modificada entre suas categorias."
    )
  ),
  file.path(pasta, "descricao_perfil_referencia.txt")
)

# ============================================================
# 5. FORMATAR OS GRÁFICOS
# ============================================================

graficos_finais <- list()

for (v in variaveis_importantes) {
  
  g <- lista_graficos[[v]]
  
  if (is.null(g)) {
    stop("Gráfico não encontrado: ", v)
  }
  
  graficos_finais[[v]] <- g +
    labs(
      title = titulos[[v]],
      subtitle = paste(
        "Demais características fixadas no perfil de referência"
      ),
      x = "Nota de ingresso prevista",
      y = "Densidade",
      color = NULL,
      caption = NULL
    ) +
    theme_minimal(base_size = 12) +
    theme(
      plot.title = element_text(
        face = "bold",
        size = 14
      ),
      plot.subtitle = element_text(
        size = 10,
        color = "gray40"
      ),
      legend.position = "bottom",
      legend.title = element_blank(),
      legend.text = element_text(size = 9),
      panel.grid.minor = element_blank()
    )
}

# ============================================================
# 6. SALVAR OS SEIS GRÁFICOS INDIVIDUAIS
# ============================================================

for (v in variaveis_importantes) {
  
  ggsave(
    filename = file.path(
      pasta,
      paste0(nomes_arquivos[[v]], ".png")
    ),
    plot = graficos_finais[[v]],
    width = 10,
    height = 7,
    dpi = 300,
    bg = "white"
  )
  
  cat("Salvo:", titulos[[v]], "\n")
}

# ============================================================
# 7. CRIAR PAINEL CONJUNTO
# ============================================================

painel_final <- patchwork::wrap_plots(
  graficos_finais[variaveis_importantes],
  ncol = 2
) +
  patchwork::plot_annotation(
    title = "Distribuições previstas das notas de ingresso na UFES",
    subtitle = paste(
      "Modelo GAMLSS–SN1 |",
      "demais características mantidas em um perfil de referência"
    ),
    caption = paste(
      "Fonte: elaboração própria com base nos dados",
      "de ingressantes da UFES (2019–2023).",
      "Os valores do perfil estão documentados",
      "em perfil_referencia.csv."
    ),
    theme = theme(
      plot.title = element_text(
        face = "bold",
        size = 18
      ),
      plot.subtitle = element_text(
        size = 12
      ),
      plot.caption = element_text(
        size = 9,
        color = "gray40"
      )
    )
  )

print(painel_final)

ggsave(
  filename = file.path(
    pasta,
    "painel_distribuicoes_previstas_final.png"
  ),
  plot = painel_final,
  width = 16,
  height = 18,
  dpi = 300,
  bg = "white"
)

cat("\nArquivos salvos em:\n",
    normalizePath(pasta), "\n")
