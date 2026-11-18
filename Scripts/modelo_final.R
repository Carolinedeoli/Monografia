
#===============================================================================

# TESTANDO INCLUSÃO DE VARIÁVEL FORMAÇÃO NO MODELO FINAL

#===============================================================================

#===============================================================================
# BIBLIOTECAS
#===============================================================================

library(gamlss)
library(gamlss.dist)
library(dplyr)
library(ggplot2)
library(car)
library(readr)
library(MASS)
library(gamlss.ggplots)
library(ggplot2)

#===============================================================================
# PREPARANDO BASE
#===============================================================================


# Substituir pelo nome da variável
nova_variavel <- "FORMAÇÃO"

# Recuperar a variável da base original
dados_teste <- dados %>%
  dplyr::select(
    dplyr::all_of(c(
      "desempenho",
      variaveis,
      nova_variavel
    ))
  ) %>%
  dplyr::filter(
    dplyr::if_all(
      dplyr::all_of(c("desempenho", variaveis)),
      ~ !is.na(.x)
    )
  ) %>%
  dplyr::mutate(
    dplyr::across(
      dplyr::all_of(variaveis),
      as.factor
    )
  ) %>%
  droplevels() %>%
  as.data.frame()

# Conferir número de observações
nrow(dados_teste)

# Verificar valores ausentes na variável
sum(is.na(dados_teste[[nova_variavel]]))

# Distribuição da nova variável
table(
  dados_teste[[nova_variavel]],
  useNA = "ifany"
)

identical(
  dados_teste[, names(dados_modelo), drop = FALSE],
  dados_modelo
)

#===============================================================================
# AJUSTANDO NOVA VARIÁVEL NO MODELO
#===============================================================================

# Modelo de referência
modelo_base <- modelo_SN1_final_TCC

# Conferências
stopifnot(
  isTRUE(modelo_base$converged),
  nrow(dados_teste) == modelo_base$N,
  !anyNA(dados_teste[[nova_variavel]]),
  identical(
    dados_teste[, names(dados_modelo), drop = FALSE],
    dados_modelo
  )
)

# Conferir que a variável possui variação
stopifnot(
  length(unique(dados_teste[[nova_variavel]])) > 1
)

# Função para acrescentar a variável
ajustar_inclusao <- function(parametro) {
  
  formulas <- list(
    mu = formula(modelo_base, what = "mu"),
    sigma = formula(modelo_base, what = "sigma"),
    nu = formula(modelo_base, what = "nu")
  )
  
  # Acrescentar a variável somente ao parâmetro testado
  formulas[[parametro]] <- update(
    formulas[[parametro]],
    paste("~ . +", nova_variavel)
  )
  
  ajuste <- tryCatch(
    gamlss(
      formula = formulas$mu,
      sigma.formula = formulas$sigma,
      nu.formula = formulas$nu,
      family = SN1(),
      data = dados_teste,
      start.from = modelo_base,
      control = gamlss.control(
        n.cyc = 500,
        c.crit = 0.0001,
        mu.step = 0.5,
        sigma.step = 0.5,
        nu.step = 0.2,
        trace = FALSE
      )
    ),
    error = function(e) {
      message(
        "Erro em ", parametro, ": ",
        conditionMessage(e)
      )
      NULL
    }
  )
  
  ajuste
}

# Ajustar os três modelos
modelo_novo_mu <- ajustar_inclusao("mu")
modelo_novo_sigma <- ajustar_inclusao("sigma")
modelo_novo_nu <- ajustar_inclusao("nu")



#===============================================================================
# TABELA DE TESTES DE RAZÃO DE VEROSSIMILHANÇA 
#===============================================================================

modelos_novos <- list(
  mu = modelo_novo_mu,
  sigma = modelo_novo_sigma,
  nu = modelo_novo_nu
)

tabela_inclusao <- do.call(
  rbind,
  lapply(names(modelos_novos), function(parametro) {
    
    mod <- modelos_novos[[parametro]]
    
    if (is.null(mod) || !isTRUE(mod$converged)) {
      return(data.frame(
        Parametro = parametro,
        LR = NA_real_,
        GL = NA_real_,
        p_valor = NA_real_,
        AIC = NA_real_,
        BIC = NA_real_,
        Convergiu = FALSE,
        Valido = FALSE
      ))
    }
    
    LR <- deviance(modelo_base) - deviance(mod)
    GL <- mod$df.fit - modelo_base$df.fit
    
    valido <- is.finite(LR) &&
      GL > 0 &&
      LR >= -1e-4 &&
      mod$N == modelo_base$N
    
    data.frame(
      Parametro = parametro,
      LR = LR,
      GL = GL,
      p_valor = if (valido)
        pchisq(
          max(LR, 0),
          df = GL,
          lower.tail = FALSE
        )
      else NA_real_,
      AIC = as.numeric(GAIC(mod, k = 2)),
      BIC = as.numeric(GAIC(mod, k = log(mod$N))),
      Convergiu = TRUE,
      Valido = valido
    )
  })
)

print(tabela_inclusao, row.names = FALSE)


# COMPARAR COM MODELO ORIGINAL


# Indicadores da SN1 final sem a nova variável
c(
  Deviance = deviance(modelo_base),
  AIC = GAIC(modelo_base, k = 2),
  BIC = GAIC(modelo_base, k = log(modelo_base$N))
)

#===============================================================================
# TESTANDO SELEÇÃO FORWARD
#===============================================================================

# Modelos já ajustados com FORMAÇÃO
modelo_formacao_mu <- modelo_novo_mu
modelo_formacao_sigma <- modelo_novo_sigma
modelo_formacao_nu <- modelo_novo_nu

# 1. Conferir convergência
sapply(
  list(
    mu = modelo_formacao_mu,
    sigma = modelo_formacao_sigma,
    nu = modelo_formacao_nu
  ),
  function(m) m$converged
)

# 2.Testar FORMAÇÃO simultaneamente em μ e ν

modelo_formacao_mu_nu <- gamlss(
  formula = formula(modelo_formacao_mu, what = "mu"),
  sigma.formula = formula(modelo_formacao_mu, what = "sigma"),
  nu.formula = update(
    formula(modelo_formacao_mu, what = "nu"),
    ~ . + `FORMAÇÃO`
  ),
  family = SN1(),
  data = dados_teste,
  start.from = modelo_formacao_mu,
  control = gamlss.control(
    n.cyc = 500,
    c.crit = 0.0001,
    mu.step = 0.5,
    sigma.step = 0.5,
    nu.step = 0.2,
    trace = FALSE
  )
)

# 3.Conferir ajuste
c(
  Convergiu = modelo_formacao_mu_nu$converged,
  Deviance = deviance(modelo_formacao_mu_nu),
  AIC = GAIC(modelo_formacao_mu_nu, k = 2),
  BIC = GAIC(
    modelo_formacao_mu_nu,
    k = log(modelo_formacao_mu_nu$N)
  )
)

# 4.Teste LR: FORMAÇÃO acrescenta informação em nu
# quando já está presente em mu?
if (
  isTRUE(modelo_formacao_mu_nu$converged) &&
  deviance(modelo_formacao_mu_nu) <=
  deviance(modelo_formacao_mu) + 1e-4
) {
  LR.test(
    modelo_formacao_mu,
    modelo_formacao_mu_nu
  )
}

#Depois, testar FORMAÇÃO também em σ

stopifnot(
  isTRUE(modelo_formacao_mu_nu$converged),
  deviance(modelo_formacao_mu_nu) <=
    deviance(modelo_formacao_mu) + 1e-4
)

modelo_formacao_todos <- gamlss(
  formula = formula(modelo_formacao_mu_nu, what = "mu"),
  sigma.formula = update(
    formula(modelo_formacao_mu_nu, what = "sigma"),
    ~ . + `FORMAÇÃO`
  ),
  nu.formula = formula(modelo_formacao_mu_nu, what = "nu"),
  family = SN1(),
  data = dados_teste,
  start.from = modelo_formacao_mu_nu,
  control = gamlss.control(
    n.cyc = 500,
    c.crit = 0.0001,
    mu.step = 0.5,
    sigma.step = 0.5,
    nu.step = 0.2,
    trace = FALSE
  )
)

# Conferir convergência
modelo_formacao_todos$converged

# Teste LR: contribuição adicional em sigma
if (
  isTRUE(modelo_formacao_todos$converged) &&
  deviance(modelo_formacao_todos) <=
  deviance(modelo_formacao_mu_nu) + 1e-4
) {
  LR.test(
    modelo_formacao_mu_nu,
    modelo_formacao_todos
  )
}

# 5. Comparar os modelos

modelos_comparacao <- list(
  Original = modelo_base,
  Formacao_mu = modelo_formacao_mu,
  Formacao_sigma = modelo_formacao_sigma,
  Formacao_nu = modelo_formacao_nu,
  Formacao_mu_nu = modelo_formacao_mu_nu,
  Formacao_todos = modelo_formacao_todos
)

tabela_comparacao <- do.call(
  rbind,
  lapply(names(modelos_comparacao), function(nome) {
    
    m <- modelos_comparacao[[nome]]
    
    data.frame(
      Modelo = nome,
      Parametros = m$df.fit,
      Deviance = deviance(m),
      AIC = GAIC(m, k = 2),
      BIC = GAIC(m, k = log(m$N)),
      Convergiu = m$converged
    )
  })
)

print(
  tabela_comparacao[
    order(tabela_comparacao$AIC),
  ],
  row.names = FALSE
)


# 6.Modelo completo com FORMAÇÃO nos três parâmetros
modelo_completo <- modelo_formacao_todos

# 7. Modelos reduzidos já disponíveis
modelos_reduzidos <- list(
  sem_sigma = modelo_formacao_mu_nu
)

# 8.Teste de exclusão de FORMAÇÃO em sigma
LR_sigma <- deviance(modelo_formacao_mu_nu) -
  deviance(modelo_completo)

GL_sigma <- modelo_completo$df.fit -
  modelo_formacao_mu_nu$df.fit

p_sigma <- pchisq(
  LR_sigma,
  df = GL_sigma,
  lower.tail = FALSE
)

data.frame(
  Parametro = "sigma",
  LR = LR_sigma,
  GL = GL_sigma,
  p_valor = p_sigma
)

#===============================================================================
# ANÁLISE DE RESÍDUOS
#===============================================================================

# Modelo candidato
modelo_SN1_com_formacao <- modelo_formacao_todos

# Resíduos quantílicos normalizados
residuos_formacao <- residuals(
  modelo_SN1_com_formacao
)

# Estatísticas dos resíduos
c(
  Media = mean(residuos_formacao),
  Variancia = var(residuos_formacao),
  Desvio_padrao = sd(residuos_formacao),
  Assimetria = mean(
    (residuos_formacao - mean(residuos_formacao))^3
  ) / sd(residuos_formacao)^3,
  Curtose = mean(
    (residuos_formacao - mean(residuos_formacao))^4
  ) / sd(residuos_formacao)^4
)

# Diagnóstico gráfico padrão
plot(modelo_SN1_com_formacao)

# Painel com quatro diagnósticos
grafico_SN1_formação <- resid_plots(
  modelo_SN1_com_formacao,
  theme = "new"
)
# Worm plot
wp(modelo_SN1_com_formacao)


# QQ-plot com envelope de 95%
png(
  "QQplot_envelope_SN1_formacao.png",
  width = 1800,
  height = 1400,
  res = 220
)

car::qqPlot(
  residuals(modelo_formacao_todos),
  distribution = "norm",
  envelope = 0.95,
  main = "QQ-plot com envelope — SN1 com Formação",
  xlab = "Quantis teóricos da Normal",
  ylab = "Resíduos quantílicos normalizados",
  pch = 16,
  cex = 0.5,
  col = "black"
)

dev.off()

qq_envelope(modelo_formacao_todos, "QQ-plot com envelope — SN1 com Formação")

# ============================================================
# SALVAR RESULTADOS - SN1 COM FORMAÇÃO
# ============================================================

# 1. Criar subpasta dentro da pasta do modelo final
# Ajuste apenas se sua pasta principal tiver outro nome

pasta_final <- "DOCUMENTOS_ENEM/RESULTADOS/modelo_final_TCC"

pasta_formacao <- file.path(
  pasta_final,
  "modelo_com_formacao"
)

dir.create(
  pasta_formacao,
  recursive = TRUE,
  showWarnings = FALSE
)

# 2. Salvar summary completo
capture.output(
  summary(modelo_formacao_todos),
  file = file.path(
    pasta_formacao,
    "summary_SN1_com_formacao.txt"
  )
)

# 3. Salvar comparação de modelos
write.csv2(
  tabela_comparacao,
  file.path(pasta_formacao, "comparacao_modelos.csv"),
  row.names = FALSE
)

# 4. Salvar testes LR da inclusão isolada
write.csv2(
  tabela_inclusao,
  file.path(pasta_formacao, "testes_LR_formacao.csv"),
  row.names = FALSE
)

# 5. Salvar testes LR sequenciais
testes_sequenciais <- data.frame(
  Inclusao = c(
    "FORMAÇÃO em mu",
    "FORMAÇÃO em nu, após mu",
    "FORMAÇÃO em sigma, após mu e nu"
  ),
  LR = c(
    deviance(modelo_base) -
      deviance(modelo_novo_mu),
    
    deviance(modelo_novo_mu) -
      deviance(modelo_formacao_mu_nu),
    
    deviance(modelo_formacao_mu_nu) -
      deviance(modelo_formacao_todos)
  ),
  GL = c(
    modelo_novo_mu$df.fit - modelo_base$df.fit,
    
    modelo_formacao_mu_nu$df.fit -
      modelo_novo_mu$df.fit,
    
    modelo_formacao_todos$df.fit -
      modelo_formacao_mu_nu$df.fit
  )
)

testes_sequenciais$p_valor <- with(
  testes_sequenciais,
  pchisq(LR, df = GL, lower.tail = FALSE)
)

write.csv2(
  testes_sequenciais,
  file.path(pasta_formacao, "testes_LR_sequenciais.csv"),
  row.names = FALSE
)

# 6. Estatísticas dos resíduos
r <- as.numeric(residuals(modelo_formacao_todos))

estatisticas_residuos <- data.frame(
  N = length(r),
  Media = mean(r),
  Variancia = var(r),
  Desvio_padrao = sd(r),
  Assimetria = mean((r - mean(r))^3) / sd(r)^3,
  Curtose = mean((r - mean(r))^4) / sd(r)^4,
  Minimo = min(r),
  Maximo = max(r)
)

write.csv2(
  estatisticas_residuos,
  file.path(pasta_formacao, "estatisticas_residuos.csv"),
  row.names = FALSE
)

# 7. Salvar QQ-plot com envelope
png(
  file.path(
    pasta_formacao,
    "QQplot_envelope_SN1_formacao.png"
  ),
  width = 1800,
  height = 1400,
  res = 220
)

car::qqPlot(
  r,
  distribution = "norm",
  envelope = 0.95,
  main = "QQ-plot com envelope - SN1 com Formação",
  xlab = "Quantis teóricos da Normal",
  ylab = "Resíduos quantílicos normalizados",
  pch = 16,
  cex = 0.5,
  col = "black"
)

dev.off()

# 8. Salvar worm plot
png(
  file.path(
    pasta_formacao,
    "wormplot_SN1_formacao.png"
  ),
  width = 1800,
  height = 1400,
  res = 220
)

gamlss::wp(modelo_formacao_todos)

dev.off()

# 9. Salvar fórmulas dos três parâmetros
capture.output(
  {
    cat("MODELO SN1 COM FORMAÇÃO\n\n")
    
    cat("MU:\n")
    print(formula(modelo_formacao_todos, what = "mu"))
    
    cat("\nSIGMA:\n")
    print(formula(modelo_formacao_todos, what = "sigma"))
    
    cat("\nNU:\n")
    print(formula(modelo_formacao_todos, what = "nu"))
  },
  file = file.path(pasta_formacao, "formulas_modelo.txt")
)

# 10. Salvar modelo ajustado
saveRDS(
  modelo_formacao_todos,
  file.path(
    pasta_formacao,
    "modelo_SN1_com_formacao.rds"
  )
)

# 11. Conferir os arquivos
list.files(pasta_formacao)

#==============================================================================
par(mfrow = c(2, 2), mar = c(4, 4, 3, 1))

# 1. Resíduos versus localização ajustada
plot(
  fitted(modelo_formacao_todos, what = "mu"),
  r,
  pch = 16,
  cex = 0.35,
  col = adjustcolor("black", alpha.f = 0.15),
  xlab = "Localização ajustada (mu)",
  ylab = "Resíduos quantílicos",
  main = "Resíduos vs. mu"
)
abline(h = 0, col = "red", lty = 2)

# 2. Histograma e densidade normal padrão
hist(
  r,
  breaks = 50,
  probability = TRUE,
  main = "Distribuição dos resíduos",
  xlab = "Resíduos quantílicos"
)
curve(
  dnorm(x),
  add = TRUE,
  col = "red",
  lwd = 2
)

# 3. QQ-plot
qqnorm(r, main = "QQ-plot Normal")
qqline(r, col = "red", lwd = 2)

# 4. Resíduos por índice
plot(
  seq_along(r),
  r,
  pch = 16,
  cex = 0.35,
  col = adjustcolor("black", alpha.f = 0.15),
  xlab = "Índice da observação",
  ylab = "Resíduos quantílicos",
  main = "Resíduos por observação"
)
abline(h = 0, col = "red", lty = 2)

#===============================================================================

# A adequação do modelo SN1 com a variável formação foi avaliada por meio dos 
# resíduos quantílicos normalizados. Observou-se distribuição aproximadamente 
# simétrica e centrada em zero, com boa concordância com a distribuição Normal 
# padrão na região central. O QQ-plot e o worm plot indicaram pequenas discrepâncias
# nas caudas, enquanto o gráfico dos resíduos em função da localização ajustada não 
# evidenciou tendências sistemáticas pronunciadas, sugerindo adequação global 
# satisfatória do ajuste, embora permaneçam desvios nas observações extremas.

#===============================================================================
# PAINEL DE DIAGNÓSTICOS — MODELO SN1 COM FORMAÇÃO
#===============================================================================

# Resíduos quantílicos normalizados
r <- as.numeric(residuals(modelo_formacao_todos))

# Localização ajustada
mu_ajustado <- fitted(modelo_formacao_todos, what = "mu")

par(
  mfrow = c(2, 2),
  mar = c(4.5, 4.5, 3, 1.5),
  mgp = c(2.7, 0.8, 0)
)

#--------------------------------------------------
# 1. RESÍDUOS VERSUS LOCALIZAÇÃO AJUSTADA
#--------------------------------------------------

plot(
  mu_ajustado,
  r,
  pch = 16,
  cex = 0.3,
  col = adjustcolor("black", alpha.f = 0.12),
  xlab = expression("Localização ajustada (" * mu * ")"),
  ylab = "Resíduos quantílicos",
  main = "Resíduos vs. localização"
)

abline(h = 0, col = "red3", lty = 2, lwd = 1.5)

#--------------------------------------------------
# 2. HISTOGRAMA E NORMAL PADRÃO
#--------------------------------------------------

hist(
  r,
  breaks = 50,
  probability = TRUE,
  col = "gray85",
  border = "white",
  main = "Distribuição dos resíduos",
  xlab = "Resíduos quantílicos",
  ylab = "Densidade"
)

curve(
  dnorm(x),
  add = TRUE,
  col = "red3",
  lwd = 2
)

#--------------------------------------------------
# 3. QQ-PLOT COM ENVELOPE DE 95%
#--------------------------------------------------

car::qqPlot(
  r,
  distribution = "norm",
  envelope = 0.95,
  line = "quartiles",
  id = FALSE,
  main = "QQ-plot com envelope de 95%",
  xlab = "Quantis teóricos",
  ylab = "Quantis amostrais",
  col = "gray35",
  pch = 16,
  cex = 0.35
)

#--------------------------------------------------
# 4. WORM PLOT
#--------------------------------------------------

gamlss::wp(
  modelo_formacao_todos,
  xlim.all = 5,
  ylim.all = 0.5
)

# Restaurar layout
par(mfrow = c(1, 1))
