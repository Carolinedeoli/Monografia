
#===============================================================================

# 1. PREPARAÇÃO DOS DADOS E PACOTES

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
# DADOS
#===============================================================================


base   <- "DOCUMENTOS_ENEM/DADOS"
dados <- read_csv(file.path(base, "base_LIMPA_ufes_sisu.csv"), show_col_types = FALSE)

# Variáveis explicativas
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
  "modalidade_agrupada"
)

# Separar as mesmas observações para todos os modelos
dados_modelo <- dados %>%
  dplyr::select(all_of(c("desempenho", variaveis))) %>%
  dplyr::filter(if_all(everything(), ~ !is.na(.x))) %>%
  dplyr::mutate(across(all_of(variaveis), as.factor)) %>%
  droplevels() %>%
  as.data.frame()

#----------------------------

# VERIFICAÇÕES 

# Número de observações da base original
n_original <- nrow(dados)

# Número de observações utilizadas no modelo
n_modelo <- nrow(dados_modelo)

# Número de observações excluídas
n_excluidas <- n_original - n_modelo

# Percentual de exclusão
perc_excluidas <- 100 * n_excluidas / n_original

cat("Observações originais:", n_original, "\n")
cat("Observações no modelo:", n_modelo, "\n")
cat("Observações excluídas:", n_excluidas, "\n")
cat("Percentual excluído:", round(perc_excluidas, 2), "%\n")
dados %>%
  dplyr::select(all_of(c("desempenho", variaveis))) %>%
  dplyr::summarise(
    dplyr::across(
      everything(),
      ~ sum(is.na(.x))
    )
  ) %>%
  tidyr::pivot_longer(
    cols = everything(),
    names_to = "Variavel",
    values_to = "Quantidade_NA"
  ) %>%
  dplyr::arrange(desc(Quantidade_NA))

sapply(
  dados_modelo[variaveis_ref],
  function(x) levels(x)[1]
)





#--------------------------------------------------

# Fórmula completa para mu
formula_mu <- reformulate(
  variaveis,
  response = "desempenho"
)

# Fórmula completa para sigma
formula_sigma <- reformulate(variaveis)

formula_mu
formula_sigma

#===============================================================================

# 2. AJUSTE DE MODELO - NORMAL COMPLETO 

#===============================================================================

#Começamos com todas as variáveis tanto em \(\mu\) quanto em \(\sigma\).


modelo_NO_completo <- gamlss(
  formula = formula_mu,
  sigma.formula = formula_sigma,
  family = NO(),
  data = dados_modelo,
  control = gamlss.control(
    n.cyc = 200,
    trace = FALSE
  )
)

summary(modelo_NO_completo)

# Verificar convergência
modelo_NO_completo$converged

# Critérios de informação
GAIC(modelo_NO_completo, k = 2)  # AIC
GAIC(modelo_NO_completo, k = log(nrow(dados_modelo))) # BIC

# Desvio global
deviance(modelo_NO_completo)

# Teste LR para retirada das variáveis de sigma
teste_sigma <- MASS::dropterm(
  modelo_NO_completo,
  what = "sigma",
  test = "Chisq"
)

print(teste_sigma)


#===============================================================================

# 3.SELEÇÃO BACKWARD PARA A VARIÂNCIA

#===============================================================================

# Em vez de retirar as variáveis manualmente, foi criada uma função que:
# 1. Receba o modelo completo.
# 2. Retire uma variável de sigma por vez, mantendo mu inalterado.
# 3. Compare cada modelo reduzido com o modelo atual pelo teste LR.
# 4. Retire a variável com maior p-valor, desde que seja maior ou igual a 0,05.
# 5. Repita até que todas as retiradas restantes sejam estatisticamente significativas.
# A função também armazenará o histórico dos testes para documentarmos a seleção no TCC.



backward_sigma <- function(modelo, alpha = 0.05,
                           verbose = TRUE) {
  
  if (!isTRUE(modelo$converged)) {
    stop("O modelo inicial não convergiu.")
  }
  
  atual <- modelo
  historico <- list()
  exclusoes <- list()
  etapa <- 1
  
  repeat {
    
    termos <- attr(
      terms(formula(atual, what = "sigma")),
      "term.labels"
    )
    
    if (length(termos) == 0) break
    
    testes <- lapply(termos, function(var) {
      
      # Retirar somente de sigma
      reduzido <- tryCatch(
        update(
          atual,
          formula. = reformulate(
            setdiff(termos, var)
          ),
          what = "sigma"
        ),
        error = function(e) NULL
      )
      
      if (is.null(reduzido)) return(NULL)
      if (!isTRUE(reduzido$converged)) return(NULL)
      
      # Verificar se mu permaneceu igual
      if (!identical(
        formula(atual, what = "mu"),
        formula(reduzido, what = "mu")
      )) {
        stop("A fórmula de mu foi alterada.")
      }
      
      # Teste da razão de verossimilhanças
      lr <- tryCatch(
        LR.test(
          reduzido,
          atual,
          print = FALSE
        ),
        error = function(e) NULL
      )
      
      if (is.null(lr)) return(NULL)
      
      if (!is.finite(lr$p.val) ||
          !is.finite(lr$df) ||
          lr$df <= 0 ||
          !is.finite(lr$chi) ||
          lr$chi < 0) {
        return(NULL)
      }
      
      list(
        variavel = var,
        modelo = reduzido,
        LR = as.numeric(lr$chi),
        gl = as.numeric(lr$df),
        p_valor = as.numeric(lr$p.val),
        AIC = as.numeric(GAIC(reduzido, k = 2))
      )
    })
    
    testes <- Filter(Negate(is.null), testes)
    
    if (length(testes) == 0) {
      warning("Nenhuma retirada pôde ser avaliada.")
      break
    }
    
    tabela <- do.call(
      rbind,
      lapply(testes, function(x) {
        data.frame(
          etapa = etapa,
          variavel = x$variavel,
          LR = x$LR,
          gl = x$gl,
          p_valor = x$p_valor,
          AIC = x$AIC
        )
      })
    )
    
    tabela <- tabela[
      order(-tabela$p_valor),
      ,
      drop = FALSE
    ]
    
    historico[[etapa]] <- tabela
    
    if (verbose) {
      cat("\n========================\n")
      cat("ETAPA:", etapa, "\n")
      print(tabela, row.names = FALSE)
    }
    
    # Variável com maior p-valor
    candidato <- tabela$variavel[1]
    maior_p <- tabela$p_valor[1]
    
    # Critério de parada
    if (maior_p < alpha) {
      if (verbose) {
        cat("\nCritério de parada atingido.\n")
      }
      break
    }
    
    indice <- which(
      vapply(
        testes,
        function(x) x$variavel == candidato,
        logical(1)
      )
    )[1]
    
    atual <- testes[[indice]]$modelo
    
    exclusoes[[etapa]] <- data.frame(
      etapa = etapa,
      variavel_retirada = candidato,
      LR = tabela$LR[1],
      gl = tabela$gl[1],
      p_valor = maior_p,
      AIC_modelo = GAIC(atual, k = 2)
    )
    
    if (verbose) {
      cat(
        "\nRetirada:", candidato,
        "\np-valor:", maior_p,
        "\nAIC:", GAIC(atual, k = 2),
        "\n"
      )
    }
    
    etapa <- etapa + 1
  }
  
  list(
    modelo_final = atual,
    historico = do.call(rbind, historico),
    exclusoes = if (length(exclusoes) > 0)
      do.call(rbind, exclusoes) else data.frame(),
    formula_sigma = formula(atual, what = "sigma")
  )
}


#A função utiliza update(..., what = "sigma"), que permite alterar apenas a fórmula da 
#dispersão, mantendo os demais componentes do GAMLSS.

#===============================================================================

# 4. APLICANDO AO MODELO NORMAL - TODAS AS VARIÁVEIS EM MU

#===============================================================================

selecao_NO <- backward_sigma(
  modelo_NO_completo,
  alpha = 0.05
)

modelo_NO_final <- selecao_NO$modelo_final

# Fórmula final da dispersão
selecao_NO$formula_sigma

# Histórico de todas as comparações
selecao_NO$historico

# Resultado final
summary(modelo_NO_final)

# Comparação entre completo e final
GAIC(
  modelo_NO_completo,
  modelo_NO_final,
  k = 2
)

#===============================================================================
# VERIFICAÇÃO DE RESÍDUOS
#===============================================================================

# QQ-plot com envelope de 95%
car::qqPlot(
  residuals(modelo_NO_final),
  distribution = "norm",
  envelope = 0.95,
  line = "quartiles",
  id = FALSE,
  main = "QQ-plot - Normal selecionada",
  xlab = "Quantis teóricos",
  ylab = "Resíduos quantílicos"
)

# Worm plot
gamlss::wp(modelo_NO_final)

#===============================================================================

# 5. APLICANDO AO MODELO SKEW NORMAL (SN1) - TODAS AS VARIÁVEIS EM MU E ASSIMETRIA CONSTANTE

#===============================================================================

#===============================================================================
# 1. PRIMEIRO AJUSTE DA SN1
#===============================================================================

# Todas as covariáveis em mu e sigma; nu constante.
# Esse ajuste foi utilizado para obter valores iniciais.

SN1_anterior <- gamlss(
  formula = formula_mu,
  sigma.formula = formula_sigma,
  nu.formula = ~ 1,
  family = SN1(),
  data = dados_modelo,
  control = gamlss.control(
    n.cyc = 200,
    trace = FALSE
  )
)

SN1_anterior$converged
#===============================================================================
# 2. EXTRAÇÃO DOS VALORES INICIAIS
#===============================================================================

# Devido a falta de convergência, foi guardado o ajuste anterior do modelo normal skew completo para iniciar o modelo SK1

mu_inicial <- fitted(SN1_anterior, what = "mu")
sigma_inicial <- fitted(SN1_anterior, what = "sigma")
nu_inicial <- fitted(SN1_anterior, what = "nu")

# Conferir se os valores são válidos
stopifnot(
  all(is.finite(mu_inicial)),
  all(is.finite(sigma_inicial) & sigma_inicial > 0),
  all(is.finite(nu_inicial))
)

# Verificar amplitudes
summary(mu_inicial)
summary(sigma_inicial)
summary(nu_inicial)

#===============================================================================
# 3. NOVO AJUSTE DA SN1 COM VALORES INICIAIS
#===============================================================================

modelo_SN1_teste <- gamlss(
  formula = formula_mu,
  sigma.formula = formula_sigma,
  nu.formula = ~ 1,
  family = SN1(),
  data = dados_modelo,
  
  mu.start = mu_inicial,
  sigma.start = sigma_inicial,
  nu.start = nu_inicial,
  
  control = gamlss.control(
    n.cyc = 500,
    c.crit = 0.001,
    mu.step = 0.5,
    sigma.step = 0.5,
    nu.step = 0.2,
    trace = TRUE
  )
)

# Conferir convergência
modelo_SN1_teste$converged
modelo_SN1_teste$iter

#===============================================================================
# 4. DEFINIR O MODELO SN1 COMPLETO
#===============================================================================

stopifnot(isTRUE(modelo_SN1_teste$converged))

modelo_SN1_completo <- modelo_SN1_teste

#===============================================================================
# 5. SELEÇÃO BACKWARD DE SIGMA
#===============================================================================

# Utiliza a função backward_sigma() definida anteriormente.

selecao_SN1 <- backward_sigma(
  modelo_SN1_completo,
  alpha = 0.05
)

# Extrair o modelo final
modelo_SN1_final <- selecao_SN1$modelo_final

# Variáveis excluídas
selecao_SN1$exclusoes

# Fórmula final de sigma
selecao_SN1$formula_sigma

# Resumo
summary(modelo_SN1_final)

# Critérios de informação
GAIC(modelo_SN1_final, k = 2)
GAIC(modelo_SN1_final, k = log(modelo_SN1_final$N))

#===============================================================================
# VERIFICAÇÃO DE RESÍDUOS
#===============================================================================


par(mfrow = c(1, 2))

car::qqPlot(
  residuals(modelo_NO_final),
  distribution = "norm",
  envelope = 0.95,
  line = "quartiles",
  id = FALSE,
  main = "Normal selecionada"
)

car::qqPlot(
  residuals(modelo_SN1_final),
  distribution = "norm",
  envelope = 0.95,
  line = "quartiles",
  id = FALSE,
  main = "Skew Normal selecionada"
)

par(mfrow = c(1, 1))

# Worm plot
gamlss::wp(modelo_SN1_final)

# Estatísticas dos resíduos quantílicos
r <- residuals(modelo_SN1_final)

c(
  media = mean(r),
  variancia = var(r),
  assimetria = mean((r - mean(r))^3) / sd(r)^3,
  curtose = mean((r - mean(r))^4) / sd(r)^4
)


# O modelo GAMLSS com distribuição Skew Normal tipo 1 apresentou melhora substancial 
# em relação ao modelo Normal, tanto pelos critérios AIC e BIC quanto pela análise 
# dos resíduos quantílicos normalizados. O QQ-plot indicou maior proximidade dos 
# quantis observados com os quantis teóricos da distribuição Normal padrão, 
# com pequenos desvios remanescentes nas extremidades. 
# 
# Esses resultados sugerem que a incorporação de assimetria negativa contribuiu
# para uma representação mais adequada da distribuição condicional do desempenho.
#A análise dos resíduos quantílicos normalizados do modelo GAMLSS com distribuição 
# Skew Normal tipo 1 indicou média de -0,0010, variância de 0,9985, assimetria de 
# -0,0081 e curtose de 2,9428, valores próximos aos esperados sob uma distribuição Normal padrão.
# O QQ-plot apresentou elevada proximidade dos resíduos com os quantis teóricos, 
# evidenciando melhora substancial em relação ao modelo Normal.
# Entretanto, o worm plot revelou desvios sistemáticos nas regiões extremas,
# sugerindo que a SN1, embora adequada globalmente, ainda apresenta limitações na 
# representação das caudas. Esses resultados motivam a avaliação da distribuição Skew t tipo 2,
# que incorpora maior flexibilidade para modelar assimetria e comportamento das caudas.

#===============================================================================

# 6. APLICANDO AO MODELO SKEW t (ST2) - TODAS AS VARIÁVEIS EM MU E ASSIMETRIA CONSTANTE E TAU CONSTANTE

#===============================================================================

# Nessas distribuições, nu representa um parâmetro relacionado à assimetria.
# Na ST2, tau também permite controlar o comportamento das caudas. 
# Nesta primeira etapa, ambos permanecerão constantes. 

modelo_ST2_completo <- gamlss(
  formula = formula_mu,
  sigma.formula = formula_sigma,
  nu.formula = ~ 1,
  tau.formula = ~ 1,
  family = ST2(),
  data = dados_modelo,
  control = gamlss.control(
    n.cyc = 500,
    c.crit = 0.001,
    trace = TRUE
  )
)

# Diagnóstico inicial
c(
  convergiu = modelo_ST2_completo$converged,
  iteracoes = modelo_ST2_completo$iter,
  deviance = modelo_ST2_completo$G.deviance,
  AIC = GAIC(modelo_ST2_completo, k = 2)
)

# Parâmetros adicionais
coef(modelo_ST2_completo, what = "nu")
coef(modelo_ST2_completo, what = "tau")

#===============================================================================
# SELEÇÃO BACKWARD EM SIGMA PARA ST2
#===============================================================================

selecao_ST2 <- backward_sigma(
  modelo_ST2_completo,
  alpha = 0.05
)

# Extrair modelo selecionado
modelo_ST2_final <- selecao_ST2$modelo_final

# Verificar variáveis excluídas
selecao_ST2$exclusoes

# Fórmula final de sigma
selecao_ST2$formula_sigma

# Resumo
summary(modelo_ST2_final)

#===============================================================================
# VERIFICAÇÃO DE RESÍDUOS
#===============================================================================

# Resíduos quantílicos normalizados
r_ST2 <- residuals(modelo_ST2_final)

# QQ-plot com envelope de 95%
car::qqPlot(
  r_ST2,
  distribution = "norm",
  envelope = 0.95,
  line = "quartiles",
  id = FALSE,
  pch = 16,
  cex = 0.5,
  main = "QQ-plot - ST2 selecionada",
  xlab = "Quantis teóricos da Normal",
  ylab = "Resíduos quantílicos normalizados"
)

gamlss::wp(
  modelo_ST2_final
)

# Estatísticas dos resíduos da ST2
estatisticas_ST2 <- c(
  media = mean(r_ST2),
  variancia = var(r_ST2),
  assimetria = mean((r_ST2 - mean(r_ST2))^3) /
    sd(r_ST2)^3,
  curtose = mean((r_ST2 - mean(r_ST2))^4) /
    sd(r_ST2)^4
)

round(estatisticas_ST2, 4)

#===============================================================================

# 7. COMPARANDO DISTRIBUIÇÕES

#===============================================================================

estatisticas_residuos <- function(modelo) {
  
  r <- residuals(modelo)
  
  data.frame(
    Media = mean(r),
    Variancia = var(r),
    Assimetria = mean((r - mean(r))^3) / sd(r)^3,
    Curtose = mean((r - mean(r))^4) / sd(r)^4
  )
}

comparacao_residuos <- dplyr::bind_rows(
  Normal = estatisticas_residuos(modelo_NO_final),
  SN1 = estatisticas_residuos(modelo_SN1_final),
  ST2 = estatisticas_residuos(modelo_ST2_final),
  .id = "Distribuicao"
)

print(
  round(
    comparacao_residuos[-1],
    4
  )
)


modelos_finais <- list(
  Normal = modelo_NO_final,
  Skew_Normal = modelo_SN1_final,
  Skew_t_ST2 = modelo_ST2_final
)

comparacao <- do.call(
  rbind,
  lapply(names(modelos_finais), function(nome) {
    
    mod <- modelos_finais[[nome]]
    
    data.frame(
      Distribuicao = nome,
      Parametros = mod$df.fit,
      Deviance = deviance(mod),
      AIC = GAIC(mod, k = 2),
      BIC = GAIC(
        mod,
        k = log(nrow(dados_modelo))
      ),
      Convergiu = mod$converged
    )
  })
)

comparacao <- comparacao %>%
  arrange(AIC)

print(comparacao)

#===============================================================================

# 8. DIAGNOSTICOS DOS RESÍDUOS: QQ-plot com envelope

#===============================================================================


# resíduos quantílicos normalizados, que, sob um modelo GAMLSS corretamente especificado,
# devem apresentar comportamento aproximadamente Normal padrão,
# independentemente da distribuição escolhida para a resposta.


# Função para QQ-plot com envelope
qq_envelope <- function(modelo, titulo) {
  
  residuos <- residuals(modelo)
  
  car::qqPlot(
    residuos,
    distribution = "norm",
    envelope = 0.95,
    line = "quartiles",
    main = titulo,
    xlab = "Quantis teóricos da Normal",
    ylab = "Resíduos quantílicos normalizados",
    id = FALSE,
    pch = 16,
    cex = 0.5
  )
}

# Comparação dos três gráficos juntos


par(mfrow = c(1, 3))

qq_envelope(modelo_NO_final, "Normal")
qq_envelope(modelo_SN1_final, "Skew Normal")
qq_envelope(modelo_ST2_final, "Skew t - ST2")

par(mfrow = c(1, 1))

# Gráficos separados
# qq_envelope(modelo_NO_final, "Normal")
# qq_envelope(modelo_SN1_final, "Skew Normal")
# qq_envelope(modelo_ST2_final, "Skew t - ST2")

#-------------------------------------------------------------------------------
#worm plot, especialmente para investigar desvios de assimetria e curtose

wp(modelo_NO_final)
wp(modelo_SN1_final)
wp(modelo_ST2_final)

#-------------------------------------------------------------------------------

#===============================================================================
# SALVANDO ARQUIVOS SUMMARYS EM TXT
#===============================================================================

# Criar pasta para os resultados
# dir.create(
#   "resultados_gamlss/summaries",
#   recursive = TRUE,
#   showWarnings = FALSE
# )
# 
# # Modelos selecionados
# modelos <- list(
#   Normal = modelo_NO_final,
#   SN1 = modelo_SN1_final,
#   ST2 = modelo_ST2_final
# )
# 
# # Salvar cada summary em um arquivo TXT
# for (nome in names(modelos)) {
#   
#   arquivo <- file.path(
#     "resultados_gamlss/summaries",
#     paste0("summary_", nome, ".txt")
#   )
#   
#   capture.output(
#     summary(modelos[[nome]]),
#     file = arquivo
#   )
# }

# summary_NO_final <- summary(modelo_NO_final)
# summary_SN1_final <- summary(modelo_SN1_final)
# summary_ST2_final <- summary(modelo_ST2_final)


#===============================================================================

# 9. TESTANDO TIRAR ESCOLARIDADE PAI DOS MODELOS, FAZENDO TESTE DE RAZÃO DE VEROSSIMILHANÇA

#===============================================================================

#===============================================================================
# MODELO NORMAL
#===============================================================================

# Retirar escolaridade paterna
NO_sem_pai <- update(
  modelo_NO_final,
  formula. = ~ . - escolaridade_pai,
  what = "sigma"
)

# Retirar tempo de espera
NO_sem_tempo <- update(
  modelo_NO_final,
  formula. = ~ . - TEMPO_ESPERA_AGRUPADO,
  what = "sigma"
)

# Retirar as duas variáveis
NO_sem_ambas <- update(
  NO_sem_pai,
  formula. = ~ . - TEMPO_ESPERA_AGRUPADO,
  what = "sigma"
)


#===============================================================================
#  MODELO SN1
#===============================================================================


# Retirar escolaridade paterna
SN1_sem_pai <- update(
  modelo_SN1_final,
  formula. = ~ . - escolaridade_pai,
  what = "sigma"
)

# Retirar tempo de espera
SN1_sem_tempo <- update(
  modelo_SN1_final,
  formula. = ~ . - TEMPO_ESPERA_AGRUPADO,
  what = "sigma"
)

# Retirar as duas variáveis
SN1_sem_ambas <- update(
  SN1_sem_pai,
  formula. = ~ . - TEMPO_ESPERA_AGRUPADO,
  what = "sigma"
)

#===============================================================================
#  MODELO ST2
#===============================================================================


# Retirar escolaridade paterna
ST2_sem_pai <- update(
  modelo_ST2_final,
  formula. = ~ . - escolaridade_pai,
  what = "sigma"
)

# Retirar tempo de espera
ST2_sem_tempo <- update(
  modelo_ST2_final,
  formula. = ~ . - TEMPO_ESPERA_AGRUPADO,
  what = "sigma"
)

# Retirar as duas variáveis
ST2_sem_ambas <- update(
  ST2_sem_pai,
  formula. = ~ . - TEMPO_ESPERA_AGRUPADO,
  what = "sigma"
)

#===============================================================================
# CONVERGÊNCIA DOS MODELOS
#===============================================================================

modelos_reduzidos <- list(
  NO_sem_pai = NO_sem_pai,
  NO_sem_tempo = NO_sem_tempo,
  NO_sem_ambas = NO_sem_ambas,
  SN1_sem_pai = SN1_sem_pai,
  SN1_sem_tempo = SN1_sem_tempo,
  SN1_sem_ambas = SN1_sem_ambas,
  ST2_sem_pai = ST2_sem_pai,
  ST2_sem_tempo = ST2_sem_tempo,
  ST2_sem_ambas = ST2_sem_ambas
)

sapply(modelos_reduzidos, function(m) {
  c(
    convergiu = m$converged,
    iteracoes = m$iter,
    AIC = GAIC(m, k = 2)
  )
})

#===============================================================================
# RAZÃO DE VEROSSIMILHANÇA
#===============================================================================

comparar_LR <- function(completo, reduzido, nome) {
  
  if (!isTRUE(completo$converged) ||
      !isTRUE(reduzido$converged)) {
    stop(paste("Modelo não convergente:", nome))
  }
  
  if (completo$N != reduzido$N) {
    stop("Os modelos usam tamanhos amostrais diferentes.")
  }
  
  gl <- completo$df.fit - reduzido$df.fit
  
  if (gl <= 0) {
    stop("Verifique o aninhamento dos modelos.")
  }
  
  LR <- deviance(reduzido) - deviance(completo)
  
  if (LR < -1e-5) {
    warning(paste(
      "Desvio reduzido menor que o completo:",
      nome,
      "- verificar convergência e ótimo encontrado."
    ))
  }
  
  p_valor <- pchisq(
    max(LR, 0),
    df = gl,
    lower.tail = FALSE
  )
  
  data.frame(
    Comparacao = nome,
    LR = LR,
    GL = gl,
    p_valor = p_valor,
    AIC_completo = GAIC(completo, k = 2),
    AIC_reduzido = GAIC(reduzido, k = 2),
    BIC_reduzido = GAIC(
      reduzido,
      k = log(reduzido$N)
    ),
    Decisao = ifelse(
      p_valor >= 0.05,
      "Retirada admissível",
      "Manter variável"
    )
  )
}

#===============================================================================
# TESTES MODELO NORMAL
#===============================================================================

testes_NO <- rbind(
  comparar_LR(
    modelo_NO_final,
    NO_sem_pai,
    "NO: retirar pai"
  ),
  comparar_LR(
    modelo_NO_final,
    NO_sem_tempo,
    "NO: retirar tempo"
  ),
  comparar_LR(
    NO_sem_pai,
    NO_sem_ambas,
    "NO: retirar tempo após pai"
  ),
  comparar_LR(
    modelo_NO_final,
    NO_sem_ambas,
    "NO: retirar ambas conjuntamente"
  ),
  comparar_LR(
    NO_sem_tempo,
    NO_sem_ambas,
    "NO: retirar pai após tempo"
  )
)

print(testes_NO)

#===============================================================================
#TESTES MODELO SN1
#===============================================================================


testes_SN1 <- rbind(
  comparar_LR(
    modelo_SN1_final,
    SN1_sem_pai,
    "SN1: retirar pai"
  ),
  comparar_LR(
    modelo_SN1_final,
    SN1_sem_tempo,
    "SN1: retirar tempo"
  ),
  comparar_LR(
    SN1_sem_pai,
    SN1_sem_ambas,
    "SN1: retirar tempo após pai"
  ),
  comparar_LR(
    modelo_SN1_final,
    SN1_sem_ambas,
    "SN1: retirar ambas conjuntamente"
  ),
  comparar_LR(
    SN1_sem_tempo,
    SN1_sem_ambas,
    "SN1: retirar pai após tempo"
  )
)

print(testes_SN1)

#===============================================================================
# TESTES MODELO ST2
#===============================================================================

testes_ST2 <- rbind(
  comparar_LR(
    modelo_ST2_final,
    ST2_sem_pai,
    "ST2: retirar pai"
  ),
  comparar_LR(
    modelo_ST2_final,
    ST2_sem_tempo,
    "ST2: retirar tempo"
  ),
  comparar_LR(
    ST2_sem_pai,
    ST2_sem_ambas,
    "ST2: retirar tempo após pai"
  ),
  comparar_LR(
    modelo_ST2_final,
    ST2_sem_ambas,
    "ST2: retirar ambas conjuntamente"
  ),
  comparar_LR(
    ST2_sem_tempo,
    ST2_sem_ambas,
    "ST2: retirar pai após tempo"
  )
)

print(testes_ST2)

#===============================================================================
# TABELA FINAL DOS TESTES DE VEROSSIMILHANÇA
#===============================================================================

tabela_LR <- bind_rows(
  testes_NO,
  testes_SN1,
  testes_ST2
) %>%
  mutate(
    across(
      c(LR, p_valor, AIC_completo,
        AIC_reduzido, BIC_reduzido),
      ~ round(.x, 4)
    )
  )

print(tabela_LR, row.names = FALSE)

# Salvar os resultados
# dir.create(
#   "resultados_gamlss",
#   showWarnings = FALSE
# )
# 
# write.csv(
#   tabela_LR,
#   "resultados_gamlss/testes_LR_retiradas_manuais.csv",
#   row.names = FALSE
# )


#Nos modelos iniciais NO, SN1 e ST2, os testes de razão de verossimilhanças 
#indicaram a permanência da escolaridade paterna e do tempo de espera.


#===============================================================================

# 10. MODELAGEM DO PARÂMETRO MU 

#===============================================================================


#A seleção das covariáveis do parâmetro de localização ($\mu$) foi realizada 
#inicialmente pelo procedimento backward, utilizando o Critério de Informação de Akaike (AIC),
#sem que fossem identificadas exclusões que melhorassem o ajuste dos modelos NO, SN1 e ST2. 
#Posteriormente, foram aplicados testes da razão de verossimilhanças para avaliar
#a contribuição individual das variáveis explicativas. Os resultados sustentaram 
#a manutenção das covariáveis em $\mu$ nessa etapa inicial da modelagem.

#===============================================================================
# TESTANDO PRIMEIRO stepwise AIC
#===============================================================================

# Seleção em mu por AIC
step_mu_NO <- stepGAIC(
  modelo_NO_final,
  what = "mu",
  direction = "backward",
  k = 2,
  trace = TRUE
)

step_mu_SN1 <- stepGAIC(
  modelo_SN1_final,
  what = "mu",
  direction = "backward",
  k = 2,
  trace = TRUE
)

step_mu_ST2 <- stepGAIC(
  modelo_ST2_final,
  what = "mu",
  direction = "backward",
  k = 2,
  trace = TRUE
)

# Fórmulas selecionadas
formula(step_mu_NO, what = "mu")
formula(step_mu_SN1, what = "mu")
formula(step_mu_ST2, what = "mu")


#===============================================================================
# COMPARAÇÕES
#===============================================================================

# Histórico da seleção pelo AIC
step_mu_NO$anova
step_mu_SN1$anova
step_mu_ST2$anova

# Comparar os AICs antes e depois
GAIC(modelo_NO_final, step_mu_NO, k = 2)
GAIC(modelo_SN1_final, step_mu_SN1, k = 2)
GAIC(modelo_ST2_final, step_mu_ST2, k = 2)

#===============================================================================
# TESTAR VARIÁVEIS DE MU PELA RAZÃO DE VEROSSIMILHANÇA
#===============================================================================

# Testes LR de exclusão individual em mu

teste_mu_NO <- MASS::dropterm(
  modelo_NO_final,
  what = "mu",
  test = "Chisq"
)

teste_mu_SN1 <- MASS::dropterm(
  modelo_SN1_final,
  what = "mu",
  test = "Chisq"
)

teste_mu_ST2 <- MASS::dropterm(
  modelo_ST2_final,
  what = "mu",
  test = "Chisq"
)

# Resultados
print(teste_mu_NO)
print(teste_mu_SN1)
print(teste_mu_ST2)

#===============================================================================
# SALVANDO RESULTADOS PARA DOCUMENTO
#===============================================================================

#comparação

# dir.create(
#   "resultados_gamlss/tabelas",
#   recursive = TRUE,
#   showWarnings = FALSE
# )

modelos_finais <- list(
  Normal = modelo_NO_final,
  SN1 = modelo_SN1_final,
  ST2 = modelo_ST2_final
)

tabela_modelos <- bind_rows(
  lapply(names(modelos_finais), function(nome) {
    m <- modelos_finais[[nome]]
    
    data.frame(
      Distribuicao = nome,
      N = m$N,
      Parametros = m$df.fit,
      Deviance = deviance(m),
      AIC = GAIC(m, k = 2),
      BIC = GAIC(m, k = log(m$N)),
      Convergencia = m$converged
    )
  })
)

# write.csv2(
#   tabela_modelos,
#   "resultados_gamlss/tabelas/01_comparacao_modelos.csv",
#   row.names = FALSE
# )


# Histórico de seleções


# Todas as comparações avaliadas pelo backward
historico_sigma <- bind_rows(
  Normal = selecao_NO$historico,
  SN1 = selecao_SN1$historico,
  ST2 = selecao_ST2$historico,
  .id = "Distribuicao"
)

# Somente as exclusões efetivamente realizadas
exclusoes_sigma <- bind_rows(
  Normal = selecao_NO$exclusoes,
  SN1 = selecao_SN1$exclusoes,
  ST2 = selecao_ST2$exclusoes,
  .id = "Distribuicao"
)

# write.csv2(
#   historico_sigma,
#   "resultados_gamlss/tabelas/02_historico_backward_sigma.csv",
#   row.names = FALSE
# )
# 
# write.csv2(
#   exclusoes_sigma,
#   "resultados_gamlss/tabelas/03_exclusoes_sigma.csv",
#   row.names = FALSE
# )

# testes em mu


tabela_mu <- bind_rows(
  Normal = as.data.frame(teste_mu_NO),
  SN1 = as.data.frame(teste_mu_SN1),
  ST2 = as.data.frame(teste_mu_ST2),
  .id = "Distribuicao"
)

# write.csv2(
#   tabela_mu,
#   "resultados_gamlss/tabelas/04_testes_LR_mu.csv",
#   row.names = TRUE
# )

# métricas


tabela_retiradas <- bind_rows(
  Normal = testes_NO,
  SN1 = testes_SN1,
  ST2 = testes_ST2,
  .id = "Distribuicao"
)

# write.csv2(
#   tabela_retiradas,
#   "resultados_gamlss/tabelas/05_testes_LR_manuais.csv",
#   row.names = FALSE
# )

#===============================================================================
# ANÁLISE DE RESÍDUOS
#===============================================================================
estatisticas_residuos <- function(modelo) {
  r <- as.numeric(residuals(modelo))
  
  data.frame(
    Media = mean(r),
    Variancia = var(r),
    Assimetria = mean((r - mean(r))^3) / sd(r)^3,
    Curtose = mean((r - mean(r))^4) / sd(r)^4
  )
}

tabela_residuos <- bind_rows(
  Normal = estatisticas_residuos(modelo_NO_final),
  SN1 = estatisticas_residuos(modelo_SN1_final),
  ST2 = estatisticas_residuos(modelo_ST2_final),
  .id = "Distribuicao"
)

# write.csv2(
#   tabela_residuos,
#   "resultados_gamlss/tabelas/06_estatisticas_residuos.csv",
#   row.names = FALSE
# )

#===============================================================================
# GRÁFICOS
#===============================================================================

# Painel com quatro diagnósticos
grafico_SN1 <- resid_plots(
  modelo_SN1_final,
  theme = "new"
)

print(grafico_SN1)

# # Criar pasta
# dir.create(
#   "resultados_gamlss/graficos",
#   recursive = TRUE,
#   showWarnings = FALSE
# )
# 
# # Salvar em PDF
# ggsave(
#   "resultados_gamlss/graficos/Diagnosticos_SN1.pdf",
#   plot = grafico_SN1,
#   width = 12,
#   height = 9
# )
# 
# # Salvar em PNG
# ggsave(
#   "resultados_gamlss/graficos/Diagnosticos_SN1.png",
#   plot = grafico_SN1,
#   width = 12,
#   height = 9,
#   dpi = 300
# )

#=========================================================================


modelos <- list(
  Normal = modelo_NO_final,
  SN1 = modelo_SN1_final,
  ST2 = modelo_ST2_final
)

for (nome in names(modelos)) {
  
  grafico <- resid_plots(
    modelos[[nome]],
    theme = "new"
  )
  
  ggsave(
    filename = file.path(
      "resultados_gamlss/graficos",
      paste0("Diagnosticos_", nome, ".png")
    ),
    plot = grafico,
    width = 12,
    height = 9
  )
}

resid_plots(modelo_SN1_final, theme = "new")
resid_plots(modelo_NO_final, theme = "new")


# # Criar a pasta, caso não exista
# dir.create(
#   "resultados_gamlss/summaries",
#   recursive = TRUE,
#   showWarnings = FALSE
# )
# 
# # Salvar o summary da ST2
# capture.output(
#   summary(modelo_ST2_final),
#   file = "resultados_gamlss/summaries/summary_ST2.txt"
# )
# 
# # Salvar objeto existente em TXT
# capture.output(
#   print(summary_ST2_final),
#   file = "resultados_gamlss/summaries/summary_ST2_final.txt"
# )


#============================================================================

# 11. MODELANDO A ASSIMETRIA EM SN1

#============================================================================

#Começando como referência, o modelo final de sigma e todas as covariáveis em mu


# Verificação inicial para verificar a viabilidade de contribuição das variáveis na modelagem da assimetria.

# Essa abordagem funciona como uma triagem inicial, permitindo identificar covariáveis que, individualmente, melhoram o ajuste em relação ao modelo com assimetria constante.


# Modelo de referência
modelo_nu0 <- modelo_SN1_final

# Verificar sua estrutura atual
formula(modelo_nu0, what = "mu")
formula(modelo_nu0, what = "sigma")
formula(modelo_nu0, what = "nu")

# Variáveis candidatas
variaveis_nu <- c(
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
  "modalidade_agrupada"
)


# Ajustar cada candidata individualmente em nu
modelos_nu <- list()

for (var in variaveis_nu) {
  
  cat("\nTestando:", var, "\n")
  flush.console()
  
  ajuste <- tryCatch(
    update(
      modelo_nu0,
      formula. = reformulate(var),
      what = "nu"
    ),
    error = function(e) {
      message("Erro em ", var, ": ", conditionMessage(e))
      NULL
    }
  )
  
  modelos_nu[[var]] <- ajuste
}

resultados_nu <- lapply(
  names(modelos_nu),
  function(var) {
    
    mod <- modelos_nu[[var]]
    
    if (is.null(mod) || !isTRUE(mod$converged)) {
      return(data.frame(
        Variavel = var,
        LR = NA_real_,
        GL = NA_real_,
        p_valor = NA_real_,
        AIC = NA_real_,
        BIC = NA_real_,
        Convergiu = FALSE
      ))
    }
    
    gl <- mod$df.fit - modelo_nu0$df.fit
    
    lr <- deviance(modelo_nu0) - deviance(mod)
    
    p <- if (gl > 0 && lr >= 0) {
      pchisq(lr, df = gl, lower.tail = FALSE)
    } else {
      NA_real_
    }
    
    data.frame(
      Variavel = var,
      LR = lr,
      GL = gl,
      p_valor = p,
      AIC = GAIC(mod, k = 2),
      BIC = GAIC(mod, k = log(mod$N)),
      Convergiu = TRUE
    )
  }
)

tabela_nu <- do.call(rbind, resultados_nu)

tabela_nu <- tabela_nu[
  order(tabela_nu$p_valor, na.last = TRUE),
]

print(tabela_nu, row.names = FALSE)

#===============================================================================
#   TESTANDO CENTRO E MODALIDADE DE INGRESSO NA ASSIMETRIA
#===============================================================================

# Com os resultados preliminares foram testados modelos iniciais para saber o comportamento e se os modelos convergiam

# Durante o ajuste dos modelos SN1 com assimetria variável,
# foram encontradas dificuldades numéricas de otimização. 


# Modelo com CENTRO em nu
modelo_SN1_nu_centro <- modelos_nu[["CENTRO"]]

# Confirmar convergência
modelo_SN1_nu_centro$converged

# Adicionar modalidade de ingresso a nu
modelo_SN1_nu_centro_modalidade <- update(
  modelo_SN1_nu_centro,
  formula. = ~ CENTRO + modalidade_agrupada,
  what = "nu"
)

# Verificar convergência
modelo_SN1_nu_centro_modalidade$converged

# Verificar fórmula
formula(
  modelo_SN1_nu_centro_modalidade,
  what = "nu"
)

# Verificando convergência dos modelos
stopifnot(
  modelo_SN1_final$converged,
  modelo_SN1_nu_centro$converged,
  modelo_SN1_nu_centro_modalidade$converged
)

#===============================================================================
# COMPARANDO MODELOS INICIAIS 
#===============================================================================

# 1. Modelo original vs CENTRO
LR.test(
  modelo_SN1_final,
  modelo_SN1_nu_centro
)

# 2. CENTRO vs CENTRO + modalidade
LR.test(
  modelo_SN1_nu_centro,
  modelo_SN1_nu_centro_modalidade
)


# 3. Original vs CENTRO + modalidade
LR.test(
  modelo_SN1_final,
  modelo_SN1_nu_centro_modalidade
)

# Comparar AIC e BIC
GAIC(
  modelo_SN1_final,
  modelo_SN1_nu_centro,
  modelo_SN1_nu_centro_modalidade,
  k = 2
)

GAIC(
  modelo_SN1_final,
  modelo_SN1_nu_centro,
  modelo_SN1_nu_centro_modalidade,
  k = log(nrow(dados_modelo))
)

#============================================================================

# 12. MODELANDO A ASSIMETRIA USANDO MESMA TÉCNICA DO SIGMA

#============================================================================


# RESUMO DAS ETAPAS APLICADAS

# 1. Ajustar a SN1 com todas as 12 variáveis em \(\nu\).
# 2. Tentar uma primeira seleção backward por testes LR.
# 3. Investigar inconsistências numéricas encontradas nos ajustes.
# 4. Reajustar o modelo completo com melhores valores iniciais.
# 5. Executar um backward mais robusto, chegando às quatro variáveis selecionadas.

#===============================================================================
# 1. AJUSTAR SN1 COM TODAS AS VARIÁVEIS EM NU
#===============================================================================

#--------------------------------------------------------------------------------------------------------------------
# Como fo encontrado problemas numéricos ao estimar a assimetria,
# foi utilizado o modelo com CENTRO em \(\nu\) como ponto de partida para os valores iniciais.

#Esse modelo passou a permitir que localização, escala e assimetria variassem de acordo com características dos ingressantes.
#--------------------------------------------------------------------------------------------------------------------

# Fórmula completa de nu
formula_nu_completa <- ~
  FAIXA_ETARIA_AGRUPADA +
  TP_SEXO +
  TP_COR_RACA +
  escolaridade_pai +
  escolaridade_mae +
  renda_agrupada +
  TP_LINGUA +
  internet +
  DEP_ADM_AGRUPADA +
  TEMPO_ESPERA_AGRUPADO +
  CENTRO +
  modalidade_agrupada

# Modelo completo
modelo_SN1_nu_completo <- gamlss(
  formula = formula_mu,
  sigma.formula = selecao_SN1$formula_sigma,
  nu.formula = formula_nu_completa,
  family = SN1(),
  data = dados_modelo,
  
  start.from = modelo_SN1_nu_centro,
  
  control = gamlss.control(
    n.cyc = 500,
    c.crit = 0.001,
    mu.step = 0.5,
    sigma.step = 0.5,
    nu.step = 0.2,
    trace = TRUE
  )
)

# Conferir ajuste
modelo_SN1_nu_completo$converged
modelo_SN1_nu_completo$iter
deviance(modelo_SN1_nu_completo)
GAIC(modelo_SN1_nu_completo, k = 2)


# Verificar consistência do ajuste completo
stopifnot(
  isTRUE(modelo_SN1_nu_completo$converged)
)

deviance(modelo_SN1_nu_completo)
deviance(modelo_SN1_nu_centro)

# O completo não deveria apresentar desvio maior
deviance(modelo_SN1_nu_completo) <=
  deviance(modelo_SN1_nu_centro) + 1e-4

#===============================================================================
#   2. EXECUTAR BACKWARD EM NU
#===============================================================================

#--------------------------------------------------------------------------------------------------------------------

# A função segue mesmo esquema realizado em sigma e verifica se os modelos convergiram e se utilizam a mesma amostra.

# Foi necessária a realização de um novo método posteriormente, uma vez que:

# Foi identificado um problema na otimização: alguns ajustes reduzidos apresentavam valores de deviance inferiores aos obtidos para o modelo completo.
# 
# Essa situação não deveria ocorrer quando os modelos são corretamente ajustados em seus respectivos máximos de verossimilhança.
# 
# Assim, o problema não era necessariamente a escolha do backward, mas a confiabilidade numérica dos ajustes usados para calcular os testes LR.
# 
# O procedimento precisava ser revisado antes de considerarmos sua seleção definitiva.

#--------------------------------------------------------------------------------------------------------------------


# # FUNÇÃO BACKARD
# backward_nu <- function(modelo, alpha = 0.05,
#                         verbose = TRUE) {
#   
#   if (!isTRUE(modelo$converged)) {
#     stop("O modelo inicial não convergiu.")
#   }
#   
#   atual <- modelo
#   historico <- list()
#   exclusoes <- list()
#   etapa <- 1
#   
#   repeat {
#     
#     termos <- attr(
#       terms(formula(atual, what = "nu")),
#       "term.labels"
#     )
#     
#     if (length(termos) == 0) break
#     
#     testes <- lapply(termos, function(var) {
#       
#       if (verbose) {
#         cat(
#           "\nEtapa:", etapa,
#           "| Testando retirada de:", var, "\n"
#         )
#         flush.console()
#       }
#       
#       # Retirar apenas um termo de nu
#       reduzido <- tryCatch(
#         update(
#           atual,
#           formula. = reformulate(
#             setdiff(termos, var)
#           ),
#           what = "nu"
#         ),
#         error = function(e) {
#           message(conditionMessage(e))
#           NULL
#         }
#       )
#       
#       if (is.null(reduzido)) return(NULL)
#       if (!isTRUE(reduzido$converged)) return(NULL)
#       
#       # Conferir tamanho amostral e fórmulas fixas
#       if (reduzido$N != atual$N) return(NULL)
#       
#       if (!identical(
#         formula(atual, what = "mu"),
#         formula(reduzido, what = "mu")
#       )) return(NULL)
#       
#       if (!identical(
#         formula(atual, what = "sigma"),
#         formula(reduzido, what = "sigma")
#       )) return(NULL)
#       
#       gl <- atual$df.fit - reduzido$df.fit
#       
#       # LR = deviance reduzido - deviance completo
#       LR <- deviance(reduzido) - deviance(atual)
#       
#       if (!is.finite(LR) || gl <= 0) return(NULL)
#       
#       # LR negativo indica possível problema numérico
#       if (LR < -1e-4) {
#         warning(paste(
#           "Possível problema de otimização ao retirar:",
#           var
#         ))
#         return(NULL)
#       }
#       
#       LR <- max(LR, 0)
#       
#       p_valor <- pchisq(
#         LR,
#         df = gl,
#         lower.tail = FALSE
#       )
#       
#       list(
#         variavel = var,
#         modelo = reduzido,
#         LR = LR,
#         gl = gl,
#         p_valor = p_valor,
#         AIC = GAIC(reduzido, k = 2),
#         BIC = GAIC(reduzido, k = log(reduzido$N))
#       )
#     })
#     
#     testes_validos <- Filter(Negate(is.null), testes)
#     
#     # Não tomar decisões com testes faltantes
#     if (length(testes_validos) != length(termos)) {
#       warning(
#         "Nem todas as exclusões foram avaliadas ",
#         "validamente. Seleção interrompida."
#       )
#       break
#     }
#     
#     tabela <- do.call(
#       rbind,
#       lapply(testes_validos, function(x) {
#         data.frame(
#           Etapa = etapa,
#           Variavel = x$variavel,
#           LR = x$LR,
#           GL = x$gl,
#           p_valor = x$p_valor,
#           AIC = x$AIC,
#           BIC = x$BIC
#         )
#       })
#     )
#     
#     tabela <- tabela[
#       order(-tabela$p_valor),
#       ,
#       drop = FALSE
#     ]
#     
#     historico[[etapa]] <- tabela
#     
#     if (verbose) {
#       cat("\n========== ETAPA", etapa, "==========\n")
#       print(tabela, row.names = FALSE)
#     }
#     
#     candidato <- tabela$Variavel[1]
#     maior_p <- tabela$p_valor[1]
#     
#     if (maior_p < alpha) {
#       if (verbose) {
#         cat("\nCritério de parada atingido.\n")
#       }
#       break
#     }
#     
#     indice <- which(vapply(
#       testes_validos,
#       function(x) x$variavel == candidato,
#       logical(1)
#     ))[1]
#     
#     atual <- testes_validos[[indice]]$modelo
#     
#     exclusoes[[etapa]] <- data.frame(
#       Etapa = etapa,
#       Variavel_retirada = candidato,
#       LR = tabela$LR[1],
#       GL = tabela$GL[1],
#       p_valor = maior_p,
#       AIC = GAIC(atual, k = 2),
#       BIC = GAIC(atual, k = log(atual$N))
#     )
#     
#     if (verbose) {
#       cat("\nVariável retirada:", candidato, "\n")
#     }
#     
#     etapa <- etapa + 1
#   }
#   
#   list(
#     modelo_final = atual,
#     historico = if (length(historico))
#       do.call(rbind, historico) else data.frame(),
#     exclusoes = if (length(exclusoes))
#       do.call(rbind, exclusoes) else data.frame(),
#     formula_nu = formula(atual, what = "nu")
#   )
# }
# 
# #Como o modelo completo convergiu e apresentou desvio menor que os 
# #modelos reduzidos anteriores, foi dado inicio a seleção:
# 
# 
# # Verificar novamente a convergência
# stopifnot(
#   isTRUE(modelo_SN1_nu_completo$converged)
# )
# 
# # Executar seleção backward pelo LR
# selecao_nu_SN1 <- backward_nu(
#   modelo_SN1_nu_completo,
#   alpha = 0.05,
#   verbose = TRUE
# )
# 
# # Modelo selecionado
# modelo_SN1_nu_final <- selecao_nu_SN1$modelo_final
# 
# # Histórico das exclusões
# selecao_nu_SN1$exclusoes
# 
# # Fórmula final de nu
# selecao_nu_SN1$formula_nu
# 
# 
# # Histórico completo dos testes
# selecao_nu_SN1$historico
# 
# # Resumo
# summary(modelo_SN1_nu_final)
# 
# #===============================================================================
# # 3. Comparar com a SN1 original 
# #===============================================================================
# 
# # Comparação de critérios
# GAIC(
#   modelo_SN1_final,
#   modelo_SN1_nu_final,
#   k = 2
# )
# 
# GAIC(
#   modelo_SN1_final,
#   modelo_SN1_nu_final,
#   k = log(nrow(dados_modelo))
# )
# 
# 
# # Teste da razão de verossimilhança
# 
# 
# if (
#   length(attr(
#     terms(formula(modelo_SN1_nu_final, what = "nu")),
#     "term.labels"
#   )) > 0 &&
#   deviance(modelo_SN1_nu_final) <=
#   deviance(modelo_SN1_final) + 1e-4
# ) {
#   LR.test(
#     modelo_SN1_final,
#     modelo_SN1_nu_final
#   )
# }
# 
# #===============================================================================
# # 4. ANÁLISE DE RESÍDUOS
# #===============================================================================
# 
# car::qqPlot(
#   residuals(modelo_SN1_nu_final),
#   distribution = "norm",
#   envelope = 0.95,
#   line = "quartiles",
#   id = FALSE,
#   main = "SN1 - Assimetria modelada"
# )
# 
# gamlss::wp(modelo_SN1_nu_final)
# 

#===============================================================================
# TESTANDO OUTRO PROCEDIMENTO
#===============================================================================

#--------------------------------------------------------------------------------------------------------------------

# O dropterm() avalia a exclusão individual de cada variável, comparando o modelo completo com modelos reduzidos.
# 
# Ele tem uma finalidade semelhante à primeira etapa da função backward, mas não executa sozinho todas as exclusões sucessivas.
# 
# Durante esses testes, a exclusão de modalidade_agrupada apresentou um comportamento inesperado.
# 
# Em particular, o modelo sem modalidade em \(\nu\) conseguiu um deviance menor que o modelo completo inicialmente ajustado.

#--------------------------------------------------------------------------------------------------------------------

# Testar a retirada de cada variável de nu
teste_nu_completo <- MASS::dropterm(
  modelo_SN1_nu_completo,
  what = "nu",
  test = "Chisq"
)

print(teste_nu_completo)


# Modelo sem modalidade em nu
modelo_nu_sem_modalidade <- update(
  modelo_SN1_nu_completo,
  formula. = ~ . - modalidade_agrupada,
  what = "nu"
)

# Conferir convergência e deviance
modelo_nu_sem_modalidade$converged
deviance(modelo_nu_sem_modalidade)
deviance(modelo_SN1_nu_completo)

#===============================================================================
# MODELO SEM MODALIDADE COMO PONTO INICIAL
#===============================================================================

#Para tentar encontrar uma solução melhor, utilizamos o modelo sem modalidade como referência inicial.

modelo_SN1_nu_completo_2 <- gamlss(
  formula = formula_mu,
  sigma.formula = selecao_SN1$formula_sigma,
  nu.formula = formula_nu_completa,
  family = SN1(),
  data = dados_modelo,
  
  # Inicialização pelo modelo sem modalidade
  start.from = modelo_nu_sem_modalidade,
  
  control = gamlss.control(
    n.cyc = 500,
    c.crit = 0.0001,
    mu.step = 0.5,
    sigma.step = 0.5,
    nu.step = 0.2,
    trace = TRUE
  )
)

# Conferir resultado
c(
  convergiu = modelo_SN1_nu_completo_2$converged,
  iteracoes = modelo_SN1_nu_completo_2$iter,
  deviance = deviance(modelo_SN1_nu_completo_2),
  AIC = GAIC(modelo_SN1_nu_completo_2, k = 2)
)

# Modelo completo com melhor ajuste
modelo_SN1_nu_completo <- modelo_SN1_nu_completo_2

# Testes LR para cada variável de nu
teste_nu_2 <- MASS::dropterm(
  modelo_SN1_nu_completo,
  what = "nu",
  test = "Chisq"
)

print(teste_nu_2)

#===============================================================================
# TENTANDO AJUSTAR NOVAMENTE A ASSIMETRIA
#===============================================================================

#--------------------------------------------------------------------------------------------------------------------

# Foi encontrado um ajuste completo melhor, porém isso não resolve automaticamente os problemas de otimização de todos os modelos reduzidos.

#Durante o backward, cada retirada de variável exige um novo ajuste. 
#Esses modelos também poderiam convergir para soluções numericamente inferiores, comprometendo as comparações LR.

#Foi então desenvolvida uma função mais robusta para ajustar cada modelo reduzido.

#--------------------------------------------------------------------------------------------------------------------

# Melhor ajuste completo obtido anteriormente
modelo_nu_inicial <- modelo_SN1_nu_completo_2

stopifnot(isTRUE(modelo_nu_inicial$converged))

# Conferir fórmulas
formula(modelo_nu_inicial, what = "mu")
formula(modelo_nu_inicial, what = "sigma")
formula(modelo_nu_inicial, what = "nu")

# Conferir indicadores
c(
  N = modelo_nu_inicial$N,
  Parametros = modelo_nu_inicial$df.fit,
  Deviance = deviance(modelo_nu_inicial),
  AIC = GAIC(modelo_nu_inicial, k = 2)
)

#===============================================================================
# FUNÇÕES USADAS
#===============================================================================


#Em vez de utilizar somente update(), essa função passou a realizar duas tentativas de ajuste para cada exclusão.
#Em seguida, a função mantém somente os modelos que convergiram e apresentaram deviance finito
#Por fim, seleciona o ajuste com menor deviance


ajustar_reduzido_nu <- function(modelo, variavel) {
  
  termos <- attr(
    terms(formula(modelo, what = "nu")),
    "term.labels"
  )
  
  nova_formula <- reformulate(
    setdiff(termos, variavel)
  )
  
  # Primeira tentativa: update convencional
  tentativa1 <- tryCatch(
    update(
      modelo,
      formula. = nova_formula,
      what = "nu"
    ),
    error = function(e) NULL
  )
  
  # Segunda tentativa: reinicializar pelo modelo atual
  tentativa2 <- tryCatch(
    gamlss(
      formula = formula(modelo, what = "mu"),
      sigma.formula = formula(modelo, what = "sigma"),
      nu.formula = nova_formula,
      family = SN1(),
      data = dados_modelo,
      start.from = modelo,
      control = gamlss.control(
        n.cyc = 500,
        c.crit = 0.0001,
        mu.step = 0.5,
        sigma.step = 0.5,
        nu.step = 0.2,
        trace = FALSE
      )
    ),
    error = function(e) NULL
  )
  
  candidatos <- Filter(
    function(x) {
      !is.null(x) &&
        isTRUE(x$converged) &&
        is.finite(deviance(x))
    },
    list(tentativa1, tentativa2)
  )
  
  if (length(candidatos) == 0) {
    return(NULL)
  }
  
  # Selecionar o ajuste com menor deviance
  desvios <- vapply(
    candidatos,
    deviance,
    numeric(1)
  )
  
  candidatos[[which.min(desvios)]]
}
# Função backward completa para \(\nu\)


backward_nu <- function(modelo,
                        alpha = 0.05,
                        verbose = TRUE) {
  
  if (!isTRUE(modelo$converged)) {
    stop("O modelo inicial não convergiu.")
  }
  
  atual <- modelo
  
  historico <- list()
  exclusoes <- list()
  
  etapa <- 1
  motivo_parada <- ""
  
  repeat {
    
    termos <- attr(
      terms(formula(atual, what = "nu")),
      "term.labels"
    )
    
    if (length(termos) == 0) {
      motivo_parada <- "Todos os termos foram retirados."
      break
    }
    
    if (verbose) {
      cat("\n============================\n")
      cat("ETAPA", etapa, "\n")
      cat("Variáveis restantes:", length(termos), "\n")
      flush.console()
    }
    
    resultados <- list()
    modelos_reduzidos <- list()
    
    for (var in termos) {
      
      if (verbose) {
        cat("Testando:", var, "\n")
        flush.console()
      }
      
      reduzido <- ajustar_reduzido_nu(
        modelo = atual,
        variavel = var
      )
      
      modelos_reduzidos[[var]] <- reduzido
      
      if (is.null(reduzido)) {
        
        resultados[[var]] <- data.frame(
          Etapa = etapa,
          Variavel = var,
          LR = NA_real_,
          GL = NA_real_,
          p_valor = NA_real_,
          AIC = NA_real_,
          BIC = NA_real_,
          Status = "Falha no ajuste"
        )
        
        next
      }
      
      # Conferir que apenas nu foi alterado
      mesmas_formulas <-
        identical(
          formula(atual, what = "mu"),
          formula(reduzido, what = "mu")
        ) &&
        identical(
          formula(atual, what = "sigma"),
          formula(reduzido, what = "sigma")
        )
      
      mesma_amostra <- atual$N == reduzido$N
      
      GL <- atual$df.fit - reduzido$df.fit
      
      LR <- deviance(reduzido) - deviance(atual)
      
      valido <- mesmas_formulas &&
        mesma_amostra &&
        GL > 0 &&
        is.finite(LR) &&
        LR >= -1e-4
      
      if (!valido) {
        
        resultados[[var]] <- data.frame(
          Etapa = etapa,
          Variavel = var,
          LR = LR,
          GL = GL,
          p_valor = NA_real_,
          AIC = GAIC(reduzido, k = 2),
          BIC = GAIC(reduzido, k = log(atual$N)),
          Status = "Verificar ajuste"
        )
        
        next
      }
      
      LR <- max(0, LR)
      
      p <- pchisq(
        LR,
        df = GL,
        lower.tail = FALSE
      )
      
      resultados[[var]] <- data.frame(
        Etapa = etapa,
        Variavel = var,
        LR = LR,
        GL = GL,
        p_valor = p,
        AIC = GAIC(reduzido, k = 2),
        BIC = GAIC(reduzido, k = log(atual$N)),
        Status = "OK"
      )
    }
    
    tabela <- do.call(rbind, resultados)
    rownames(tabela) <- NULL
    
    tabela <- tabela[
      order(-tabela$p_valor, na.last = TRUE),
      ,
      drop = FALSE
    ]
    
    historico[[etapa]] <- tabela
    
    if (verbose) {
      print(tabela, row.names = FALSE)
    }
    
    # Impedir decisões com candidatos não avaliados
    if (any(tabela$Status != "OK")) {
      motivo_parada <- paste(
        "Interrompido: existem ajustes",
        "que precisam ser verificados."
      )
      break
    }
    
    candidato <- tabela$Variavel[1]
    maior_p <- tabela$p_valor[1]
    
    if (maior_p < alpha) {
      motivo_parada <- paste(
        "Seleção concluída: todos os termos",
        "restantes são significativos pelo LR."
      )
      break
    }
    
    # Atualizar apenas nu
    atual <- modelos_reduzidos[[candidato]]
    
    exclusoes[[etapa]] <- data.frame(
      Etapa = etapa,
      Variavel_retirada = candidato,
      LR = tabela$LR[1],
      GL = tabela$GL[1],
      p_valor = maior_p,
      AIC = GAIC(atual, k = 2),
      BIC = GAIC(atual, k = log(atual$N))
    )
    
    if (verbose) {
      cat(
        "\nVariável retirada:", candidato,
        "\np-valor:", maior_p,
        "\nAIC:", GAIC(atual, k = 2),
        "\n"
      )
    }
    
    etapa <- etapa + 1
  }
  
  list(
    modelo_final = atual,
    
    historico = if (length(historico) > 0)
      do.call(rbind, historico)
    else data.frame(),
    
    exclusoes = if (length(exclusoes) > 0)
      do.call(rbind, exclusoes)
    else data.frame(),
    
    formula_nu = formula(atual, what = "nu"),
    
    motivo_parada = motivo_parada
  )
}

#===============================================================================
# EXECUTAR SELEÇÃO
#===============================================================================


# Executar backward em nu
selecao_nu_SN1 <- backward_nu(
  modelo = modelo_nu_inicial,
  alpha = 0.05,
  verbose = TRUE
)

# Verificar se concluiu ou foi interrompido
selecao_nu_SN1$motivo_parada

# Histórico de todos os testes
selecao_nu_SN1$historico

# Variáveis efetivamente retiradas
selecao_nu_SN1$exclusoes

# Fórmula de nu após o procedimento
selecao_nu_SN1$formula_nu


# Só prosseguir se a seleção concluiu pelo critério LR
if (!grepl(
  "^Seleção concluída",
  selecao_nu_SN1$motivo_parada
)) {
  stop("A seleção ainda não foi concluída.")
}

modelo_SN1_nu_final <- selecao_nu_SN1$modelo_final


#===============================================================================
# COMPARAÇÃO MODELOS
#===============================================================================


# Comparação dos modelos
GAIC(
  modelo_SN1_final,
  modelo_SN1_nu_centro,
  modelo_SN1_nu_final,
  k = 2
)

# Comparação por BIC
GAIC(
  modelo_SN1_final,
  modelo_SN1_nu_centro,
  modelo_SN1_nu_final,
  k = log(nrow(dados_modelo))
)

# Fórmulas do modelo selecionado
formula(modelo_SN1_nu_final, what = "mu")
formula(modelo_SN1_nu_final, what = "sigma")
formula(modelo_SN1_nu_final, what = "nu")

#===============================================================================
# VERIFICAR RESÍDUOS
#===============================================================================

# Resíduos quantílicos normalizados
r_nu <- residuals(modelo_SN1_nu_final)

# QQ-plot com envelope
car::qqPlot(
  r_nu,
  distribution = "norm",
  envelope = 0.95,
  line = "quartiles",
  id = FALSE,
  main = "SN1 - Assimetria selecionada",
  xlab = "Quantis teóricos",
  ylab = "Resíduos quantílicos"
)

# Worm plot do modelo SN1 com assimetria selecionada
gamlss::wp(
  modelo_SN1_nu_final,
  xlim.all = 5,
  ylim.all = 0.5
)


# Estatísticas dos resíduos
c(
  media = mean(r_nu),
  variancia = var(r_nu),
  assimetria = mean((r_nu - mean(r_nu))^3) / sd(r_nu)^3,
  curtose = mean((r_nu - mean(r_nu))^4) / sd(r_nu)^4
)

resid_plots(modelo_SN1_nu_final, theme = "new")

#===============================================================================
#       SALVAR INFORMAÇÕES ASSIMETRIA
#===============================================================================

# dir.create(
#   "resultados_gamlss/assimetria",
#   recursive = TRUE,
#   showWarnings = FALSE
# )
# 
# # Histórico completo do backward
# write.csv2(
#   selecao_nu_SN1$historico,
#   "resultados_gamlss/assimetria/historico_backward_nu.csv",
#   row.names = FALSE
# )
# 
# # Exclusões realizadas
# write.csv2(
#   selecao_nu_SN1$exclusoes,
#   "resultados_gamlss/assimetria/exclusoes_nu.csv",
#   row.names = FALSE
# )
# 
# # Guardar o modelo e o estado da seleção
# saveRDS(
#   selecao_nu_SN1,
#   "resultados_gamlss/assimetria/selecao_nu_SN1.rds"
# )
# 
# # Salvar summary apenas se a seleção concluiu
# if (grepl(
#   "^Seleção concluída",
#   selecao_nu_SN1$motivo_parada
# )) {
#   capture.output(
#     summary(modelo_SN1_nu_final),
#     file = "resultados_gamlss/assimetria/summary_SN1_nu_final.txt"
#   )
# }



#=========================================================================

#        ETAPA DE VALIDAÇÃO DO MODELO ESCOLHIDO

#=========================================================================

#validar a estrutura de \(\mu\) e \(\sigma\) e comparar todos os modelos 
#candidatos para escolher aquele que será apresentado como modelo final do TCC.

#===============================================================================
# 1. PREPARAR A AVALIAÇÃO DO MODELO SELECIONADO
#===============================================================================

# Confirmar que o backward de nu foi concluído
print(selecao_nu_SN1$motivo_parada)

if (!grepl(
  "^Seleção concluída",
  selecao_nu_SN1$motivo_parada
)) {
  stop("A seleção de nu ainda não foi concluída.")
}

# Modelo selecionado em nu
modelo_SN1_nu_final <- selecao_nu_SN1$modelo_final

stopifnot(isTRUE(modelo_SN1_nu_final$converged))

# Conferir fórmulas
formula(modelo_SN1_nu_final, what = "mu")
formula(modelo_SN1_nu_final, what = "sigma")
formula(modelo_SN1_nu_final, what = "nu")

# Indicadores
c(
  N = modelo_SN1_nu_final$N,
  Parametros = modelo_SN1_nu_final$df.fit,
  Deviance = deviance(modelo_SN1_nu_final),
  AIC = GAIC(modelo_SN1_nu_final, k = 2),
  BIC = GAIC(
    modelo_SN1_nu_final,
    k = log(modelo_SN1_nu_final$N)
  )
)

#===============================================================================
# 2. REAVALIAR AS VARIÁVEIS DE MU
#===============================================================================

# Modelo de referência
modelo_atual <- modelo_SN1_nu_final

# Fórmula de mu sem internet
formula_mu_sem_internet <- update(
  formula(modelo_atual, what = "mu"),
  ~ . - internet
)

# Ajuste explícito, preservando sigma e nu
modelo_mu_sem_internet <- tryCatch(
  gamlss(
    formula = formula_mu_sem_internet,
    sigma.formula = formula(modelo_atual, what = "sigma"),
    nu.formula = formula(modelo_atual, what = "nu"),
    family = SN1(),
    data = dados_modelo,
    start.from = modelo_atual,
    control = gamlss.control(
      n.cyc = 500,
      c.crit = 0.0001,
      mu.step = 0.5,
      sigma.step = 0.5,
      nu.step = 0.2,
      trace = TRUE
    )
  ),
  error = function(e) {
    message("Erro: ", conditionMessage(e))
    NULL
  }
)

# Resultado do ajuste
if (!is.null(modelo_mu_sem_internet)) {
  print(c(
    Convergiu = modelo_mu_sem_internet$converged,
    Deviance = deviance(modelo_mu_sem_internet),
    AIC = GAIC(modelo_mu_sem_internet, k = 2)
  ))
}


if (
  !is.null(modelo_mu_sem_internet) &&
  isTRUE(modelo_mu_sem_internet$converged)
) {
  LR <- deviance(modelo_mu_sem_internet) -
    deviance(modelo_SN1_nu_final)
  
  GL <- modelo_SN1_nu_final$df.fit -
    modelo_mu_sem_internet$df.fit
  
  print(c(
    LR = LR,
    GL = GL,
    p_valor = if (LR >= 0 && GL > 0)
      pchisq(LR, GL, lower.tail = FALSE)
    else NA_real_
  ))
}

#===============================================================================
# 3. REAVALIAR AS VARIÁVEIS DE SIGMA
#===============================================================================

# Testes de exclusão das variáveis de sigma

formula_sigma_sem_lingua <- update(
  formula(modelo_SN1_nu_final, what = "sigma"),
  ~ . - TP_LINGUA
)

modelo_sigma_sem_lingua <- tryCatch(
  gamlss(
    formula = formula(modelo_SN1_nu_final, what = "mu"),
    sigma.formula = formula_sigma_sem_lingua,
    nu.formula = formula(modelo_SN1_nu_final, what = "nu"),
    family = SN1(),
    data = dados_modelo,
    start.from = modelo_SN1_nu_final,
    control = gamlss.control(
      n.cyc = 500,
      c.crit = 0.0001,
      mu.step = 0.5,
      sigma.step = 0.5,
      nu.step = 0.2,
      trace = TRUE
    )
  ),
  error = function(e) {
    message("Erro: ", conditionMessage(e))
    NULL
  }
)

if (!is.null(modelo_sigma_sem_lingua)) {
  print(c(
    Convergiu = modelo_sigma_sem_lingua$converged,
    Deviance = deviance(modelo_sigma_sem_lingua),
    AIC = GAIC(modelo_sigma_sem_lingua, k = 2)
  ))
}

#===============================================================================
# SE TUDO CONVERGIU, TESTAR TODAS AS EXCLUSÕES
#===============================================================================

testar_exclusoes <- function(modelo, parametro,
                             dados = dados_modelo) {
  
  stopifnot(
    parametro %in% c("mu", "sigma", "nu"),
    isTRUE(modelo$converged)
  )
  
  # Fórmulas do modelo original
  formulas <- list(
    mu = formula(modelo, what = "mu"),
    sigma = formula(modelo, what = "sigma"),
    nu = formula(modelo, what = "nu")
  )
  
  # Variáveis presentes no parâmetro avaliado
  termos <- attr(
    terms(formulas[[parametro]]),
    "term.labels"
  )
  
  resultados <- lapply(termos, function(variavel) {
    
    # Copiar as fórmulas originais
    fm <- formulas
    
    # Retirar somente a variável testada
    termos_restantes <- setdiff(termos, variavel)
    
    if (parametro == "mu") {
      fm$mu <- reformulate(
        termos_restantes,
        response = "desempenho"
      )
    } else {
      fm[[parametro]] <- reformulate(
        termos_restantes
      )
    }
    
    # Ajustar o modelo reduzido
    ajuste <- tryCatch(
      gamlss(
        formula = fm$mu,
        sigma.formula = fm$sigma,
        nu.formula = fm$nu,
        family = SN1(),
        data = dados,
        start.from = modelo,
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
          "Erro ao retirar ", variavel,
          " de ", parametro, ": ",
          conditionMessage(e)
        )
        NULL
      }
    )
    
    convergiu <- !is.null(ajuste) &&
      isTRUE(ajuste$converged)
    
    LR <- if (convergiu) {
      deviance(ajuste) - deviance(modelo)
    } else NA_real_
    
    GL <- if (convergiu) {
      modelo$df.fit - ajuste$df.fit
    } else NA_real_
    
    valido <- convergiu &&
      is.finite(LR) &&
      GL > 0 &&
      LR >= -1e-4 &&
      ajuste$N == modelo$N
    
    data.frame(
      Parametro = parametro,
      Variavel = variavel,
      LR = LR,
      GL = GL,
      p_valor = if (valido) {
        pchisq(
          max(LR, 0),
          df = GL,
          lower.tail = FALSE
        )
      } else NA_real_,
      AIC = if (convergiu) {
        as.numeric(GAIC(ajuste, k = 2))
      } else NA_real_,
      BIC = if (convergiu) {
        as.numeric(GAIC(
          ajuste,
          k = log(ajuste$N)
        ))
      } else NA_real_,
      Convergiu = convergiu,
      Valido = valido
    )
  })
  
  tabela <- do.call(rbind, resultados)
  rownames(tabela) <- NULL
  
  tabela <- tabela[
    order(-tabela$p_valor, na.last = TRUE),
    ,
    drop = FALSE
  ]
  
  return(tabela)
}


# Testes de exclusão em mu

tabela_mu <- testar_exclusoes(
  modelo = modelo_SN1_nu_final,
  parametro = "mu"
)

print(tabela_mu, row.names = FALSE)


# Testes de exclusão em sigma
tabela_sigma <- testar_exclusoes(
  modelo = modelo_SN1_nu_final,
  parametro = "sigma"
)

print(tabela_sigma, row.names = FALSE)

#===============================================================================
# 4. RETIRANDO MANUALMENTE A ESCOLARIDADE DO PAI DE MU
#===============================================================================

# A interpretação será:
#   - \(p<0{,}05\): há evidência de contribuição da variável em \(\mu\).
# - \(p\geq0{,}05\): a variável é candidata à exclusão de \(\mu\).
# - LR negativo: é necessário investigar a otimização antes de decidir.

#Se aparecerem termos com \(p\geq0{,}05\), a próxima seção permite realizar o backward.

# Função geral para backward por LR

# Modelo atual
modelo_atual <- modelo_SN1_nu_final

# Fórmula de mu sem escolaridade_pai
formula_mu_reduzida <- update(
  formula(modelo_atual, what = "mu"),
  ~ . - escolaridade_pai
)

# Reajustar preservando sigma e nu
modelo_SN1_mu_sem_pai <- gamlss(
  formula = formula_mu_reduzida,
  sigma.formula = formula(modelo_atual, what = "sigma"),
  nu.formula = formula(modelo_atual, what = "nu"),
  family = SN1(),
  data = dados_modelo,
  start.from = modelo_atual,
  control = gamlss.control(
    n.cyc = 500,
    c.crit = 0.0001,
    mu.step = 0.5,
    sigma.step = 0.5,
    nu.step = 0.2,
    trace = TRUE
  )
)

# Conferir ajuste
c(
  convergiu = modelo_SN1_mu_sem_pai$converged,
  iteracoes = modelo_SN1_mu_sem_pai$iter,
  deviance = deviance(modelo_SN1_mu_sem_pai),
  AIC = GAIC(modelo_SN1_mu_sem_pai, k = 2),
  BIC = GAIC(
    modelo_SN1_mu_sem_pai,
    k = log(modelo_SN1_mu_sem_pai$N)
  )
)

#===============================================================================
# REPETIR TESTES
#===============================================================================


# Confirmar convergência
stopifnot(isTRUE(modelo_SN1_mu_sem_pai$converged))

# Reavaliar mu
tabela_mu_2 <- testar_exclusoes(
  modelo = modelo_SN1_mu_sem_pai,
  parametro = "mu"
)

print(tabela_mu_2, row.names = FALSE)



#===============================================================================
# REAVALIAR SIGMA E NU
#===============================================================================

# Modelo após a seleção de mu
modelo_atual <- modelo_SN1_mu_sem_pai

# Testar sigma
tabela_sigma_2 <- testar_exclusoes(
  modelo = modelo_atual,
  parametro = "sigma"
)

print(tabela_sigma_2, row.names = FALSE)

#-------------------------------------------------------------------------------
# NÃO CONVERGIU ESSA TABELA_NU
# Testar nu
tabela_nu_2 <- testar_exclusoes(
  modelo = modelo_atual,
  parametro = "nu"
)

print(tabela_nu_2, row.names = FALSE)
#-------------------------------------------------------------------------------


#===============================================================================
#  TESTANDO EXCLUSÃO DE ESCOLARIDADE PAI
#===============================================================================

# Após a exclusão da escolaridade paterna do parâmetro de localização ($\mu$), 
# sua contribuição para o parâmetro de assimetria ($\nu$) foi reavaliada. 
# O modelo reduzido apresentou convergência após 513 iterações, 
# e o teste da razão de verossimilhanças indicou perda significativa de ajuste com
# a retirada da variável ($LR=105{,}908$; $gl=7$; $p<0{,}001$).
# Dessa forma, a escolaridade paterna foi mantida em $\nu$, assim como no parâmetro de escala ($\sigma$),
# permanecendo ausente apenas de $\mu$.

# Modelo atual, após a seleção de mu
modelo_atual <- modelo_SN1_mu_sem_pai

# Fórmula reduzida de nu
formula_nu_sem_pai <- update(
  formula(modelo_atual, what = "nu"),
  ~ . - escolaridade_pai
)

# Primeira tentativa
modelo_nu_sem_pai_1 <- tryCatch(
  gamlss(
    formula = formula(modelo_atual, what = "mu"),
    sigma.formula = formula(modelo_atual, what = "sigma"),
    nu.formula = formula_nu_sem_pai,
    family = SN1(),
    data = dados_modelo,
    start.from = modelo_atual,
    control = gamlss.control(
      n.cyc = 1000,
      c.crit = 0.0001,
      mu.step = 0.5,
      sigma.step = 0.5,
      nu.step = 0.1,
      trace = TRUE
    )
  ),
  error = function(e) {
    message("Erro: ", conditionMessage(e))
    NULL
  }
)

# Resultado
if (!is.null(modelo_nu_sem_pai_1)) {
  c(
    convergiu = modelo_nu_sem_pai_1$converged,
    iteracoes = modelo_nu_sem_pai_1$iter,
    deviance = deviance(modelo_nu_sem_pai_1),
    AIC = GAIC(modelo_nu_sem_pai_1, k = 2)
  )
}

#===============================================================================

#Após a reavaliação do parâmetro de localização, foram realizados novos testes 
#da razão de verossimilhanças para verificar a permanência das covariáveis no
#parâmetro de assimetria ($\nu$). Todos os modelos reduzidos apresentaram convergência,
#e as exclusões individuais da escolaridade paterna ($LR=105{,}908$), 
#renda familiar ($LR=67{,}366$), tempo de espera ($LR=196{,}376$) e centro de ensino 
#($LR=188{,}339$) resultaram em perdas estatisticamente significativas de ajuste ($p<0{,}001$).
#Dessa forma, as quatro variáveis foram mantidas na especificação de $\nu$.


# Variáveis ainda não verificadas manualmente
variaveis_testar <- c(
  "renda_agrupada",
  "TEMPO_ESPERA_AGRUPADO",
  "CENTRO"
)

resultados <- lapply(variaveis_testar, function(var) {
  
  cat("\nTestando retirada de:", var, "\n")
  
  formula_reduzida <- update(
    formula(modelo_atual, what = "nu"),
    paste("~ . -", var)
  )
  
  reduzido <- tryCatch(
    gamlss(
      formula = formula(modelo_atual, what = "mu"),
      sigma.formula = formula(modelo_atual, what = "sigma"),
      nu.formula = formula_reduzida,
      family = SN1(),
      data = dados_modelo,
      start.from = modelo_atual,
      control = gamlss.control(
        n.cyc = 1000,
        c.crit = 0.0001,
        mu.step = 0.5,
        sigma.step = 0.5,
        nu.step = 0.1,
        trace = FALSE
      )
    ),
    error = function(e) {
      message(conditionMessage(e))
      NULL
    }
  )
  
  if (is.null(reduzido)) {
    return(data.frame(
      Variavel = var,
      Convergiu = FALSE,
      Iteracoes = NA,
      LR = NA,
      GL = NA,
      p_valor = NA
    ))
  }
  
  LR <- deviance(reduzido) - deviance(modelo_atual)
  GL <- modelo_atual$df.fit - reduzido$df.fit
  
  valido <- isTRUE(reduzido$converged) &&
    reduzido$N == modelo_atual$N &&
    GL > 0 &&
    is.finite(LR) &&
    LR >= -1e-4
  
  data.frame(
    Variavel = var,
    Convergiu = reduzido$converged,
    Iteracoes = reduzido$iter,
    LR = if (valido) max(LR, 0) else NA,
    GL = if (valido) GL else NA,
    p_valor = if (valido) {
      pchisq(max(LR, 0), df = GL,
             lower.tail = FALSE)
    } else NA
  )
})

tabela_verificacao <- dplyr::bind_rows(resultados)

print(tabela_verificacao, row.names = FALSE)

#===============================================================================
#  CONFIRMANDO O MODELOS E AS VARIÁVEIS APÓS RETIRAR ESCOLARIDADE PAI
#===============================================================================

# Teste LR
LR.test(
  modelo_nu_sem_pai_1,
  modelo_SN1_mu_sem_pai
)

# Comparação AIC e BIC
GAIC(
  modelo_SN1_mu_sem_pai,
  modelo_nu_sem_pai_1,
  k = 2
)

GAIC(
  modelo_SN1_mu_sem_pai,
  modelo_nu_sem_pai_1,
  k = log(modelo_SN1_mu_sem_pai$N)
)

#===============================================================================
# 5. SALVANDO MODELO FINAL (SEM A VARIÁVEL FORMAÇÃO)
#===============================================================================

modelo_SN1_final_TCC <- modelo_SN1_mu_sem_pai

stopifnot(isTRUE(modelo_SN1_final_TCC$converged))

dir.create(
  "resultados_gamlss/comparacao_final",
  recursive = TRUE,
  showWarnings = FALSE
)

saveRDS(
  modelo_SN1_final_TCC,
  "resultados_gamlss/comparacao_final/modelo_SN1_final_TCC.rds"
)


# Diretório principal
pasta <- "resultados_gamlss/modelo_final_TCC"

pastas <- file.path(
  pasta,
  c(
    "01_testes_LR",
    "02_summary",
    "03_residuos",
    "04_modelos"
  )
)

invisible(lapply(
  pastas,
  dir.create,
  recursive = TRUE,
  showWarnings = FALSE
))

pasta_LR <- pastas[1]
pasta_summary <- pastas[2]
pasta_residuos <- pastas[3]
pasta_modelos <- pastas[4]


# Salvando tabelas



#===============================================================================
# TABELAS FINAIS DOS TESTES LR
#===============================================================================

# Manter os resultados das seleções de mu e sigma
tabela_mu_final <- tabela_mu_2
tabela_sigma_final <- tabela_sigma_2

# Modelo completo de referência
modelo_referencia <- modelo_SN1_final_TCC

#--------------------------------------------------
# 1. TESTE DA ESCOLARIDADE PATERNA EM NU
#--------------------------------------------------

stopifnot(
  isTRUE(modelo_referencia$converged),
  isTRUE(modelo_nu_sem_pai_1$converged),
  modelo_referencia$N == modelo_nu_sem_pai_1$N
)

LR_pai <- deviance(modelo_nu_sem_pai_1) -
  deviance(modelo_referencia)

GL_pai <- modelo_referencia$df.fit -
  modelo_nu_sem_pai_1$df.fit

stopifnot(
  is.finite(LR_pai),
  LR_pai >= -1e-4,
  GL_pai > 0
)

tabela_pai <- data.frame(
  Parametro = "nu",
  Variavel = "escolaridade_pai",
  LR = max(LR_pai, 0),
  GL = GL_pai,
  p_valor = pchisq(
    max(LR_pai, 0),
    df = GL_pai,
    lower.tail = FALSE
  ),
  Convergiu = TRUE,
  Valido = TRUE
)

#--------------------------------------------------
# 2. TESTES DAS OUTRAS TRÊS VARIÁVEIS
#--------------------------------------------------

stopifnot(
  nrow(tabela_verificacao) == 3,
  all(tabela_verificacao$Convergiu),
  all(is.finite(tabela_verificacao$LR)),
  all(is.finite(tabela_verificacao$p_valor))
)

tabela_demais <- data.frame(
  Parametro = "nu",
  Variavel = tabela_verificacao$Variavel,
  LR = tabela_verificacao$LR,
  GL = tabela_verificacao$GL,
  p_valor = tabela_verificacao$p_valor,
  Convergiu = tabela_verificacao$Convergiu,
  Valido = TRUE
)

#--------------------------------------------------
# 3. REUNIR OS QUATRO TESTES
#--------------------------------------------------

tabela_nu_final <- dplyr::bind_rows(
  tabela_pai,
  tabela_demais
)

# Ordenar pelo valor-p
tabela_nu_final <- tabela_nu_final[
  order(tabela_nu_final$p_valor),
  ,
  drop = FALSE
]

rownames(tabela_nu_final) <- NULL

print(tabela_nu_final, row.names = FALSE)



# Ordenar as tabelas
tabela_mu_final <- tabela_mu_final[
  order(-tabela_mu_final$p_valor), ]
tabela_sigma_final <- tabela_sigma_final[
  order(-tabela_sigma_final$p_valor), ]


# Exportar para Excel/CSV
write.csv2(
  tabela_mu_final,
  file.path(pasta_LR, "testes_mu.csv"),
  row.names = FALSE
)

write.csv2(
  tabela_sigma_final,
  file.path(pasta_LR, "testes_sigma.csv"),
  row.names = FALSE
)

write.csv2(
  tabela_nu_final,
  file.path(pasta_LR, "testes_nu.csv"),
  row.names = FALSE
)

# Histórico do backward de nu
write.csv2(
  selecao_nu_SN1$historico,
  file.path(pasta_LR, "historico_backward_nu.csv"),
  row.names = FALSE
)

write.csv2(
  selecao_nu_SN1$exclusoes,
  file.path(pasta_LR, "exclusoes_backward_nu.csv"),
  row.names = FALSE
)

# Comparaçã entre modelos


comparacoes <- data.frame(
  Comparacao = c(
    "SN1 nu constante vs nu selecionado",
    "SN1 antes vs depois retirada pai de mu",
    "SN1 final vs sem pai em nu"
  ),
  LR = c(
    deviance(modelo_SN1_final) -
      deviance(modelo_SN1_nu_final),
    
    deviance(modelo_SN1_mu_sem_pai) -
      deviance(modelo_SN1_nu_final),
    
    deviance(modelo_nu_sem_pai_1) -
      deviance(modelo_SN1_final_TCC)
  ),
  GL = c(
    modelo_SN1_nu_final$df.fit -
      modelo_SN1_final$df.fit,
    
    modelo_SN1_nu_final$df.fit -
      modelo_SN1_mu_sem_pai$df.fit,
    
    modelo_SN1_final_TCC$df.fit -
      modelo_nu_sem_pai_1$df.fit
  )
)

# Corrigir orientação do teste da segunda linha:
# O modelo reduzido deve ser o sem pai em mu

comparacoes$LR[2] <-
  deviance(modelo_SN1_mu_sem_pai) -
  deviance(modelo_SN1_nu_final)

comparacoes$p_valor <- ifelse(
  comparacoes$LR >= 0 & comparacoes$GL > 0,
  pchisq(
    pmax(comparacoes$LR, 0),
    comparacoes$GL,
    lower.tail = FALSE
  ),
  NA_real_
)

write.csv2(
  comparacoes,
  file.path(pasta_LR, "comparacoes_LR_modelos.csv"),
  row.names = FALSE
)

print(comparacoes)

# Salvando summary


# Summary completo do modelo final
capture.output(
  summary(modelo_SN1_final_TCC),
  file = file.path(
    pasta_summary,
    "summary_SN1_final.txt"
  )
)

# Extrair coeficientes dos parâmetros
extrair_coeficientes <- function(modelo, parametro) {
  
  tab <- summary(
    modelo,
    save = TRUE
  )
  
  # Extração diretamente do objeto
  estimativas <- coef(modelo, what = parametro)
  erros <- sqrt(diag(
    vcov(modelo, what = parametro)
  ))
  
  data.frame(
    Termo = names(estimativas),
    Estimativa = as.numeric(estimativas),
    Erro_Padrao = as.numeric(erros),
    Estatistica = as.numeric(estimativas / erros),
    row.names = NULL
  )
}


#===============================================================================
#  ANÁLISE DE RESÍDUOS MODELO FINAL
#===============================================================================


# Resíduos quantílicos
r <- as.numeric(residuals(modelo_SN1_final_TCC))

# Estatísticas descritivas
media <- mean(r)
desvio <- sd(r)

estatisticas_residuos <- data.frame(
  Estatistica = c(
    "N",
    "Média",
    "Variância",
    "Desvio-padrão",
    "Assimetria",
    "Curtose",
    "Mínimo",
    "Q1",
    "Mediana",
    "Q3",
    "Máximo"
  ),
  
  Valor = c(
    length(r),
    media,
    var(r),
    desvio,
    mean((r - media)^3) / desvio^3,
    mean((r - media)^4) / desvio^4,
    min(r),
    unname(quantile(r, 0.25)),
    median(r),
    unname(quantile(r, 0.75)),
    max(r)
  )
)

print(estatisticas_residuos)

# write.csv2(
#   estatisticas_residuos,
#   file.path(
#     pasta_residuos,
#     "estatisticas_residuos.csv"
#   ),
#   row.names = FALSE
# )

# Salvar os resíduos individualmente
# write.csv2(
#   data.frame(
#     Observacao = seq_along(r),
#     Residuo_Quantilico = r
#   ),
#   file.path(pasta_residuos, "residuos_quantile.csv"),
#   row.names = FALSE
# )
# 
# 
# png(
#   file.path(pasta_residuos, "qqplot_SN1_final.png"),
#   width = 1800,
#   height = 1400,
#   res = 200
# )

car::qqPlot(
  r,
  distribution = "norm",
  envelope = 0.95,
  line = "quartiles",
  id = FALSE,
  main = "QQ-plot dos resíduos quantílicos - SN1",
  xlab = "Quantis teóricos",
  ylab = "Quantis dos resíduos"
)



# png(
#   file.path(pasta_residuos, "wormplot_SN1_final.png"),
#   width = 1800,
#   height = 1400,
#   res = 200
# )

gamlss::wp(
  modelo_SN1_final_TCC,
  xlim.all = 5,
  ylim.all = 0.5
)




# png(
#   file.path(pasta_residuos, "diagnosticos_SN1_final.png"),
#   width = 2200,
#   height = 1800,
#   res = 200
# )

par(mfrow = c(2, 2), mar = c(4, 4, 3, 1))

# 1. Resíduos versus localização ajustada
plot(
  fitted(modelo_SN1_final_TCC, what = "mu"),
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
# TABELA COMPARATIVA DOS MODELOS
#===============================================================================

modelos_tcc <- list(
  "Normal - sigma selecionado" = modelo_NO_final,
  "SN1 - nu constante" = modelo_SN1_final,
  "ST2 - sigma selecionado" = modelo_ST2_final,
  "SN1 - nu com CENTRO" = modelo_SN1_nu_centro,
  "SN1 - nu completo reajustado" = modelo_SN1_nu_completo_2,
  "SN1 - nu selecionado" = modelo_SN1_nu_final,
  "SN1 - final (mu, sigma, nu)" = modelo_SN1_final_TCC
)

tabela_comparacao <- do.call(
  rbind,
  lapply(names(modelos_tcc), function(nome) {
    
    m <- modelos_tcc[[nome]]
    
    data.frame(
      Modelo = nome,
      N = m$N,
      Parametros = m$df.fit,
      Deviance = as.numeric(deviance(m)),
      AIC = as.numeric(GAIC(m, k = 2)),
      BIC = as.numeric(GAIC(m, k = log(m$N))),
      Convergiu = isTRUE(m$converged)
    )
  })
)

rownames(tabela_comparacao) <- NULL

# Verificar mesma dimensão da amostra

stopifnot(
  all(tabela_comparacao$N == modelo_SN1_final_TCC$N)
)

tabela_comparacao <- tabela_comparacao[
  order(tabela_comparacao$AIC),
]

print(tabela_comparacao, row.names = FALSE)

# write.csv2(
#   tabela_comparacao,
#   file.path(pasta_modelos, "comparacao_modelos.csv"),
#   row.names = FALSE
# )

# # Exportar tabela para LaTeX
# tabela_latex <- knitr::kable(
#   tabela_comparacao[
#     tabela_comparacao$Convergiu,
#     c("Modelo", "Parametros", "Deviance", "AIC", "BIC")
#   ],
#   format = "latex",
#   booktabs = TRUE,
#   digits = 1,
#   row.names = FALSE,
#   caption = "Comparação dos modelos GAMLSS ajustados",
#   label = "tab:modelos_gamlss"
# )
# 
# writeLines(
#   as.character(tabela_latex),
#   file.path(pasta_modelos, "comparacao_modelos.tex")
# )
# 
# # Salvar informações da sessão para reprodutibilidade
# capture.output(
#   sessionInfo(),
#   file = file.path(pasta_modelos, "sessionInfo.txt")
# )
