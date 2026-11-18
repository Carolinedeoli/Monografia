
#-------------------------------------------------------------------------------

#      ANÁLISE DESCRITIVA - BASE UFES x ENEM (SÓ QUEM DEU MATCH NAS 5 NOTAS)
#      Versão organizada a partir de analise_descritiva_ufes_cloude.R
#      Gráficos e regras de cálculo originais preservados.
#
# ROTEIRO:
#   0   Bibliotecas, estilo e funções auxiliares
#   1-4 Perfil geral, notas e cursos
#   5-9 Desempenho, estrutura acadêmica e análises complementares
#   10  Mapas de calor por faixa de renda (17 + painel conjunto)
#   11  Exportações centralizadas, conferência e relatório
#
# Todos os PNGs são exportados ao final e conferidos individualmente.

#-------------------------------------------------------------------------------

library(readr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(scales)
library(stringr)
library(stats)
library(patchwork)
base   <- "DOCUMENTOS_ENEM/DADOS"
base2 <-  "figures"
saida  <- file.path(base2, "fig_ufes")
dir.create(saida, showWarnings = FALSE, recursive = TRUE)

# ---- Carrega a base de match (troque para df_ufes_match se já estiver no ambiente) ----
df <- read_csv(file.path(base, "base_LIMPA_ufes_sisu.csv"), show_col_types = FALSE)


#===============================================================================
# 0. ESTILO PADRONIZADO (extraído dos boxplots da monografia)
#===============================================================================
# Tudo que define a aparência dos gráficos está concentrado neste bloco.
# Para mudar uma cor ou fonte em TODOS os gráficos, altere só aqui.

# ---- 0.1 Paleta ---------------------------------------------------------------
# Cores lidas diretamente dos boxplots da monografia:
COR_VIOLINO   <- "#D6EBF2"   # azul-claro do violino (fundo do boxplot)
COR_CAIXA     <- "#F7FBFC"   # preenchimento da caixa
COR_CONTORNO  <- "#333333"   # contorno da caixa, bigodes e mediana
COR_GRADE     <- "#EBEBEB"   # linhas de grade

# Azuis usados nos demais gráficos (barras, linhas, mapas de calor):
AZUL_ESCURO   <- "#08306B"
AZUL_PRINCIPAL <- "#2171B5"  # cor principal (barras, linhas, histogramas)
AZUL_MEDIO    <- "#4393C3"
AZUL_CLARO    <- "#6BAED6"
AZUL_SUAVE    <- "#C6DBEF"

# Degradê dos mapas de calor (claro -> escuro)
PALETA_CALOR <- c("#E0F3FF", "#B3DDF2", "#80BFE5", "#4A9BD3",
                  "#2171B5", "#08519C", "#08306B")

# Paleta qualitativa (ancorada no azul; distinguível para daltonismo e em P&B)
# Paleta qualitativa: alegre, porém dessaturada (âncora azul + tons pastel)
PALETA_QUALITATIVA <- c("#1F4E8C",  # azul escuro (âncora)
                        "#7FB8E0",  # azul claro
                        "#F0B63C",  # amarelo mostarda
                        "#5FBF9A",  # verde menta
                        "#EE8F7D",  # coral
                        "#A99BD1",  # lilás
                        "#EDA3C4",  # rosa
                        "#9AA5B1")  # cinza azulado

pal_azul <- function(n) {
  if (n <= 1) return(AZUL_PRINCIPAL)
  if (n == 2) return(c(AZUL_ESCURO, AZUL_CLARO))
  # 3 a 4 categorias: rampa de azul
  if (n <= 4) {
    return(grDevices::colorRampPalette(
      c(AZUL_ESCURO, AZUL_PRINCIPAL, AZUL_CLARO, "#9ECAE1"))(n))
  }
  # 5 ou mais: paleta qualitativa (interpola só se passar de 8)
  cores <- PALETA_QUALITATIVA
  if (n > length(cores)) cores <- grDevices::colorRampPalette(cores)(n)
  cores[seq_len(n)]
}

# Devolve um vetor nomeado de cores para as categorias de x (use em
# scale_fill_manual / scale_color_manual).
cores_azul <- function(x) {
  niveis <- if (is.factor(x)) levels(droplevels(x)) else sort(unique(as.character(x)))
  niveis <- niveis[!is.na(niveis)]
  stats::setNames(pal_azul(length(niveis)), niveis)
}

# ---- 0.2 Tema -------------------------------------------------------------------
# Mesmo visual dos boxplots da monografia: fundo branco, grade clara,
# títulos de painel em negrito, rótulos de eixo em cinza escuro.
tema_tcc <- function(base_size = 12, legenda = "bottom") {
  theme_minimal(base_size = base_size) +
    theme(
      plot.title       = element_text(face = "bold", size = base_size + 3,
                                      hjust = 0.5, margin = margin(b = 6)),
      plot.subtitle    = element_text(colour = "grey30", hjust = 0.5,
                                      margin = margin(b = 10)),
      axis.title       = element_text(size = base_size + 1),
      axis.text        = element_text(colour = "grey30"),
      panel.grid.major = element_line(colour = COR_GRADE, linewidth = 0.4),
      panel.grid.minor = element_blank(),
      strip.text       = element_text(face = "bold", size = base_size - 1),
      legend.position  = legenda,
      legend.title     = element_text(face = "bold"),
      plot.background  = element_rect(fill = "white", colour = NA)
    )
}

theme_set(tema_tcc())

# Rótulo de porcentagem para eixos
fmt_pct <- function(x) paste0(x, "%")

# ---- 0.3 Boxplot padrão da monografia (violino azul + caixa clara) -----------------
# Versões do ggplot2 >= 3.5.0 permitem engrossar só a mediana.
ggplot2_novo <- utils::packageVersion("ggplot2") >= "3.5.0"

geom_boxplot_tcc <- function(largura_caixa = 0.35, largura_violino = 0.9,
                             violino = TRUE, coef = 1.5) {
  camadas <- list()
  if (violino) {
    camadas <- c(camadas, list(
      geom_violin(fill = COR_VIOLINO, colour = NA, width = largura_violino,
                  scale = "width", trim = TRUE)
    ))
  }
  caixa <- if (ggplot2_novo) {
    geom_boxplot(width = largura_caixa, fill = COR_CAIXA, colour = COR_CONTORNO,
                 linewidth = 0.4, median.linewidth = 1.2,
                 outlier.shape = NA, coef = coef)
  } else {
    geom_boxplot(width = largura_caixa, fill = COR_CAIXA, colour = COR_CONTORNO,
                 size = 0.4, fatten = 3, outlier.shape = NA, coef = coef)
  }
  c(camadas, list(caixa))
}

# Gráfico completo no layout da monografia: categorias no eixo vertical,
# valor no eixo horizontal.
#  - ordenar_mediana = TRUE : categorias ordenadas pela mediana (maior no topo)
#  - ordenar_mediana = FALSE: respeita a ordem dos níveis do fator (1º nível no topo)
grafico_boxplot_tcc <- function(dados, categoria, valor = "desempenho",
                                titulo = NULL, subtitulo = NULL,
                                rotulo_categoria = NULL,
                                rotulo_valor = "Desempenho",
                                ordenar_mediana = FALSE,
                                violino = TRUE) {
  d <- dados[!is.na(dados[[categoria]]) & !is.na(dados[[valor]]), , drop = FALSE]
  if (is.factor(d[[categoria]])) d[[categoria]] <- droplevels(d[[categoria]])
  if (ordenar_mediana) {
    d$cat_plot <- stats::reorder(d[[categoria]], d[[valor]], FUN = stats::median)
  } else {
    f <- if (is.factor(d[[categoria]])) droplevels(d[[categoria]]) else factor(d[[categoria]])
    d$cat_plot <- factor(f, levels = rev(levels(f)))
  }
  ggplot(d, aes(x = cat_plot, y = .data[[valor]])) +
    geom_boxplot_tcc(violino = violino) +
    coord_flip() +
    labs(title = titulo, subtitle = subtitulo,
         x = rotulo_categoria, y = rotulo_valor) +
    tema_tcc() +
    theme(legend.position = "none")
}

# ---- 0.4 Tabela de medidas no formato da monografia -------------------------------
# Colunas: Categoria | n | Média | DP | Mín | Q1 | Mediana | Q3 | Máx
tab_medidas <- function(dados, grupo, valor = "desempenho", casas = 2) {
  dados %>%
    filter(!is.na(.data[[grupo]]), !is.na(.data[[valor]])) %>%
    group_by(Categoria = .data[[grupo]]) %>%
    summarise(
      n       = n(),
      `Média` = mean(.data[[valor]]),
      DP      = sd(.data[[valor]]),
      `Mín`   = min(.data[[valor]]),
      Q1      = quantile(.data[[valor]], 0.25),
      Mediana = median(.data[[valor]]),
      Q3      = quantile(.data[[valor]], 0.75),
      `Máx`   = max(.data[[valor]]),
      .groups = "drop"
    ) %>%
    mutate(Categoria = as.character(Categoria),
           across(c(`Média`, DP, `Mín`, Q1, Mediana, Q3, `Máx`),
                  ~ round(as.numeric(.x), casas)))
}

# Nomes legíveis das colunas de nota
ROTULOS_AREAS <- c(
  NU_NOTA_CN      = "Ciências da Natureza",
  NU_NOTA_CH      = "Ciências Humanas",
  NU_NOTA_LC      = "Linguagens",
  NU_NOTA_MT      = "Matemática",
  NU_NOTA_REDACAO = "Redação",
  desempenho      = "Desempenho geral"
)


#===========================================================================
# VERIFICAÇÃO DE REPETIÇÃO DE NOMES DE ALUNOS
#--------------------------------------------
df %>%
  count(NOME) %>%
  filter(n > 1) %>%
  arrange(desc(n))

nomes_repetidos <- df %>%
  group_by(NOME,`ANO INGRESSO`, SEMESTRE) %>%
  filter(n() > 1) %>%
  arrange(NOME) %>%
  ungroup()

#=============================================================================


# Ordens "corretas" das categóricas rotuladas (senão o ggplot ordena por
# ordem alfabética, o que não faz sentido pra renda/escolaridade/idade).
ORDEM_RENDA <- c(
  "Sem renda", "Até 1 SM", "De 1 a 1.5 SM", "De 1.5 a 2 SM", "De 2 a 2.5 SM",
  "De 2.5 a 3 SM", "De 3 a 4 SM", "De 4 a 5 SM", "De 5 a 6 SM", "De 6 a 7 SM",
  "De 7 a 8 SM", "De 8 a 9 SM", "De 9 a 10 SM", "De 10 a 12 SM", "De 12 a 15 SM",
  "De 15 a 20 SM", "Acima de 20 SM"
)

ORDEM_ESCOLARIDADE_MAE <- c("Nunca estudou", "Não completou EF (até 5º ano)",
                             "Não completou EF (até 9º ano)", "EF completo", "Médio completo",
                             "Superior completo", "Pós-graduação", "Não sabe")
ORDEM_ESCOLARIDADE_PAI <- c("Nunca estudou", "Não completou EF (até 5º ano)",
                            "Não completou EF (até 9º ano)", "EF completo", "Médio completo",
                            "Superior completo", "Pós-graduação", "Não sabe")

ORDEM_FAIXA_ETARIA <- c("Menor de 17", "17 anos", "18 anos", "19 anos", "20 anos", "21 anos",
                          "22 anos", "23 anos", "24 anos", "25 anos", "26 a 30", "31 a 35",
                          "36 a 40", "41 a 45", "46 a 50", "51 a 55", "56 a 60", "61 a 65",
                          "66 a 70", "Maior de 70")

df <- df %>%
  mutate(renda_familiar   = factor(renda_familiar,   levels = ORDEM_RENDA),
         escolaridade_mae = factor(escolaridade_mae, levels = ORDEM_ESCOLARIDADE_MAE),
         escolaridade_pai = factor(escolaridade_pai, levels = ORDEM_ESCOLARIDADE_PAI),
         TP_FAIXA_ETARIA  = factor(TP_FAIXA_ETARIA,  levels = ORDEM_FAIXA_ETARIA))

# Padroniza denominações históricas do mesmo curso
df <- df %>%
  mutate(
    CURSO = case_when(
      CURSO %in% c("Estatística", "Estatística e Ciência de Dados") ~ "Estatística",
      TRUE ~ CURSO
    )
  )

# Curso de interesse: sempre aparece nos gráficos comparativos de cursos
CURSOS_INTERESSE <- c("Estatística")
TOP_N_DESEMPENHO <- 15

# Colunas de nota usadas em toda a seção 7
NOTAS_COLS <- c("NU_NOTA_CN", "NU_NOTA_CH", "NU_NOTA_LC", "NU_NOTA_MT", "NU_NOTA_REDACAO", "desempenho")

#===============================================================================
# 1. PERFIL GERAL DOS INGRESSANTES
#===============================================================================

#-------------------------------------------------------------------------------
# 1.1 Sexo
#-------------------------------------------------------------------------------
tab_sexo_ano <- df %>%
  count(ano_sisu, TP_SEXO, name = "n") %>%
  group_by(ano_sisu) %>%
  mutate(pct = round(100 * n / sum(n), 1)) %>%
  ungroup()

tab_sexo_geral <- df %>%
  count(TP_SEXO, name = "n") %>%
  mutate(pct = round(100 * n / sum(n), 1))

g_sexo <- ggplot(tab_sexo_ano,
                 aes(x = ano_sisu, y = pct, color = TP_SEXO, group = TP_SEXO)) +
  geom_line(linewidth = 1.1) +
  geom_point(size = 3) +
  scale_color_manual(values = cores_azul(tab_sexo_ano$TP_SEXO)) +
  scale_x_continuous(breaks = sort(unique(tab_sexo_ano$ano_sisu))) +
  scale_y_continuous(limits = c(0, 100), labels = fmt_pct) +
  labs(title = "Distribuição dos ingressantes por sexo",
       subtitle = "Percentual de ingressantes por ano do Sisu",
       x = "Ano Sisu", y = "Percentual", color = "Sexo")

#-------------------------------------------------------------------------------
# 1.2 Cor/raça
#-------------------------------------------------------------------------------
tab_raca_ano <- df %>%
  filter(!is.na(TP_COR_RACA)) %>%
  count(ano_sisu, TP_COR_RACA, name = "n") %>%
  group_by(ano_sisu) %>%
  mutate(pct = round(100 * n / sum(n), 1)) %>%
  ungroup()

tab_raca_geral <- df %>%
  filter(!is.na(TP_COR_RACA)) %>%
  count(TP_COR_RACA, name = "n") %>%
  mutate(pct = round(100 * n / sum(n), 1)) %>%
  arrange(desc(n))

g_raca <- ggplot(tab_raca_ano,
                 aes(x = ano_sisu, y = pct, color = TP_COR_RACA, group = TP_COR_RACA)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2.5) +
  scale_color_manual(values = cores_azul(tab_raca_ano$TP_COR_RACA)) +
  scale_x_continuous(breaks = sort(unique(tab_raca_ano$ano_sisu))) +
  scale_y_continuous(labels = fmt_pct) +
  labs(title = "Distribuição dos ingressantes por cor/raça",
       subtitle = "Percentual por ano do Sisu",
       x = "Ano Sisu", y = "Percentual", color = "Cor/raça")

#-------------------------------------------------------------------------------
# 1.3 Faixa etária
#-------------------------------------------------------------------------------
tab_idade_ano <- df %>%
  count(ano_sisu, TP_FAIXA_ETARIA, name = "n") %>%
  group_by(ano_sisu) %>%
  mutate(pct = round(100 * n / sum(n), 1)) %>%
  ungroup()

tab_idade_geral <- df %>%
  count(TP_FAIXA_ETARIA, name = "n") %>%
  mutate(pct = round(100 * n / sum(n), 1))

g_idade <- ggplot(tab_idade_geral, aes(x = TP_FAIXA_ETARIA, y = pct)) +
  geom_col(fill = AZUL_PRINCIPAL, width = 0.8) +
  scale_y_continuous(labels = fmt_pct) +
  labs(title = "Distribuição etária dos ingressantes", x = "Faixa etária", y = "Percentual") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#-------------------------------------------------------------------------------
# 1.4 Modalidade de ingresso
#-------------------------------------------------------------------------------
tab_modalidade_ano <- df %>%
  count(ano_sisu, MODALIDADE_PADRONIZADA, name = "n") %>%
  group_by(ano_sisu) %>%
  mutate(pct = 100 * n / sum(n)) %>%
  ungroup()

tab_modalidade_geral <- df %>%
  count(MODALIDADE_PADRONIZADA, name = "n") %>%
  mutate(pct = round(100 * n / sum(n), 1)) %>%
  arrange(desc(n))

# (a versão anterior deste gráfico, com escala fixa, era sobrescrita mais
#  abaixo no script pela versão com escala livre; ficou só esta)
g_modalidade <- ggplot(tab_modalidade_ano,
                       aes(x = ano_sisu, y = pct, group = 1)) +
  geom_line(linewidth = 1, color = AZUL_ESCURO) +
  geom_point(size = 2.3, color = AZUL_ESCURO) +
  facet_wrap(~ MODALIDADE_PADRONIZADA, ncol = 3, scales = "free_y") +
  scale_x_continuous(breaks = sort(unique(tab_modalidade_ano$ano_sisu))) +
  scale_y_continuous(labels = fmt_pct) +
  labs(title = "Evolução das modalidades de ingresso",
       subtitle = "Participação de cada modalidade entre os ingressantes por ano",
       x = "Ano Sisu", y = "Percentual")

#-------------------------------------------------------------------------------
# 1.5 Renda familiar
#-------------------------------------------------------------------------------
tab_renda_geral <- df %>%
  count(renda_familiar, name = "n") %>%
  mutate(pct = round(100 * n / sum(n), 1))

tab_renda_ano <- df %>%
  count(ano_sisu, renda_familiar, name = "n") %>%
  group_by(ano_sisu) %>%
  mutate(pct = round(100 * n / sum(n), 1)) %>%
  ungroup()

g_renda <- ggplot(tab_renda_geral, aes(x = renda_familiar, y = pct)) +
  geom_col(fill = AZUL_PRINCIPAL, width = 0.8) +
  scale_y_continuous(labels = fmt_pct) +
  labs(title = "Distribuição da renda familiar", x = "Faixa de renda", y = "Percentual") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))



# ============================================================
# DESEMPENHO SEGUNDO A RENDA FAMILIAR
# ============================================================

# Base para análise
df_renda <- df %>%
  mutate(
    renda_familiar = forcats::fct_na_value_to_level(
      renda_familiar,
      level = "Não Informado"
    )
  )

# Conferir a ordem
levels(df_renda$renda_familiar)

# Estatísticas descritivas por renda familiar
tab_desempenho_renda <- df_renda %>%
  group_by(renda_familiar) %>%
  summarise(
    n = n(),
    media = mean(desempenho, na.rm = TRUE),
    mediana = median(desempenho, na.rm = TRUE),
    dp = sd(desempenho, na.rm = TRUE),
    .groups = "drop"
  )

# Boxplot do desempenho segundo a renda familiar
g_desempenho_renda <- grafico_boxplot_tcc(
  df_renda,
  categoria = "renda_familiar",
  titulo = "Desempenho segundo a renda familiar",
  subtitulo = "Notas de ingresso na UFES por faixa de renda",
  rotulo_categoria = "Renda familiar",
  ordenar_mediana = FALSE
)

# Preserva este gráfico da distribuição: o nome g_desempenho_renda
# será usado novamente na seção 5 para um gráfico de médias.
g_desempenho_renda_distribuicao <- g_desempenho_renda

# Tabela completa de medidas descritivas
tab_medidas_renda <- tab_medidas(
  df_renda,
  "renda_familiar"
)

# Guarda também as medidas com a classe "Não Informado".
tab_medidas_renda_com_na <- tab_medidas_renda

# Visualizar
g_desempenho_renda
tab_medidas_renda

#-------------------------------------------------------------------------------
# 1.6 Escolaridade materna e paterna
#-------------------------------------------------------------------------------

# 1.6.1 Escolaridade Materna
tab_escolaridade_geral <- df %>%
  count(escolaridade_mae, name = "n") %>%
  mutate(pct = round(100 * n / sum(n), 1))

g_escolaridade <- ggplot(tab_escolaridade_geral,
                         aes(x = escolaridade_mae, y = pct)) +
  geom_col(fill = AZUL_PRINCIPAL, width = 0.8) +
  scale_y_continuous(labels = fmt_pct) +
  labs(title = "Escolaridade materna dos ingressantes",
       x = "Escolaridade materna", y = "Percentual") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# 1.6.2 Escolaridade Paterna
tab_escolaridade_pai <- df %>%
  count(escolaridade_pai, name = "n") %>%
  mutate(pct = round(100 * n / sum(n), 1))

g_escolaridade_pai <- ggplot(tab_escolaridade_pai,
                             aes(x = escolaridade_pai, y = pct)) +
  geom_col(fill = AZUL_PRINCIPAL, width = 0.8) +
  scale_y_continuous(labels = fmt_pct) +
  labs(title = "Escolaridade paterna dos ingressantes",
       x = "Escolaridade paterna", y = "Percentual") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#-------------------------------------------------------------------------------
# 1.7 Tipo de escola e dependência administrativa
#-------------------------------------------------------------------------------
tab_tipo_escola_ano <- df %>%
  count(ano_sisu, tipo_escola, name = "n") %>%
  group_by(ano_sisu) %>%
  mutate(pct = round(100 * n / sum(n), 1)) %>%
  ungroup()

tab_tipo_escola_geral <- df %>%
  count(tipo_escola, name = "n") %>%
  mutate(pct = round(100 * n / sum(n), 1)) %>%
  arrange(desc(n))

g_tipo_escola <- ggplot(tab_tipo_escola_ano,
                        aes(x = ano_sisu, y = pct, color = tipo_escola, group = tipo_escola)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2.5) +
  scale_color_manual(values = cores_azul(tab_tipo_escola_ano$tipo_escola)) +
  scale_x_continuous(breaks = sort(unique(tab_tipo_escola_ano$ano_sisu))) +
  scale_y_continuous(labels = fmt_pct) +
  labs(title = "Origem escolar dos ingressantes",
       subtitle = "Distribuição percentual por ano do Sisu",
       x = "Ano Sisu", y = "Percentual", color = "Tipo de escola")



#---------- 
# SEM FILTRO 

tab_dep_adm_ano <- df %>%
  filter(!is.na(dep_adm)) %>%
  count(ano_sisu, dep_adm, name = "n") %>%
  group_by(ano_sisu) %>%
  mutate(pct = round(100 * n / sum(n), 1)) %>%
  ungroup()

tab_dep_adm_geral <- df %>%
  count(dep_adm, name = "n") %>%
  mutate(pct = round(100 * n / sum(n), 1)) %>%
  arrange(desc(n))

g_dep_adm <- ggplot(
  tab_dep_adm_ano,
  aes(x = ano_sisu, y = pct, color = dep_adm, group = dep_adm)
) +
  geom_line(linewidth = 1) +
  geom_point(size = 2.5) +
  scale_color_manual(values = cores_azul(tab_dep_adm_ano$dep_adm)) +
  scale_x_continuous(breaks = sort(unique(tab_dep_adm_ano$ano_sisu))) +
  scale_y_continuous(labels = fmt_pct) +
  labs(
    title = "Dependência administrativa da escola de origem",
    subtitle = "Distribuição percentual dos ingressantes da UFES",
    x = "Ano Sisu",
    y = "Percentual",
    color = "Dependência")


df %>%
  summarise(
    total = n(),
    informados = sum(!is.na(dep_adm) &
                       dep_adm != "Não Informado"),
    nao_informados = total - informados,
    percentual_informados = round(100 * informados / total, 2),
    percentual_nao_informados = round(100 * nao_informados / total, 2)
  )
#===============================================================================
# 2. NOTAS E DESEMPENHO GERAL
#===============================================================================
resumo_notas_geral <- df %>%
  summarise(across(all_of(NOTAS_COLS),
                   list(media = ~mean(.x, na.rm = TRUE),
                        mediana = ~median(.x, na.rm = TRUE),
                        dp = ~sd(.x, na.rm = TRUE),
                        q1 = ~quantile(.x, .25, na.rm = TRUE),
                        q3 = ~quantile(.x, .75, na.rm = TRUE),
                        min = ~min(.x, na.rm = TRUE),
                        max = ~max(.x, na.rm = TRUE)),
                   .names = "{.col}_{.fn}"))

df_notas_long <- df %>%
  dplyr::select(CURSO, ano_sisu, all_of(NOTAS_COLS)) %>%
  pivot_longer(cols = all_of(NOTAS_COLS), names_to = "area", values_to = "nota")

g_notas_dist <- ggplot(df_notas_long, aes(x = nota)) +
  geom_histogram(bins = 40, fill = AZUL_PRINCIPAL, color = "white", linewidth = 0.15) +
  facet_wrap(~ area, scales = "free", labeller = as_labeller(ROTULOS_AREAS)) +
  labs(title = "Distribuição das notas dos ingressantes da UFES", x = "Nota", y = "Frequência")

# Tabela de medidas (formato da monografia): uma linha por área
tab_medidas_notas <- df_notas_long %>%
  mutate(area = factor(unname(ROTULOS_AREAS[area]), levels = unname(ROTULOS_AREAS))) %>%
  tab_medidas("area", "nota")

# Desempenho geral ao longo do tempo
tab_desempenho_ano <- df %>%
  group_by(ano_sisu) %>%
  summarise(n = n(),
            media = mean(desempenho, na.rm = TRUE),
            mediana = median(desempenho, na.rm = TRUE),
            dp = sd(desempenho, na.rm = TRUE),
            .groups = "drop")

g_desempenho_ano <- ggplot(tab_desempenho_ano, aes(x = ano_sisu, y = media)) +
  geom_line(linewidth = 1, color = AZUL_ESCURO) +
  geom_point(size = 3, color = AZUL_ESCURO) +
  scale_x_continuous(breaks = sort(unique(tab_desempenho_ano$ano_sisu))) +
  labs(title = "Evolução do desempenho médio dos ingressantes",
       x = "Ano Sisu", y = "Desempenho médio")

#===============================================================================
# 3. DESEMPENHO POR CURSO
#===============================================================================
resumo_notas_curso <- df %>%
  filter(!is.na(CURSO), !is.na(desempenho)) %>%
  group_by(CURSO) %>%
  summarise(
    n = n(),
    across(
      all_of(NOTAS_COLS),
      list(
        media   = ~mean(.x, na.rm = TRUE),
        q1      = ~quantile(.x, 0.25, na.rm = TRUE),
        mediana = ~median(.x, na.rm = TRUE),
        q3      = ~quantile(.x, 0.75, na.rm = TRUE),
        aiq     = ~IQR(.x, na.rm = TRUE),
        dp      = ~sd(.x, na.rm = TRUE)
      ),
      .names = "{.col}_{.fn}"
    ),
    .groups = "drop"
  ) %>%
  arrange(desc(desempenho_media))

# Top 15 por desempenho + Estatística obrigatoriamente incluída
top_desempenho <- resumo_notas_curso %>%
  slice_max(desempenho_media, n = TOP_N_DESEMPENHO, with_ties = FALSE)

cursos_grafico <- union(top_desempenho$CURSO, CURSOS_INTERESSE)

ranking_cursos_grafico <- resumo_notas_curso %>%
  filter(CURSO %in% cursos_grafico) %>%
  arrange(desc(desempenho_media))

# Ordem única dos cursos (maior desempenho médio primeiro), usada em TODOS os
# gráficos por curso para que a leitura seja consistente entre eles.
ordem_cursos <- ranking_cursos_grafico$CURSO

# Ranking: barras azuis, com o curso de interesse destacado em azul escuro
g_ranking_cursos <- ggplot(ranking_cursos_grafico,
                           aes(x = reorder(CURSO, desempenho_media),
                               y = desempenho_media,
                               fill = CURSO %in% CURSOS_INTERESSE)) +
  geom_col(width = 0.8) +
  scale_fill_manual(values = c("TRUE" = AZUL_ESCURO, "FALSE" = AZUL_CLARO),
                    guide = "none") +
  coord_flip() +
  labs(title = "Cursos com maior desempenho médio",
       subtitle = "Top 15 da UFES e Estatística como curso de interesse (em destaque)",
       x = NULL, y = "Desempenho médio")

# Distribuição individual, não apenas média (layout boxplot da monografia)
g_boxplot_cursos <- grafico_boxplot_tcc(
  df %>% filter(CURSO %in% cursos_grafico),
  categoria = "CURSO",
  titulo = "Distribuição do desempenho nos cursos selecionados",
  subtitulo = "Top 15 por desempenho e Estatística",
  rotulo_categoria = NULL,
  rotulo_valor = "Desempenho",
  ordenar_mediana = TRUE
)

# Evolução anual dos cursos selecionados
tab_desempenho_curso_ano <- df %>%
  filter(CURSO %in% cursos_grafico) %>%
  group_by(ano_sisu, CURSO) %>%
  summarise(n = n(),
            media = mean(desempenho, na.rm = TRUE),
            mediana = median(desempenho, na.rm = TRUE),
            .groups = "drop")

g_desempenho_curso_ano <- ggplot(tab_desempenho_curso_ano,
                                 aes(x = ano_sisu, y = media, group = 1)) +
  geom_line(linewidth = .8, color = AZUL_ESCURO) +
  geom_point(size = 2, color = AZUL_ESCURO) +
  facet_wrap(~ factor(CURSO, levels = ordem_cursos), ncol = 3) +
  scale_x_continuous(breaks = sort(unique(tab_desempenho_curso_ano$ano_sisu))) +
  labs(title = "Evolução do desempenho nos cursos selecionados",
       subtitle = "Mesma escala de desempenho em todos os painéis",
       x = "Ano Sisu", y = "Desempenho médio")

# Perfil das cinco áreas do ENEM nos cursos selecionados
perfil_notas_curso <- df %>%
  filter(CURSO %in% cursos_grafico) %>%
  group_by(CURSO) %>%
  summarise(CN = mean(NU_NOTA_CN, na.rm = TRUE),
            CH = mean(NU_NOTA_CH, na.rm = TRUE),
            LC = mean(NU_NOTA_LC, na.rm = TRUE),
            MT = mean(NU_NOTA_MT, na.rm = TRUE),
            Redação = mean(NU_NOTA_REDACAO, na.rm = TRUE),
            .groups = "drop") %>%
  pivot_longer(-CURSO, names_to = "area", values_to = "media")

g_areas_curso <- ggplot(perfil_notas_curso,
                        aes(x = factor(area, levels = c("CN", "CH", "LC", "MT", "Redação")),
                            y = factor(CURSO, levels = rev(ordem_cursos)),
                            fill = media)) +
  geom_tile(color = "white", linewidth = 0.3) +
  scale_fill_gradientn(colours = PALETA_CALOR, name = "Média") +
  labs(title = "Perfil de notas por área nos cursos selecionados",
       x = "Área do ENEM", y = NULL) +
  theme(panel.grid = element_blank(),
        legend.key.width = unit(1.8, "cm"))

# Tabela de medidas por curso (formato da monografia)
tab_medidas_curso <- tab_medidas(df %>% filter(CURSO %in% cursos_grafico), "CURSO") %>%
  arrange(desc(`Média`))

#===============================================================================
# 4. PERFIL DOS CURSOS COM MAIOR DESEMPENHO
#===============================================================================
# Proporções são calculadas dentro de cada curso.

# Sexo
tab_sexo_cursos <- df %>%
  filter(CURSO %in% cursos_grafico, !is.na(TP_SEXO)) %>%
  count(CURSO, TP_SEXO, name = "n") %>%
  group_by(CURSO) %>% mutate(pct = 100 * n / sum(n)) %>% ungroup()

g_sexo_cursos <- ggplot(tab_sexo_cursos,
                        aes(x = factor(CURSO, levels = rev(ordem_cursos)),
                            y = pct, fill = TP_SEXO)) +
  geom_col(position = "fill", width = 0.8, color = "white", linewidth = 0.2) + coord_flip() +
  scale_fill_manual(values = cores_azul(tab_sexo_cursos$TP_SEXO)) +
  scale_y_continuous(labels = percent) +
  labs(title = "Composição por sexo nos cursos selecionados", x = NULL, y = "Proporção", fill = "Sexo")

# Cor/raça
tab_raca_cursos <- df %>%
  filter(CURSO %in% cursos_grafico, !is.na(TP_COR_RACA)) %>%
  count(CURSO, TP_COR_RACA, name = "n") %>%
  group_by(CURSO) %>% mutate(pct = 100 * n / sum(n)) %>% ungroup()

g_raca_cursos <- ggplot(tab_raca_cursos,
                        aes(x = factor(CURSO, levels = rev(ordem_cursos)),
                            y = pct, fill = TP_COR_RACA)) +
  geom_col(position = "fill", width = 0.8, color = "white", linewidth = 0.2) + coord_flip() +
  scale_fill_manual(values = cores_azul(tab_raca_cursos$TP_COR_RACA)) +
  scale_y_continuous(labels = percent) +
  labs(title = "Composição por cor/raça nos cursos selecionados", x = NULL, y = "Proporção", fill = "Cor/raça")

# Tipo de escola
tab_escola_cursos <- df %>%
  filter(CURSO %in% cursos_grafico, !is.na(tipo_escola)) %>%
  count(CURSO, tipo_escola, name = "n") %>%
  group_by(CURSO) %>% mutate(pct = 100 * n / sum(n)) %>% ungroup()

g_escola_cursos <- ggplot(tab_escola_cursos,
                          aes(x = factor(CURSO, levels = rev(ordem_cursos)),
                              y = pct, fill = tipo_escola)) +
  geom_col(position = "fill", width = 0.8, color = "white", linewidth = 0.2) + coord_flip() +
  scale_fill_manual(values = cores_azul(tab_escola_cursos$tipo_escola)) +
  scale_y_continuous(labels = percent) +
  labs(title = "Origem escolar nos cursos selecionados", x = NULL, y = "Proporção", fill = "Tipo de escola")

# Modalidade
tab_modalidade_cursos <- df %>%
  filter(CURSO %in% cursos_grafico, !is.na(MODALIDADE_PADRONIZADA)) %>%
  count(CURSO, MODALIDADE_PADRONIZADA, name = "n") %>%
  group_by(CURSO) %>% mutate(pct = 100 * n / sum(n)) %>% ungroup()

g_modalidade_cursos <- ggplot(tab_modalidade_cursos,
                              aes(x = factor(CURSO, levels = rev(ordem_cursos)),
                                  y = pct, fill = MODALIDADE_PADRONIZADA)) +
  geom_col(position = "fill", width = 0.8, color = "white", linewidth = 0.2) + coord_flip() +
  scale_fill_manual(values = cores_azul(tab_modalidade_cursos$MODALIDADE_PADRONIZADA)) +
  scale_y_continuous(labels = percent) +
  labs(title = "Modalidade de ingresso nos cursos selecionados", x = NULL, y = "Proporção", fill = "Modalidade")

# Renda e escolaridade: tabelas completas para os cursos selecionados
tab_renda_curso <- df %>%
  filter(CURSO %in% cursos_grafico) %>%
  count(CURSO, renda_familiar, name = "n") %>%
  group_by(CURSO) %>% mutate(pct = round(100 * n / sum(n), 1)) %>% ungroup()

g_renda_curso <- ggplot(tab_renda_curso, aes(x = renda_familiar, y = pct)) +
  geom_col(fill = AZUL_PRINCIPAL, width = 0.8) +
  facet_wrap(~ factor(CURSO, levels = ordem_cursos), ncol = 3) +
  scale_y_continuous(labels = fmt_pct) +
  labs(title = "Distribuição da renda familiar por curso",
       subtitle = "Cursos com maiores desempenhos e Estatística",
       x = "Renda familiar", y = "Percentual de ingressantes") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5, size = 7),
        strip.text = element_text(face = "bold", size = 9))

tab_escolaridade_mae_curso <- df %>%
  filter(CURSO %in% cursos_grafico) %>%
  count(CURSO, escolaridade_mae, name = "n") %>%
  group_by(CURSO) %>% mutate(pct = round(100 * n / sum(n), 1)) %>% ungroup()

g_escolaridade_curso <- ggplot(tab_escolaridade_mae_curso,
                               aes(x = escolaridade_mae, y = pct)) +
  geom_col(fill = AZUL_PRINCIPAL, width = 0.8) +
  facet_wrap(~ factor(CURSO, levels = ordem_cursos), ncol = 3) +
  scale_y_continuous(labels = fmt_pct) +
  labs(title = "Distribuição da escolaridade materna por curso",
       subtitle = "Cursos com maiores desempenhos e Estatística",
       x = "Escolaridade materna", y = "Percentual de ingressantes") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5, size = 7),
        strip.text = element_text(face = "bold", size = 9))

tab_escolaridade_pai_curso <- df %>%
  filter(CURSO %in% cursos_grafico) %>%
  count(CURSO, escolaridade_pai, name = "n") %>%
  group_by(CURSO) %>% mutate(pct = round(100 * n / sum(n), 1)) %>% ungroup()

g_escolaridade_curso_pai <- ggplot(tab_escolaridade_pai_curso,
                                   aes(x = escolaridade_pai, y = pct)) +
  geom_col(fill = AZUL_PRINCIPAL, width = 0.8) +
  facet_wrap(~ factor(CURSO, levels = ordem_cursos), ncol = 3) +
  scale_y_continuous(labels = fmt_pct) +
  labs(title = "Distribuição da escolaridade paterna por curso",
       subtitle = "Cursos com maiores desempenhos e Estatística",
       x = "Escolaridade paterna", y = "Percentual de ingressantes") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5, size = 7),
        strip.text = element_text(face = "bold", size = 9))


#===============================================================================
# 5. DESEMPENHO E CARACTERÍSTICAS SOCIOECONÔMICAS
#===============================================================================
#-------------------------------------------------------------------------------
# 5.1 Renda familiar x desempenho
#-------------------------------------------------------------------------------
# Não substitui o gráfico de distribuição preservado na seção 1.5.
tab_desempenho_renda <- df %>%
  filter(!is.na(renda_familiar)) %>%
  group_by(renda_familiar) %>%
  summarise(n = n(), media = mean(desempenho, na.rm = TRUE),
            mediana = median(desempenho, na.rm = TRUE),
            dp = sd(desempenho, na.rm = TRUE), .groups = "drop")

g_desempenho_renda <- ggplot(tab_desempenho_renda,
                             aes(x = renda_familiar, y = media, group = 1)) +
  geom_line(linewidth = 1, color = AZUL_ESCURO) +
  geom_point(size = 2.5, color = AZUL_ESCURO) +
  labs(title = "Desempenho médio segundo a renda familiar",
       x = "Renda familiar", y = "Desempenho médio") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

tab_medidas_renda <- tab_medidas(df, "renda_familiar")

#-------------------------------------------------------------------------------
# 5.2 Escolaridade materna e paterna x desempenho
#-------------------------------------------------------------------------------
tab_desempenho_escolaridade_mae <- df %>%
  group_by(escolaridade_mae) %>%
  summarise(n = n(), media = mean(desempenho, na.rm = TRUE),
            mediana = median(desempenho, na.rm = TRUE),
            dp = sd(desempenho, na.rm = TRUE), .groups = "drop")

g_desempenho_escolaridade_mae <- ggplot(tab_desempenho_escolaridade_mae,
                                        aes(x = escolaridade_mae, y = media, group = 1)) +
  geom_line(linewidth = 1, color = AZUL_ESCURO) +
  geom_point(size = 2.5, color = AZUL_ESCURO) +
  labs(title = "Desempenho médio segundo a escolaridade materna",
       x = "Escolaridade materna", y = "Desempenho médio") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

tab_desempenho_escolaridade_pai <- df %>%
  group_by(escolaridade_pai) %>%
  summarise(n = n(), media = mean(desempenho, na.rm = TRUE),
            mediana = median(desempenho, na.rm = TRUE),
            dp = sd(desempenho, na.rm = TRUE), .groups = "drop")

g_desempenho_escolaridade_pai <- ggplot(tab_desempenho_escolaridade_pai,
                                        aes(x = escolaridade_pai, y = media, group = 1)) +
  geom_line(linewidth = 1, color = AZUL_ESCURO) +
  geom_point(size = 2.5, color = AZUL_ESCURO) +
  labs(title = "Desempenho médio segundo a escolaridade paterna",
       x = "Escolaridade paterna", y = "Desempenho médio") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

tab_medidas_escolaridade_mae <- tab_medidas(df, "escolaridade_mae")
tab_medidas_escolaridade_pai <- tab_medidas(df, "escolaridade_pai")

#-------------------------------------------------------------------------------
# 5.3 Cor/raça x desempenho
#-------------------------------------------------------------------------------
tab_desempenho_raca <- df %>%
  filter(!is.na(TP_COR_RACA)) %>%
  group_by(TP_COR_RACA) %>%
  summarise(n = n(), media = mean(desempenho, na.rm = TRUE),
            mediana = median(desempenho, na.rm = TRUE),
            dp = sd(desempenho, na.rm = TRUE), .groups = "drop")

g_desempenho_raca <- grafico_boxplot_tcc(
  df, categoria = "TP_COR_RACA",
  titulo = "Distribuição do desempenho segundo cor/raça",
  rotulo_categoria = "Cor/raça", ordenar_mediana = TRUE
)

tab_medidas_raca <- tab_medidas(df, "TP_COR_RACA")

#-------------------------------------------------------------------------------
# 5.4 Tipo de escola x desempenho
#-------------------------------------------------------------------------------
tab_desempenho_escola <- df %>%
  filter(!is.na(tipo_escola), tipo_escola != "Não Informado") %>%
  group_by(tipo_escola) %>%
  summarise(n = n(), media = mean(desempenho, na.rm = TRUE),
            mediana = median(desempenho, na.rm = TRUE),
            dp = sd(desempenho, na.rm = TRUE), .groups = "drop")

g_desempenho_escola <- grafico_boxplot_tcc(
  df %>% filter(!is.na(tipo_escola), tipo_escola != "Não Informado"),
  categoria = "tipo_escola",
  titulo = "Desempenho segundo o tipo de escola de origem",
  rotulo_categoria = "Tipo de escola", ordenar_mediana = TRUE
)

tab_medidas_escola <- tab_medidas(
  df %>% filter(tipo_escola != "Não Informado"), "tipo_escola"
)

#-------------------------------------------------------------------------------
# 5.5 Dependência administrativa x desempenho
#-------------------------------------------------------------------------------
tab_desempenho_dep <- df %>%
  group_by(dep_adm) %>%
  summarise(n = n(), media = mean(desempenho, na.rm = TRUE),
            mediana = median(desempenho, na.rm = TRUE),
            dp = sd(desempenho, na.rm = TRUE), .groups = "drop")

g_desempenho_dep <- grafico_boxplot_tcc(
  df %>% filter(tipo_escola == "Pública", !is.na(dep_adm), dep_adm != "Não Informado"),
  categoria = "dep_adm",
  titulo = "Desempenho segundo a dependência administrativa",
  subtitulo = "Ingressantes provenientes de escolas públicas",
  rotulo_categoria = "Dependência administrativa", ordenar_mediana = TRUE
)

tab_medidas_dep <- tab_medidas(
  df %>% filter(tipo_escola == "Pública", dep_adm != "Não Informado"), "dep_adm"
)

# Versões da rede pública são mantidas antes da análise sem filtro.
# Não altera nenhum elemento do gráfico, somente mantém uma referência.
g_desempenho_dep_publica <- g_desempenho_dep
# A tabela-resumo da rede pública deve aplicar o mesmo filtro do gráfico.
# A tabela anterior (tab_desempenho_dep) foi calculada sem filtro.
tab_desempenho_dep_publica <- df %>%
  filter(tipo_escola == "Pública", !is.na(dep_adm), dep_adm != "Não Informado") %>%
  group_by(dep_adm) %>%
  summarise(n = n(), media = mean(desempenho, na.rm = TRUE),
            mediana = median(desempenho, na.rm = TRUE),
            dp = sd(desempenho, na.rm = TRUE), .groups = "drop")
tab_medidas_dep_publica <- tab_medidas_dep

#------------------------
# SEM FILTRO DE ESCOLA

# Base com todas as escolas e NA como "Não Informado"
df_dep <- df %>%
  mutate(
    dep_adm = tidyr::replace_na(
      as.character(dep_adm),
      "Não Informado"
    )
  )

# Estatísticas descritivas por dependência administrativa
tab_desempenho_dep <- df_dep %>%
  group_by(dep_adm) %>%
  summarise(
    n = n(),
    media = mean(desempenho, na.rm = TRUE),
    mediana = median(desempenho, na.rm = TRUE),
    dp = sd(desempenho, na.rm = TRUE),
    .groups = "drop"
  )

# Boxplot de desempenho de todas as escolas
g_desempenho_dep <- grafico_boxplot_tcc(
  df_dep,
  categoria = "dep_adm",
  titulo = "Desempenho segundo a dependência administrativa",
  subtitulo = "Ingressantes da UFES de todas as redes de ensino",
  rotulo_categoria = "Dependência administrativa",
  ordenar_mediana = TRUE
)

# Tabela de medidas descritivas
tab_medidas_dep <- tab_medidas(
  df_dep,
  "dep_adm"
)


#===============================================================================
# 6. ESTRUTURA ACADÊMICA: CAMPUS, CENTRO E CURSO
#===============================================================================

if (all(c("CAMPUS", "CENTRO") %in% names(df))) {
  
  #===========================================================================
  # 6.1. DESEMPENHO POR CAMPUS
  #===========================================================================
  
  tab_desempenho_campus <- df %>%
    filter(!is.na(CAMPUS)) %>%
    group_by(CAMPUS) %>%
    summarise(
      n = sum(!is.na(desempenho)),
      media = mean(desempenho, na.rm = TRUE),
      mediana = median(desempenho, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    arrange(desc(media))
  
  # Gráfico de linhas - média
  g_media_campus <- ggplot(
    tab_desempenho_campus,
    aes(x = reorder(CAMPUS, media), y = media, group = 1)
  ) +
    geom_line(color = AZUL_ESCURO, linewidth = 0.8) +
    geom_point(color = AZUL_ESCURO, size = 2.5) +
    labs(
      title = "Desempenho médio por campus",
      x = "Campus",
      y = "Desempenho médio"
    )
  
  # Boxplot - distribuição (layout da monografia)
  g_boxplot_campus <- grafico_boxplot_tcc(
    df,
    categoria = "CAMPUS",
    titulo = "Distribuição do desempenho por campus",
    rotulo_categoria = "Campus",
    ordenar_mediana = TRUE
  )
  
  tab_medidas_campus <- tab_medidas(df, "CAMPUS") %>% arrange(desc(`Média`))
  
  
  #===========================================================================
  # 6.2. DESEMPENHO POR CENTRO DE ENSINO
  #===========================================================================
  
  tab_desempenho_centro <- df %>%
    filter(!is.na(CENTRO)) %>%
    group_by(CENTRO) %>%
    summarise(
      n = sum(!is.na(desempenho)),
      media = mean(desempenho, na.rm = TRUE),
      mediana = median(desempenho, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    arrange(desc(media))
  
  # Gráfico de linhas - média
  g_media_centro <- ggplot(
    tab_desempenho_centro,
    aes(x = reorder(CENTRO, media), y = media, group = 1)
  ) +
    geom_line(color = AZUL_ESCURO, linewidth = 0.8) +
    geom_point(color = AZUL_ESCURO, size = 2.5) +
    labs(
      title = "Desempenho médio por centro de ensino",
      x = "Centro de ensino",
      y = "Desempenho médio"
    )
  
  # Boxplot - distribuição (layout da monografia)
  g_boxplot_centro <- grafico_boxplot_tcc(
    df,
    categoria = "CENTRO",
    titulo = "Distribuição do desempenho por centro de ensino",
    rotulo_categoria = "Centro de ensino",
    ordenar_mediana = TRUE
  )
  
  tab_medidas_centro <- tab_medidas(df, "CENTRO") %>% arrange(desc(`Média`))
  
  
  #===========================================================================
  # 6.3. VISUALIZAÇÃO
  #===========================================================================
  
  print(g_media_campus)
  print(g_boxplot_campus)
  
  print(g_media_centro)
  print(g_boxplot_centro)
  
}

#===============================================================================
# 7. ANO DE CONCLUSÃO DO ENSINO MÉDIO E TEMPO DE ESPERA ATÉ O INGRESSO
#===============================================================================
#===============================================================================
# 7.1. PADRONIZAÇÃO DO ANO DE CONCLUSÃO
#===============================================================================

# Agrupar todos os anos anteriores a 2007 em uma única categoria.
# Corrigir os participantes que concluiriam o ensino médio no ano da prova.

df <- df %>%
  mutate(
    TP_ANO_CONCLUIU = as.character(TP_ANO_CONCLUIU),
    
    TP_ANO_CONCLUIU = case_when(
      TP_ST_CONCLUSAO == "Concluirá o Ensino Médio nesse ano" ~
        as.character(ANO),
      
      str_detect(TP_ANO_CONCLUIU, "^Antes de") ~
        "Antes de 2007",
      
      suppressWarnings(as.numeric(TP_ANO_CONCLUIU)) < 2007 ~
        "Antes de 2007",
      
      TRUE ~ TP_ANO_CONCLUIU
    )
  )


#===============================================================================
# 7.2. CÁLCULO DO TEMPO ATÉ O INGRESSO
#===============================================================================

# O ENEM de determinado ano é normalmente utilizado para ingresso no ano
# seguinte. Portanto, descontamos um ano para representar o tempo adicional
# de espera após a primeira oportunidade regular de ingresso.

df <- df %>%
  mutate(
    # Ano de conclusão numérico
    ANO_CONCLUSAO_EM = suppressWarnings(
      as.integer(TP_ANO_CONCLUIU)
    ),
    
    # Ano de ingresso numérico
    ANO_INGRESSO_NUM = as.integer(
      as.character(`ANO INGRESSO`)
    ),
    
    # Diferença bruta entre os anos
    TEMPO_ATE_INGRESSO = ANO_INGRESSO_NUM - ANO_CONCLUSAO_EM,
    
    # Tempo adicional de espera
    TEMPO_ESPERA_INGRESSO = case_when(
      TEMPO_ATE_INGRESSO >= 1 ~ TEMPO_ATE_INGRESSO - 1,
      TEMPO_ATE_INGRESSO == 0 ~ 0,
      TRUE ~ NA_real_
    )
  )


#===============================================================================
# 7.3. CLASSIFICAÇÃO INDIVIDUAL DO TEMPO DE ESPERA
#===============================================================================

# Mantém todos os intervalos separadamente, sem agrupamento.
# Para conclusões anteriores a 2007, registra um limite inferior de espera.

df <- df %>%
  mutate(
    TEMPO_INGRESSO_CATEGORIA = case_when(
      TP_ANO_CONCLUIU == "Antes de 2007" &
        !is.na(ANO_INGRESSO_NUM) ~
        paste0(
          "Acima de ",
          ANO_INGRESSO_NUM - 2007 - 1,
          " anos"
        ),
      
      TEMPO_ESPERA_INGRESSO == 0 ~ "Sem espera",
      
      TEMPO_ESPERA_INGRESSO == 1 ~ "1 ano",
      
      TEMPO_ESPERA_INGRESSO > 1 ~
        paste0(TEMPO_ESPERA_INGRESSO, " anos"),
      
      TEMPO_ATE_INGRESSO < 0 ~ "Inconsistente",
      
      TRUE ~ "Não informado"
    )
  )


#===============================================================================
# 7.4. AGRUPAMENTO DO TEMPO DE ESPERA
#===============================================================================

# Os tempos de 0 a 3 anos permanecem separados.
# Intervalos maiores são agrupados conforme a distribuição observada.
# Conclusões anteriores a 2007 entram em "Mais de 10 anos",
# considerando ingressos a partir de 2019.

df <- df %>%
  mutate(
    TEMPO_ESPERA_AGRUPADO = case_when(
      TP_ANO_CONCLUIU == "Antes de 2007" &
        !is.na(ANO_INGRESSO_NUM) &
        ANO_INGRESSO_NUM >= 2019 ~
        "Mais de 10 anos",
      
      TEMPO_ESPERA_INGRESSO == 0 ~ "Sem espera",
      TEMPO_ESPERA_INGRESSO == 1 ~ "1 ano",
      TEMPO_ESPERA_INGRESSO == 2 ~ "2 anos",
      TEMPO_ESPERA_INGRESSO == 3 ~ "3 anos",
      
      between(TEMPO_ESPERA_INGRESSO, 4, 5) ~
        "4 a 5 anos",
      
      between(TEMPO_ESPERA_INGRESSO, 6, 10) ~
        "6 a 10 anos",
      
      TEMPO_ESPERA_INGRESSO > 10 ~
        "Mais de 10 anos",
      
      TRUE ~ "Não informado"
    ),
    
    TEMPO_ESPERA_AGRUPADO = factor(
      TEMPO_ESPERA_AGRUPADO,
      levels = c(
        "Sem espera",
        "1 ano",
        "2 anos",
        "3 anos",
        "4 a 5 anos",
        "6 a 10 anos",
        "Mais de 10 anos",
        "Não informado"
      )
    )
  )


#===============================================================================
# 7.5. TABELAS E ESTATÍSTICAS DESCRITIVAS
#===============================================================================

# Frequência dos intervalos individuais

tab_tempo_espera <- df %>%
  count(TEMPO_INGRESSO_CATEGORIA, name = "n") %>%
  mutate(
    percentual = round(100 * n / sum(n), 2)
  ) %>%
  arrange(desc(n))


# Frequência dos intervalos agrupados

tab_tempo_agrupado <- df %>%
  count(TEMPO_ESPERA_AGRUPADO, name = "n") %>%
  mutate(
    percentual = round(100 * n / sum(n), 2)
  )


# Estatísticas descritivas dos tempos exatos

resumo_tempo_espera <- df %>%
  summarise(
    n_total = n(),
    n_validos = sum(!is.na(TEMPO_ESPERA_INGRESSO)),
    n_ausentes = sum(is.na(TEMPO_ESPERA_INGRESSO)),
    
    media = mean(TEMPO_ESPERA_INGRESSO, na.rm = TRUE),
    mediana = median(TEMPO_ESPERA_INGRESSO, na.rm = TRUE),
    dp = sd(TEMPO_ESPERA_INGRESSO, na.rm = TRUE),
    
    q1 = quantile(TEMPO_ESPERA_INGRESSO, 0.25, na.rm = TRUE),
    q3 = quantile(TEMPO_ESPERA_INGRESSO, 0.75, na.rm = TRUE),
    
    minimo = min(TEMPO_ESPERA_INGRESSO, na.rm = TRUE),
    maximo = max(TEMPO_ESPERA_INGRESSO, na.rm = TRUE)
  )

# Desempenho segundo o tempo de espera (formato da monografia)
tab_medidas_espera <- tab_medidas(df, "TEMPO_ESPERA_AGRUPADO")

# Para visualizar as tabelas no RStudio (opcional):
# View(tab_tempo_espera); View(tab_tempo_agrupado); View(resumo_tempo_espera)


#===============================================================================
# 7.6. GRÁFICO: DISTRIBUIÇÃO DO TEMPO DE ESPERA
#===============================================================================

g_tempo_espera <- ggplot(
  tab_tempo_agrupado,
  aes(
    x = TEMPO_ESPERA_AGRUPADO,
    y = percentual
  )
) +
  geom_col(fill = AZUL_PRINCIPAL, width = 0.8) +
  labs(
    title = "Tempo de espera até o ingresso na UFES",
    subtitle = "Intervalo entre o término do Ensino Médio e o Ingresso na UFES",
    x = "Tempo de espera",
    y = "Percentual (%)"
  ) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

#===============================================================================
# 7.7. GRÁFICO: DESEMPENHO SEGUNDO O TEMPO DE ESPERA
#===============================================================================

g_desempenho_espera <- grafico_boxplot_tcc(
  df %>% filter(!is.na(desempenho)),
  categoria = "TEMPO_ESPERA_AGRUPADO",
  titulo = "Desempenho segundo o tempo de espera até o ingresso",
  rotulo_categoria = "Tempo de espera",
  ordenar_mediana = FALSE
)


#===============================================================================
# 7.8. VISUALIZAÇÃO DOS GRÁFICOS
#===============================================================================

print(g_tempo_espera)
print(g_desempenho_espera)


#===============================================================================
# 8. NÚMERO DE MORADORES NA RESIDÊNCIA
#===============================================================================

# Tabela de frequência
tab_moradores <- df %>%
  filter(!is.na(moradores)) %>%
  count(moradores, name = "n") %>%
  mutate(
    moradores = as.integer(as.character(moradores)),
    percentual = round(100 * n / sum(n), 2)
  ) %>%
  arrange(moradores)

g_moradores_percentual <- ggplot(
  tab_moradores,
  aes(x = factor(moradores), y = percentual)
) +
  geom_col(fill = AZUL_PRINCIPAL, width = 0.9) +
  labs(
    title = "Distribuição do número de moradores na residência",
    x = "Número de moradores",
    y = "Percentual (%)"
  )

#===============================================================================
# 9. COMPARAÇÃO DE ESCOLARIDADE DO PAI E DA MÃE
#===============================================================================

ordem_escolaridade <- c(
  "Nunca estudou",
  "Não completou EF (até 5º ano)",
  "Não completou EF (até 9º ano)",
  "EF completo",
  "Médio completo",
  "Superior completo",
  "Pós-graduação"
)

df_escolaridade <- df %>%
  dplyr::select(escolaridade_pai, escolaridade_mae, desempenho) %>%
  pivot_longer(
    cols = c(escolaridade_pai, escolaridade_mae),
    names_to = "Responsável",
    values_to = "Escolaridade"
  ) %>%
  mutate(
    Responsável = dplyr::recode(
      Responsável,
      escolaridade_pai = "Pai",
      escolaridade_mae = "Mãe"
    ),
    Escolaridade = factor(
      Escolaridade,
      levels = ordem_escolaridade
    )
  )

tab_escolaridade_comparacao <- df_escolaridade %>%
  filter(!is.na(Escolaridade)) %>%
  count(Responsável, Escolaridade) %>%
  group_by(Responsável) %>%
  mutate(percentual = 100 * n / sum(n)) %>%
  ungroup()

# Pai = azul escuro, Mãe = azul claro (usado nos gráficos pai x mãe)
CORES_RESPONSAVEL <- c("Pai" = AZUL_ESCURO, "Mãe" = AZUL_CLARO)

g_escolaridade_comparacao <- ggplot(
  tab_escolaridade_comparacao,
  aes(x = Escolaridade, y = percentual, fill = Responsável)
) +
  geom_col(position = position_dodge(width = 0.8), width = 0.75,
           color = COR_CONTORNO, linewidth = 0.2) +
  scale_fill_manual(values = CORES_RESPONSAVEL) +
  scale_y_continuous(labels = fmt_pct) +
  labs(
    title = "Escolaridade do pai e da mãe",
    x = "Escolaridade",
    y = "Percentual (%)",
    fill = NULL
  ) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

print(g_escolaridade_comparacao)

tab_escolaridade_cruzada <- df %>%
  mutate(
    escolaridade_pai = factor(
      escolaridade_pai, levels = ordem_escolaridade
    ),
    escolaridade_mae = factor(
      escolaridade_mae, levels = ordem_escolaridade
    )
  ) %>%
  filter(
    !is.na(escolaridade_pai),
    !is.na(escolaridade_mae)
  ) %>%
  count(escolaridade_pai, escolaridade_mae, name = "n")


g_escolaridade_cruzada <- ggplot(
  tab_escolaridade_cruzada,
  aes(x = escolaridade_mae, y = escolaridade_pai, fill = n)
) +
  geom_tile(color = "white", linewidth = 0.3) +
  scale_fill_gradientn(
    colours = PALETA_CALOR,
    trans = "sqrt",
    name = "Ingressantes"
  ) +
  labs(
    title = "Relação entre a escolaridade do pai e da mãe",
    x = "Escolaridade da mãe",
    y = "Escolaridade do pai"
  ) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid = element_blank(),
    legend.key.width = unit(1.8, "cm")
  )

print(g_escolaridade_cruzada)

#------------------------------
# Desempenho médio segundo a escolaridade (pai x mãe)
# (antes este objeto se chamava tab_desempenho_escolaridade /
#  g_desempenho_escolaridade e sobrescrevia o da seção 5.2; renomeado
#  para _pais. O arquivo 05_desempenho_escolaridade.* continua sendo
#  exportado a partir deste objeto, como na versão anterior.)

tab_desempenho_escolaridade_pais <- df_escolaridade %>%
  group_by(Responsável, Escolaridade) %>%
  summarise(
    n = n(),
    media = mean(desempenho),
    mediana = median(desempenho),
    .groups = "drop"
  )

g_desempenho_escolaridade_pais <- ggplot(
  tab_desempenho_escolaridade_pais,
  aes(
    x = Escolaridade,
    y = media,
    group = Responsável,
    color = Responsável,
    shape = Responsável
  )
) +
  geom_line(linewidth = 0.9) +
  geom_point(size = 2.5) +
  scale_color_manual(values = CORES_RESPONSAVEL) +
  labs(
    title = "Desempenho médio segundo a escolaridade dos pais",
    x = "Escolaridade",
    y = "Desempenho médio",
    color = NULL,
    shape = NULL
  ) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

print(g_desempenho_escolaridade_pais)


#===============================================================================
# DESEMPENHO MÉDIO SEGUNDO A ESCOLARIDADE DO PAI E DA MÃE
#===============================================================================

# Tabela com desempenho médio por combinação

tab_desempenho_pais <- df %>%
  filter(
    !is.na(escolaridade_pai),
    !is.na(escolaridade_mae),
    !is.na(desempenho)
  ) %>%
  group_by(escolaridade_pai, escolaridade_mae) %>%
  summarise(
    n = n(),
    media = mean(desempenho),
    mediana = median(desempenho),
    dp = sd(desempenho),
    .groups = "drop"
  ) %>%
  mutate(
    escolaridade_pai = factor(
      escolaridade_pai,
      levels = ordem_escolaridade
    ),
    escolaridade_mae = factor(
      escolaridade_mae,
      levels = ordem_escolaridade
    )
  )

# Mapa de calor

g_desempenho_pais <- ggplot(
  tab_desempenho_pais,
  aes(
    x = escolaridade_mae,
    y = escolaridade_pai,
    fill = media
  )
) +
  geom_tile(
    color = "white",
    linewidth = 0.3
  ) +
  scale_fill_gradientn(
    colours = PALETA_CALOR,
    name = "Desempenho médio"
  ) +
  labs(
    title = "Desempenho médio segundo a escolaridade dos pais",
    x = "Escolaridade da mãe",
    y = "Escolaridade do pai"
  ) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid = element_blank(),
    legend.key.width = unit(1.8, "cm")
  )

print(g_desempenho_pais)


#===============================================================================
# ANÁLISES COMPLEMENTARES: MODALIDADE, RENDA/ESCOLARIDADE E ORIGEM ESCOLAR
#===============================================================================

# A. Desempenho por modalidade de ingresso
tab_desempenho_modalidade <- df %>%
  filter(!is.na(MODALIDADE_PADRONIZADA), !is.na(desempenho)) %>%
  group_by(MODALIDADE_PADRONIZADA) %>%
  summarise(n = n(), media = mean(desempenho), mediana = median(desempenho),
            dp = sd(desempenho), q1 = quantile(desempenho, .25),
            q3 = quantile(desempenho, .75), .groups = "drop") %>%
  arrange(desc(media))

g_desempenho_modalidade <- grafico_boxplot_tcc(
  df %>% filter(!is.na(MODALIDADE_PADRONIZADA), !is.na(desempenho)),
  categoria = "MODALIDADE_PADRONIZADA",
  titulo = "Desempenho segundo a modalidade de ingresso",
  rotulo_categoria = "Modalidade de ingresso",
  rotulo_valor = "Desempenho no ENEM",
  ordenar_mediana = TRUE
)

tab_medidas_modalidade <- tab_medidas(df, "MODALIDADE_PADRONIZADA") %>%
  arrange(desc(`Média`))

# B. Renda x escolaridade dos responsáveis x desempenho
# Exclui "Não sabe" das categorias de escolaridade.
df_renda_escolaridade <- df %>%
  dplyr::select(renda_familiar, escolaridade_pai, escolaridade_mae, desempenho) %>%
  pivot_longer(cols = c(escolaridade_pai, escolaridade_mae),
               names_to = "Responsavel", values_to = "Escolaridade") %>%
  mutate(Responsavel = dplyr::recode(Responsavel,
                              escolaridade_pai = "Pai", escolaridade_mae = "Mãe"),
         Responsavel = factor(Responsavel, levels = c("Pai", "Mãe"))) %>%
  filter(!is.na(renda_familiar), !is.na(Escolaridade),
         as.character(Escolaridade) != "Não sabe", !is.na(desempenho))

tab_renda_escolaridade <- df_renda_escolaridade %>%
  group_by(Responsavel, renda_familiar, Escolaridade) %>%
  summarise(n = n(), media = mean(desempenho),
            mediana = median(desempenho), .groups = "drop")

g_renda_escolaridade <- tab_renda_escolaridade %>%
  filter(n >= 20) %>%
  ggplot(aes(x = Escolaridade, y = renda_familiar, fill = media)) +
  geom_tile(color = "white", linewidth = .3) +
  facet_wrap(~ Responsavel, ncol = 2) +
  scale_fill_gradientn(colours = PALETA_CALOR, name = "Desempenho médio") +
  labs(title = "Desempenho segundo renda e escolaridade dos pais",
       subtitle = "Células com pelo menos 20 ingressantes",
       x = "Escolaridade", y = "Renda familiar") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        panel.grid = element_blank(),
        legend.key.width = unit(1.8, "cm"))

# C. Evolução da diferença entre escola privada e pública
tab_desempenho_escola_ano <- df %>%
  filter(tipo_escola %in% c("Pública", "Privada"),
         !is.na(ano_sisu), !is.na(desempenho)) %>%
  group_by(ano_sisu, tipo_escola) %>%
  summarise(n = n(), media = mean(desempenho),
            mediana = median(desempenho), dp = sd(desempenho), .groups = "drop")

g_desempenho_escola_ano <- ggplot(tab_desempenho_escola_ano,
                                  aes(x = ano_sisu, y = media, color = tipo_escola, group = tipo_escola)) +
  geom_line(linewidth = 1) + geom_point(size = 2.8) +
  scale_color_manual(values = c("Pública" = AZUL_CLARO, "Privada" = AZUL_ESCURO)) +
  scale_x_continuous(breaks = sort(unique(tab_desempenho_escola_ano$ano_sisu))) +
  labs(title = "Evolução do desempenho segundo a origem escolar",
       x = "Ano do Sisu", y = "Desempenho médio", color = "Tipo de escola")

tab_diferenca_escola <- tab_desempenho_escola_ano %>%
  dplyr::select(ano_sisu, tipo_escola, media) %>%
  pivot_wider(names_from = tipo_escola, values_from = media) %>%
  mutate(diferenca = Privada - Pública)

g_diferenca_escola <- ggplot(tab_diferenca_escola,
                             aes(x = ano_sisu, y = diferenca)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
  geom_line(color = AZUL_PRINCIPAL, linewidth = 1) +
  geom_point(color = AZUL_PRINCIPAL, size = 3) +
  scale_x_continuous(breaks = sort(unique(tab_diferenca_escola$ano_sisu))) +
  labs(title = "Diferença de desempenho entre escolas privadas e públicas",
       subtitle = "Média da rede privada menos média da rede pública",
       x = "Ano do Sisu", y = "Diferença entre médias (pontos)")

#===============================================================================
# D. PERÍODO (ANO/SEMESTRE) DE INGRESSO
#-------------------------------------------------------------------------------
#===============================================================================
if ("SEMESTRE" %in% names(df)) {
  
  df <- df %>%
    mutate(PERIODO_INGRESSO = paste0(`ANO INGRESSO`, "/", SEMESTRE))
  
  tab_desempenho_semestre <- df %>%
    filter(!is.na(PERIODO_INGRESSO), !is.na(desempenho)) %>%
    group_by(PERIODO_INGRESSO) %>%
    summarise(n = n(), media = mean(desempenho),
              mediana = median(desempenho), dp = sd(desempenho),
              .groups = "drop") %>%
    arrange(PERIODO_INGRESSO)
  
  g_desempenho_semestre <- ggplot(tab_desempenho_semestre,
                                  aes(x = PERIODO_INGRESSO, y = media, group = 1)) +
    geom_line(linewidth = 1, color = AZUL_ESCURO) +
    geom_point(size = 2.5, color = AZUL_ESCURO) +
    labs(title = "Desempenho médio por período de ingresso",
         x = "Período de ingresso (ano/semestre)", y = "Desempenho médio") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  tab_semestre_curso <- df %>%
    filter(CURSO %in% cursos_grafico, !is.na(PERIODO_INGRESSO), !is.na(desempenho)) %>%
    group_by(CURSO, PERIODO_INGRESSO) %>%
    summarise(n = n(), media = mean(desempenho), .groups = "drop")
  
  g_semestre_curso <- ggplot(tab_semestre_curso,
                             aes(x = PERIODO_INGRESSO, y = media, group = 1)) +
    geom_line(linewidth = 0.8, color = AZUL_ESCURO) +
    geom_point(size = 1.8, color = AZUL_ESCURO) +
    facet_wrap(~ factor(CURSO, levels = ordem_cursos), ncol = 3) +
    labs(title = "Desempenho médio por período de ingresso e curso",
         x = "Período de ingresso (ano/semestre)", y = "Desempenho médio") +
    theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5, size = 6))
  
  tab_medidas_semestre <- tab_medidas(df, "PERIODO_INGRESSO")
}

#===============================================================================
# 10. MAPAS DE CALOR POR FAIXA DE RENDA FAMILIAR
#===============================================================================
# Os gráficos individuais e o painel conjunto são criados aqui e
# exportados na seção 11, junto com todos os demais resultados.

# 10.1 Pasta de destino dos mapas de calor
pasta_graficos <- saida

pasta_renda <- file.path(
  pasta_graficos,
  "desempenho_pais_por_renda"
)

dir.create(
  pasta_renda,
  recursive = TRUE,
  showWarnings = FALSE
)

# 10.2 Ordem das escolaridades
ordem_escolaridade <- c(
  "Nunca estudou",
  "Não completou EF (até 5º ano)",
  "Não completou EF (até 9º ano)",
  "EF completo",
  "Médio completo",
  "Superior completo",
  "Pós-graduação"
)

# 10.3 Ordem das faixas originais de renda
ordem_renda <- c(
  "Sem renda",
  "Até 1 SM",
  "De 1 a 1.5 SM",
  "De 1.5 a 2 SM",
  "De 2 a 2.5 SM",
  "De 2.5 a 3 SM",
  "De 3 a 4 SM",
  "De 4 a 5 SM",
  "De 5 a 6 SM",
  "De 6 a 7 SM",
  "De 7 a 8 SM",
  "De 8 a 9 SM",
  "De 9 a 10 SM",
  "De 10 a 12 SM",
  "De 12 a 15 SM",
  "De 15 a 20 SM",
  "Acima de 20 SM"
)

# 10.4 Preparar a base
dados_heatmap <- df %>%
  mutate(
    escolaridade_pai = as.character(escolaridade_pai),
    escolaridade_mae = as.character(escolaridade_mae),
    renda_familiar = as.character(renda_familiar),
    
    escolaridade_pai = factor(
      escolaridade_pai,
      levels = ordem_escolaridade
    ),
    
    escolaridade_mae = factor(
      escolaridade_mae,
      levels = ordem_escolaridade
    ),
    
    renda_familiar = factor(
      renda_familiar,
      levels = ordem_renda,
      ordered = TRUE
    )
  ) %>%
  filter(
    !is.na(desempenho),
    !is.na(escolaridade_pai),
    !is.na(escolaridade_mae),
    !is.na(renda_familiar)
  ) %>%
  group_by(
    renda_familiar,
    escolaridade_pai,
    escolaridade_mae
  ) %>%
  summarise(
    media = mean(desempenho),
    n = n(),
    .groups = "drop"
  ) %>%
  complete(
    renda_familiar,
    escolaridade_pai,
    escolaridade_mae,
    fill = list(n = 0)
  ) %>%
  mutate(
    escolaridade_pai = factor(
      escolaridade_pai,
      levels = ordem_escolaridade
    )
  )

# 10.5 Escala de cores comum a todos os gráficos
# Isso permite comparar as cores entre faixas de renda
limites_cor <- range(
  dados_heatmap$media,
  na.rm = TRUE
)

cores_azuis <- c(
  "#E0F3FC",
  "#9BCFEA",
  "#5BA5D5",
  "#2678B8",
  "#08306B"
)

# 10.6 Função para gerar um mapa de calor
criar_heatmap <- function(faixa, titulo = TRUE) {
  
  dados_faixa <- dados_heatmap %>%
    filter(as.character(renda_familiar) == faixa)
  
  ggplot(
    dados_faixa,
    aes(
      x = escolaridade_mae,
      y = escolaridade_pai,
      fill = media
    )
  ) +
    geom_tile(
      color = "white",
      linewidth = 0.35
    ) +
    scale_fill_gradientn(
      colours = cores_azuis,
      limits = limites_cor,
      na.value = "#F1F1F1",
      name = "Desempenho médio"
    ) +
    scale_x_discrete(drop = FALSE) +
    scale_y_discrete(drop = FALSE) +
    labs(
      title = if (titulo) paste("Renda:", faixa) else NULL,
      x = "Escolaridade da mãe",
      y = "Escolaridade do pai"
    ) +
    coord_equal() +
    theme_minimal(base_size = 11) +
    theme(
      plot.title = element_text(
        face = "bold",
        hjust = 0.5,
        size = 14
      ),
      axis.text.x = element_text(
        angle = 45,
        hjust = 1,
        size = 9
      ),
      axis.text.y = element_text(size = 9),
      panel.grid = element_blank(),
      legend.position = "bottom",
      plot.margin = margin(12, 12, 12, 12)
    )
}

# 10.7 Gerar os 17 gráficos individuais (exportação na seção 11)
graficos <- list()

for (i in seq_along(ordem_renda)) {
  
  faixa <- ordem_renda[i]
  
  grafico <- criar_heatmap(faixa)
  
  graficos[[faixa]] <- grafico
  
}

# 10.8 Criar imagem conjunta (mesma composição)
grafico_conjunto <- wrap_plots(
  graficos,
  ncol = 3
) +
  plot_layout(guides = "collect") +
  plot_annotation(
    title = "Desempenho médio segundo a escolaridade dos pais, por renda familiar",
    theme = theme(
      plot.title = element_text(
        face = "bold",
        size = 22,
        hjust = 0.5
      )
    )
  ) &
  theme(
    legend.position = "bottom"
  )

# A gravação do painel conjunto ocorre junto das demais exportações.
print(grafico_conjunto)

#===============================================================================
# 11. EXPORTAÇÃO CENTRALIZADA DE TODOS OS GRÁFICOS E TABELAS
#===============================================================================
#-------------------------------------------------------------------------------
# 11.1 Tabelas de medidas adicionais (formato da monografia)
#-------------------------------------------------------------------------------
tab_medidas_ano   <- tab_medidas(df, "ano_sisu")
tab_medidas_sexo  <- tab_medidas(df, "TP_SEXO")
tab_medidas_idade <- tab_medidas(df, "TP_FAIXA_ETARIA")
# A coluna FORMAÇÃO não aparece em todas as versões da base de dados.
# O cálculo só é feito se a coluna existir, sem interromper as imagens.
if ("FORMAÇÃO" %in% names(df)) {
  tab_desempenho_formacao <- tab_medidas(df, "FORMAÇÃO")
} else {
  message("Coluna FORMAÇÃO ausente: tabela de formação não será exportada.")
}
#-------------------------------------------------------------------------------
# 11.2 Gráficos
#-------------------------------------------------------------------------------
G <- function(arquivo, objeto, largura, altura, dpi = 300) {
  list(arquivo = arquivo, objeto = objeto, largura = largura, altura = altura, dpi = dpi)
}

graficos_export <- list(
  # 01 - Perfil geral
  G("01_sexo",                "g_sexo",                  8,  5),
  G("01_raca",                "g_raca",                  9,  5),
  G("01_idade",               "g_idade",                 9,  6),
  G("01_modalidade",          "g_modalidade",           11,  8),
  G("01_renda",               "g_renda",                10,  6),
  G("01_desempenho_renda_distribuicao", "g_desempenho_renda_distribuicao", 10, 6),
  G("01_escolaridade_mae",    "g_escolaridade",          9,  6),
  G("01_escolaridade_pai",    "g_escolaridade_pai",      9,  6),
  G("01_tipo_escola",         "g_tipo_escola",           8,  5),
  G("01_dependencia",         "g_dep_adm",               8,  5),
  # 02 - Notas e desempenho geral
  G("02_notas_distribuicao",  "g_notas_dist",           10,  7),
  G("02_desempenho_ano",      "g_desempenho_ano",        8,  5),
  # 03 - Cursos
  G("03_ranking_cursos",      "g_ranking_cursos",        9,  7),
  G("03_boxplot_cursos",      "g_boxplot_cursos",       10,  8),
  G("03_desempenho_curso_ano","g_desempenho_curso_ano", 12, 10),
  G("03_areas_curso",         "g_areas_curso",           9,  8),
  # 04 - Perfil dos cursos
  G("04_sexo_cursos",         "g_sexo_cursos",          10,  8),
  G("04_raca_cursos",         "g_raca_cursos",          10,  8),
  G("04_escola_cursos",       "g_escola_cursos",        10,  8),
  G("04_modalidade_cursos",   "g_modalidade_cursos",    11,  8),
  G("grafico_renda_por_curso",            "g_renda_curso",            14, 10),
  G("grafico_escolaridade_mae_por_curso", "g_escolaridade_curso",     14, 10),
  G("grafico_escolaridade_pai_por_curso", "g_escolaridade_curso_pai", 14, 10),
  # 05 - Socioeconômico x desempenho
  G("05_desempenho_renda",              "g_desempenho_renda",              10, 6),
  G("05_desempenho_escolaridade",       "g_desempenho_escolaridade_pais",   9, 6),
  G("05_desempenho_escolaridade_mae",   "g_desempenho_escolaridade_mae",    9, 6),
  G("05_desempenho_escolaridade_pai",   "g_desempenho_escolaridade_pai",    9, 6),
  G("05_desempenho_raca",               "g_desempenho_raca",                8, 5),
  G("05_desempenho_escola",             "g_desempenho_escola",              8, 4),
  G("05_desempenho_dependencia",        "g_desempenho_dep",                 8, 4),
  G("05_desempenho_dependencia_rede_publica", "g_desempenho_dep_publica", 8, 4),
  G("05_desempenho_modalidade",         "g_desempenho_modalidade",         10, 7),
  G("05_renda_escolaridade",            "g_renda_escolaridade",            12, 9),
  G("05_desempenho_escola_ano",         "g_desempenho_escola_ano",          8, 5),
  G("05_diferenca_escola",              "g_diferenca_escola",               8, 5),
  # 06 - Campus e centro
  G("06_media_campus",        "g_media_campus",          8,  5),
  G("06_desempenho_campus",   "g_boxplot_campus",        8,  5),
  G("06_media_centro",        "g_media_centro",          8,  6),
  G("06_desempenho_centro",   "g_boxplot_centro",        8,  6),
  # 07 - Tempo de espera e período de ingresso
  G("07_tempo_espera",        "g_tempo_espera",          8,  5.5),
  G("07_desempenho_espera",   "g_desempenho_espera",     8,  6),
  G("07_desempenho_semestre", "g_desempenho_semestre",   7,  5),
  G("07_semestre_curso",      "g_semestre_curso",       12, 10),
  # 08 e 09 - Moradores e escolaridade dos pais
  G("08_moradores",                   "g_moradores_percentual",         8, 5),
  G("09_escolaridade_pai_mae",        "g_escolaridade_comparacao",     10, 6),
  G("09_escolaridade_cruzada",        "g_escolaridade_cruzada",         9, 7),
  G("09_desempenho_pais_mapa_calor",  "g_desempenho_pais",              9, 7)
)

# A lista acima contém TODOS os gráficos simples, incluindo as duas
# visualizações que eram sobrescritas no script original.
# Os 17 mapas de calor e o painel são conferidos logo em seguida.
# Ambiente onde foram criados os objetos: funciona tanto no RStudio quanto
# ao executar source(..., local = TRUE), evitando exportações silenciosas vazias.
ambiente_resultados <- environment()

graficos_ok <- character(0)
graficos_pulados <- character(0)
mapas_ok <- character(0)
mapas_pulados <- character(0)
registros_exportacao <- list()

registrar_saida <- function(tipo, nome, caminho, status, detalhe = "") {
  existe <- file.exists(caminho)
  bytes <- if (existe) as.numeric(file.info(caminho)$size) else NA_real_
  registros_exportacao[[length(registros_exportacao) + 1L]] <<- data.frame(
    tipo = tipo,
    nome = nome,
    caminho = caminho,
    status = status,
    bytes = bytes,
    detalhe = detalhe,
    stringsAsFactors = FALSE
  )
  invisible(NULL)
}

salvar_png <- function(grafico, caminho, largura, altura, dpi = 300,
                       limitsize = TRUE) {
  tryCatch({
    ggplot2::ggsave(
      filename = caminho, plot = grafico,
      width = largura, height = altura, dpi = dpi,
      bg = "white", limitsize = limitsize
    )
    if (!file.exists(caminho) || is.na(file.info(caminho)$size) ||
        file.info(caminho)$size <= 0) {
      return("O arquivo PNG não foi encontrado ou está vazio após ggsave().")
    }
    NULL  # sucesso; retorna uma mensagem apenas em caso de falha
  }, error = function(e) conditionMessage(e))
}

# 11.2.1 Gráficos gerais, perfis e desempenho
for (item in graficos_export) {
  caminho <- file.path(saida, paste0(item$arquivo, ".png"))
  if (!exists(item$objeto, envir = ambiente_resultados, inherits = FALSE)) {
    detalhe <- paste("Objeto ausente:", item$objeto)
  } else {
    detalhe <- salvar_png(get(item$objeto, envir = ambiente_resultados, inherits = FALSE), caminho,
                          item$largura, item$altura, item$dpi)
  }
  status <- if (is.null(detalhe)) "SALVO" else "FALHA"
  registrar_saida("grafico", item$arquivo, caminho, status,
                  if (is.null(detalhe)) "" else detalhe)
  if (identical(status, "SALVO")) {
    graficos_ok <- c(graficos_ok, item$arquivo)
  } else {
    graficos_pulados <- c(graficos_pulados, paste(item$arquivo, "-", detalhe))
  }
}

# 11.2.2 Mapas de calor por renda, com nomes e dimensões originais
for (i in seq_along(ordem_renda)) {
  nome <- sprintf("%02d_mapa_calor_renda", i)
  caminho <- file.path(pasta_renda, paste0(nome, ".png"))
  detalhe <- salvar_png(graficos[[ordem_renda[i]]], caminho,
                        largura = 10, altura = 7, dpi = 300)
  status <- if (is.null(detalhe)) "SALVO" else "FALHA"
  registrar_saida("mapa_renda", nome, caminho, status,
                  if (is.null(detalhe)) "" else detalhe)
  if (identical(status, "SALVO")) {
    mapas_ok <- c(mapas_ok, nome)
  } else {
    mapas_pulados <- c(mapas_pulados, paste(nome, "-", detalhe))
  }
}

# 11.2.3 Painel conjunto dos 17 mapas (dimensões originais)
nome_painel <- "00_todos_mapas_calor_renda"
caminho_painel <- file.path(pasta_renda, paste0(nome_painel, ".png"))
detalhe_painel <- salvar_png(grafico_conjunto, caminho_painel,
                            largura = 24, altura = 35, dpi = 200,
                            limitsize = FALSE)
status_painel <- if (is.null(detalhe_painel)) "SALVO" else "FALHA"
registrar_saida("painel_renda", nome_painel, caminho_painel,
                status_painel, if (is.null(detalhe_painel)) "" else detalhe_painel)
if (identical(status_painel, "SALVO")) {
  mapas_ok <- c(mapas_ok, nome_painel)
} else {
  mapas_pulados <- c(mapas_pulados, paste(nome_painel, "-", detalhe_painel))
}

#-------------------------------------------------------------------------------
# 11.3 Tabelas
#-------------------------------------------------------------------------------
Tb <- function(arquivo, objeto) list(arquivo = arquivo, objeto = objeto)

# 11.3.1 Tabelas de frequência e resumos (CSV padrão, mesmos nomes de antes)
tabelas_export <- list(
  # 01
  Tb("01_sexo_por_ano",               "tab_sexo_ano"),
  Tb("01_sexo_geral",                 "tab_sexo_geral"),
  Tb("01_raca_por_ano",               "tab_raca_ano"),
  Tb("01_raca_geral",                 "tab_raca_geral"),
  Tb("01_idade_por_ano",              "tab_idade_ano"),
  Tb("01_idade_geral",                "tab_idade_geral"),
  Tb("01_modalidade_por_ano",         "tab_modalidade_ano"),
  Tb("01_modalidade_geral",           "tab_modalidade_geral"),
  Tb("01_renda_geral",                "tab_renda_geral"),
  Tb("01_renda_por_ano",              "tab_renda_ano"),
  Tb("01_escolaridade_mae_geral",     "tab_escolaridade_geral"),
  Tb("01_escolaridade_pai_geral",     "tab_escolaridade_pai"),
  Tb("01_tipo_escola_por_ano",        "tab_tipo_escola_ano"),
  Tb("01_tipo_escola_geral",          "tab_tipo_escola_geral"),
  Tb("01_dependencia_por_ano",        "tab_dep_adm_ano"),
  Tb("01_dependencia_geral",          "tab_dep_adm_geral"),
  # 02
  Tb("02_resumo_notas_geral",         "resumo_notas_geral"),
  Tb("02_desempenho_por_ano",         "tab_desempenho_ano"),
  # 03
  Tb("03_resumo_notas_por_curso",     "resumo_notas_curso"),
  Tb("03_ranking_cursos_selecionados","ranking_cursos_grafico"),
  Tb("03_desempenho_curso_ano",       "tab_desempenho_curso_ano"),
  Tb("03_perfil_areas_cursos",        "perfil_notas_curso"),
  # 04
  Tb("04_sexo_cursos_selecionados",         "tab_sexo_cursos"),
  Tb("04_raca_cursos_selecionados",         "tab_raca_cursos"),
  Tb("04_escola_cursos_selecionados",       "tab_escola_cursos"),
  Tb("04_modalidade_cursos_selecionados",   "tab_modalidade_cursos"),
  Tb("04_renda_cursos_selecionados",        "tab_renda_curso"),
  Tb("04_escolaridade_cursos_selecionados", "tab_escolaridade_mae_curso"),
  Tb("04_escolaridade_pai_cursos_selecionados", "tab_escolaridade_pai_curso"),
  # 05
  Tb("05_desempenho_renda",            "tab_desempenho_renda"),
  Tb("05_desempenho_escolaridade",     "tab_desempenho_escolaridade_pais"),
  Tb("05_desempenho_escolaridade_mae", "tab_desempenho_escolaridade_mae"),
  Tb("05_desempenho_escolaridade_pai", "tab_desempenho_escolaridade_pai"),
  Tb("05_desempenho_raca",             "tab_desempenho_raca"),
  Tb("05_desempenho_tipo_escola",      "tab_desempenho_escola"),
  Tb("05_desempenho_dependencia",      "tab_desempenho_dep"),
  Tb("05_desempenho_dependencia_rede_publica", "tab_desempenho_dep_publica"),
  Tb("05_desempenho_modalidade",       "tab_desempenho_modalidade"),
  Tb("05_renda_escolaridade",          "tab_renda_escolaridade"),
  Tb("05_desempenho_escola_ano",       "tab_desempenho_escola_ano"),
  Tb("05_diferenca_escola",            "tab_diferenca_escola"),
  # 06
  Tb("06_desempenho_campus",           "tab_desempenho_campus"),
  Tb("06_desempenho_centro",           "tab_desempenho_centro"),
  # 07
  Tb("07_tempo_espera",                "tab_tempo_espera"),
  Tb("07_tempo_espera_agrupado",       "tab_tempo_agrupado"),
  Tb("07_resumo_tempo_espera",         "resumo_tempo_espera"),
  Tb("07_desempenho_semestre",         "tab_desempenho_semestre"),
  Tb("07_semestre_curso",              "tab_semestre_curso"),
  # 08 e 09
  Tb("08_moradores",                   "tab_moradores"),
  Tb("09_escolaridade_pai_mae",        "tab_escolaridade_comparacao"),
  Tb("09_escolaridade_cruzada",        "tab_escolaridade_cruzada"),
  Tb("09_desempenho_pais",             "tab_desempenho_pais")
)

# 11.3.2 Tabelas de MEDIDAS no formato da monografia
# (Categoria | n | Média | DP | Mín | Q1 | Mediana | Q3 | Máx)
# Salvas com ";" como separador e vírgula decimal, para abrir direto no Excel
# em português e facilitar a passagem para o LaTeX.
tabelas_medidas_export <- list(
  Tb("medidas_02_notas_por_area",          "tab_medidas_notas"),
  Tb("medidas_02_desempenho_por_ano",      "tab_medidas_ano"),
  Tb("medidas_01_desempenho_por_sexo",     "tab_medidas_sexo"),
  Tb("medidas_01_desempenho_por_idade",    "tab_medidas_idade"),
  Tb("medidas_03_desempenho_por_curso",    "tab_medidas_curso"),
  Tb("medidas_05_desempenho_por_renda",    "tab_medidas_renda"),
  Tb("medidas_01_desempenho_por_renda_com_na", "tab_medidas_renda_com_na"),
  Tb("medidas_05_desempenho_escolaridade_mae", "tab_medidas_escolaridade_mae"),
  Tb("medidas_05_desempenho_escolaridade_pai", "tab_medidas_escolaridade_pai"),
  Tb("medidas_05_desempenho_por_raca",     "tab_medidas_raca"),
  Tb("medidas_05_desempenho_por_tipo_escola", "tab_medidas_escola"),
  Tb("medidas_05_desempenho_por_dependencia", "tab_medidas_dep"),
  Tb("medidas_05_desempenho_por_dependencia_rede_publica", "tab_medidas_dep_publica"),
  Tb("medidas_05_desempenho_por_modalidade",  "tab_medidas_modalidade"),
  Tb("medidas_06_desempenho_por_campus",   "tab_medidas_campus"),
  Tb("medidas_06_desempenho_por_centro",   "tab_medidas_centro"),
  Tb("medidas_07_desempenho_por_tempo_espera", "tab_medidas_espera"),
  Tb("medidas_07_desempenho_por_periodo_ingresso", "tab_medidas_semestre")
)

tabelas_ok <- character(0)
tabelas_puladas <- character(0)

exportar_tabela <- function(item, escritor) {
  caminho <- file.path(saida, paste0(item$arquivo, ".csv"))
  if (!exists(item$objeto, envir = ambiente_resultados, inherits = FALSE)) {
    detalhe <- paste("Objeto ausente:", item$objeto)
  } else {
    detalhe <- tryCatch({
      escritor(get(item$objeto, envir = ambiente_resultados, inherits = FALSE), caminho)
      if (!file.exists(caminho) || is.na(file.info(caminho)$size) ||
          file.info(caminho)$size <= 0) {
        "Arquivo CSV não encontrado ou vazio após a exportação."
      } else NULL
    }, error = function(e) conditionMessage(e))
  }
  status <- if (is.null(detalhe)) "SALVO" else "FALHA"
  registrar_saida("tabela", item$arquivo, caminho, status,
                  if (is.null(detalhe)) "" else detalhe)
  if (identical(status, "SALVO")) {
    tabelas_ok <<- c(tabelas_ok, item$arquivo)
  } else {
    tabelas_puladas <<- c(tabelas_puladas,
                          paste(item$arquivo, "-", detalhe))
  }
  invisible(NULL)
}

for (item in tabelas_export)         exportar_tabela(item, readr::write_csv)
for (item in tabelas_medidas_export) exportar_tabela(item, readr::write_excel_csv2)

# 11.3.3 Tabela de desempenho por formação: mantém o CSV original
if ("FORMAÇÃO" %in% names(df)) {
  exportar_tabela(
    Tb("desempenho_por_formacao", "tab_desempenho_formacao"),
    function(dados, caminho) {
      write.csv2(dados, file = caminho,
                 row.names = FALSE, fileEncoding = "UTF-8")
    }
  )
}

#-------------------------------------------------------------------------------
# 11.4 Conferência final de arquivos efetivamente gravados
#-------------------------------------------------------------------------------
relatorio <- dplyr::bind_rows(registros_exportacao)
# Uma segunda checagem impede marcar como salvo um arquivo ausente/vazio.
existe_e_tem_conteudo <- file.exists(relatorio$caminho)
tamanho_final <- rep(NA_real_, nrow(relatorio))
tamanho_final[existe_e_tem_conteudo] <-
  as.numeric(file.info(relatorio$caminho[existe_e_tem_conteudo])$size)
relatorio$bytes <- tamanho_final
relatorio$status[relatorio$status == "SALVO" &
                   (!existe_e_tem_conteudo | is.na(tamanho_final) |
                      tamanho_final <= 0)] <- "FALHA"
relatorio$detalhe[relatorio$status == "FALHA" &
                    relatorio$detalhe == ""] <- "Arquivo ausente ou vazio na conferência final."

caminho_relatorio <- file.path(saida, "relatorio_exportacao.csv")
readr::write_excel_csv2(relatorio, caminho_relatorio)

n_graficos <- length(graficos_export) + length(ordem_renda) + 1L
n_tabelas <- length(tabelas_export) + length(tabelas_medidas_export) +
  as.integer("FORMAÇÃO" %in% names(df))
quantidade_salva <- sum(relatorio$status == "SALVO")
quantidade_falhas <- sum(relatorio$status == "FALHA")

resumo_exportacao <- c(
  paste("Pasta de saída:", normalizePath(saida, mustWork = FALSE)),
  paste("Gráficos esperados:", n_graficos),
  paste("Gráficos gravados:", sum(relatorio$status == "SALVO" &
                                  relatorio$tipo != "tabela")),
  paste("Tabelas esperadas:", n_tabelas),
  paste("Tabelas gravadas:", sum(relatorio$status == "SALVO" &
                                 relatorio$tipo == "tabela")),
  paste("Total de falhas:", quantidade_falhas),
  paste("Relatório detalhado:", caminho_relatorio)
)
writeLines(resumo_exportacao, file.path(saida, "resumo_exportacao.txt"))
cat("\n", paste(resumo_exportacao, collapse = "\n"), "\n", sep = "")

if (quantidade_falhas > 0L) {
  avisos <- relatorio[relatorio$status == "FALHA", c("nome", "detalhe")]
  cat("\nATENÇÃO: arquivos não exportados:\n")
  for (j in seq_len(nrow(avisos))) {
    cat(" -", avisos$nome[j], ": ", avisos$detalhe[j], "\n", sep = "")
  }
  warning("Alguns arquivos não foram salvos. Verifique relatorio_exportacao.csv.",
          call. = FALSE)
} else {
  message("Conferência concluída: todos os gráficos e tabelas esperados foram salvos.")
}

