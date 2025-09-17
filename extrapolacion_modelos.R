libs <- c(
  "broom", "car", "cowplot", "dplyr", "evyth", "extrafont", "fixest",
  "ggplot2", "ggrepel", "ggtext", "glue", "gridExtra", "googlesheets4",
  "gt", "janitor", "knitr", "lmtest", "lubridate", "magick", "modelr",
  "modelsummary", "openxlsx", "patchwork", "plm", "purrr", "readr",
  "readxl", "sandwich", "scales", "stringr", "tibble", "tidytext",
  "tidyverse", "treemapify", "tseries", "urca"
)

lapply(libs, library, character.only = TRUE)




# ------------------------------------------------------------
# 1. IMPORTACIÓN DE MICRODATOS EVYTH 
# ------------------------------------------------------------

# Disponible en linea en https://datos.gob.ar/dataset/turismo-encuesta-viajes-turismo-hogares-evyth---microdatos/archivo/turismo_645e5505-68ee-4cfa-90f9-fcc9a4a34a85
base_turismo_interno <- read.csv("TESIS/bases/evyth_microdatos.csv", header = TRUE, sep = ",")

# 1.1 CHEQUEOS DE CONSISTENCIA DE LA BASE
# Aca se hizo un chequeo para corroborar que hay pares origen-destino con años sin datos. Por ejemplo CABA_CABA tiene datos en 2019-2023, cuando según la definición de "viaje turístico" seria imposible,
# sin embargo, se verificó que la mayoría de casos son "segunda vivienda del hogar"

chequeo <- base_turismo_interno %>%
  select(anio, pondera, aglomerado_origen, provincia_destino,px08) %>%
  crear_etiqueta(c("provincia_destino", "px08")) %>% 
  #filter(aglomerado_origen==32) %>%
  group_by(anio, aglomerado_origen, provincia_destino, px08) %>%
  #filter(provincia_destino=="CABA") %>%
  summarise(viajes=sum(pondera)) %>% 
  ungroup()



# ------------------------------------------------------------
# 2. EXTRAPOLACIÓN AL TOTAL DE LA POBLACIÓN
# ------------------------------------------------------------

# Como la EVYTH solo releva información sobre los viajes realizados por residentes en los principales aglomerados urbanos del país (aprox 60% de la población)
# se implementó un procedimiento de extrapolación basado en el ajuste por población (a través de los datos de EPH)
# Aca se armó un tabla manual que agrupa los aglomerados en sus provincias correspondientes. 
# Hay dos casos particulares: 
# 1.“Viedma - Carmen de Patagones” se decidió asignar este aglomerado completamente a la provincia de Río Negro
# 2.“San Nicolás - Villa Constitución” se optó por asignar el aglomerado completamente a la provincia de Santa Fe
aglomerado_a_prov <- read_xlsx("TESIS/bases/aglomerado_a_prov.xlsx")

# Funcion para correr el script de trabajo de EPH
source("TESIS/EPH.R")
factor <- read.csv("TESIS/bases/factor.csv")

viajes_evyth_ponderados <- base_turismo_interno %>%
  filter(tipo_visitante == 1 & anio!=2024) %>%
  select(anio, trimestre,id_hogar, id_viajes, miembro, aglomerado_origen, provincia_destino, pondera) %>%
  mutate(AGLOMERADO=aglomerado_origen) %>% 
  left_join(aglomerado_a_prov, by = "AGLOMERADO") %>%
  evyth::crear_etiqueta("provincia_destino") %>%
  mutate(
    provincia_destino = case_when(
      provincia_destino %in% c("Buenos Aires (Resto)", "Partidos del GBA (Pcia. Bs. As.)") ~ "Buenos Aires",
      TRUE ~ provincia_destino
    )) %>% 
  group_by(provincia, provincia_destino, anio) %>%
  summarise(
    viajes_ponderados = sum(pondera, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  ungroup() %>% 
  relocate(anio, .before = 1)


viajes_evyth_extrapolados <- viajes_evyth_ponderados %>%
  left_join(factor %>% select(anio, provincia, factor_extrapolacion), by =c("anio", "provincia")) %>%
  mutate(viajes_extrapolados = round(viajes_ponderados * factor_extrapolacion, 1))

#Aca se compararon los resultados de mi extrapolacion con la que hizo MINTUR 
#https://www.yvera.tur.ar/sinta/informe/documentos/descarga/5ad66590815e6574002378.pdf
corroboracion <- viajes_evyth_extrapolados %>%
  group_by (anio) %>%
  summarise(viajes_extrapolados=sum(viajes_extrapolados)/1000000,
            viajes_ponderados=sum(viajes_ponderados)/1000000)

# ------------------------------------------------------------
# 3. ESTIMACION DE LOS MODELOS 
# ------------------------------------------------------------

#Aca se arma la tabla principal incluyendo todas las variables que irá tomando el modelo. 
#Para ello, se utilizacion fuentes de información externas:
#VAB: se utilizó el trabajo conjunto de CEPAL & CEPXXI: "Desagregación provincial del valor agregado bruto de la Argentina, base 2004"
#https://www.cepal.org/es/publicaciones/47900-desagregacion-provincial-valor-agregado-bruto-la-argentina-base-2004
#se tomó el vab total de cada provincia

#Funcion para correr el script de trabajo VAB
source("TESIS/VAB.R")
vab_provincias <- read.csv("tesis/bases/vab_provincias.csv") %>% 
  select(anio,provincia, vab)

#VAB TURISTICO: de la misma fuente de información anterior se tomó el vab de hoteles para calcular la incidencia sobre el total del vab provincial
vab_turismo_provincias <- read.csv("tesis/bases/vab_turismo_provincias.csv") %>% 
  select(anio,provincia, porc_hoteles)

#ITCRM: se tomó el promedio anual del Indice de Tipo de Cambio Real Multilateral del Banco Central para los años 2012-2023
#itcrm <- read.xlsx("tesis/bases/ITCRM.xlsx") 

# ------------------------------------------------------------
# 3.1 MODELOS MCO
# ------------------------------------------------------------

# RESUMEN MODELOS: 
# MODELO 1: Log población origen y destino. Usando lm 
# MODELO 2: Modelo 1 + Log VAB origen y destino. Usando lm
# MODELO 3: Modelo 2 + Log VAB turístico origen y destino. Usando lm
# MODELO 4: Modelo 1 + efectos fijos (origen, destino y año). Usando feols() de fixest
# MODELO 5: Modelo 2 + efectos fijos (origen, destino y año). Usando feols() de fixest
# MODELO 6: Modelo 3 + efectos fijos (origen, destino y año). Usando feols() de fixest

#Base a partir de las proyecciones de población por provincia de INDEC
poblacion_total_anios <- read.xlsx("tesis/bases/poblacion_2012_2024.xlsx")

#Armo la base de datos completa con todas las variables que voy a usar en los modelos
viajes_od <- viajes_evyth_extrapolados %>% 
  select(-factor_extrapolacion) %>% 
  left_join(poblacion_total_anios, by = c("anio", "provincia")) %>%
  rename(poblacion_origen = poblacion) %>%
  left_join(poblacion_total_anios, by = c("anio", "provincia_destino" = "provincia")) %>%
  rename(poblacion_destino = poblacion)

viajes_od <- viajes_od %>%
  left_join(vab_provincias, by = c("anio", "provincia")) %>%
  rename(vab_origen = vab) %>%
  left_join(vab_provincias, by = c("anio", "provincia_destino" = "provincia")) %>%
  rename(vab_destino = vab)

viajes_od <- viajes_od %>%
  left_join(vab_turismo_provincias, by = c("anio", "provincia")) %>%
  rename(vab_turismo_origen = porc_hoteles) %>%
  left_join(vab_turismo_provincias, by = c("anio", "provincia_destino" = "provincia")) %>%
  rename(vab_turismo_destino = porc_hoteles)

viajes_od <- viajes_od %>%
  left_join(itcrm, by = "anio") %>%
  rename(itcrm = ITCRM_prom)



viajes <- viajes_od %>%
  mutate(
    viajes_extrapolados_mill=viajes_extrapolados/1000000,
    poblacion_origen_mill=poblacion_origen/1000000,
    poblacion_destino_mill=poblacion_destino/1000000
  ) %>% 
  mutate(
    log_viajes = log(viajes_extrapolados_mill),  
    log_pob_origen = log(poblacion_origen_mill),
    log_pob_destino = log(poblacion_destino_mill),
    log_vab_origen = log(vab_origen),
    log_vab_destino = log(vab_destino),
    log_vab_turismo_origen = log(vab_turismo_origen),
    log_vab_turismo_destino = log(vab_turismo_destino)
  )


#Chequear si la base tiene 0

vars <- c("vab_origen", "vab_destino","vab_turismo_origen", "vab_turismo_destino", "viajes_extrapolados_mill", "poblacion_origen_mill", "poblacion_destino_mill")

# Cantidad de ceros en cada variable
sapply(viajes[vars], function(x) sum(x == 0, na.rm = TRUE))

# MODELO 1: MCO de viajes y población (origen y destino)
modelo_mco_1 <- lm(
  log_viajes ~ log_pob_origen + log_pob_destino,
  data = viajes
)

# Modelo 2: agregando VAB
modelo_mco_2 <- lm(
  log_viajes ~ log_pob_origen + log_pob_destino + log_vab_origen + log_vab_destino,
  data = viajes
)

# Modelo 3: agregando VAB turístico
modelo_mco_3 <- lm(
  log_viajes ~ log_pob_origen + log_pob_destino + log_vab_origen + log_vab_destino + log_vab_turismo_origen + log_vab_turismo_destino,
  data = viajes
)

#AGREGO EFECTOS FIJOS POR AÑO, ORIGEN Y DESTINO

# Modelo 1 con efectos fijos
modelo_mco_1_ef <- feols(
  log_viajes ~ log_pob_origen + log_pob_destino | provincia + provincia_destino + anio,
  data = viajes
)

# Modelo 2 con efectos fijos
modelo_mco_2_ef <- feols(
  log_viajes ~ log_pob_origen + log_pob_destino + log_vab_origen + log_vab_destino | provincia + provincia_destino + anio,
  data = viajes
)

# Modelo 3 con efectos fijos
modelo_mco_3_ef <- feols(
  log_viajes ~ log_pob_origen + log_pob_destino + log_vab_origen + log_vab_destino + log_vab_turismo_origen + log_vab_turismo_destino | provincia + provincia_destino + anio,
  data = viajes
)

summary(modelo_mco_1)
summary(modelo_mco_2)
summary(modelo_mco_3)
summary(modelo_mco_1_ef)
summary(modelo_mco_2_ef)
summary(modelo_mco_3_ef)

modelos_list_MCO <- list(
  "Modelo 1" = modelo_mco_1,
  "Modelo 2" = modelo_mco_2,
  "Modelo 3" = modelo_mco_3,
  "Modelo 1 (ef)" = modelo_mco_1_ef,
  "Modelo 2 (ef)" = modelo_mco_2_ef,
  "Modelo 3 (ef)" = modelo_mco_3_ef
)

# Extraer coeficientes con broom::tidy y agregar columna modelo
coef_MCO <- bind_rows(
  lapply(names(modelos_list_MCO), function(nombre_modelo) {
    tidy(modelos_list_MCO[[nombre_modelo]], conf.int = FALSE) %>%
      filter(term %in% c(
        "log_pob_origen", "log_pob_destino", 
        "log_vab_origen", "log_vab_destino", 
        "log_vab_turismo_origen", "log_vab_turismo_destino"
      )) %>%
      mutate(modelo = nombre_modelo) %>%
      rename(
        variable = term,
        estimate = estimate,
        std.error = std.error
      ) %>%
      select(modelo, variable, estimate, std.error)
  })
)

#Grafico coef MCO
coef_MCO <- coef_MCO %>%
  filter(!is.na(estimate)) %>%
  mutate(modelo = fct_rev(factor(modelo, levels = c(
    "Modelo 1", "Modelo 2", "Modelo 3", 
    "Modelo 1 (ef)", "Modelo 2 (ef)", "Modelo 3 (ef)"
  )))) %>% 
  mutate(variable = recode(variable,
                           log_pob_destino = "Log Población destino",
                           log_pob_origen = "Log Población origen",
                           log_vab_destino = "Log VAB destino",
                           log_vab_origen = "Log VAB origen",
                           log_vab_turismo_destino = "Log VAB turístico destino",
                           log_vab_turismo_origen = "Log VAB turístico origen"
  ))

grafico_coef_MCO <- ggplot(coef_MCO, aes(y = modelo, x = estimate)) +
  geom_segment(aes(x = 0, xend = estimate, yend = modelo, color = variable), linewidth = 1.2)+
  geom_point(aes(color = variable), size = 2.5) +
  geom_errorbarh(aes(xmin = estimate - 1.96 * std.error, xmax = estimate + 1.96 * std.error, color = variable), height = 0.3) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey40") +
  facet_wrap(~ variable, scales = "free_x", nrow = 2) +
  theme_minimal(base_size = 13) +
  labs(
    x = "Coeficiente estimado",
    y = "",
    title = "Coeficientes estimados en modelos MCO con barras de error 95% Intervalo de Confianza",
    color = "Variable"
  ) +
  theme(
    strip.text = element_text(face = "bold"),
    panel.spacing = unit(1.2, "lines"),
    legend.position = "none"
  )

grafico_coef_MCO

ggsave(filename = "TESIS/salidas/grafico_coef_MCO.png", plot = grafico_coef_MCO, width = 12, height = 10)


#TABLAS COMPARATIVA DE RESULTADOS
modelos_sin_ef <- list(
  `Modelo 1` = modelo_mco_1,
  `Modelo 2` = modelo_mco_2,
  `Modelo 3` = modelo_mco_3
)

modelos_con_ef <- list(
  `Modelo 1 (ef)` = modelo_mco_1_ef,
  `Modelo 2 (ef)` = modelo_mco_2_ef,
  `Modelo 3 (ef)` = modelo_mco_3_ef
)

modelos_todos <- c(modelos_sin_ef, modelos_con_ef)

# Extraer coeficientes y errores
coefs_limpios <- map_dfr(names(modelos_todos), function(nombre) {
  tidy(modelos_todos[[nombre]]) %>%
    mutate(
      modelo = nombre,
      signif = case_when(
        p.value < 0.01 ~ "***",
        p.value < 0.05 ~ "**",
        p.value < 0.1 ~ "*",
        TRUE ~ ""
      ),
      valor_coef = sprintf("%.2f", estimate),
      valor_se = sprintf("%.2f", std.error),
      valor_fmt = paste0(valor_coef, signif)
    )
}) %>%
  select(term, modelo, valor_fmt, valor_se) %>%
  pivot_wider(names_from = modelo, values_from = c(valor_fmt, valor_se), names_sep = "__") %>%
  arrange(term)

modelos_base <- c("Modelo 1", "Modelo 2", "Modelo 3")
columnas_orden <- c(rbind(modelos_base, paste0(modelos_base, " (ef)")))

tabla_coef_list <- list()

for(i in seq_len(nrow(coefs_limpios))) {
  fila <- coefs_limpios[i, ]
  var_name <- fila$term
  if (var_name == "(Intercept)") var_name <- "Intercepto"
  
  fila_coef <- c(Variable = var_name)
  fila_se <- c(Variable = "")
  
  for(m in columnas_orden) {
    col_coef <- paste0("valor_fmt__", m)
    col_se <- paste0("valor_se__", m)
    
    val_coef <- ifelse(col_coef %in% names(fila), fila[[col_coef]], NA)
    val_se <- ifelse(col_se %in% names(fila), fila[[col_se]], NA)
    
    # Cambiar punto decimal por coma en coef y error
    val_coef_fmt <- ifelse(is.na(val_coef), "", str_replace(val_coef, "\\.", ","))
    val_se_fmt <- ifelse(is.na(val_se), "", str_replace(val_se, "\\.", ","))
    
    fila_coef[[m]] <- val_coef_fmt
    fila_se[[m]] <- ifelse(val_se_fmt == "", "", paste0("(", val_se_fmt, ")"))
  }
  
  tabla_coef_list[[length(tabla_coef_list)+1]] <- fila_coef
  tabla_coef_list[[length(tabla_coef_list)+1]] <- fila_se
}

tabla_coef_df <- bind_rows(tabla_coef_list) %>%
  mutate(across(-Variable, ~replace(., . == "NA", ""))) %>%
  select(
    "Variable",
    `Modelo 1`, `Modelo 1 (ef)`,
    `Modelo 2`, `Modelo 2 (ef)`,
    `Modelo 3`, `Modelo 3 (ef)`
  )

# Flag para aplicar estilos
tabla_coef_df <- tabla_coef_df %>%
  mutate(
    es_error = Variable == "",
    es_error_intercepto = es_error & lag(Variable) == "Intercepto",
    aplicar_cursiva = es_error & !es_error_intercepto
  )

tabla_coef_gt <- tabla_coef_df %>%
  select(-es_error, -es_error_intercepto, -aplicar_cursiva) %>%
  gt() %>%
  # Negrita en encabezados
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_column_labels(everything())
  ) %>%
  
  # Fondo gris columnas con ef (cuerpo y títulos)
  tab_style(
    style = cell_fill(color = "#f2f2f2"),
    locations = cells_body(columns = c("Modelo 1 (ef)", "Modelo 2 (ef)", "Modelo 3 (ef)"))
  ) %>%
  tab_style(
    style = cell_fill(color = "#f2f2f2"),
    locations = cells_column_labels(columns = c("Modelo 1 (ef)", "Modelo 2 (ef)", "Modelo 3 (ef)"))
  ) %>%
  
  # Cursiva sólo para errores estándar, excepto error estándar del intercepto
  tab_style(
    style = cell_text(style = "italic"),
    locations = cells_body(rows = tabla_coef_df$aplicar_cursiva)
  ) %>%
  
  # Centrar errores estándar (filas vacías en Variable)
  tab_style(
    style = cell_text(align = "center"),
    locations = cells_body(columns = -Variable)
  ) %>%
  tab_style(
    style = cell_borders(sides = "bottom", color = "transparent", weight = px(0)),
    locations = cells_body(rows = tabla_coef_df$Variable != "")
  ) %>%
  # Eliminar borde superior en filas de error estándar (Variable == "")
  tab_style(
    style = cell_borders(sides = "top", color = "transparent", weight = px(0)),
    locations = cells_body(rows = tabla_coef_df$Variable == "")
  ) %>%
  # Mantener bordes de tabla
  tab_options(
    table.border.top.width = px(1),
    table.border.bottom.width = px(1),
    data_row.padding = px(2)
  )

gtsave(tabla_coef_gt, "tesis/salidas/tabla_coeficientes_MCO.png")

# ============================
# TABLA DE INDICADORES DE BONDAD DE AJUSTE
# ============================

modelos_indicadores <- modelos_todos

# Extraer indicadores
indicadores_df <- map_dfr(names(modelos_indicadores), function(nombre) {
  mod <- modelos_indicadores[[nombre]]
  
  if (inherits(mod, "lm")) {
    s <- summary(mod)
    r2 <- s$r.squared
    r2_adj <- s$adj.r.squared
    n <- s$df[1] + s$df[2]
    error_res <- s$sigma
  } else if(inherits(mod, "fixest")) {
    fit <- fixest::fitstat(mod, type = c("r2", "ar2"))
    r2 <- as.numeric(fit[["r2"]])
    r2_adj <- as.numeric(fit[["ar2"]])
    n <- nobs(mod)
    error_res <- sqrt(deviance(mod) / df.residual(mod))
  } else {
    stop("Modelo no soportado")
  }
  
  tibble(
    Variable = c("R²", "R² ajustado", "Error estándar residual", "N"),
    valor_fmt = c(
      formatC(r2, format = "f", digits = 2, decimal.mark = ","),
      formatC(r2_adj, format = "f", digits = 2, decimal.mark = ","),
      formatC(error_res, format = "f", digits = 2, decimal.mark = ","),
      formatC(n, big.mark = ".", decimal.mark = ",")
    ),
    modelo = nombre
  )
}) %>%
  pivot_wider(names_from = modelo, values_from = valor_fmt)

# Mantener orden de columnas
indicadores_df <- indicadores_df %>%
  select(Variable, all_of(names(modelos_todos)))

# Identificar columnas con "(ef)"
cols_ef <- names(indicadores_df)[grepl("\\(ef\\)", names(indicadores_df))]

indicadores_df <- indicadores_df %>%
  # Cambiar texto "N" a "N° observaciones"
  mutate(Variable = ifelse(Variable == "N", "N° observaciones", Variable)) %>%
  # Reordenar columnas para intercalar Modelo y Modelo (ef)
  select(
    Variable,
    `Modelo 1`, `Modelo 1 (ef)`,
    `Modelo 2`, `Modelo 2 (ef)`,
    `Modelo 3`, `Modelo 3 (ef)`
  )

# Luego la tabla gt
tabla_indicadores_gt <- indicadores_df %>%
  gt() %>%
  tab_header(title = md("**Indicadores de bondad de ajuste Modelos MCO**")) %>%
  cols_label(
    Variable = md("**Indicador**"),
    `Modelo 1` = md("**Modelo 1**"),
    `Modelo 1 (ef)` = md("**Modelo 1 (ef)**"),
    `Modelo 2` = md("**Modelo 2**"),
    `Modelo 2 (ef)` = md("**Modelo 2 (ef)**"),
    `Modelo 3` = md("**Modelo 3**"),
    `Modelo 3 (ef)` = md("**Modelo 3 (ef)**")
  ) %>%
  tab_style(
    style = list(cell_fill(color = "#f0f0f0")),
    locations = cells_body(columns = c(`Modelo 1 (ef)`, `Modelo 2 (ef)`, `Modelo 3 (ef)`))
  ) %>%
  tab_style(
    style = list(cell_fill(color = "#f0f0f0")),
    locations = cells_column_labels(columns = all_of(cols_ef))
  ) %>%
  cols_align(
    align = "left",
    columns = c(Variable)
  ) %>%
  cols_align(
    align = "center",
    columns = c(`Modelo 1`, `Modelo 1 (ef)`, `Modelo 2`, `Modelo 2 (ef)`, `Modelo 3`, `Modelo 3 (ef)`)
  ) %>%
  tab_options(data_row.padding = px(3))


gtsave(tabla_indicadores_gt, "tesis/salidas/tabla_bondad_ajuste_MCO.png")

# ------------------------------------------------------------
# 3.1.2 MODELOS MCO (Pruebas adicionales de bondad del ajuste)
# ------------------------------------------------------------


# QQ-PLOT COMPARATIVO 
qq1 <- ggplot(data = data.frame(resid = resid(modelo_mco_1)), aes(sample = resid)) +
  stat_qq() +
  stat_qq_line() +
  ggtitle("QQ-Plot 1: Modelo MCO 1 (sin efectos fijos)")

qq2 <- ggplot(data = data.frame(resid = resid(modelo_mco_2)), aes(sample = resid)) +
  stat_qq() +
  stat_qq_line() +
  ggtitle("QQ-Plot 2: Modelo MCO 2 (sin efectos fijos)")

qq3 <- ggplot(data = data.frame(resid = resid(modelo_mco_3)), aes(sample = resid)) +
  stat_qq() +
  stat_qq_line() +
  ggtitle("QQ-Plot 3: Modelo MCO 3 (sin efectos fijos)")

qq4 <- ggplot(data = data.frame(resid = resid(modelo_mco_1)), aes(sample = resid)) +
  stat_qq() +
  stat_qq_line() +
  ggtitle("QQ-Plot 1: Modelo MCO 1 (con efectos fijos)")

qq5 <- ggplot(data = data.frame(resid = resid(modelo_mco_2)), aes(sample = resid)) +
  stat_qq() +
  stat_qq_line() +
  ggtitle("QQ-Plot 2: Modelo MCO 2 (con efectos fijos)")

qq6 <- ggplot(data = data.frame(resid = resid(modelo_mco_3)), aes(sample = resid)) +
  stat_qq() +
  stat_qq_line() +
  ggtitle("QQ-Plot 3: Modelo MCO 3 (con efectos fijos)")

grafico_qq_mco <- grid.arrange(qq1, qq2, qq3, ncol = 3)
grafico_qq_mco_ef <- grid.arrange(qq4, qq5, qq6, ncol = 3)
ggsave(filename = "TESIS/salidas/grafico_qq_mco.png", plot = grafico_qq_mco, width = 14, height = 6)
ggsave(filename = "TESIS/salidas/grafico_qq_mco_ef.png", plot = grafico_qq_mco_ef, width = 14, height = 6)



# 2. HETEROSCEDASTICIDAD: Test de Breusch-Pagan
bp_1 <- bptest(modelo_mco_1)
bp_2 <- bptest(modelo_mco_2)
bp_3 <- bptest(modelo_mco_3)


# 3. MULTICOLINEALIDAD: VIF 
vif_1 <- mean(vif(modelo_mco_1))
vif_2 <- mean(vif(modelo_mco_2))
vif_3 <- mean(vif(modelo_mco_3))


# 4. AUTOCORRELACIÓN DE RESIDUOS: Test de Breusch-Godfrey
bg_1 <- bgtest(modelo_mco_1, order = 1)
bg_2 <- bgtest(modelo_mco_2, order = 1)
bg_3 <- bgtest(modelo_mco_3, order = 1)


# 5. RESUMEN DE MÉTRICAS
format_pval <- function(p) {
  if (p < 0.001) {
    return("<0.001")
  } else {
    return(format(round(p, 3), nsmall = 3))
  }
}


diagnosticos <- tibble(
  Indicador = c(
    "Breusch-Pagan (heteroscedasticidad) - p-valor",
    "VIF promedio (multicolinealidad)",
    "Breusch-Godfrey (autocorrelación residuos) - p-valor"
  ),
  `Modelo 1` = c(
    format_pval(bp_1$p.value),
    sprintf("%.3f", vif_1),
    format_pval(bg_1$p.value)
  ),
  `Modelo 2` = c(
    format_pval(bp_2$p.value),
    sprintf("%.3f", vif_2),
    format_pval(bg_2$p.value)
  ),
  `Modelo 3` = c(
    format_pval(bp_3$p.value),
    sprintf("%.3f", vif_3),
    format_pval(bg_3$p.value)
  )
)


diagnosticos_MCO <- diagnosticos %>%
  gt() %>%
  tab_header(
    title = "Comparación de Indicadores de Diagnóstico de Bondad de Ajuste"
  ) %>%
  fmt_missing(
    missing_text = "-"
  ) %>%
  cols_label(
    Indicador = "Indicador",
    `Modelo 1` = "Modelo 1",
    `Modelo 2` = "Modelo 2",
    `Modelo 3` = "Modelo 3"
  )


gtsave(diagnosticos_MCO, "TESIS/salidas/diagnosticos_MCO.png")


# ------------------------------------------------------------
# 3.2 MODELOS PPML
# ------------------------------------------------------------

# RESUMEN MODELOS:

# MODELO 1: Modelo PPML solo con población (sin efectos fijos)
# MODELO 2: Modelo 1 + VAB total (sin efectos fijos)
# MODELO 3: Modelo 2 + VAB turistico (sin efectos fijos)
# MODELO 4: Modelo 1 + efectos fijos (origen, destino y año). 
# MODELO 5: Modelo 2 + efectos fijos (origen, destino y año). 
# MODELO 6: Modelo 3 + efectos fijos (origen, destino y año). 


# Aclaracion de la base de datos: aca se tuvo que completar los pares origen-destino que no tenian viajes para algunos años 
# esto es normal por el tamaño de la muestra de la encuesta, pero para trabajar los modelos PPML se necesitan todos los pares 
# completos. Solo se completo con 0, la variable "viajes_extrapolados_mill". El resto de las variables se rellenó con datos observados.
# Es decir, se usó datos de la misma provincia y año para poblar las variables que faltaban.

#ARMO LA BASE 

viajes_ppml <- viajes %>% 
  select(anio, provincia, provincia_destino, viajes_extrapolados_mill, 
         poblacion_origen_mill, poblacion_destino_mill, 
         vab_origen, vab_destino, 
         vab_turismo_origen, vab_turismo_destino) %>% 
  mutate(id_par = paste(provincia, provincia_destino, sep = "_")) %>%
  complete(id_par, anio = 2012:2023, 
           fill = list(viajes_extrapolados_mill = 0)) %>%
  separate(id_par, into = c("provincia", "provincia_destino"), sep = "_", remove = FALSE) %>%
  group_by(provincia, anio) %>%
  fill(poblacion_origen_mill, vab_origen, vab_turismo_origen, .direction = "downup") %>%
  ungroup() %>%
  group_by(provincia_destino, anio) %>%
  fill(poblacion_destino_mill, vab_destino, vab_turismo_destino, .direction = "downup") %>%
  ungroup() %>%
  mutate(across(
    c(poblacion_origen_mill, poblacion_destino_mill, 
      vab_origen, vab_destino, 
      vab_turismo_origen, vab_turismo_destino),
    ~ ifelse(is.na(.), 1, .)
  )) %>%
  mutate(
    log_pob_origen = log(poblacion_origen_mill),
    log_pob_destino = log(poblacion_destino_mill),
    log_vab_origen = log(vab_origen),
    log_vab_destino = log(vab_destino),
    log_vab_turismo_origen = log(vab_turismo_origen),
    log_vab_turismo_destino = log(vab_turismo_destino)
  ) %>%
  mutate(
    provincia = factor(provincia),
    provincia_destino = factor(provincia_destino),
    anio = factor(anio)
  )

#ESTIMO MODELOS PPML 
# Modelo 1: solo población (sin efectos fijos)
modelo1 <- fepois(
  viajes_extrapolados_mill ~ log_pob_origen + log_pob_destino,
  data = viajes_ppml
)

# Modelo 2: Modelo 1 + VAB total (sin efectos fijos)
modelo2 <- fepois(
  viajes_extrapolados_mill ~ log_pob_origen + log_pob_destino + log_vab_origen + log_vab_destino,
  data = viajes_ppml
)

# Modelo 3: Modelo 2 + VAB turístico (sin efectos fijos)
modelo3 <- fepois(
  viajes_extrapolados_mill ~ log_pob_origen + log_pob_destino + 
    log_vab_origen + log_vab_destino + 
    log_vab_turismo_origen + log_vab_turismo_destino,
  data = viajes_ppml
)

# Modelo 4: Modelo 1 + efectos fijos (origen, destino, año)
modelo4 <- fepois(
  viajes_extrapolados_mill ~ log_pob_origen + log_pob_destino | provincia + provincia_destino + anio,
  data = viajes_ppml
)

# Modelo 5: Modelo 2 + efectos fijos (origen, destino, año)
modelo5 <- fepois(
  viajes_extrapolados_mill ~ log_pob_origen + log_pob_destino + log_vab_origen + log_vab_destino | provincia + provincia_destino + anio,
  data = viajes_ppml
)

# Modelo 6: Modelo 3 + efectos fijos (origen, destino, año)
modelo6 <- fepois(
  viajes_extrapolados_mill ~ log_pob_origen + log_pob_destino + 
    log_vab_origen + log_vab_destino + 
    log_vab_turismo_origen + log_vab_turismo_destino | provincia + provincia_destino + anio,
  data = viajes_ppml
)


summary(modelo1)
summary(modelo2)
summary(modelo3)
summary(modelo4)
summary(modelo5)
summary(modelo6)

modelos_list <- list(
  "Modelo 1" = modelo1,
  "Modelo 2" = modelo2,
  "Modelo 3" = modelo3,
  "Modelo 1 (ef)" = modelo4,
  "Modelo 2 (ef)" = modelo5,
  "Modelo 3 (ef)" = modelo6
)

# Extraer coeficientes con broom::tidy y agregar columna modelo
coef_ppml <- bind_rows(
  lapply(names(modelos_list), function(nombre_modelo) {
    tidy(modelos_list[[nombre_modelo]], conf.int = FALSE) %>%
      filter(term %in% c(
        "log_pob_origen", "log_pob_destino", 
        "log_vab_origen", "log_vab_destino", 
        "log_vab_turismo_origen", "log_vab_turismo_destino"
      )) %>%
      mutate(modelo = nombre_modelo) %>%
      rename(
        variable = term,
        estimate = estimate,
        std.error = std.error
      ) %>%
      select(modelo, variable, estimate, std.error)
  })
)

#Grafico PPML
coef_plot <- coef_ppml %>%
  filter(!is.na(estimate)) %>%
  mutate(modelo = fct_rev(factor(modelo, levels = c(
    "Modelo 1", "Modelo 2", "Modelo 3", 
    "Modelo 1 (ef)", "Modelo 2 (ef)", "Modelo 3 (ef)"
  )))) %>% 
  mutate(variable = recode(variable,
                           log_pob_destino = "Log Población destino",
                           log_pob_origen = "Log Población origen",
                           log_vab_destino = "Log VAB destino",
                           log_vab_origen = "Log VAB origen",
                           log_vab_turismo_destino = "Log VAB turístico destino",
                           log_vab_turismo_origen = "Log VAB turístico origen"
  ))

grafico_coef_PPML <- ggplot(coef_plot, aes(y = modelo, x = estimate)) +
  geom_segment(aes(x = 0, xend = estimate, yend = modelo, color = variable), linewidth = 1.2)+
  geom_point(aes(color = variable), size = 2.5) +
  geom_errorbarh(aes(xmin = estimate - 1.96 * std.error, xmax = estimate + 1.96 * std.error, color = variable), height = 0.3) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey40") +
  facet_wrap(~ variable, scales = "free_x", nrow = 2) +
  theme_minimal(base_size = 13) +
  labs(
    x = "Coeficiente estimado",
    y = "",
    title = "Coeficientes estimados en modelos PPML con barras de error 95% Intervalo de Confianza",
    color = "Variable"
  ) +
  theme(
    strip.text = element_text(face = "bold"),
    panel.spacing = unit(1.2, "lines"),
    legend.position = "none"
  )

grafico_coef_PPML

ggsave(filename = "TESIS/salidas/grafico_coef_PPML.png", plot = grafico_coef_PPML, width = 12, height = 10)


#Tablas comparativas todos los modelos (con y sin efectos)

#Tabla coeficientes

tabla_coef <- tibble::tibble(
  Variable = c(
    "Intercepto", 
    "log_pob_origen", 
    "log_pob_destino", 
    "log_vab_origen", 
    "log_vab_destino", 
    "log_vab_turismo_origen", 
    "log_vab_turismo_destino"
  ),
  `Modelo 1` = c(
    "-3.511*** (0.072)", 
    "0.898*** (0.034)", 
    "0.855*** (0.034)", 
    "", "", "", ""
  ),
  `Modelo 1 (ef)` = c(
    "", 
    "-0.383 (0.511)", 
    "2.785*** (0.635)", 
    "", "", "", ""
  ),
  `Modelo 2` = c(
    "-5.927*** (1.276)", 
    "0.571*** (0.096)", 
    "0.929*** (0.108)", 
    "0.328*** (0.092)", 
    "-0.072 (0.101)", 
    "", ""
  ),
  `Modelo 2 (ef)` = c(
    "", 
    "-0.367 (0.569)", 
    "2.811*** (0.661)", 
    "-0.028 (0.277)", 
    "-0.068 (0.346)", 
    "", ""
  ),
  `Modelo 3` = c(
    "-2.563 (1.393)", 
    "0.634*** (0.100)", 
    "1.048*** (0.107)", 
    "0.290** (0.092)", 
    "-0.104 (0.097)", 
    "0.118 (0.072)", 
    "0.371*** (0.069)"
  ),
  `Modelo 3 (ef)` = c(
    "", 
    "-0.873 (0.633)", 
    "1.989** (0.691)", 
    "0.061 (0.269)", 
    "0.107 (0.363)", 
    "0.117** (0.045)", 
    "0.202*** (0.059)"
  )
)

# Función para formatear coeficiente y error en HTML con salto de línea <br> y formato numérico
formatear_coef_html <- function(vec) {
  sapply(vec, function(text) {
    if (is.na(text) || text == "") {
      return("")
    }
    coef_part <- sub("\\s*\\(.*\\)$", "", text)  # coef y asteriscos
    error_part <- sub(".*\\(([^)]+)\\)$", "\\1", text)  # error dentro de paréntesis
    
    # Extraer número y asteriscos del coef
    num_coef <- sub("(\\-?[0-9\\.]+).*", "\\1", coef_part)
    signif <- sub("[0-9\\.\\-]+", "", coef_part)
    
    # Formatear números con coma decimal y punto miles
    num_coef_f <- formatC(as.numeric(num_coef), format = "f", digits = 2, big.mark = ".", decimal.mark = ",")
    error_f <- formatC(as.numeric(error_part), format = "f", digits = 2, decimal.mark = ",")
    
    paste0(num_coef_f, signif, "<br><i>(", error_f, ")</i>")
  }, USE.NAMES = FALSE)
}

# Aplicar formateo a todas las columnas menos la variable
tabla_coef_formateada <- tabla_coef %>%
  mutate(across(-Variable, formatear_coef_html))

# Crear la tabla gt
gt_coef <- tabla_coef_formateada %>%
  gt() %>%
  tab_header(
    title = "Coeficientes estimados",
    subtitle = "Modelos PPML: comparación con y sin efectos fijos"
  ) %>%
  tab_style(
    style = cell_fill(color = "#f0f0f0"),
    locations = cells_body(
      columns = c(`Modelo 1 (ef)`, `Modelo 2 (ef)`, `Modelo 3 (ef)`)
    )
  ) %>%
  tab_style(
    style = cell_fill(color = "#f0f0f0"),
    locations = cells_column_labels(
      columns = c(`Modelo 1 (ef)`, `Modelo 2 (ef)`, `Modelo 3 (ef)`)
    )
  ) %>% 
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_column_labels(
      columns = everything()
    )
  ) %>% 
  cols_label(
    Variable = "Variable",
    `Modelo 1` = "Modelo 1",
    `Modelo 1 (ef)` = "Modelo 1 (ef)",
    `Modelo 2` = "Modelo 2",
    `Modelo 2 (ef)` = "Modelo 2 (ef)",
    `Modelo 3` = "Modelo 3",
    `Modelo 3 (ef)` = "Modelo 3 (ef)"
  ) %>%
  fmt_markdown(columns = everything()) %>%  # para interpretar etiquetas HTML
  cols_align(align = "center", columns = -Variable) %>%
  tab_options(
    table.font.size = px(13),
    data_row.padding = px(5)
  )


# Para la tabla de bondad de ajuste, formateamos números con formato español (coma decimal, punto miles)
tabla_bondad <- data.frame(
  Indicador = c("Pseudo R²", "Log-verosimilitud", "BIC", "N° observaciones"),
  `Modelo 1`       = c(0.3045,  -1312.0, 2650.4, 6756),
  `Modelo 1 (ef)`  = c(0.3091,  -1246.3, 3021.7, 6756),
  `Modelo 2`       = c(0.3068,  -1305.7, 2655.5, 6756),
  `Modelo 2 (ef)`  = c(0.3080,  -1246.3, 3039.3, 6756),
  `Modelo 3`       = c(0.3151,  -1287.9, 2637.5, 6756),
  `Modelo 3 (ef)`  = c(0.3071,  -1246.0, 3056.3, 6756)
)


tabla_bondad_fmt <- tabla_bondad %>%
  mutate(across(-Indicador, ~ formatC(., format = "f", big.mark = ".", decimal.mark = ",", digits = 2)))

# Tabla bondad de ajuste
gt_bondad <- tabla_bondad_fmt %>%  # <- Usás la tabla formateada
  gt() %>%
  tab_header(
    title = "Indicadores de bondad de ajuste",
    subtitle = "Comparación entre modelos con y sin efectos fijos"
  ) %>%
  cols_label(
    Indicador = md("**Indicador**"),
    Modelo.1 = md("**Modelo 1**"),
    Modelo.1..ef. = md("**Modelo 1 (ef)**"),
    Modelo.2 = md("**Modelo 2**"),
    Modelo.2..ef. = md("**Modelo 2 (ef)**"),
    Modelo.3 = md("**Modelo 3**"),
    Modelo.3..ef. = md("**Modelo 3 (ef)**")
  ) %>%
  tab_style(
    style = cell_fill(color = "#f0f0f0"),
    locations = cells_body(columns = c("Modelo.1..ef.", "Modelo.2..ef.", "Modelo.3..ef."))
  ) %>%
  tab_style(
    style = cell_fill(color = "#f0f0f0"),
    locations = cells_column_labels(columns = c("Modelo.1..ef.", "Modelo.2..ef.", "Modelo.3..ef."))
  ) %>%
  cols_align(align = "center") %>%
  tab_options(
    table.font.size = px(13),
    data_row.padding = px(5)
  )


gtsave(gt_coef, "TESIS/salidas/tabla_coeficientes_ppml.png")
gtsave(gt_bondad, "TESIS/salidas/tabla_bondad_ajuste_ppml.png")


# Grafico residuos

# Modelo 1
df1 <- viajes_ppml %>%
  mutate(
    predicho = predict(modelo1, type = "response"),
    residuo_crudo = viajes_extrapolados_mill - predicho
  )

# Modelo 2
df2 <- viajes_ppml %>%
  mutate(
    predicho = predict(modelo2, type = "response"),
    residuo_crudo = viajes_extrapolados_mill - predicho
  )

# Modelo 3
df3 <- viajes_ppml %>%
  mutate(
    predicho = predict(modelo3, type = "response"),
    residuo_crudo = viajes_extrapolados_mill - predicho
  )

# Modelo 3.1
df4 <- viajes_ppml %>%
  mutate(
    predicho = predict(modelo4, type = "response"),
    residuo_crudo = viajes_extrapolados_mill - predicho
  )

# Modelo 4
df5 <- viajes_ppml %>%
  mutate(
    predicho = predict(modelo5, type = "response"),
    residuo_crudo = viajes_extrapolados_mill - predicho
  )

# Gráficos individuales
g1 <- ggplot(df1, aes(x = predicho, y = residuo_crudo)) +
  geom_point(alpha = 0.4, size = 0.7, color = "steelblue") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
  labs(title = "Modelo 1", x = "Predicción", y = "Residuo crudo") +
  theme_minimal()

g2 <- ggplot(df2, aes(x = predicho, y = residuo_crudo)) +
  geom_point(alpha = 0.4, size = 0.7, color = "steelblue") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
  labs(title = "Modelo 2", x = "Predicción", y = "Residuo crudo") +
  theme_minimal()

g3 <- ggplot(df3, aes(x = predicho, y = residuo_crudo)) +
  geom_point(alpha = 0.4, size = 0.7, color = "steelblue") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
  labs(title = "Modelo 3", x = "Predicción", y = "Residuo crudo") +
  theme_minimal()

g4 <- ggplot(df4, aes(x = predicho, y = residuo_crudo)) +
  geom_point(alpha = 0.4, size = 0.7, color = "steelblue") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
  labs(title = "Modelo 3 (sin ef. año)", x = "Predicción", y = "Residuo crudo") +
  theme_minimal()

g5 <- ggplot(df5, aes(x = predicho, y = residuo_crudo)) +
  geom_point(alpha = 0.4, size = 0.7, color = "steelblue") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
  labs(title = "Modelo 4 (sin ef. año)", x = "Predicción", y = "Residuo crudo") +
  theme_minimal()

panel1 <- (g1 | g2 | g3) + plot_annotation(
  title = "Residuos vs predicciones – Modelos con efectos fijos por año, origen y destino"
)

panel2 <- (g4 | g5) + plot_annotation(
  title = "Residuos vs predicciones – Modelos sin efectos fijos por año"
)

grafico_residuos_ppml <- panel1 / panel2

grafico_residuos_ppml

ggsave("tesis/salidas/grafico_residuos_ppml.png", plot = grafico_residuos_ppml, width = 10, height = 8, dpi = 300)

prep_residuos <- function(modelo, data, y_var){
  mu_hat <- predict(modelo, type = "response")        # fitted values
  y      <- data[[y_var]]
  
  # Cálculo manual de residuos Pearson y deviance ----------------------------
  resid_crudo    <- y - mu_hat
  resid_pearson  <- resid_crudo / sqrt(mu_hat)        # para Poisson
  # deviance (Poisson) – cuidado con y = 0
  term           <- ifelse(y == 0, 0, y * log(y / mu_hat))
  resid_deviance <- sign(resid_crudo) * sqrt(2 * (term - resid_crudo))
  
  data.frame(predicho = mu_hat,
             resid_crudo = resid_crudo,
             resid_pearson = resid_pearson,
             resid_deviance = resid_deviance)
}

#----- Construir un named list de tus modelos --------------------------------
modelos <- list(
  "Modelo 1"              = modelo1,
  "Modelo 2"              = modelo2,
  "Modelo 3"              = modelo3,
  "Modelo 1 (sin ef. año)"= modelo4,
  "Modelo 2 (sin ef. año)"= modelo5,
  "Modelo 3 (sin ef. año)"= modelo6
)

#----- Generar plots ----------------------------------------------------------
# Elegí aquí qué tipo de residuo querés visualizar:
tipo_residuo <- "resid_deviance"   # alternativas: resid_crudo, resid_pearson

graficos <- lapply(names(modelos), function(nombre){
  df <- prep_residuos(modelos[[nombre]], viajes_ppml, "viajes_extrapolados_mill")
  
  ggplot(df, aes(x = predicho, y = .data[[tipo_residuo]])) +
    geom_point(alpha = 0.4, size = 0.7, color = "steelblue") +
    geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
    labs(title = nombre, x = "Predicción", 
         y = gsub("_", " ", sub("resid_", "Residuo ", tipo_residuo))) +
    theme_minimal()
})

# Organizar en panel 3+2 como antes
panel1 <- wrap_plots(graficos[1:3], nrow = 1) + 
  plot_annotation(title = "Residuos vs. predicciones – Modelos con efectos fijos por año, origen y destino")
panel2 <- wrap_plots(graficos[4:6], nrow = 1) + 
  plot_annotation(title = "Residuos vs. predicciones – Modelos sin efectos fijos por año")

grafico_residuos_ppml <- panel1 / panel2

print(grafico_residuos_ppml)

ggsave("tesis/salidas/grafico_residuos_ppml_devPear.png",
       plot   = grafico_residuos_ppml,
       width  = 10, height = 8, dpi = 300)







# g1 <- ggplot(df1, aes(x = predicho, y = residuo_crudo)) +
#   geom_point(alpha = 0.3, color = "steelblue") +
#   geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
#   geom_smooth(method = "loess", color = "red", se = FALSE) +
#   scale_x_continuous(trans = "log1p") +  # log(1 + x) para evitar problemas con 0
#   labs(title = "Modelo 1 mejorado", x = "Predicción (log scale)", y = "Residuo crudo") +
#   theme_minimal()


