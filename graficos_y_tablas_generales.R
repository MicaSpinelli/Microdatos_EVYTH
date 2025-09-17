#LIBRERIAS UTILIZADAS

# Instala pacman si no está instalado
if (!require("pacman")) install.packages("pacman")

# Carga o instala automáticamente todos los paquetes necesarios
pacman::p_load(
  broom,
  cowplot,
  dplyr,
  evyth,
  extrafont,
  ggplot2,
  ggrepel,
  ggtext,
  ggraph,
  glue,
  googlesheets4,
  gt,
  igraph,
  janitor,
  lubridate,
  openxlsx,
  readr,
  scales,
  tidytext,
  tidyr,
  tidyverse,
  treemapify
)

message("Todos los paquetes fueron cargados correctamente.")


# CARGA DE BASES DE DATOS-----------------------------------  --------------------
base_turismo_interno <- read.csv("TESIS/bases/evyth_microdatos.csv", header = TRUE, sep = ",")

#DATASET turistas, gasto y pernoctes
turismo_interno <-base_turismo_interno %>%
  filter(tipo_visitante == 1) %>%
  mutate(pondera=ifelse(is.na(pondera),w_adelanto,pondera)) %>% 
  group_by(anio,trimestre) %>% 
  summarise(tur = sum(pondera,na.rm=T),
            gasto_nominal = sum(pondera * gasto_pc,na.rm=T),
            pernoct = sum(pondera*px07,na.rm=T))

turismo_interno_anio <-base_turismo_interno %>%
  filter(tipo_visitante == 1) %>%
  mutate(pondera=ifelse(is.na(pondera),w_adelanto,pondera)) %>% 
  group_by(anio) %>% 
  summarise(tur = sum(pondera,na.rm=T),
            tur_mill=tur/1000000,
            gasto_nominal = sum(pondera * gasto_pc,na.rm=T),
            pernoct = sum(pondera*px07,na.rm=T))

####GRAFICO 1: EVOLUCION DEL TURISMO INTERNO 2012-2024" (TURISTAS)
grafico_1 <- turismo_interno_anio %>% 
  filter(anio!=2024) %>% 
  ggplot(aes(x = anio, y = tur_mill, fill = tur)) +
  geom_col() +  # Gráfico de columnas
  scale_fill_gradient(low = "lightblue", high = "blue") +  # Colores según valores
  geom_text(aes(label = format(round(tur_mill, 1), big.mark = ".", decimal.mark = ",", nsmall = 1)),
            vjust = -0.5, size = 4)+
  scale_x_continuous(breaks = unique(turismo_interno_anio %>% filter(anio != 2024))$anio) +  # Mostrar todos los años
  labs(
    title = "Evolución del Turismo Interno 2012-2023",
    subtitle = "Millones de turistas por año",
    caption = "Fuente:Encuesta de Viajes y Turismo de los Hogares.",
    x = NULL,
    y = NULL
  ) +
  theme_minimal() +
  theme(legend.position = "none",
        axis.text.y = element_blank(),     # Quitar los números del eje Y
        axis.ticks.y = element_blank(),    # Quitar las marcas del eje Y
        axis.line.y = element_blank()      # Quitar la línea del eje Y (si estuviera)
  )  

grafico_1

ggsave(filename = "TESIS/salidas/grafico_1.png", plot = grafico_1, width = 8, height = 6)

#####GRAFICO 2: EVOLUCIÓN POR TRIMESTRE
data_grafico_2 <- turismo_interno %>%
  filter(anio!=2024) %>% 
  select(year=anio, quart=trimestre, tur) %>%
  mutate(turistas = round(tur/1000000,1),
         period = paste0(year,"-",quart),
         quart = factor(quart),
         etiqueta = "Turistas \n(en millones)") 


max_turistas_por_quart <- data_grafico_2 %>%
  group_by(quart) %>%
  summarise(max_turistas = max(turistas))


grafico_2 <- ggplot(data_grafico_2, aes(x = period, y = turistas, fill = quart)) +
  geom_col() +  # Gráfico de columnas
  scale_fill_manual(values = c("1" = "red", "2" = "green", "3" = "blue", "4" = "orange")) +
  labs(
    title = "Evolución del turismo interno según trimestre",
    x = "Período (año-trimestre)",
    y = "Millones de turistas",
    fill = "Turistas"
  ) +
  facet_wrap(~ quart, scales = "free_x", labeller = as_labeller(c("1" = "1°trim", "2" = "2°trim", "3" = "3°trim", "4" = "4°trim"))) +  # Facetas por trimestre (romanos)
  theme_minimal() +
  scale_y_continuous(limits = c(0, 15))+
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none")+  
  geom_hline(data = max_turistas_por_quart, 
             aes(yintercept = max_turistas), 
             linetype = "dashed", color = "black", size = 1)+
  geom_text(data = max_turistas_por_quart, 
            aes(x = 1, y = max_turistas, 
                label = paste0("Max: ",formatC(max_turistas, format = "f", big.mark = ".", decimal.mark = ",", digits = 1), " millones")),
            vjust = -0.5, hjust=-0.1, color = "black", size = 4)  # Agregar el valor encima de la línea
grafico_2

ggsave(filename = "TESIS/salidas/grafico_2.png", plot = grafico_2, width = 8, height = 6)




#TABLAS DE CARACTERISTICAS DEL VIAJE Y DEL PERFIL DEL TURISTA
#Etiqueto las variables de caracterización de viajes y del perfil del turista
evyth_turistas_internos <- base_turismo_interno %>% 
  filter(tipo_visitante == 1) %>% 
  mutate(px09 = ifelse(px09 == 9, 99,  px09),
         px10_1   = ifelse(px10_1 == 9 , 99,  px10_1),
         px13   = ifelse(px13 == 9, 99,px13),
         across(starts_with("pxb16_1_"), ~ case_when(. == 0 ~ 1,
                                                     . == 1 ~ 2)),
         p006_agrup = case_when(p006_agrup == 0 ~ 1,
                                p006_agrup == 1 ~ 2,
                                p006_agrup == 2 ~ 3,
                                p006_agrup == 3 ~ 4,
                                p006_agrup == 4 ~ 5, TRUE ~ p006_agrup),
         p007 = case_when(p007 == 0 ~ NA_real_, TRUE ~ p007),
         cond_act = case_when(cond_act == 0 ~ 4, TRUE ~ cond_act)) #Agrego cambios que se hacen en la base de microdatos para tener mismas etiquetas

# Medio de transporte
transporte <- evyth_turistas_internos %>% 
  filter(!is.na(px09)) %>% 
  evyth::crear_etiqueta("px09") %>% 
  mutate(medio_transporte = case_when(str_detect(px09,"Automóvil") ~ "Auto",
                                      str_detect(px09,"bus")~ "Ómnibus",
                                      str_detect(px09,"Avión") ~ "Avión",
                                      TRUE ~ "Otros")) %>% 
  group_by(anio, medio_transporte) %>%
  summarise(n = sum(pondera)) %>% 
  ungroup() %>% 
  group_by(anio) %>% 
  mutate(part = n/sum(n)) %>% 
  rename("categoria"=medio_transporte) %>% 
  mutate(variable = "medio_transporte",
         grupo = "viaje")

# Motivo del viaje
motivo <- evyth_turistas_internos %>% 
  filter(!is.na(px10_1)) %>% 
  evyth::crear_etiqueta("px10_1") %>%
  mutate(motivo = case_when(
    str_detect(px10_1,"Trabajo") ~ "Trabajo, negocios",
    str_detect(px10_1,"ocio") ~ "Vacaciones, ocio",
    str_detect(px10_1,"familiar") ~ "Visita a familiares y amigos",
    TRUE ~ "Otros")) %>% 
  group_by(anio,motivo) %>%
  summarise(n = sum(pondera)) %>% 
  ungroup() %>% 
  group_by(anio) %>% 
  mutate(part = n/sum(n)) %>% 
  rename("categoria"=motivo) %>% 
  mutate(variable = "motivo",
         grupo = "viaje")

# Tipo de alojamiento
tipo_alojamiento <- evyth_turistas_internos %>% 
  filter(!is.na(px08_agrup)) %>% 
  evyth::crear_etiqueta("px08_agrup") %>%
  group_by(anio,px08_agrup) %>%
  summarise(n = sum(pondera)) %>% 
  ungroup() %>% 
  group_by(anio) %>% 
  mutate(part = n/sum(n)) %>% 
  rename("categoria"=px08_agrup) %>% 
  mutate(variable = "tipo_alojamiento",
         grupo = "viaje")

#Quintiles
quintiles <- evyth_turistas_internos %>% 
  filter(!is.na(quintil_pcf_visitante)) %>% 
  evyth::crear_etiqueta("quintil_pcf_visitante") %>%
  group_by(anio, quintil_pcf_visitante) %>%
  summarise(n = sum(pondera)) %>% 
  ungroup() %>% 
  mutate(quintil = case_when(quintil_pcf_visitante %in% c("Primer quintil","Segundo quintil") ~ "Quintiles 1 y 2",
                             quintil_pcf_visitante %in% c("Tercer quintil","Cuarto quintil") ~ "Quintiles 3 y 4",
                             quintil_pcf_visitante == "Quinto quintil" ~ "Quintil 5")) %>% 
  group_by(anio, quintil) %>%
  summarise(n=sum(n)) %>% 
  group_by(anio) %>% 
  mutate(part = n/sum(n)) %>% 
  rename("categoria"= quintil) %>% 
  mutate(variable = "quintil",
         grupo = "visitante")

#Edad en tramos
edad_tramos <- evyth_turistas_internos %>% 
  filter(!is.na(p006_agrup)) %>% 
  evyth::crear_etiqueta("p006_agrup") %>%
  group_by(anio,p006_agrup) %>%
  summarise(n = sum(pondera)) %>% 
  ungroup() %>% 
  group_by(anio) %>% 
  mutate(part = n/sum(n)) %>% 
  rename("categoria"=p006_agrup) %>% 
  mutate(variable = "edad_tramos",
         grupo = "visitante")

#Género
sexo <- evyth_turistas_internos %>% 
  filter(!is.na(p005)) %>% 
  evyth::crear_etiqueta("p005") %>%
  group_by(anio,p005) %>%
  summarise(n = sum(pondera)) %>% 
  ungroup() %>% 
  group_by(anio) %>% 
  mutate(part = n/sum(n)) %>% 
  rename("categoria"=p005) %>% 
  mutate(variable = "sexo",
         grupo = "visitante")


#Ocupación
ocupacion <- evyth_turistas_internos %>% 
  filter(!is.na(cond_act)) %>% 
  evyth::crear_etiqueta("cond_act") %>%
  group_by(anio,cond_act) %>%
  summarise(n = sum(pondera)) %>% 
  ungroup() %>% 
  group_by(anio) %>% 
  mutate(part = n/sum(n)) %>% 
  rename("categoria"=cond_act) %>% 
  mutate(variable = "ocupacion",
         grupo = "visitante")

#Educación
educacion <- evyth_turistas_internos %>% 
  filter(!is.na(nivel_ed)) %>% 
  evyth::crear_etiqueta("nivel_ed") %>%
  group_by(anio,nivel_ed) %>%
  summarise(n = sum(pondera)) %>% 
  ungroup() %>% 
  group_by(anio) %>% 
  mutate(part = n/sum(n)) %>% 
  rename("categoria"=nivel_ed) %>% 
  mutate(variable = "educacion",
         grupo = "visitante")

tabla_perfiles <- bind_rows(transporte,motivo,tipo_alojamiento,
                            quintiles,edad_tramos,sexo, ocupacion, educacion)


perfiles_split_interno <- tabla_perfiles %>%
  mutate_at(c(3,4), as.double) %>% 
  group_by(grupo) %>% 
  nest()

# TABLA COMPLETA - cacaracteristicas_visitante ####
visitante_interno <- perfiles_split_interno$data[[2]]

caracteristicas_visitante <- visitante_interno %>%
  filter(anio != 2024) %>% 
  mutate(
    variable = recode(variable,
                      "edad_tramos" = "Edad en tramos",
                      "quintil" = "Quintiles",
                      "sexo" = "Género",
                      "ocupacion" = "Condición actividad",
                      "educacion" = "Máximo nivel educativo")
  ) %>%
  rename(participacion = part,
         categorias = categoria) %>%
  select(variable, categorias, anio, participacion) %>%
  pivot_wider(
    names_from = anio,
    values_from = participacion
  ) %>%
  group_by(variable) %>%
  gt(rowname_col = "categorias") %>%
  fmt_percent(
    columns = where(is.numeric),
    decimals = 1,
    dec_mark = ","
  ) %>%
  cols_align(
    align = "center",
    columns = where(is.numeric)
  ) %>%
  cols_label_with(
    fn = ~ md(glue::glue("**{.}**"))
  ) %>%
  tab_spanner(
    label = md("**Participación**"),
    columns = where(is.numeric)
  ) %>%
  fmt_missing(
    columns = everything(),
    missing_text = "//"
  ) %>%
  tab_header(
    title = md("Características del **turista**"),
    subtitle = ""
  ) %>%
  tab_source_note(
    source_note = md("**Fuente**: Encuesta de Viajes y Turismo de los Hogares (EVyTH)")
  ) %>%
  tab_options(
    table.align = "center",
    row_group.font.weight = "bold",
    table.width = 900,
    table.font.size = 12,
    data_row.padding = px(1)
  )



caracteristicas_visitante
gtsave(caracteristicas_visitante,"TESIS/salidas/caracteristicas_visitante.png")

# TABLA COMPLETA - caracteristicas_viaje ####

viaje_interno <- perfiles_split_interno$data[[1]]

caracteristicas_viaje <- viaje_interno %>%
  filter(anio != 2024) %>% 
  mutate(variable= recode(variable,
                          "medio_transporte"="Medio de transporte",
                          "tipo_alojamiento"="Tipo de alojamiento", 
                          "motivo"="Motivo del viaje")) %>% 
  rename(participacion = part,
         categorias = categoria) %>%
  select(variable, categorias, anio, participacion) %>%
  pivot_wider(
    names_from = anio,
    values_from = participacion
  ) %>%
  group_by(variable) %>%
  gt(rowname_col = "categorias") %>%
  fmt_percent(
    columns = where(is.numeric),
    decimals = 1,
    dec_mark = ","
  ) %>%
  cols_align(
    align = "center",
    columns = where(is.numeric)
  ) %>%
  cols_label_with(
    fn = ~ md(glue::glue("**{.}**"))
  ) %>%
  tab_spanner(
    label = md("**Participación**"),
    columns = where(is.numeric)
  ) %>%
  tab_header(
    title = md("Características del **viaje**"),
    subtitle = ""
  ) %>%
  tab_source_note(
    source_note = md("**Fuente**: Encuesta de Viajes y Turismo de los Hogares (EVyTH)")
  ) %>%
  tab_options(
    table.align = "center",
    row_group.font.weight = "bold",
    table.width = 750,
    table.font.size = 12,
    data_row.padding = px(1)
  )

caracteristicas_viaje
gtsave(caracteristicas_viaje,"TESIS/salidas/caracteristicas_viaje.png")

####TABLAS DEL ANEXO

####TABLA RESUMEN VARIABLES

data_tabla1 <- base_turismo_interno %>%
  filter(anio!=2024) %>% 
  crear_etiqueta("tipo_visitante") %>% 
  group_by(anio, tipo_visitante) %>% 
  summarise(Total = sum(pondera, na.rm = T),
            Gasto = sum(pondera * gasto_pc, na.rm = T),
            Pernoctes = sum(pondera * px07, na.rm = T),
            Estadia_prom = Pernoctes/Total,
            Gasto_prom = Gasto/Total,
            Gasto_diario = Gasto/Pernoctes) %>%
  ungroup() %>% 
  mutate(Total = Total/1000,
         #Pernoctes = Pernoctes/1000,
         Gasto = Gasto/1000000) %>% 
  pivot_longer(cols = c("Total","Gasto","Pernoctes","Estadia_prom","Gasto_prom","Gasto_diario"), 
               names_to = "categoria", values_to = "cantidad") %>% 
  pivot_wider(names_from = "anio", values_from = "cantidad") %>% 
  filter(!(tipo_visitante == "Excursionista" & 
             categoria %in% c("Pernoctes","Estadia_prom","Gasto_diario")))

total <- data_tabla1 %>% 
  group_by(categoria) %>% 
  summarise(`2012` = sum(`2012`),
            `2013` = sum(`2013`),
            `2014` = sum(`2014`),
            `2015` = sum(`2015`),
            `2016` = sum(`2016`),
            `2017` = sum(`2017`),
            `2018` = sum(`2018`),
            `2019` = sum(`2019`),
            `2020` = sum(`2020`),
            `2021` = sum(`2021`),
            `2022` = sum(`2022`),
            `2023` = sum(`2023`)) %>% 
  filter(categoria %in% c("Total","Gasto")) %>% 
  mutate(tipo_visitante = "Visitantes")

tabla1 <- data_tabla1 %>% 
  rbind(total) %>% 
  filter(!(tipo_visitante %in% c("Excursionista","Turista") &
             categoria %in% c("Gasto")))

tabla1 <- tabla1[c(3,1,9,5,4,8,6,2,7),] 

tabla1 <- tabla1 %>% 
  mutate(unidad = case_when(
    categoria == "Total" ~ "miles",
    categoria == "Estadia_prom" ~ "noches",
    categoria == "Pernoctes" ~ "miles de noches",
    categoria == "Gasto" ~ "millones de $",
    TRUE ~ "$"
  ),
  categoria = case_when(
    tipo_visitante == "Turista" & categoria == "Total" ~ "Turistas",
    tipo_visitante == "Turista" & categoria == "Estadia_prom" ~ "Estadía promedio",
    tipo_visitante == "Turista" & categoria == "Gasto_prom" ~ "Gasto promedio por turista",
    tipo_visitante == "Turista" & categoria == "Gasto_diario" ~ "Gasto diario promedio por turista", 
    tipo_visitante == "Excursionista" & categoria == "Total" ~ "Excursionistas",
    tipo_visitante == "Excursionista" & categoria == "Gasto_prom" ~ "Gasto promedio por excursionista",
    tipo_visitante == "Visitantes" & categoria == "Total" ~ "Visitantes (turistas + excursionistas)",
    tipo_visitante == "Visitantes" & categoria == "Gasto" ~ "Gasto visitantes",
    TRUE ~ categoria
  ),
  var_ia = (`2023`/`2022`))

tabla_resumen <- tabla1 %>% 
  select(-tipo_visitante) %>% 
  gt() %>% 
  cols_label(
    categoria = "",
    unidad = "",
    var_ia = "Var. i.a. % 2023"
  ) %>% 
  tab_caption(
    caption = md("Visitantes, estadía promedio, pernoctes y gasto con destino en Argentina por año. Años 2012-2023")
  ) %>% 
  tab_source_note(
    source_note = md("**Fuente**: Encuesta de Viajes y Turismo de los Hogares")
  ) %>%
  
  # Agrupación por tipo de variable
  tab_row_group(
    group = "Visitantes (en miles)",
    rows = categoria %in% c("Turistas", "Excursionistas", "Visitantes (turistas+excursionistas)")
  ) %>%
  tab_row_group(
    group = "Estadía (en noches)",
    rows = categoria %in% c("Estadía promedio", "Pernoctes")
  ) %>%
  tab_row_group(
    group = "Gasto (en pesos)",
    rows = categoria %in% c("Gasto visitantes", "Gasto promedio por turista", "Gasto promedio por excursionista", "Gasto diario promedio por turista")
  ) %>%
  
  # Formateo de columnas numéricas
  fmt_number(
    columns = c(`2012`:`2023`),
    rows = categoria == "Estadía promedio",
    decimals = 1,
    dec_mark = ",",
    sep_mark = "."
  ) %>%
  fmt_number(
    columns = c(`2012`:`2023`),
    rows = categoria != "Estadía promedio",
    decimals = 0,
    dec_mark = ",",
    sep_mark = "."
  ) %>%
  fmt_percent(
    columns = var_ia,
    decimals = 1,
    dec_mark = ","
  ) %>%
  
  # Estilos destacados para filas clave
  tab_style(
    style = list(
      cell_text(weight = "bold"),
      cell_fill(color = "#f0f0f0")
    ),
    locations = cells_body(
      rows = categoria %in% c("Visitantes (en miles)", "Estadía (en noches)", "Gasto (en pesos)")
    )
  ) 

tabla_resumen
gtsave(
  tabla_resumen,
  "TESIS/salidas/tabla_resumen.png",
  vwidth = 1600,
  vheight = 2000,
  expand = c(10, 10, 10, 10)
)
####TABLA RESUMEN VARIABLES POR REGIÓN

data_destino <- base_turismo_interno %>% 
  filter(tipo_visitante == 1, anio == 2023) %>% 
  crear_etiqueta("region_destino") %>% 
  group_by(orig_dest = region_destino) %>% 
  summarise(Turistas = sum(pondera, na.rm = T),
            Pernoctes = sum(pondera * px07, na.rm = T),
            Estadia_prom = Pernoctes/Turistas,
            Gasto = sum(pondera * gasto_pc, na.rm = T),
            Gasto_prom = Gasto/Turistas,
            Gasto_diario = Gasto/Pernoctes) %>% 
  ungroup() %>% 
  mutate(Turistas = Turistas/sum(Turistas),
         Pernoctes = Pernoctes/sum(Pernoctes),
         Gasto = Gasto/sum(Gasto),
         region = "Región de destino")

data_origen <-base_turismo_interno%>% 
  filter(tipo_visitante == 1, anio == 2023) %>% 
  crear_etiqueta("region_origen") %>% 
  group_by(orig_dest = region_origen) %>% 
  summarise(Turistas = sum(pondera, na.rm = T),
            Pernoctes = sum(pondera * px07, na.rm = T),
            Estadia_prom = Pernoctes/Turistas,
            Gasto = sum(pondera * gasto_pc, na.rm = T),
            Gasto_prom = Gasto/Turistas,
            Gasto_diario = Gasto/Pernoctes) %>% 
  ungroup() %>% 
  mutate(Turistas = Turistas/sum(Turistas),
         Pernoctes = Pernoctes/sum(Pernoctes),
         Gasto = Gasto/sum(Gasto),
         region = "Región de origen")

total <- base_turismo_interno %>% 
  filter(tipo_visitante == 1, anio == 2023) %>% 
  mutate(region = "Total",
         orig_dest = "") %>% 
  group_by(region, orig_dest) %>% 
  summarise(Turistas = sum(pondera, na.rm = T),
            Pernoctes = sum(pondera * px07, na.rm = T),
            Estadia_prom = Pernoctes/Turistas,
            Gasto = sum(pondera * gasto_pc, na.rm = T),
            Gasto_prom = Gasto/Turistas,
            Gasto_diario = Gasto/Pernoctes,
  ) %>% 
  ungroup() %>% 
  mutate(Gasto = Gasto/1000000,
         Turistas = Turistas/1000,
         Pernoctes = Pernoctes/1000)


tabla2 <- rbind(total, data_destino, data_origen) %>% 
  mutate(orig_dest = str_replace(orig_dest, "Provincia de Buenos Aires", "PBA"))

tabla_resumen_region <- tabla2 %>% 
  gt(groupname_col = "region") %>% 
  cols_label(
    orig_dest = "",
    region = "",
    Turistas = md("**Turistas <br>(en miles)**"),
    Pernoctes = md("**Pernoctes <br>(en miles)**"),
    Estadia_prom = md("**Estadía promedio**"), 
    Gasto = md("**Gasto <br>(en millones de $)**"), 
    Gasto_prom = md("**Gasto promedio <br>(en $)**"), 
    Gasto_diario = md("**Gasto promedio diario <br>(en $)**")
  ) %>%
  tab_options(row_group.font.weight = "bold") %>% 
  fmt_number(columns = c(5,7,8), rows = c(2:17), decimals = 1,  dec_mark = ",", sep_mark = ".") %>%
  fmt_number(columns = c(3:8), rows = 1, decimals = 1,  dec_mark = ",", sep_mark = ".") %>%
  fmt_percent(columns = c(3,4,6), rows = c(2:17), decimals = 1, dec_mark = ",", sep_mark = ".") %>% 
  tab_source_note(
    source_note = md(
      "**Fuente**:Encuesta de Viajes y Turismo de los Hogares")
  ) %>% 
  tab_caption(caption = md(glue("Turistas con destino principal en Argentina, pernoctes, estadía promedio y gasto según región de residencia y de destino, distribución porcentual. Año 2023")))

tabla_resumen_region
gtsave(tabla_resumen_region,"TESIS/salidas/tabla_resumen_region.png")
# ------------------------------------------------------------
# 4 TABLAS, GRAFICOS Y GENERACION DE FORMULAS PARA CAPITULO METODOLOGICO
# ------------------------------------------------------------

#Otras tablas y graficos adicionales para el apartado metodologico del documento

#Comparación de modelos gravitatorios (ventajas y desventajas)

tabla_modelos <- data.frame(
  Modelo = c("MCO (log-log)", "PPML (panel)"),
  Ventajas = c(
    paste(
      "1. Interpretación directa como elasticidades (% cambio en viajes).",
      "2. Sencillo de implementar e interpretar.",
      "3. Amplia tradición y referencia en la literatura económica.",
      sep = "\n"
    ),
    paste(
      "1. Permite incluir observaciones con cero viajes sin exclusión.",
      "2. Robusto frente a heterocedasticidad en los errores.",
      "3. Permite incorporar efectos fijos múltiples (origen, destino, tiempo).",
      "4. Mejora el ajuste en datos con alta dispersión y ceros frecuentes.",
      "5. Válido incluso si los datos no siguen distribución Poisson exacta.",
      sep = "\n"
    )
  ),
  Desventajas = c(
    paste(
      "1. No permite incluir observaciones con cero viajes (log(0) no definido).",
      "2. Potencial sesgo e inconsistencia ante heterocedasticidad.",
      "3. Control limitado de heterogeneidad no observada sin efectos fijos.",
      "4. Ajuste pobre en presencia de muchos ceros o datos dispersos.",
      sep = "\n"
    ),
    paste(
      "1. Interpretación de coeficientes menos intuitiva (semi-elasticidades).",
      "2. Mayor complejidad computacional y requerimientos técnicos.",
      "3. Necesita mayor fundamentación teórica para su adecuada aplicación.",
      sep = "\n"
    )
  )
)

tabla_comparativa_modelos <- tabla_modelos %>%
  gt() %>%
  tab_header(
    title = "Ventajas y desventajas de los modelos MCO (log-log) y PPML (panel)"
  ) %>%
  cols_label(
    Modelo = md("**Modelo**"),
    Ventajas = md("**Ventajas**"),
    Desventajas = md("**Desventajas**")
  ) %>%
  fmt_markdown(columns = c(Ventajas, Desventajas)) %>%
  cols_width(
    Modelo ~ px(120),
    Ventajas ~ px(200),
    Desventajas ~ px(200)
  ) %>%
  tab_options(
    table.font.size = px(11),
    heading.title.font.size = px(16),
    row.striping.background_color = "#f9f9f9"
  )


gtsave(tabla_comparativa_modelos, "TESIS/salidas/tabla_comparativa_modelos.png")

# Comparación especificaciones de los modelos

modelos <- tibble(
  Modelo = c(
    "MCO 1", "MCO 2", "MCO 3",
    "MCO 1 (ef)", "MCO 2 (ef)", "MCO 3 (ef)",
    "PPML 1", "PPML 2", "PPML 3",
    "PPML 1 (ef)", "PPML 2 (ef)", "PPML 3 (ef)"
  ),
  Estimador = c(
    rep("MCO", 6),
    rep("PPML", 6)
  ),
  Variables = c(
    "log(Pob_origen), log(Pob_destino)",
    "Modelo MCO 1 + log(VAB_origen), log(VAB_destino)",
    "Modelo MCO 2 + % VAB hotelero (origen y destino)",
    "Igual que MCO 1",
    "Igual que MCO 2",
    "Igual que MCO 3",
    "log(Pob_origen), log(Pob_destino)",
    "Modelo PPML 1 + log(VAB_origen), log(VAB_destino)",
    "Modelo PPML 2 + % VAB hotelero (origen y destino)",
    "Igual que PPML 1",
    "Igual que PPML 2",
    "Igual que PPML 3"
  ),
  Efectos_fijos = c(
    "Ninguno",
    "Ninguno",
    "Ninguno",
    "Origen, Destino, Año",
    "Origen, Destino, Año",
    "Origen, Destino, Año",
    "Ninguno",
    "Ninguno",
    "Ninguno",
    "Origen, Destino, Año",
    "Origen, Destino, Año",
    "Origen, Destino, Año"
  ),
  Notas = c(
    "Primera aproximación: solo tamaño poblacional",
    "Agrega actividad económica total (VAB)",
    "Agrega peso del turismo (% VAB hotelero)",
    "Modelo 1 con efectos fijos múltiples",
    "Modelo 2 con efectos fijos múltiples",
    "Modelo 3 con efectos fijos múltiples",
    "PPML equivalente a MCO 1",
    "PPML equivalente a MCO 2",
    "PPML equivalente a MCO 3",
    "PPML 1 + efectos fijos múltiples",
    "PPML 2 + efectos fijos múltiples",
    "PPML 3 + efectos fijos múltiples"
  )
)

tabla_modelos_metodologia <- modelos %>%
  gt() %>%
  tab_header(
    title = "Comparación de especificaciones de modelos gravitatorios"
  ) %>%
  cols_label(
    Modelo = md("**Modelo**"),
    Estimador = md("**Estimador**"),
    Variables = md("**Variables incluidas**"),
    Efectos_fijos = md("**Efectos fijos**"),
    Notas = md("**Comentarios**")
  )

gtsave(tabla_modelos_metodologia, "TESIS/salidas/tabla_modelos_metodologia.png")

#TABLA LITERATURA

tabla_antecedentes <- data.frame(
  Autor = c(
    "Guardia, Muro y Such (2014)",
    "Llano y De la Mata (2010)",
    "Massidda y Etzo (2012)",
    "Marrocu y Paci (2013)",
    "Porto, Garbero y Espínola (2018)",
    "Andrade (2004)",
    "Brida, González y Lanzilotta (2017)"
  ),
  Pais_Region = c(
    "España",
    "España",
    "Italia",
    "Italia",
    "Cono Sur (Argentina, Brasil, Chile, Uruguay)",
    "Brasil",
    "Uruguay"
  ),
  Tipo_Turismo = c(
    "Interno",
    "Interno",
    "Interno",
    "Interno",
    "Interno e Internacional",
    "Interno",
    "Interno"
  ),
  Modelo_estimacion = c(
    "Modelo gravitatorio clásico",
    "Modelo gravitatorio aplicado a flujos monetarios",
    "Modelo gravitatorio clásico",
    "Modelo gravitatorio aumentado con efectos espaciales",
    "Modelo gravitatorio aumentado dinámico",
    "Modelo gravitatorio clásico",
    "Modelo gravitatorio clásico"
  ),
  Metodo_estimacion = c(
    "MCO",
    "MCO",
    "MCO",
    "MCO + efectos espaciales",
    "Panel dinámico, GMM-sistema",
    "Modelo gravitatorio clásico",
    "MCO, efectos fijos, PPML"
  ),
  Variables_Clave = c(
    "Tamaño económico, distancia, elasticidad ingreso >1",
    "Flujos monetarios, estructura sectorial, relaciones productivas",
    "Ingreso per cápita, precios, demanda turística",
    "Calidad de atractivos, efectos espaciales, ingreso per cápita",
    "Producto per cápita, población, distancia, competitividad, hábito turístico",
    "Ingreso per cápita, proximidad geográfica",
    "Población, ingreso, distancia, oferta turística (camas, atractivos)"
  )
)

# Tabla con gt
tabla_antecedentes <- tabla_antecedentes %>%
  gt() %>%
  tab_header(
    title = md("**Investigaciones con Modelos Gravitatorios en Turismo**"),
    subtitle = md("**Estudios sobre turismo interno e internacional y métodos de estimación**")
  ) %>%
  cols_label(
    Autor = md("**Autor(es)**"),
    Pais_Region = md("**País / Región**"),
    Tipo_Turismo = md("**Tipo de Turismo**"),
    Modelo_estimacion = md("**Modelo de Estimación**"),
    Metodo_estimacion = md("**Método de Estimación**"),
    Variables_Clave = md("**Variables Clave / Notas**")
  ) %>%
  fmt_markdown(columns = vars(Variables_Clave)) %>%
  opt_align_table_header("center") %>%
  tab_options(
    table.font.names = "Arial",
    table.font.size = 12,
    heading.title.font.size = 16,
    heading.subtitle.font.size = 12,
    data_row.padding = px(5)
  )



gtsave(tabla_antecedentes, "TESIS/salidas/tabla_antecedentes.png")


#OTROS GRAFICOS QUE FINALMENTE NO SE USARON

# ####GRAFICO 3: DISTRIBUCIÓN PORCENTUAL DE VIAJES
# 
# grafico_3 <- read.csv("TESIS/bases/comptur_poblacion_viajera_anual_serie.csv", header = TRUE, sep = ",") %>%
#   filter(indice_tiempo >= 2012) %>%
#   mutate(indice_tiempo = as.factor(indice_tiempo),
#          participacion = participacion * 100) %>%
#   ggplot(aes(x = indice_tiempo, y = participacion, fill = participacion)) +
#   geom_col() +
#   geom_text(aes(label = paste0(round(participacion, 1), "%")),
#             vjust = -0.5, size = 4) +
#   scale_fill_gradient(low = "lightblue", high = "blue") +
#   labs(
#     title = "Proporción de la población que al menos realizó un viaje en el año (2012-2022)",
#     x = NULL,
#     y = NULL,
#     caption = "Fuente: Encuesta de Viajes y Turismo de los Hogares, módulo de comportamiento turístico."
#   ) +
#   theme_minimal(base_size = 13) +
#   theme(
#     plot.title = element_text(face = "bold", size = 15, hjust = 0.5),
#     axis.text.y = element_blank(),
#     axis.ticks.y = element_blank(),
#     panel.grid.major.y = element_blank(),
#     legend.position = "none"  
#   )
# 
# grafico_3
# ggsave(filename = "TESIS/salidas/grafico_3.png", plot = grafico_3, width = 8, height = 6)
# 
# ####GRAFICO 4: MOTIVO NO VIAJE
# 
# # Cargar datos
# #datos <- read.csv("TESIS/bases/comptur_motivo_no_viaje_anual_serie.csv", sep = ",", header = TRUE)
# 
# # Procesar
# datos <- datos %>%
#   filter(!is.na(razon_noviaje), !is.na(indice_tiempo), !is.na(participacion)) %>%
#   filter(indice_tiempo >= 2012) %>%
#   mutate(
#     participacion = participacion * 100,
#     indice_tiempo = as.factor(indice_tiempo),
#     razon_noviaje = str_wrap(razon_noviaje, width = 20)  # por si los textos son largos
#   )
# 
# # Gráfico con facetas por motivo
# grafico_4 <- ggplot(datos, aes(x = indice_tiempo, y = participacion, fill = participacion)) +
#   geom_col() +
#   geom_text(aes(label = paste0(round(participacion, 1), "%")),
#             vjust = -0.5, size = 2, check_overlap = TRUE) +
#   scale_fill_gradient(low = "lightblue", high = "blue") +
#   facet_wrap(~razon_noviaje, ncol = 2) +
#   labs(
#     title = "Evolución de motivos por los que no se realizaron viajes\n(Distribución porcentual 2012-2022)",
#     x = NULL,
#     y = NULL,
#     caption = "Fuente: Encuesta de Viajes y Turismo de los Hogares, módulo de comportamiento turístico."
#   ) +
#   scale_y_continuous(expand = expansion(mult = c(0, 0.4)))+
#   theme_minimal(base_size = 13) +
#   theme(
#     strip.text = element_text(size = 8),       # Tamaño de los títulos de los facets
#     plot.title = element_text(face = "bold", size = 10, hjust = 0.5),
#     axis.text.y = element_blank(),
#     axis.ticks.y = element_blank(),
#     panel.grid.major.y = element_blank(),
#     legend.position = "none",
#     axis.text.x = element_text(angle = 45, hjust = 1, size = 9)
#   )
# ggsave(filename = "TESIS/salidas/grafico_4.png", plot = grafico_4, width = 8, height = 6)


