library(dplyr)
library(tidyr)
library(readr)
library(ggplot2)
library(evyth)
library(gt)
library(igraph)
library(ggraph)
library(geoAr)
library(sf)
library(ggrepel)
library(networkD3)
library(scales)
library(stringr)
library(readxl)
library(geosphere)


# CARGA DE BASES DE DATOS-------------------------------------------------------
base_turismo_interno <- read.csv("TESIS/bases/evyth_microdatos.csv", header = TRUE, sep = ",")%>% 
  filter(!is.na(region_destino)) 

#Excluyo los años 2020 (por pandemia) y 2024 (por datos incompletos)
n_anios_excluyendo_2020 <- n_distinct(base_turismo_interno$anio[base_turismo_interno$anio != 2024 & base_turismo_interno$anio != 2020])

# Contar viajes por par origen-destino
matriz_od_region <- base_turismo_interno %>%
  filter(tipo_visitante == 1, anio != 2024 & anio !=2020) %>%
  evyth::crear_etiqueta(c("region_destino","region_origen"))%>% 
  group_by(region_origen, region_destino) %>%
  summarise(n_viajes = sum(pondera) / n_anios_excluyendo_2020, .groups = "drop")

# Convierto a formato de matriz origen-destino
matriz_od_wide <- matriz_od_region %>%
  pivot_wider(names_from = region_destino, values_from = n_viajes, values_fill = 0)

matriz_od_wide

write_csv(matriz_od_wide, "matriz_origen_destino.csv")

# Visualizar la distribución de viajes
matriz_od_region$region_origen <- gsub(" ", "\n", matriz_od_region$region_origen)

####Armo la tabla
matriz_od_region_tabla <- base_turismo_interno %>%
  filter(tipo_visitante == 1, anio != 2024 & anio !=2020) %>%
  evyth::crear_etiqueta(c("region_destino", "region_origen")) %>%
  group_by(region_origen, region_destino) %>%
  summarise(n_viajes = sum(pondera) / n_anios_excluyendo_2020, .groups = "drop") %>% 
  pivot_wider(
    names_from = region_destino,
    values_from = n_viajes,
    values_fill = 0
  ) %>%
  mutate(TOTAL = rowSums(across(-region_origen)))  # Total por fila

# Agregar fila de totales (columna)
totales_columna <- matriz_od_region_tabla %>%
  summarise(across(-region_origen, sum)) %>%
  mutate(region_origen = "TOTAL") %>%
  select(region_origen, everything())

# Unir la fila de totales a la tabla original
matriz_od_completa <- bind_rows(matriz_od_region_tabla, totales_columna) %>% 
  rename(`Región de origen` = region_origen)

###TABLA MATRIZ ORIGEN-DESTINO
tabla_od <- matriz_od_completa %>%
  gt() %>%
  tab_header(
    title = "Matriz origen-destino promedio anual de viajes turísticos internos (2012–2023)",
    subtitle = "Número estimado de viajes anuales entre regiones (excluye 2020 y turistas únicamente)"
  ) %>%
  fmt_number(
    columns = -`Región de origen`,
    sep_mark = ".",
    dec_mark = ",",
    decimals = 0
  ) %>%
  tab_style(
    style = cell_text(weight = "bold"),
    locations = list(
      cells_body(rows = `Región de origen` == "TOTAL"),
      cells_column_labels(),
      cells_body(columns = "TOTAL")
    )
  ) %>%
  cols_width(
    everything() ~ px(100)  # Ajusta este número según cómo se vea
  ) %>% 
  tab_spanner(
    label = "Región de destino",
    columns = setdiff(names(matriz_od_completa), c("Región de origen"))  # todas las columnas excepto la primera
  )

tabla_od

gtsave(tabla_od,"TESIS/salidas/tabla_od.png",vwidth = 1600,
       vheight = 2000,
       expand = c(10, 10, 10, 10))


###### GRAFICO MATRIZ REGION
grafico_matriz_region <- ggplot(matriz_od_region, aes(x = region_origen, y = region_destino, fill = n_viajes)) +
  geom_tile() +
  scale_fill_viridis_c(labels = label_number(big.mark = ".", decimal.mark = ",")) +
  theme_minimal() +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 10)) + 
  scale_y_discrete(labels = function(y) str_wrap(y, width = 10)) +
  labs(title = "Intensidad de los flujos turísticos interregionales – promedio anual 2012–2023",
       subtitle = "Número estimado de viajes turísticos entre regiones (excluye 2020)",
       x = "Provincia de origen",
       y = "Provincia de destino",
       fill = "Número de viajes\n (promedio anual)")
grafico_matriz_region  

ggsave(filename = "TESIS/salidas/grafico_matriz_region.png", plot = grafico_matriz_region, width = 12, height = 6)


####GRAFICO SALDO

matriz_od_region <- matriz_od_region %>%
  mutate(
    region_origen = str_squish(str_trim(region_origen)),
    region_destino = str_squish(str_trim(region_destino))
  )


# Emitidos: suma por región de origen
emitidos <- matriz_od_region%>%
  group_by(region=region_origen) %>%
  summarise(emitidos = sum(n_viajes), .groups = "drop")

# Recibidos: suma por región de destino
recibidos <- matriz_od_region %>%
  group_by(region=region_destino) %>%
  summarise(recibidos = sum(n_viajes), .groups = "drop")

# Uno ambas tablas
saldo_neto <- full_join(emitidos, recibidos, 
                        by = "region") %>%
  mutate(
    emitidos = replace_na(emitidos, 0),
    recibidos = replace_na(recibidos, 0),
    saldo_neto_relativo = (recibidos - emitidos) / ((recibidos + emitidos) / 2)
  ) %>%
  arrange(desc(saldo_neto_relativo)) %>% 
  mutate(region_wrapped = str_wrap(region, width = 15))

saldo_pct <- saldo_neto$saldo_pct <- saldo_neto$saldo_neto_relativo * 100

grafico_saldo <- ggplot(saldo_neto, aes(x = reorder(region_wrapped, saldo_pct), 
                                        y = saldo_pct, fill = saldo_pct > 0)) +
  geom_col(show.legend = FALSE) +
  geom_text(aes(label = paste0(format(round(saldo_pct, 1), decimal.mark = ","), "%")),
            hjust = ifelse(saldo_neto$saldo_pct > 0, -0.1, 1.1),
            size = 3.5) +
  coord_flip() +
  scale_fill_manual(values = c("TRUE" = "#1b9e77", "FALSE" = "#d95f02")) +
  labs(
    title = "Saldo neto relativo de viajes turísticos entre regiones (2012–2023)",
    subtitle = "Relación entre viajes recibidos y emitidos (en porcentaje)",
    x = "Región",
    y = "Saldo neto relativo (%)"
  ) +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold")) +
  expand_limits(y = c(min(saldo_neto$saldo_pct) - 5, 
                      max(saldo_neto$saldo_pct) + 5))

grafico_saldo

ggsave(filename = "TESIS/salidas/grafico_saldo.png", plot = grafico_saldo, width = 12, height = 6)

####VIAJES POR RESIDENTE

poblacion_region <- read_excel("TESIS/bases/censo_2022.xlsx") %>% 
  group_by(region=region_tur) %>% 
  summarise(poblacion = sum(poblacion), .groups = "drop") %>% 
  mutate(region = str_squish(str_trim(region)))


viajes_por_residente <- emitidos %>%
  left_join(poblacion_region, by = "region") %>%
  mutate(
    viajes_por_residente = emitidos / poblacion
  )


grafico_viaje_residente <- ggplot(viajes_por_residente, 
       aes(x = reorder(region, viajes_por_residente), 
           y = viajes_por_residente)) +
  geom_col(fill = "#2C7BB6") +
  geom_text(aes(label = paste0(format(round(viajes_por_residente, 1), decimal.mark = ","), "%")),
            hjust = ifelse(viajes_por_residente$viajes_por_residente > 0, -0.1, 1.1),
            size = 3.5) +
  coord_flip() +
  labs(
    title = "Viajes emitidos por residente según región",
    x = "Región",
    y = "Viajes por habitante"
  ) +
  scale_y_continuous(labels = number_format(accuracy = 0.01)) +
  theme_minimal()

grafico_viaje_residente
ggsave(filename = "TESIS/salidas/grafico_viaje_residente.png", plot = grafico_viaje_residente, width = 12, height = 6)

# GRAFICO INTRAREGIONALES VS INTERREGIONALES
viajes_intra <- matriz_od_region %>%
  filter(region_origen == region_destino) %>%
  group_by(region_origen) %>%
  summarise(viajes_intrarregionales = sum(n_viajes)) %>%
  ggplot(aes(x = reorder(region_origen, viajes_intrarregionales), y = viajes_intrarregionales)) +
  geom_col(fill = "#4E79A7") +
  coord_flip() +
  labs(title = "Viajes intrarregionales por región", x = "Región", y = "Viajes") +
  theme_minimal()


ggsave(filename = "TESIS/salidas/viajes_intra.png", plot = viajes_intra, width = 12, height = 6)

viajes_origen <- matriz_od_region %>%
  group_by(region_origen) %>%
  summarise(total_viajes_emitidos = sum(n_viajes, na.rm = TRUE))

# 2. Total de viajes intrarregionales (origen = destino)
viajes_intrarregionales <- matriz_od_region %>%
  filter(region_origen == region_destino) %>%
  group_by(region_origen) %>%
  summarise(viajes_intrarregionales = sum(n_viajes, na.rm = TRUE))

# 3. Merge de ambos y cálculo del porcentaje
distribucion <- viajes_origen %>%
  left_join(viajes_intrarregionales, by = "region_origen") %>%
  mutate(
    viajes_intrarregionales = ifelse(is.na(viajes_intrarregionales), 0, viajes_intrarregionales),
    porcentaje_intrarregional = (viajes_intrarregionales / total_viajes_emitidos) * 100,
    etiqueta_region = gsub(" ", "\n", region_origen),  # Divide nombres en dos líneas
    etiqueta = paste0(format(round(porcentaje_intrarregional, 1), decimal.mark = ","), "%")
  )

# 4. Visualización
distribucion_intraregional <- ggplot(distribucion, aes(x = reorder(etiqueta_region, -porcentaje_intrarregional), y = porcentaje_intrarregional)) +
  geom_col(fill = "#69b3a2") +
  geom_text(aes(label = etiqueta), vjust = -0.5, size = 4) +
  labs(
    title = "Porcentaje de viajes turísticos que permanecen dentro de la misma región",
    x = "Región de origen",
    y = NULL
  ) +
  scale_y_continuous(labels = NULL, breaks = NULL) +  # Elimina el eje Y
  theme_minimal() +
  theme(
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    panel.grid.major.y = element_blank(),
    axis.text.x = element_text(size = 10),
    plot.title = element_text(face = "bold")
  )


distribucion_intraregional
ggsave(filename = "TESIS/salidas/distribucion_intraregional.png", plot = distribucion_intraregional, width = 12, height = 6)



# Calcular total de viajes recibidos por cada región destino
viajes_destino <- matriz_od_region %>%
  group_by(region_destino) %>%
  summarise(total_recibidos = sum(n_viajes))

# Calcular viajes intrarregionales (origen = destino)
viajes_intrarregion <- matriz_od_region %>%
  filter(region_origen == region_destino) %>%
  rename(region = region_destino) %>%
  left_join(viajes_destino, by = c("region" = "region_destino")) %>%
  mutate(porcentaje_intrarregional = n_viajes / total_recibidos)

# Gráfico
ggplot(viajes_intrarregion, aes(x = reorder(region, -porcentaje_intrarregional), 
                                y = porcentaje_intrarregional)) +
  geom_col(fill = "#2E86AB") +
  geom_text(aes(label = percent(porcentaje_intrarregional, accuracy = 0.1, decimal.mark = ",")), 
            vjust = -0.5, size = 4) +
  scale_y_continuous(labels = NULL, expand = expansion(mult = c(0, 0.1))) +
  labs(
    x = NULL,
    y = NULL,
    title = "Porcentaje de viajes intrarregionales respecto al total de viajes recibidos por región"
  ) +
  theme_minimal(base_family = "sans") +
  theme(
    axis.text.x = element_text(angle = 0, vjust = 1, hjust = 0.5, lineheight = 0.9),
    plot.title = element_text(size = 14, face = "bold")
  )


# Ejemplo de datos: porcentaje intrarregional según origen y destino
df <- data.frame(
  region = c("Norte Grande", "Centro", "Cuyo", "Patagonia"),
  origen = c(0.42, 0.35, 0.50, 0.55),
  destino = c(0.40, 0.38, 0.48, 0.53)
)

# Pasar a formato long para ggplot
df_long <- df %>%
  pivot_longer(cols = c("origen", "destino"), 
               names_to = "tipo", 
               values_to = "porcentaje")

# Modificar etiquetas para que tengan saltos de línea y para que tipo quede más claro
df_long$region <- factor(df_long$region, levels = c("Norte Grande", "Centro", "Cuyo", "Patagonia"))
df_long$tipo <- recode(df_long$tipo, origen = "Desde Origen", destino = "Desde Destino")

# Gráfico barras agrupadas con porcentajes arriba y eje Y sin ticks ni título
ggplot(df_long, aes(x = region, y = porcentaje, fill = tipo)) +
  geom_col(position = position_dodge(width = 0.8), width = 0.7) +
  geom_text(aes(label = scales::percent(porcentaje, accuracy = 0.1)), 
            position = position_dodge(width = 0.8), vjust = -0.5, size = 4) +
  scale_y_continuous(labels = function(x) paste0(x * 100, "%"), expand = expansion(mult = c(0, 0.1))) +
  labs(x = NULL, y = NULL, fill = NULL) +
  theme_minimal() +
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        legend.position = "top",
        axis.text.x = element_text(size = 11, face = "bold"))

# #######PRINCIPALES FLUJOS TURISTICOS
# top_emisivos <- matriz_od_provincia %>%
#   group_by(provincia_origen) %>%
#   summarise(total_viajes_emitidos = sum(n_viajes, na.rm = TRUE)) %>%
#   arrange(desc(total_viajes_emitidos)) %>% 
#   head(10)
# 
# top_emisivos_tabla <- top_emisivos %>% 
#   gt() %>%
#   tab_header(
#     title = "Top 10 Provincias más viajeras",
#     subtitle = "Basado en la matriz origen-destino de la EVYTH"
#   ) %>%
#   fmt_number(columns = total_viajes_emitidos, decimals = 0, sep_mark = ".", dec_mark = ",") %>%  # Ajusta formato
#   cols_label(provincia_origen = "Provincia", total_viajes_emitidos = "Número de Viajes") %>%
#   tab_options(table.border.top.color = "blue")
# 
# top_emisivos_tabla
# 
# gtsave(top_emisivos_tabla,"TESIS/salidas/top_emisivos_tabla.png")
# 
# #####TABLA TOP 10 PROVINCIAS MAS VISITADAS
# top_destinos <- matriz_od_provincia %>%
#   group_by(provincia_destino) %>%  # Agrupar por destino
#   summarise(total_viajes = sum(n_viajes, na.rm = TRUE)) %>%  # Sumar los viajes
#   arrange(desc(total_viajes)) %>% # Ordenar de mayor a menor
#   head(10)
# 
# top_destinos_tabla <- top_destinos %>% 
#   gt() %>%
#   tab_header(
#     title = "Top 10 Provincias Más Visitadas",
#     subtitle = "Basado en la matriz origen-destino de la EVYTH"
#   ) %>%
#   fmt_number(columns = total_viajes, decimals = 0, sep_mark = ".", dec_mark = ",") %>%  # Ajusta formato
#   cols_label(provincia_destino = "Provincia", total_viajes = "Número de Viajes") %>%
#   tab_options(table.border.top.color = "blue")
# 
# top_destinos_tabla
# 
# gtsave(top_destinos_tabla,"TESIS/salidas/top_destinos_tabla.png")
# 
# 
# ###MAPA FLUJO TURISTICO
# 
# argentina <- get_geo(geo = "ARGENTINA", level = "provincia") %>% 
#   add_geo_codes() %>% 
#   rename(provincia=name_iso)
#   
# provincias <- get_geo(geo = "ARGENTINA", level = "provincia") %>% 
#   add_geo_codes() %>% 
#   rename(provincia=name_iso) %>% 
#   mutate(centroide = st_centroid(geometry)) %>%
#   mutate(lon = st_coordinates(centroide)[,1],
#            lat = st_coordinates(centroide)[,2]) %>%
#   select(provincia, lon, lat) %>%   
#   mutate(provincia = recode(provincia,
#                             "Buenos Aires" = "Buenos Aires (Resto)",
#                             "Ciudad Autónoma de Buenos Aires" = "CABA"))
#                           
#  
# matriz_od_provincia <- matriz_od_provincia %>%
#   mutate(provincia_destino = recode(provincia_destino,
#                                     "Partidos del GBA (Pcia. Bs. As.)"="Buenos Aires (Resto)"),
#          provincia_origen = recode(provincia_origen,
#                                     "Partidos del GBA (Pcia. Bs. As.)"="Buenos Aires (Resto)",
#                                     "Entre Rios"="Entre Ríos",
#                                     "Cordoba"="Córdoba"))
#                                     
# # Calcular total de viajes
# total_viajes <- sum(matriz_od_provincia$n_viajes, na.rm = TRUE)
# 
# flujos_turisticos <- matriz_od_provincia %>%
#   left_join(provincias, by = c("provincia_origen" = "provincia")) %>%
#   rename(lon_origen = lon, lat_origen = lat) %>%
#   left_join(provincias, by = c("provincia_destino" = "provincia")) %>%
#   rename(lon_destino = lon, lat_destino = lat) %>% 
#   filter(!is.na(lon_origen) & !is.na(lat_origen) & 
#            !is.na(lon_destino) & !is.na(lat_destino) & 
#            !(lon_origen == lon_destino & lat_origen == lat_destino))   # Evita puntos idénticos
#  
# 
# top_flujos <- flujos_turisticos %>%
#   arrange(desc(n_viajes)) %>%
#   slice_head(n = 10)
# 
# ###MAPA  
# mapa_flujos <- ggplot() +
#   geom_sf(data = argentina, fill = "gray90", color = "white") +  # Mapa base
#   geom_curve(data = top_flujos, aes(x = lon_origen, y = lat_origen, 
#                                     xend = lon_destino, yend = lat_destino, 
#                                     size = n_viajes), 
#              color = "blue", curvature = 0.2, alpha = 0.7) +
#   geom_point(data = top_flujos, aes(x = lon_origen, y = lat_origen, size = n_viajes), color = "red") +
#   geom_point(data = top_flujos, aes(x = lon_destino, y = lat_destino, size = n_viajes), color = "blue") +
#   # geom_text_repel(data = top_flujos, 
#   #                 aes(x = lon_destino, y = lat_destino, label = provincia_destino),
#   #                 size = 4, color = "black", nudge_x = 0.5, nudge_y = 0.5) +
#   scale_size_continuous(name = "Cantidad de Viajes", labels = scales::comma) +
#   labs(title = "Top 10 Flujos Turísticos en Argentina",
#        subtitle = "Basado en la Encuesta EVYTH",
#        caption = "Fuente: EVYTH - Procesamiento propio") +
#   theme_minimal()
# 
# mapa_flujos
# 
# ggsave(filename = "TESIS/salidas/mapa_flujos.png", plot = mapa_flujos, width = 6, height = 6)
# 
# ###GRAFICO DE REDES
# edges <- matriz_od_provincia %>%
#   filter(n_viajes > quantile(n_viajes, 0.75))  # Solo el 25% con más viajes
# 
# # Crear el grafo
# graph <- graph_from_data_frame(edges, directed = TRUE)
# 
# # Definir colores y tamaños para destacar los nodos más importantes
# node_size <- degree(graph, mode = "in")  # Más conexiones entrantes = más grande
# edge_width <- E(graph)$n_viajes / max(E(graph)$n_viajes) * 5  # Escalar ancho
# 
# # Visualizar la red con mejor diseño
# grafico_redes <- ggraph(graph, layout = "stress") + 
#   geom_edge_link(aes(width = edge_width, alpha = 0.7), color = "gray") +
#   geom_node_point(aes(size = node_size), color = "blue") +
#   geom_node_text(aes(label = name), repel = TRUE, size = 4) +
#   theme_void() +
#   labs(title = "Red de Flujos Turísticos entre Provincias",
#        subtitle = "Conexiones más significativas")
# 
# grafico_redes
# ggsave(filename = "TESIS/salidas/grafico_redes.png", plot = grafico_redes, width = 12, height = 6)

