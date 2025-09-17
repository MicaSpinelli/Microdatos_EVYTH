library(eph)
library(dplyr)
library(readxl)
library(purrr)
library(foreign)

# Función para cargar trimestre y calcular población por aglomerado
cargar_trim <- function(anio, trimestre) {
  datos_trim <- get_microdata(year = anio, trimester = trimestre, type = "individual")
  poblacion_trim <- datos_trim %>%
    group_by(AGLOMERADO) %>%
    summarise(poblacion_trim = sum(PONDERA, na.rm = TRUE), .groups = "drop") %>%
    mutate(anio = anio, trimestre = trimestre)
  closeAllConnections()
  Sys.sleep(2)
  return(poblacion_trim)
}

# Año 2012
datos_2012 <- datos_2017 <- bind_rows(
  cargar_trim(2012, 1),
  cargar_trim(2012, 2),
  cargar_trim(2012, 3),
  cargar_trim(2012, 4)
)

poblacion_2012 <- datos_2012 %>%
  group_by(AGLOMERADO, anio) %>%
  summarise(poblacion_promedio = mean(poblacion_trim, na.rm = TRUE), .groups = "drop")

# 2013
datos_2013 <- bind_rows(
  cargar_trim(2013, 1),
  cargar_trim(2013, 2),
  cargar_trim(2013, 3),
  cargar_trim(2013, 4)
)
poblacion_2013 <- datos_2013 %>%
  group_by(AGLOMERADO, anio) %>%
  summarise(poblacion_promedio = mean(poblacion_trim, na.rm = TRUE), .groups = "drop")

# 2014
datos_2014 <- bind_rows(
  cargar_trim(2014, 1),
  cargar_trim(2014, 2),
  cargar_trim(2014, 3),
  cargar_trim(2014, 4)
)
poblacion_2014 <- datos_2014 %>%
  group_by(AGLOMERADO, anio) %>%
  summarise(poblacion_promedio = mean(poblacion_trim, na.rm = TRUE), .groups = "drop")

# 2015
datos_2015 <- bind_rows(
  cargar_trim(2015, 1),
  cargar_trim(2015, 2)
  #cargar_trim(2015, 3),
  #cargar_trim(2015, 4)
)
poblacion_2015 <- datos_2015 %>%
  group_by(AGLOMERADO, anio) %>%
  summarise(poblacion_promedio = mean(poblacion_trim, na.rm = TRUE), .groups = "drop")


# 2016

# Función que asigna la extensión correcta según el trimestre
leer_archivo_2016 <- function(trimestre) {
  ext <- ifelse(trimestre == 4, "xlsx", "xls")
  archivo <- file.path("TESIS/bases/EPH", paste0("usu_individual_T", trimestre, "16.", ext))
  
  datos <- read_excel(archivo)
  
  # Asegurar nombres en mayúsculas
  names(datos) <- toupper(names(datos))
  
  datos %>%
    group_by(AGLOMERADO) %>%
    summarise(poblacion = sum(PONDERA, na.rm = TRUE)) %>%
    mutate(anio = 2016, trimestre = trimestre)
}

# Leer los trimestres 2, 3 y 4
resultados_2016 <- map_dfr(c(2, 3, 4), leer_archivo_2016)

# Calcular el promedio anual por aglomerado
poblacion_2016 <- resultados_2016 %>%
  group_by(AGLOMERADO) %>%
  summarise(poblacion_promedio= mean(poblacion)) %>% 
  mutate(anio=2016)


# 2017
datos_2017 <- bind_rows(
  cargar_trim(2017, 1),
  cargar_trim(2017, 2),
  cargar_trim(2017, 3),
  cargar_trim(2017, 4)
)
poblacion_2017 <- datos_2017 %>%
  group_by(AGLOMERADO, anio) %>%
  summarise(poblacion_promedio = mean(poblacion_trim, na.rm = TRUE), .groups = "drop")

# 2018
datos_2018 <- bind_rows(
  cargar_trim(2018, 1),
  cargar_trim(2018, 2),
  cargar_trim(2018, 3),
  cargar_trim(2018, 4)
)
poblacion_2018 <- datos_2018 %>%
  group_by(AGLOMERADO, anio) %>%
  summarise(poblacion_promedio = mean(poblacion_trim, na.rm = TRUE), .groups = "drop")

# 2019
datos_2019 <- bind_rows(
  cargar_trim(2019, 1),
  cargar_trim(2019, 2),
  cargar_trim(2019, 3),
  cargar_trim(2019, 4)
)
poblacion_2019 <- datos_2019 %>%
  group_by(AGLOMERADO, anio) %>%
  summarise(poblacion_promedio = mean(poblacion_trim, na.rm = TRUE), .groups = "drop")

# 2020
datos_2020 <- bind_rows(
  cargar_trim(2020, 1),
  cargar_trim(2020, 2),
  cargar_trim(2020, 3),
  cargar_trim(2020, 4)
)
poblacion_2020 <- datos_2020 %>%
  group_by(AGLOMERADO, anio) %>%
  summarise(poblacion_promedio = mean(poblacion_trim, na.rm = TRUE), .groups = "drop")

# 2021
datos_2021 <- bind_rows(
  cargar_trim(2021, 1),
  cargar_trim(2021, 2),
  cargar_trim(2021, 3),
  cargar_trim(2021, 4)
)
poblacion_2021 <- datos_2021 %>%
  group_by(AGLOMERADO, anio) %>%
  summarise(poblacion_promedio = mean(poblacion_trim, na.rm = TRUE), .groups = "drop")

# 2022
datos_2022 <- bind_rows(
  cargar_trim(2022, 1),
  cargar_trim(2022, 2),
  cargar_trim(2022, 3),
  cargar_trim(2022, 4)
)
poblacion_2022 <- datos_2022 %>%
  group_by(AGLOMERADO, anio) %>%
  summarise(poblacion_promedio = mean(poblacion_trim, na.rm = TRUE), .groups = "drop")

# 2023
datos_2023 <- bind_rows(
  cargar_trim(2023, 1),
  cargar_trim(2023, 2),
  cargar_trim(2023, 3),
  cargar_trim(2023, 4)
)
poblacion_2023 <- datos_2023 %>%
  group_by(AGLOMERADO, anio) %>%
  summarise(poblacion_promedio = mean(poblacion_trim, na.rm = TRUE), .groups = "drop")

# Juntar todos los años
poblacion_eph <- bind_rows(
  poblacion_2012, poblacion_2013, poblacion_2014, poblacion_2015,
  poblacion_2016, poblacion_2017, poblacion_2018, poblacion_2019,
  poblacion_2020, poblacion_2021, poblacion_2022, poblacion_2023
)

# Leemos el archivo con la tabla de correspondencia
aglomerado_a_prov <- read_xlsx("TESIS/bases/aglomerado_a_prov.xlsx")

# Hacemos join con el dataframe de población por aglomerado y año
poblacion_eph_prov <- poblacion_eph %>%
  left_join(aglomerado_a_prov, by = "AGLOMERADO")


# Leemos población proyectada provincial
poblacion_prov_indec <- read_xlsx("TESIS/bases/poblacion_2012_2024.xlsx") 
# Supongo columnas: PROVINCIA, anio, poblacion_proyectada

# Sumamos población aglomerados por provincia y año
poblacion_aglo_prov <- poblacion_eph_prov %>%
  group_by(provincia, anio) %>%
  summarise(poblacion_aglomerados = sum(poblacion_promedio, na.rm = TRUE), .groups = "drop")

# Unimos y calculamos factor
factor <- poblacion_prov_indec %>%
  inner_join(poblacion_aglo_prov, by = c("provincia", "anio")) %>%
  mutate(factor_extrapolacion = poblacion/ poblacion_aglomerados)

write.csv(factor, file = "TESIS/bases/factor.csv", row.names = FALSE)

