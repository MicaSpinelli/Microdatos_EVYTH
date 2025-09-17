library(readxl)
library(dplyr)
library(purrr)
library(stringr)

archivo <- "tesis/bases/jurisdiccion_52sectores.xlsx"
hojas <- excel_sheets(archivo)

# Tomar solo hojas desde la segunda hasta la penúltima (provincias)
hojas_validas <- hojas[2:(length(hojas) - 1)]

# Función para limpiar años (fila 6, columnas C a V)
limpiar_anios <- function(x) {
  x %>%
    str_extract("\\d{4}") %>%
    as.numeric()
}

# Función para procesar una hoja
procesar_hoja <- function(nombre_hoja) {
  datos <- read_excel(archivo, sheet = nombre_hoja, col_names = FALSE, range = "C6:V59")
  
  # Extraer años desde fila 6 (1ra fila del rango leído)
  anios <- limpiar_anios(as.character(unlist(datos[1, ])))
  
  # Extraer datos de filas: 41, 42, 59 => en términos del rango leído: 41-6+1, etc.
  fila_hoteles <- 41 - 6 + 1
  fila_restaurantes <- 42 - 6 + 1
  fila_vab <- 59 - 6 + 1
  
  df <- data.frame(
    provincia = nombre_hoja,
    anio = anios,
    hoteles = as.numeric(unlist(datos[fila_hoteles, ])),
    restaurantes = as.numeric(unlist(datos[fila_restaurantes, ])),
    vab_total = as.numeric(unlist(datos[fila_vab, ]))
  )
  
  return(df)
}

# Procesar todas las hojas válidas
df_final <- map_dfr(hojas_validas, procesar_hoja)

nombres_corregidos <- c(
  "Buenos Aires" = "Buenos Aires",
  "Catamarca" = "Catamarca",
  "Chaco" = "Chaco",
  "Chubut" = "Chubut",
  "Ciudad De Buenos Aires" = "CABA",
  "Cordoba" = "Córdoba",
  "Corrientes" = "Corrientes",
  "Entre Rios" = "Entre Ríos",
  "Formosa" = "Formosa",
  "Jujuy" = "Jujuy",
  "La Pampa" = "La Pampa",
  "La Rioja" = "La Rioja",
  "Mendoza" = "Mendoza",
  "Misiones" = "Misiones",
  "Neuquen" = "Neuquén",
  "Rio Negro" = "Río Negro",
  "Salta" = "Salta",
  "San Juan" = "San Juan",
  "San Luis" = "San Luis",
  "Santa Cruz" = "Santa Cruz",
  "Santa Fe" = "Santa Fe",
  "Santiago Del Estero" = "Santiago del Estero",
  "Tierra Del Fuego" = "Tierra del Fuego",
  "Tucuman" = "Tucumán"
)


df_final <- df_final %>%
  mutate(
    provincia = provincia %>%
      str_replace_all("_", " ") %>%    # Reemplaza guiones bajos por espacios
      str_to_title()                  # Convierte a formato "Titulo" (Ej: "La Rioja")
  ) %>%
  mutate(
    provincia = recode(provincia, !!!nombres_corregidos)
  )


# Guardar si querés
write.csv(df_final, "tesis/bases/vab_turismo_provincias.csv", row.names = FALSE)




###VAB TOTAL
vab_provincias <- readxl::read_xlsx("TESIS/bases/vab_total.xlsx") %>%
  pivot_longer(
    cols = -provincia,          # todas las columnas menos 'provincia' se pivotan
    names_to = "anio",          # el nombre de las columnas será la variable 'anio'
    values_to = "vab"           # el valor de las celdas será la variable 'vab'
  ) %>%
  mutate(anio = as.integer(anio))

write.csv(vab_provincias, "TESIS/bases/vab_provincias.csv", row.names = FALSE)



