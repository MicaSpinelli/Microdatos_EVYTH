viajes_previaje <- read.csv("TESIS/bases/viajes_origen_destino_mes.csv")
viajes_previaje <- viajes_previaje %>% 
  mutate(anio = as.numeric(substr(mes_inicio, 1, 4)),
         provincia_origen = case_when(
           provincia_origen == "Tierra del Fuego, Antártida e Islas del Atlántico Sur"~"Tierra del Fuego",
           provincia_origen == "Ciudad Autónoma de Buenos Aires"~"CABA",
           TRUE ~ provincia_origen),
         provincia_destino = case_when(
           provincia_destino == "Tierra del Fuego, Antártida e Islas del Atlántico Sur"~"Tierra del Fuego",
           provincia_destino == "Ciudad Autónoma de Buenos Aires"~"CABA",
           TRUE ~ provincia_destino
         )) %>% 
  group_by(anio, provincia_origen, provincia_destino) %>% 
  summarise(viajeros=sum(viajeros),
            viajes=sum(viajes)) %>% 
  ungroup()
