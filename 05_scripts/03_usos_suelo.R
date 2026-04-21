
library(sf)
library(tidyverse)

# 1. Cargar masas de agua
masas_agua <- st_read("02_capas_vectoriales/02_generadas/proc_masasalmeria_conpiezometros_20260206.gpkg") %>% 
  st_make_valid() # Asegura que la geometría sea correcta

# Lista de rutas de capas de uso de suelo (una por año)
capas_suelo <- c("02_capas_vectoriales/02_generadas/proc_mucva1956sinteticaalmeria_20260213.shp", 
                 "02_capas_vectoriales/02_generadas/proc_mucva1977sinteticaalmeria_20260213.shp", 
                 "02_capas_vectoriales/02_generadas/proc_mucva1984sinteticaalmeria_20260213.shp", 
                 "02_capas_vectoriales/02_generadas/proc_mucva1984sinteticaalmeria_20260213.shp", 
                 "02_capas_vectoriales/02_generadas/proc_mucva1999sinteticaalmeria_20260213.shp", 
                 "02_capas_vectoriales/02_generadas/proc_mucva2003sinteticaalmeria_20260213.shp", 
                 "02_capas_vectoriales/02_generadas/proc_mucva2007sinteticaalmeria_20260213.shp")

crs_destino <- 25830 #ETRS89 / UTM zona 30N


for (i in 1:length(capas_suelo)) {
  capa <- st_read(capas_suelo[i], quiet = TRUE)
  if (any(!st_is_valid(capa))) {
    capa <- st_make_valid(capa)
  }
  if (st_crs(capa)$epsg != crs_destino) {
    capa <- st_transform(capa, crs_destino)
  }
  salida <- paste0(tools::file_path_sans_ext(capas_suelo[i]), "_reproj.shp")
  st_write(capa, salida, delete_dsn = TRUE, quiet = TRUE)
}



# NO -------------------------------------------------------------------------------
# Función para procesar un año
calcular_cobertura <- function(ruta_capa) {# Cargar uso de suelo del año actual
  suelo_año <- st_read(ruta_capa) %>% st_make_valid()
  año_label <- str_extract(ruta_capa, "\\d{4}") # Extrae el año del nombre del archivo # Intersección: Une la info de masas de agua con el uso de suelo
  interseccion <- st_intersection(masas_agua, suelo_año) %>%
    mutate(area_fragmento = st_area(.)) # Calcula el área de cada pedazo # Agrupar y calcular porcentajes
  resultado <- interseccion %>%
    st_drop_geometry() %>% # Trabajamos con la tabla de atributos
    group_by(ID_MASA, USO_SUELO) %>% # Ajusta con tus nombres de columna
    summarise(area_total_uso = sum(area_fragmento), .groups = "drop") %>%
    group_by(ID_MASA) %>%
    mutate(porcentaje = (as.numeric(area_total_uso) / sum(as.numeric(area_total_uso))) * 100) %>%
    mutate(anio = año_label)
  return(resultado)
}


# 2. Ejecutar para todos los años y unir en una sola tabla
tabla_final <- map_df(capas_suelo, calcular_cobertura)
