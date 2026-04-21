# Leer datos piezómetros IGME

# Ejemplo con campo de dalías

# AÑADIR NOMBRE MASA DE AGUA SUBTERRÁNEA

# Cargar librerías -------------------------------------------------------------
library(tidyverse)
library(janitor)
library(readxl)
library(stringr)

# Leer nombres archivos --------------------------------------------------------
nombres_xlsx_ruta <- list.files(
  path = "prueba_piezometros_lista", 
  pattern = ".xlsx",
  full.names = T) # Obtener ruta completa


# Crear bucle que lea todos los archivos ---------------------------------------
l_datos <- rep(list(NA),length(nombres_xlsx_ruta))  # Inicializar lista

for (i in 1:length(nombres_xlsx_ruta)) {
  df <- read_xlsx(nombres_xlsx_ruta[i], skip=1) # Leer cada dataframe y guardar como objeto
  colnames(df) <- make_clean_names(colnames(df)) # Tomar como nombres de las variables la fila 2 y pasarlos a formato r
  df <- df[,-c(1:3)] # Eliminar las columnas 1 a 3 (no info)
  df <- type.convert(df, as.is = TRUE) # Volver a inferir el tipo de variables
  l_datos[[i]] <- df
}  # Crear una lista en la que cada elemento sea un dataframe y el nombre sea el del archivo



# Pasar lista a tibble ---------------------------------------------------------
datos <- 
  enframe(l_datos, value="contenido") |> 
  unnest_wider(contenido) |> 
  unnest(c(id, hoja, octante, punto, naturaleza, cota_m, profundidad_m, 
           provincia, municipio, demarcacion, utilizacion, coordenada_x_etrs89,
           coordenada_y_etrs89, coordenada_x_utm_etrs89, coordenada_y_utm_etrs89,
           huso)) 

# Exportar archivo -------------------------------------------------------------
write.csv(datos, "04_datos_procesados/piezometros/base_piezometros_igme.csv")
