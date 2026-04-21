# Leer datos piezómetros MITECO


# Cargar librerías -------------------------------------------------------------
library(tidyverse)
library(janitor)
library(readxl)
library(stringr)

# Leer nombres archivos --------------------------------------------------------
nombres_xlsx_ruta <- list.files(
  path = "03_datos_brutos/piezometros/miteco", 
  pattern = ".xlsx",
  full.names = T) # Obtener ruta completa

nombres_xlsx <- list.files(
  path = "03_datos_brutos/piezometros/miteco",
  pattern = ".xlsx",
  full.names = F) |> # Obtener nombre del archivo
  str_remove_all(pattern = "_miteco_activos_20260312.xlsx")


# Crear bucle que lea todos los archivos ---------------------------------------
l_datos <- rep(list(NA),length(nombres_xlsx))  # Inicializar lista

for (i in 1:length(nombres_xlsx)) {
  df <- read_xlsx(nombres_xlsx_ruta[i]) # Leer cada dataframe y guardar como objeto
  df <- df |> 
    mutate(label = row_number()) # Recomputar los label
  l_datos[[i]] <- df
  names(l_datos)[[i]] <- nombres_xlsx[i]
}  # Crear una lista en la que cada elemento sea un dataframe y el nombre sea el del archivo


# Pasar lista a tibble ---------------------------------------------------------
datos <- 
  enframe(l_datos, name="subcuenca", value="contenido") |> 
  unnest_wider(contenido) |> 
  unnest(c(COD_DEM_ID, NOM_DEM, IDPIEZ, CodIdent, FechaP, Cota_NP_msnm, 
           Prof_NP_leída, Prof_NP_terreno, label)) 

# Exportar archivo -------------------------------------------------------------
write.csv(datos, "04_datos_procesados/piezometros/base_piezometros_miteco.csv")
