bibliotecas <- c("tidyverse", "readxl", "writexl", "lmomco", "SPEI", "rvest", 
                 "dplyr", "stringr", "lubridate", "tidyr", "purrr", "beepr", 
                 "rjson", "RCurl", "XML", "RJSONIO", "httr", "ggplot2", "readr")

# Verificar e instalar las bibliotecas si no están instaladas
for (libreria in bibliotecas) {
  if (!require(libreria, character.only = TRUE)) {
    install.packages(libreria)
  }
  # Cargar la biblioteca
  library(libreria, character.only = TRUE)
}