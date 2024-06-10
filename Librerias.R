bibliotecas <- c("readxl", "writexl", "lmomco", "SPEI", "rvest", 
                 "stringr", "lubridate", "tidyr", "purrr", "beepr", 
                 "rjson", "RCurl", "XML", "RJSONIO", "httr", "ggplot2", "readr",
                  "zoo", "patchwork", "forcats", "shiny", "raster", "sf", "sp",
                  "dplyr", "tidyverse", "zip", "arcgisbinding", "jsonlite") 


for (libreria in bibliotecas) {
  if (!require(libreria, character.only = TRUE)) {
    install.packages(libreria)
    if(libreria == "arcgisbinding"){
      install.packages("arcgisbinding", repos="https://r.esri.com", type="win.binary")
    }
  }
  # Cargar la biblioteca
  library(libreria, character.only = TRUE)
}
