library(tidyverse)
library(readxl)
library(writexl)

#Cargar funciones
source("C:/Users/56984/Documents/CCGUC/WebScraping/1.Descargar/Descargar_caudal_DGA.R")
source("C:/Users/56984/Documents/CCGUC/WebScraping/1.Descargar/Descargar_pp_DGA.R")
source("C:/Users/56984/Documents/CCGUC/WebScraping/1.Descargar/Descargar_temp_DGA.R")

#Fecha de inicio y metadatos
metadatos_DGA<-read_excel("C:/Users/56984/Documents/CCGUC/ANID_Sequias/BBDD/metadatos/DGA/estaciones_DGA.xlsx")
fecha_ini<-"2023"
fecha_fin<-"2023"

#Precipitación DGA
pp_DGA_2023<-descargar_pp_DGA(fecha_ini, fecha_fin, estaciones = metadatos_DGA)
write_csv(pp_DGA_2023, paste0("C:/Users/56984/Documents/CCGUC/Webscraping/BBDD/pp/DGA/bruto/pp_DGA_",fecha_fin,"_08.csv"))

#Caudal DGA
q_2023<-descargar_caudal_DGA(fecha_ini, fecha_fin, estaciones = metadatos_DGA)
write_csv(q_2023, paste0("C:/Users/56984/Documents/CCGUC/ANID_Sequias/BBDD/q/DGA/bruto/q_mean_DGA_",fecha_fin,".csv"))

#Temperatura DGA
temp_DGA_2022<-descargar_temp_DGA(fecha_ini, fecha_fin, estaciones = metadatos_DGA)

#Temp_min DGA
temp_min_DGA_2023<- temp_DGA_2023 %>%
  select(Year, Month, Day, Codigo_nacional, temp_min)
write_csv(temp_min_DGA_2023, paste0("C:/Users/56984/Documents/CCGUC/WebScraping/BBDD/temp/min/DGA/bruto/temp_min_DGA_",fecha_fin,".csv"))

#Temp_max DGA
temp_max_DGA_2023<- temp_DGA_2023 %>%
  select(Year, Month, Day, Codigo_nacional, temp_max)
write_csv(temp_max_DGA_2023, paste0("C:/Users/56984/Documents/CCGUC/WebScraping/BBDD/temp/max/DGA/bruto/temp_max_DGA_",fecha_fin,".csv"))

