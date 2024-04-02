library(tidyverse)
source(paste0(getwd(), "/1.Descargar/Descargar_niveles_pozos_DGA.R"))

estaciones_sacbad<-read.csv("C:/Users/56984/Documents/CCGUC/SACBAD/SACBAD/BBDD/metadatos/DGA+DMC/estaciones_sacbad_todas.csv")
estaciones_sacbad<-estaciones_sacbad[c(1:35),]

estaciones_pozos<-read.csv("C:/Users/56984/Documents/CCGUC/SACBAD/SACBAD/BBDD/metadatos/DGA/estaciones_sacbad_pozos_calidad.csv")
niveles_2020_2022<-niveles
write.csv2(niveles, 
           "C:/Users/56984/Documents/CCGUC/SACBAD/SACBAD/BBDD/niveles_pozos_DGA_2023.csv",
           row.names = FALSE)