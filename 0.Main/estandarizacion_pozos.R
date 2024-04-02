#Niveles de pozos y estandarización proyecto SACBAD
#Escrito por Javier Enrique Vargas Ramírez
#CCG-UC PONTIFICIA UNIVERSIDAD CATÓLICA DE CHILE

library(ggplot2)
library(ggthemes)
library(zoo)
library(tidyverse)
library(SPEI)
library(zoo)
source(paste0(getwd(), "/2.Promediar_formatear/promedio_mensual.R"))
source(paste0(getwd(), "/4.Calcular_indicadores/SGI/calcular_SGI.R"))
source(paste0(getwd(), "/4.Calcular_indicadores/ICE/Codigo_base.R"))
source(paste0(getwd(), "/4.Calcular_indicadores/IPE/calcular_IPE.R"))


## POZOS: Niveles de pozos
estaciones_pozos<-read.csv(paste0(getwd(),"/BBDD/metadatos/DGA/estaciones_sacbad_pozos_calidad_29%.csv"))

codigo_nacional_values <- estaciones_pozos$Codigo_nacional
pozo2<- read.csv2("C:/Users/56984/Documents/CCGUC/SACBAD/niveles_pozos/niveles_pozos_DGA_sacbad_1990_2019.csv")

pozo <- read.csv2("C:/Users/56984/Documents/CCGUC/SACBAD/SACBAD/BBDD/niveles_pozos/BNAT_Niveles.txt", header = TRUE, sep = ";", quote = "", stringsAsFactors = FALSE)
pozo<-Corregir_Codigo_nacional(pozo)

pozo <- pozo %>%
  rename(Month= Mes, Year= anno)
pozo<-formatear_fecha(pozo)

pozo_long <- pozo %>%
  pivot_longer(cols = -date,
               names_to = "Nombre_estacion",
               values_to = "nivel_pozo")

estaciones_sacbad_pozos <- estaciones_sacbad_pozos %>%
  mutate(Estacion = str_replace_all(Estacion, " ", "."))

pozo_long<-left_join(pozo_long, estaciones_sacbad_pozos, by="Codigo_nacional")

pozo_long <- pozo_long %>%
  filter(!is.na(Macrozona))

# Para rellenar la serie IPE: Calcula el promedio de mean_IPE12 por subcuenca y fecha
pozo_long_mean <- pozo_long %>%
  group_by(date, Nombre_subcuenca) %>%
  summarize(nivel_pozo_mean = mean(nivel_pozo, na.rm = TRUE), .groups = "drop") %>%
  mutate(Year= substr(date,4,7))  

pozo_long_mean_mean<-pozo_long_mean %>% 
  mutate(Year= substr(date,4,7)) %>% 
  group_by(Year) %>% 
  summarize(nivel_pozo_mean_mean = mean(nivel_pozo_mean, na.rm = TRUE), .groups = "drop")

# Une el resultado con el dataframe original y reemplaza los valores NA en mean_IPE12
pozo_long_rellenado <- pozo_long_mean  %>%
  left_join(pozo_long_mean_mean, by= "Year") %>%
  mutate(nivel_pozo_mean = ifelse(is.na(nivel_pozo_mean), nivel_pozo_mean_mean, nivel_pozo_mean)) %>%
  select(-nivel_pozo_mean_mean, -Year)

pozo_rellenado <- pozo_long  %>%
  left_join(pozo_long_rellenado, by= c("date", "Nombre_subcuenca")) %>%
  mutate(nivel_pozo = ifelse(is.na(nivel_pozo), nivel_pozo_mean, nivel_pozo)) %>%
  select(-nivel_pozo_mean)

pozo_rellenado <- pozo_rellenado %>%
  mutate(Month= as.numeric(substr(date, 1,2))) %>%
  mutate(Year= as.numeric(substr(date, 4,7))) %>%
  mutate(hydro_year = if_else(Month >= 4, Year, Year - 1)) %>% 
  mutate(ID_subcuenca = 
           case_when(
             Nombre_subcuenca == "Costeras entre Rios Quilimari y Petorca" ~ "CQP", 
             Nombre_subcuenca == "Petorca Medio" ~ "MP",
             Nombre_subcuenca == "Rio Ligua Alto (Estero Alicahue)" ~ "UL",
             Nombre_subcuenca == "Rio Ligua Bajo (Entre Estero Los Angeles y Desembocadura)" ~ "LL",
             Nombre_subcuenca == "Rio Ligua Medio (entre Quebrada La Cerrada y Los Angeles)" ~ "ML",
             Nombre_subcuenca == "Rio Petorca Alto (hasta despues Junta Rio Sobrante)" ~ "UP",
             Nombre_subcuenca == "Rio Petorca Bajo (Entre Las Palmas y Desembocadura)" ~ "LP",
             Nombre_subcuenca == "Rio Quilimari Entre Cajon Ingienillo y Desembocadura" ~ "LQ",
             Nombre_subcuenca == "Rio Quilimari entre muro Embalse Culimo y Bajo Cajon Ingienillo" ~ "MQ",
             Nombre_subcuenca == "Rio Quilimari hasta muro Embalse Culimo" ~ "UQ"
           )) %>% 
  mutate(Name_subcuenca = 
           case_when(
             ID_subcuenca == "CQP" ~ "Coastal Quilimari Petorca", 
             ID_subcuenca == "MP" ~ "Middle Petorca",
             ID_subcuenca == "UL" ~ "Upper Ligua",
             ID_subcuenca == "LL" ~ "Lower Ligua",
             ID_subcuenca == "ML" ~ "Middle Ligua",
             ID_subcuenca == "UP" ~ "Upper Petorca",
             ID_subcuenca == "LP" ~ "Lower Petorca",
             ID_subcuenca == "LQ" ~ "Lower Quilimari" ,
             ID_subcuenca == "MQ" ~ "Middle Quilimari",
             ID_subcuenca == "UQ" ~ "Upper Quilimari"
           ))


write.csv2(pozo_rellenado, 
           "C:/Users/56984/Documents/CCGUC/SACBAD/niveles_pozos/niveles_pozos_DGA_rellenado_sacbad_1990_2019.csv",
           row.names = FALSE)

#ESTANDARIZAR

##SGI 
niveles_1990_2019<-read.csv2("C:/Users/56984/Documents/CCGUC/SACBAD/niveles_pozos/niveles_pozos_DGA_sacbad_rellenado_1990_2019.csv")
#niveles_1990_2020$date<-as.Date(paste("01", niveles_1990_2020$date, sep = "/"), format = "%d/%m/%Y")
#niveles_1990_2020<-na.omit(niveles_1990_2020)
niveles_1990_2019<-niveles_1990_2019 %>% select( date, nivel_pozo, Codigo_nacional)
niveles_1990_2019<-niveles_1990_2019 %>% pivot_wider(names_from = Codigo_nacional, values_from = nivel_pozo)
niveles_1990_2019_ts<-zoo(niveles_1990_2019[, -1], order.by = niveles_1990_2019$date)

spi_result <- lapply(niveles_1990_2019_ts, function(station_data) {
  spi(as.numeric(station_data), scale = 12, distribution = "Gamma")
})

# Create an empty dataframe to store the vectors
spi_matrix <- data.frame(date = niveles_1990_2019$date)

# Iterate through colnames of spi_result
for (col_name in names(spi_result)) {
  # Extract SPI values as a vector
  spi_vector <- as.numeric(spi_result[[col_name]]$fitted)
  
  #spi_vector[spi_vector == -Inf | spi_vector == Inf | is.infinite(spi_vector)] <- NA
  
  # Add the vector to the dataframe
  spi_matrix[[col_name]] <- spi_vector
}

#format
SGI12<-spi_matrix %>% 
  pivot_longer(cols= -date,names_to= "Codigo_nacional", values_to="SGI_12") %>%
  left_join(estaciones_pozos, by= "Codigo_nacional")  %>% 
  mutate(Year = as.numeric(substr(date, 4,7))) %>%
  mutate(Month = as.numeric(substr(date, 1, 2))) %>%
  mutate(hydro_year = if_else(Month >= 4, Year, Year - 1)) %>% 
  mutate(ID_subcuenca = 
           case_when(
             Nombre_subcuenca == "Costeras entre Rios Quilimari y Petorca" ~ "CQP", 
             Nombre_subcuenca == "Petorca Medio" ~ "MP",
             Nombre_subcuenca == "Rio Ligua Alto (Estero Alicahue)" ~ "UL",
             Nombre_subcuenca == "Rio Ligua Bajo (Entre Estero Los Angeles y Desembocadura)" ~ "LL",
             Nombre_subcuenca == "Rio Ligua Medio (entre Quebrada La Cerrada y Los Angeles)" ~ "ML",
             Nombre_subcuenca == "Rio Petorca Alto (hasta despues Junta Rio Sobrante)" ~ "UP",
             Nombre_subcuenca == "Rio Petorca Bajo (Entre Las Palmas y Desembocadura)" ~ "LP",
             Nombre_subcuenca == "Rio Quilimari Entre Cajon Ingienillo y Desembocadura" ~ "LQ",
             Nombre_subcuenca == "Rio Quilimari entre muro Embalse Culimo y Bajo Cajon Ingienillo" ~ "MQ",
             Nombre_subcuenca == "Rio Quilimari hasta muro Embalse Culimo" ~ "UQ"
           )) %>% 
  mutate(Name_subcuenca = 
           case_when(
             ID_subcuenca == "CQP" ~ "Coastal Quilimari Petorca", 
             ID_subcuenca == "MP" ~ "Middle Petorca",
             ID_subcuenca == "UL" ~ "Upper Ligua",
             ID_subcuenca == "LL" ~ "Lower Ligua",
             ID_subcuenca == "ML" ~ "Middle Ligua",
             ID_subcuenca == "UP" ~ "Upper Petorca",
             ID_subcuenca == "LP" ~ "Lower Petorca",
             ID_subcuenca == "LQ" ~ "Lower Quilimari" ,
             ID_subcuenca == "MQ" ~ "Middle Quilimari",
             ID_subcuenca == "UQ" ~ "Upper Quilimari"
           ))

write.csv2(SGI12, "C:/Users/56984/Documents/CCGUC/SACBAD/SGI/SGI_12_1991_2019.csv", row.names = FALSE)
#Promedio por subcuenca

SGI12_mean_subcuencas<-SGI12 %>% group_by(date, ID_subcuenca) %>% 
  summarize(SGI_12_mean = mean(SGI_12, na.rm=TRUE)) 

SGI12_mean_hydro_year<- SGI12_mean_subcuencas %>%
  mutate(Year = as.numeric(substr(date, 4,7))) %>%
  mutate(Month = as.numeric(substr(date, 1, 2))) %>%
  mutate(hydro_year = if_else(Month >= 4, Year, Year - 1)) %>% 
  group_by(hydro_year, ID_subcuenca) %>%
  summarize(SGI_12_mean = mean(SGI_12_mean, na.rm=TRUE))  %>%
  filter(hydro_year >= 1990)

write.csv()

#Formato seba
SGI12<- SGI12 %>% select(date, ID_subcuenca, SGI_12)   %>%
  pivot_wider(names_from= date, values_from = SGI_12 )
