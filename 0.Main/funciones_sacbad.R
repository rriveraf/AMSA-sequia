
library(tidyverse)

pp_bruta_a_sacbad<-function(serie_bruta, estaciones_sacbad, variable){
serie_sacbad<-serie_bruta %>% 
  group_by(Codigo_nacional, Year, Month) %>% 
  summarize(pp_month=sum(get(variable), na.rm=TRUE)) %>% 
  mutate(Codigo_nacional = as.character(Codigo_nacional)) %>% 
  full_join(estaciones_sacbad, by="Codigo_nacional") %>% 
  filter(!is.na(Estacion)) %>% 
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

 return(serie_sacbad)
#write.csv2(q, 
          # "C:/Users/56984/Documents/CCGUC/SACBAD/caudales/q_mean_monthly_DGA_sacbad_1990_2023.csv", 
          # row.names = FALSE)
}

