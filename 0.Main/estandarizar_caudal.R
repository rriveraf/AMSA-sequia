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
source(paste0(getwd(), "/1.Descargar/actualizar_bbdd.R"))

#############################################################################
#       PARTE 1: Descargar y procesar serie de caudal                       #
#############################################################################

#Descargar ultimos datos de serie de Cuadal
#actualizar_datos(directorio = getwd(),
#                 variable = "caudal",
#                 fuente = "dga")

#METADATA estaciones seleccionadas SACBAD
estaciones_caudal<-read.csv(paste0(getwd(), "/BBDD/metadatos/DGA/estaciones_sacbad_caudal_calidad.csv"))

#Leer base de datos cruda antigua
q_2023_7<-read.csv("C:/Users/56984/Documents/CCGUC/SACBAD/SACBAD/BBDD/q/DGA/bruto/q_mean_DGA_1990_2023_7.csv")

#Para Sobreescribir con los datos más actualizados del año
q_2022_12<-q_2023_7 %>%
            filter(Year<2023)

#Leer base de datos cruda nueva
q_2023_12<-read.csv("C:/Users/56984/Documents/CCGUC/SACBAD/SACBAD/BBDD/q/DGA/bruto/q_mean_DGA_2023_2023_12.csv")
colnames(q_2023_11)[5]<-"q_mean"

#Unir base de datos antigua con base de datos nueva
q<-rbind(q_2022_12,q_2023_11)

#Filtrar las estaciones sacbad para escribir base de datos cruda diaria
codigo_nacional_values <- estaciones_caudal$Codigo_nacional
q_sacbad <- q %>%
  filter(Codigo_nacional %in% codigo_nacional_values)
#Output base de datos cruda diaria
write.csv2(q_sacbad , paste0(getwd(), "/BBDD/q/DGA/bruto/q_daily_mean_DGA_1990_2023_12.csv") , row.names = F)

#Resumir Mensualmente y extraer estaciones SACBAD 
q<-promedio_mensual(q, filtro = 100)

q<-q %>% 
  left_join(estaciones_caudal, by="Codigo_nacional") %>% 
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
             ID_subcuenca == "UL" ~ "Uqer Ligua",
             ID_subcuenca == "LL" ~ "Lower Ligua",
             ID_subcuenca == "ML" ~ "Middle Ligua",
             ID_subcuenca == "UP" ~ "Uqer Petorca",
             ID_subcuenca == "LP" ~ "Lower Petorca",
             ID_subcuenca == "LQ" ~ "Lower Quilimari" ,
             ID_subcuenca == "MQ" ~ "Middle Quilimari",
             ID_subcuenca == "UQ" ~ "Uqer Quilimari"
           ))

write.csv2(q, 
           paste0(getwd(),"/BBDD/q/DGA/procesado/q_mean_monthly_DGA_sacbad_1990_2023_12_crudo.csv"), 
           row.names = FALSE)

#RELLENAR la serie:
#serie de datos reemplazando valores NA con promedios por cuenca, año y mes
q<-read.csv2(paste0(getwd(),"/BBDD/q/DGA/procesado/q_mean_monthly_DGA_sacbad_1990_2023_12_crudo.csv"))

#Calcular valores promedio
q_month_mean_subcuencas <- q %>%
  group_by(Year, Month, Nombre_subcuenca) %>%
  summarize(q_month_mean_subcuenca = mean(q_mean, na.rm = TRUE))

options(scipen = 999)
q_month_mean<-q %>%
  group_by(Year, Month) %>%
  summarize(q_month_mean = mean(q_mean, na.rm = TRUE))
options(scipen = 0)

# Reemplazar NA's con datos promedio
q_month_mean_subcuencas <- q_month_mean_subcuencas  %>%
  left_join(q_month_mean, by= c("Year", "Month")) %>%
  mutate(q_month_mean_subcuenca = ifelse(is.na(q_month_mean_subcuenca), q_month_mean, q_month_mean_subcuenca)) %>%
  select(-q_month_mean)

q_rellenado <- q  %>%
  left_join(q_month_mean_subcuencas, by= c("Year", "Month", "Nombre_subcuenca")) %>%
  mutate(q_mean = ifelse(is.na(q_mean), q_month_mean_subcuenca, q_mean)) %>%
  select(-q_month_mean_subcuenca)

#Output
write.csv2(q_rellenado, paste0(getwd(), "/BBDD/q/DGA/procesado/q_mensual_estaciones_SACBAD_1990_2023_12_rellenado.csv"), row.names = FALSE)



#RESUMEN de datos en formato SACBAD
#Por año hidrológico
q_hydro_year_last_month <- q_rellenado %>%
  filter(Month ==3)

write.csv2(q_hydro_year_last_month,  paste0(getwd(), "/BBDD/q/DGA/procesado/q_last_hydro_month_estaciones_1990_2022.csv"), row.names = FALSE)

# Por año hidrológico y por subcuencas
q_hydro_year_subcuencas<-q_hydro_year_last_month %>% 
  group_by(Nombre_subcuenca, hydro_year) %>%
  summarize(q_mean_last_hydro_month=mean(q_mean, na.rm = TRUE)) %>%
  pivot_wider(names_from = hydro_year, values_from = q_mean_last_hydro_month)

write.csv2(q_hydro_year_subcuencas, paste0(getwd(), "/BBDD/q/DGA/procesado/q_last_hydro_month_subcuencas_1990_2022.csv"), row.names = FALSE)




#############################################################################
#             PARTE 2: Estandarizacion de la serie de tiempo:               #
#############################################################################

##ICE 
q_nueva<-read.csv2(paste0(getwd(), "/BBDD/q/DGA/procesado/q_mensual_estaciones_SACBAD_1990_2023_12_rellenado.csv"))
##ICE 6 Y 12 DGA
#q_nueva<-read.csv(paste0("C:/Users/56984/Documents/CCGUC/ANID_Sequias/BBDD/q/DGA/matriz/q_mean_DGA",fecha_fin,".csv"))
Carac_gamma_ICE<-read.csv(paste0(getwd(),"/BBDD/coeficientes/DGA/ICE/Coeficientes_ICE_1991_2020.csv"))
Carac_gamma_ICE<-Corregir_Codigo_nacional(Carac_gamma_ICE)

q_nueva<-q_nueva %>% formatear_fecha() %>% 
  select(date, Codigo_nacional, q_mean) %>%
  pivot_wider(names_from= Codigo_nacional, values_from = q_mean)

q_nueva<-data.frame(q_nueva) %>%
  Corregir_Codigo_nacional()

ICE3<-calcular_ICE(q_nueva , Carac_gamma_ICE, acumulacion = 3)
ICE6<-calcular_ICE(q_nueva , Carac_gamma_ICE, acumulacion = 6)
ICE12<-calcular_ICE(q_nueva , Carac_gamma_ICE, acumulacion = 12)

ICE3<-ICE3 %>% 
  mutate(date=row.names(ICE3)) %>%
  mutate(
    Month = as.numeric(substr(date, 1, 2)),
    Year = as.numeric(substr(date, 4, 7)))  %>%
  pivot_longer(
    cols = c("05100001-3", "05110002-6", "05200001-7"),
    names_to = "Codigo_nacional",
    values_to = "ICE3") %>%
  left_join(estaciones_caudal,by="Codigo_nacional") %>%
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
             ID_subcuenca == "UL" ~ "Uqer Ligua",
             ID_subcuenca == "LL" ~ "Lower Ligua",
             ID_subcuenca == "ML" ~ "Middle Ligua",
             ID_subcuenca == "UP" ~ "Uqer Petorca",
             ID_subcuenca == "LP" ~ "Lower Petorca",
             ID_subcuenca == "LQ" ~ "Lower Quilimari" ,
             ID_subcuenca == "MQ" ~ "Middle Quilimari",
             ID_subcuenca == "UQ" ~ "Uqer Quilimari"
           ))


ICE6<-ICE6 %>% 
  mutate(date=row.names(ICE6)) %>%
  mutate(
    Month = as.numeric(substr(date, 1, 2)),
    Year = as.numeric(substr(date, 4, 7)))  %>%
  pivot_longer(
    cols = c("05100001-3", "05110002-6", "05200001-7"),
    names_to = "Codigo_nacional",
    values_to = "ICE6") %>%
  left_join(estaciones_caudal,by="Codigo_nacional") %>%
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
             ID_subcuenca == "UL" ~ "Uqer Ligua",
             ID_subcuenca == "LL" ~ "Lower Ligua",
             ID_subcuenca == "ML" ~ "Middle Ligua",
             ID_subcuenca == "UP" ~ "Uqer Petorca",
             ID_subcuenca == "LP" ~ "Lower Petorca",
             ID_subcuenca == "LQ" ~ "Lower Quilimari" ,
             ID_subcuenca == "MQ" ~ "Middle Quilimari",
             ID_subcuenca == "UQ" ~ "Uqer Quilimari"
           ))
  

ICE12<-ICE12 %>% 
  mutate(date=row.names(ICE12)) %>%
  mutate(
    Month = as.numeric(substr(date, 1, 2)),
    Year = as.numeric(substr(date, 4, 7)))  %>%
  pivot_longer(
    cols = c("05100001-3", "05110002-6", "05200001-7"),
    names_to = "Codigo_nacional",
    values_to = "ICE12") %>%
  left_join(estaciones_caudal,by="Codigo_nacional") %>%
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
             ID_subcuenca == "UL" ~ "Uqer Ligua",
             ID_subcuenca == "LL" ~ "Lower Ligua",
             ID_subcuenca == "ML" ~ "Middle Ligua",
             ID_subcuenca == "UP" ~ "Uqer Petorca",
             ID_subcuenca == "LP" ~ "Lower Petorca",
             ID_subcuenca == "LQ" ~ "Lower Quilimari" ,
             ID_subcuenca == "MQ" ~ "Middle Quilimari",
             ID_subcuenca == "UQ" ~ "Uqer Quilimari"
           ))

write.csv2(ICE3, paste0(getwd(),"/BBDD/indicadores/ICE/ICE3/ICE3_estaciones_monthly_1991_2023.csv"), row.names=FALSE)
write.csv2(ICE6, paste0(getwd(),"/BBDD/indicadores/ICE/ICE6/ICE6_estaciones_monthly_1991_2023.csv"), row.names=FALSE)
write.csv2(ICE12, paste0(getwd(),"/BBDD/indicadores/ICE/ICE12/ICE12_estaciones_monthly_1991_2023.csv"), row.names=FALSE)

#Extraer el valor del último mes del año hidrológico (marzo)
ICE3_last_month <- ICE3 %>% filter(Month ==3) 
ICE6_last_month <- ICE6 %>% filter(Month ==3) 
ICE12_last_month <- ICE12 %>% filter(Month ==3) 

write.csv2(ICE3_last_month, paste0(getwd(),"/BBDD/indicadores/ICE/ICE3/ICE3_estaciones_last_month_1991_2022.csv"), row.names=FALSE)
write.csv2(ICE6_last_month, paste0(getwd(),"/BBDD/indicadores/ICE/ICE6/ICE6_estaciones_last_month_1991_2022.csv"), row.names=FALSE)
write.csv2(ICE12_last_month, paste0(getwd(),"/BBDD/indicadores/ICE/ICE12/ICE12_estaciones_last_month_1991_2022.csv"), row.names=FALSE)

#Formato resumen para ICE_marzo, años como columnas, subcuencas como filas
ICE3_resumen_last_month<-ICE3_last_month %>% pivot_wider(names_from = hydro_year, values_from = ICE3)
ICE6_resumen_last_month<-ICE6_last_month %>% pivot_wider(names_from = hydro_year, values_from = ICE6)
ICE12_resumen_last_month<-ICE12_last_month %>% pivot_wider(names_from = hydro_year, values_from = ICE12)


write.csv2(ICE3_resumen_last_month, paste0(getwd(),"/BBDD/indicadores/ICE/ICE3/ICE3_resumen_last_month_1991_2022.csv"), row.names=FALSE)
write.csv2(ICE6_resumen_last_month, paste0(getwd(),"/BBDD/indicadores/ICE/ICE6/ICE6_resumen_last_month_1991_2022.csv"), row.names=FALSE)
write.csv2(ICE12_resumen_last_month, paste0(getwd(),"/BBDD/indicadores/ICE/ICE12/ICE12_resumen_last_month_1991_2022.csv"), row.names=FALSE)


#Promedio por año hidrológico

#ICE6_mean_hydro_year<- ICE6 %>% 
#  select(hydro_year, Month, ID_subcuenca, Name_subcuenca, ICE6) %>% 
#  group_by(hydro_year, ID_subcuenca)   %>% 
#  summarize(ICE6_mean_hydro_year = mean(ICE6))

#ICE12_mean_hydro_year<- ICE12 %>% 
#  select(hydro_year, Month, ID_subcuenca, Name_subcuenca, ICE12) %>% 
#  group_by(hydro_year, ID_subcuenca)   %>% 
#  summarize(ICE12_mean_hydro_year = mean(ICE12))

#write.csv2(ICE6_mean_hydro_year, "C:/Users/56984/Documents/CCGUC/SACBAD/ICE/ICE6/ICE6_subcuencas_mean_hydro_year_1991_2022.csv", row.names=FALSE)
#write.csv2(ICE12_mean_hydro_year, "C:/Users/56984/Documents/CCGUC/SACBAD/ICE/ICE12/ICE12_subcuencas_mean_hydro_year_1991_2022.csv", row.names=FALSE)

#Formato resumen para ICE_mean_hydro_year, años como columnas, subcuencas como filas
#ICE6_resumen<-ICE6_mean_hydro_year %>% pivot_wider(names_from = hydro_year, values_from = ICE6_mean_hydro_year)
#ICE12_resumen<-ICE12_mean_hydro_year %>% pivot_wider(names_from = hydro_year, values_from = ICE12_mean_hydro_year)

#write.csv2(ICE6_resumen, "C:/Users/56984/Documents/CCGUC/SACBAD/ICE/ICE6/ICE6_resumen_mean_hydro_year_1991_2022.csv", row.names=FALSE)
#write.csv2(ICE12_resumen, "C:/Users/56984/Documents/CCGUC/SACBAD/ICE/ICE12/ICE12_resumen_mean_hydro_year_1991_2022.csv", row.names=FALSE)


















