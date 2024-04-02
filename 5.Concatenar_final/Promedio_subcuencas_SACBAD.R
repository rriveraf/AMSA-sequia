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


#Leer metadatos estaciones todas
#metadatos_DGA_DMC<-read.csv(paste0(getwd(), "/BBDD/metadatos/DGA+DMC/estaciones_DGA+DMC.csv"))

#Leer metadatos estaciones en cuencas de estudio SACBAD
estaciones_sacbad_pp<-read.csv2(paste0(getwd(),"/BBDD/metadatos/DGA+DMC/estaciones_sacbad_calidad_pp_70%.csv"))

#leer preciptiaciones
pp<-read.csv2("C:/Users/56984/Documents/CCGUC/SACBAD/pp/DGA+DMC/pp_mensual_estaciones_SACBAD_1951_2023_rellenado.csv")

#Formateamos
pp<-pp %>% select(Year, Month, Codigo_nacional, pp_month) %>% 
  pivot_wider (names_from = Codigo_nacional, values_from = pp_month) %>%
  mutate(Month = if_else(nchar(Month) < 2, paste0("0",as.character(Month)), as.character(Month))) %>%
  mutate(date = paste0(Month, "/" , Year)) %>% 
  select(date, everything()) %>% 
  select(-Month, -Year)

#Separamos las estaciones DGA de las estaciones DMC
pp_DMC<-NULL
pp_DGA<-NULL
k<-1
j<-1
for(i in names(pp)) {
  if (nchar(i) < 7){
    print(paste0("columna ",  i , " es DMC y vamos en la colname ", k))
    pp_DMC<-cbind(pp_DMC, pp[[i]])
    colnames(pp_DMC)[k]<- i
    k<-k+1
  
  } else { 
    pp_DGA<-cbind(pp_DGA, pp[[i]])  
    colnames(pp_DGA)[j]<- i
    j<-j+1
    }
}
pp_DGA<-cbind(pp[,c(1)], pp_DGA)
pp_DMC<-as.data.frame(pp_DMC)


IPE3<-calcular_ipe(pp_acumulada_mensual = pp_DGA, acumulacion = 3 , fuente = "dga" , directorio = getwd())

#Leer indicador de entrada
IPE12<-read.csv("C:/Users/56984/Documents/CCGUC/SACBAD/SACBAD/BBDD/indicadores/IPE/DGA+DMC/IPE12/IPE_12_DGA+DMC_1951_2023.csv")

#Formatear
#IPE12<-as.data.frame(IPE12)

IPE12<-Corregir_Codigo_nacional(IPE12)
#IPE12<-formatear_fecha(IPE12)

#Filtrar estaciones para cuencas de estudio SACBAD
# Use select with one_of to choose only columns present in estaciones_sacbad metadata
IPE12 <- IPE12 %>%
  select(date, one_of(estaciones_sacbad$Codigo_nacional))

IPE12 <- IPE12  %>%
  pivot_longer(cols = -date, names_to = "Codigo_nacional", values_to = "IPE12")

IPE12<- left_join(IPE12, estaciones_sacbad, by= "Codigo_nacional")

write.csv(IPE12, paste0(getwd(), "/BBDD/indicadores/IPE/DGA+DMC/IPE12/IPE12_1951_2023_07_SACBAD.csv"), row.names = FALSE)

# Para rellenar la serie IPE: Calcula el promedio de mean_IPE12 por subcuenca y fecha
IPE12_mean <- IPE12 %>%
  group_by(date) %>%
  summarize(mean_IPE12 = mean(IPE12, na.rm = TRUE), .groups = "drop")

# Une el resultado con el dataframe original y reemplaza los valores NA en mean_IPE12
IPE12_rellenado <- IPE12 %>%
  left_join(IPE12_mean, by = c("date")) %>%
  mutate(IPE12 = ifelse(is.na(IPE12), mean_IPE12, IPE12)) %>%
  select(-mean_IPE12)

write.csv(IPE12_rellenado, paste0(getwd(), "/BBDD/indicadores/IPE/DGA+DMC/IPE12/IPE12_1951_2023_07_SACBAD_rellenado.csv"), row.names = FALSE)

IPE12_hydro_year <- IPE12_rellenado %>%
  mutate(date = as.Date(paste("01", date, sep = "/"), format = "%d/%m/%Y")) %>%
  mutate(hydro_year = if_else(month(date) >= 4, year(date), year(date) - 1)) %>%
  group_by(Nombre_subcuenca, ID, hydro_year, date) %>%
  summarize(mean_IPE12 = mean(IPE12, na.rm = TRUE)) %>%
  filter(hydro_year>1990)

write.csv(IPE12_hydro_year, paste0(getwd(), "/BBDD/indicadores/IPE/DGA+DMC/IPE12/IPE12_1951_2023_07_SACBAD_hydro_year_rellenado.csv"), row.names = FALSE)

IPE12_sacbad_mensual<- IPE12 %>%
  mutate(date = as.Date(paste("01", date, sep = "/"), format = "%d/%m/%Y")) %>%
  group_by(Nombre_subcuenca, date) %>%
  summarize(mean_IPE12 = mean(IPE12, na.rm = TRUE))  %>% 
  filter(year(date)>1990)

write.csv(IPE12_sacbad_mensual, paste0(getwd(), "/BBDD/indicadores/IPE/DGA+DMC/IPE12/IPE12_1951_2023_07_SACBAD_mensual.csv"), row.names = FALSE)

IPE12_hydro_year$date <- as.Date(IPE12_hydro_year$date)


## Cuadal

q<-read.csv("C:/Users/56984/Documents/CCGUC/SACBAD/SACBAD/BBDD/q/DGA/bruto/q_mean_DGA_1990_2023.csv")
q<-q %>% 
  group_by(Codigo_nacional, Year, Month) %>% 
  summarize(q_mean_monthly=mean(q_mean, na.rm=TRUE))%>% 
  left_join(estaciones_caudal, by="Codigo_nacional") %>% 
  na.omit() %>%
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
write.csv2(q, 
           "C:/Users/56984/Documents/CCGUC/SACBAD/caudales/q_mean_monthly_DGA_sacbad_1990_2023.csv", 
           row.names = FALSE)

##Estandarizar
##ICE 
q_nueva<-read.csv2("C:/Users/56984/Documents/CCGUC/SACBAD/caudales/q_mean_monthly_DGA_sacbad_1990_2023.csv")
##ICE 6 Y 12 DGA
#q_nueva<-read.csv(paste0("C:/Users/56984/Documents/CCGUC/ANID_Sequias/BBDD/q/DGA/matriz/q_mean_DGA",fecha_fin,".csv"))
Carac_gamma_ICE<-read.csv("C:/Users/56984/Documents/CCGUC/ANID_Sequias/BBDD/coeficientes/DGA/ICE/Coeficientes_ICE_1991_2020.csv")
Carac_gamma_ICE<-Corregir_Codigo_nacional(Carac_gamma_ICE)

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
  left_join(estaciones_sacbad,by="Codigo_nacional") %>%
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


ICE6<-ICE6 %>% 
  mutate(date=row.names(ICE6)) %>%
  mutate(
    Month = as.numeric(substr(date, 1, 2)),
    Year = as.numeric(substr(date, 4, 7)))  %>%
  pivot_longer(
    cols = c("05100001-3", "05110002-6", "05200001-7"),
    names_to = "Codigo_nacional",
    values_to = "ICE6") %>%
  left_join(estaciones_sacbad,by="Codigo_nacional") %>%
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
  

ICE12<-ICE12 %>% 
  mutate(date=row.names(ICE12)) %>%
  mutate(
    Month = as.numeric(substr(date, 1, 2)),
    Year = as.numeric(substr(date, 4, 7)))  %>%
  pivot_longer(
    cols = c("05100001-3", "05110002-6", "05200001-7"),
    names_to = "Codigo_nacional",
    values_to = "ICE12") %>%
  left_join(estaciones_sacbad,by="Codigo_nacional") %>%
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

write.csv2(ICE3, "C:/Users/56984/Documents/CCGUC/SACBAD/ICE/ICE3/ICE3_estaciones_monthly_1991_2023.csv", row.names=FALSE)
write.csv2(ICE6, "C:/Users/56984/Documents/CCGUC/SACBAD/ICE/ICE6/ICE6_estaciones_monthly_1991_2023.csv", row.names=FALSE)
write.csv2(ICE12, "C:/Users/56984/Documents/CCGUC/SACBAD/ICE/ICE12/ICE12_estaciones_monthly_1991_2023.csv", row.names=FALSE)

#Extraer el valor del último mes del año hidrológico (marzo)
ICE6_last_month <- ICE6 %>% filter(Month ==3) %>% select(hydro_year, ID_subcuenca, Name_subcuenca, ICE6)
ICE12_last_month <- ICE12 %>% filter(Month ==3) %>% select(hydro_year, ID_subcuenca, Name_subcuenca, ICE12)

write.csv2(ICE6_last_month, "C:/Users/56984/Documents/CCGUC/SACBAD/ICE/ICE6/ICE6_estaciones_last_month_1991_2022.csv", row.names=FALSE)
write.csv2(ICE12_last_month, "C:/Users/56984/Documents/CCGUC/SACBAD/ICE/ICE12/ICE12_estaciones_last_month_1991_2022.csv", row.names=FALSE)

#Promedio por año hidrológico
ICE6_mean_hydro_year<- ICE6 %>% 
  select(hydro_year, Month, ID_subcuenca, Name_subcuenca, ICE6) %>% 
  group_by(hydro_year, ID_subcuenca)   %>% 
  summarize(ICE6_mean_hydro_year = mean(ICE6))

ICE12_mean_hydro_year<- ICE12 %>% 
  select(hydro_year, Month, ID_subcuenca, Name_subcuenca, ICE12) %>% 
  group_by(hydro_year, ID_subcuenca)   %>% 
  summarize(ICE12_mean_hydro_year = mean(ICE12))

write.csv2(ICE6_mean_hydro_year, "C:/Users/56984/Documents/CCGUC/SACBAD/ICE/ICE6/ICE6_subcuencas_mean_hydro_year_1991_2022.csv", row.names=FALSE)
write.csv2(ICE12_mean_hydro_year, "C:/Users/56984/Documents/CCGUC/SACBAD/ICE/ICE12/ICE12_subcuencas_mean_hydro_year_1991_2022.csv", row.names=FALSE)

#Formato resumen para ICE_mean_hydro_year, años como columnas, subcuencas como filas
ICE6_resumen<-ICE6_mean_hydro_year %>% pivot_wider(names_from = hydro_year, values_from = ICE6_mean_hydro_year)
ICE12_resumen<-ICE12_mean_hydro_year %>% pivot_wider(names_from = hydro_year, values_from = ICE12_mean_hydro_year)

write.csv2(ICE6_resumen, "C:/Users/56984/Documents/CCGUC/SACBAD/ICE/ICE6/ICE6_resumen_mean_hydro_year_1991_2022.csv", row.names=FALSE)
write.csv2(ICE12_resumen, "C:/Users/56984/Documents/CCGUC/SACBAD/ICE/ICE12/ICE12_resumen_mean_hydro_year_1991_2022.csv", row.names=FALSE)

#Formato resumen para ICE_marzo, años como columnas, subcuencas como filas
ICE6_resumen_last_month<-ICE6_last_month %>% pivot_wider(names_from = hydro_year, values_from = ICE6)
ICE12_resumen_last_month<-ICE12_last_month %>% pivot_wider(names_from = hydro_year, values_from = ICE12)

write.csv2(ICE6_resumen_last_month, "C:/Users/56984/Documents/CCGUC/SACBAD/ICE/ICE6/ICE6_resumen_last_month_1991_2022.csv", row.names=FALSE)
write.csv2(ICE12_resumen_last_month, "C:/Users/56984/Documents/CCGUC/SACBAD/ICE/ICE12/ICE12_resumen_last_month_1991_2022.csv", row.names=FALSE)

# Resumen de hydro_year por Subcuencas

q_rellenado_subcuencas<-q %>% 
  group_by(Nombre_subcuenca, hydro_year) %>%
  summarize(q_mean_subcuenca=mean(q_mean_anual)) %>%
  pivot_wider(names_from = hydro_year, values_from = q_mean_subcuenca)

write.csv2(q_rellenado_subcuencas, 
           "C:/Users/56984/Documents/CCGUC/SACBAD/caudales/q_mean_hydro_year_subcuencas_sacbad_1990_2023.csv", 
           row.names = FALSE)






## POZOS: Niveles de pozos
estaciones_pozos<-read.csv(paste0(getwd(),"/BBDD/metadatos/DGA/estaciones_sacbad_pozos_calidad_29%.csv"))
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

##SGIE 6 Y 12 DGA
Carac_gamma_SGI<-read.csv("C:/Users/56984/Documents/CCGUC/SACBAD/SACBAD/BBDD/coeficientes/DGA/SGI/Coeficientes_SGI12_1991_2020.csv")
#Carac_gamma_SGI<-Corregir_Codigo_nacional(Carac_gamma_SGI)













#Precipitación

pp<-read.csv2("C:/Users/56984/Documents/CCGUC/WebScraping/BBDD/pp/DGA+DMC/pp_DGA+DMC_1989_2023.csv")
pp<-Corregir_Codigo_nacional(pp)

#Formateamos hasta llegar a estaciones sacbad seleccionadas con pp mensual
pp <- pp %>%
  mutate_all(as.numeric) %>%
  pivot_longer(cols = -c(Year, Month), 
               names_to = "Codigo_nacional", 
               values_to = "pp_month")  %>%
  filter(Year>=1990) %>%
  left_join(estaciones_sacbad, by="Codigo_nacional") %>%
  filter(!is.na(Nombre_subcuenca)) %>%
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


write.csv2(pp, "C:/Users/56984/Documents/CCGUC/SACBAD/pp/DGA+DMC/pp_mensual_estaciones_SACBAD_1951_2023.csv", row.names = FALSE)


# Para rellenar la serie IPE: Calcula el promedio de mean_IPE12 por subcuenca y fecha
pp_month_mean_subcuencas <- pp %>%
  group_by(Year, Month,Nombre_subcuenca) %>%
  summarize(pp_month_mean_subcuenca = mean(pp_month, na.rm = TRUE))

options(scipen = 999)
pp_month_mean<-pp %>%
  group_by(Year, Month) %>%
  summarize(pp_month_mean = mean(pp_month, na.rm = TRUE))
options(scipen = 0)

# Une el resultado con el dataframe original y reemplaza los valores NA en 
pp_month_mean_subcuencas <- pp_month_mean_subcuencas  %>%
  left_join(pp_month_mean, by= c("Year", "Month")) %>%
  mutate(pp_month_mean_subcuenca = ifelse(is.na(pp_month_mean_subcuenca), pp_month_mean, pp_month_mean_subcuenca)) %>%
  select(-pp_month_mean)

pp_rellenado <- pp  %>%
  left_join(pp_month_mean_subcuencas, by= c("Year", "Month", "Nombre_subcuenca")) %>%
  mutate(pp_month = ifelse(is.na(pp_month), pp_month_mean_subcuenca, pp_month)) %>%
  select(-pp_month_mean_subcuenca)

write.csv2(pp_rellenado, "C:/Users/56984/Documents/CCGUC/SACBAD/pp/DGA+DMC/pp_mensual_estaciones_SACBAD_1951_2023_rellenado.csv", row.names = FALSE)

pp_hydro_year_estaciones <- pp_rellenado %>%
  group_by(hydro_year, Codigo_nacional) %>%
  summarize(pp_acc_hydro_year= sum(pp_month)) %>%
  left_join(estaciones_sacbad_pp, by="Codigo_nacional") 

write.csv2(pp_hydro_year_estaciones, "C:/Users/56984/Documents/CCGUC/SACBAD/pp/DGA+DMC/pp_hydro_year_estaciones_SACBAD_1990_2023_rellenado.csv", row.names = FALSE)

# Resumen de hydr_year por Subcuencas

pp_hydro_year_subcuencas<-pp_hydro_year_estaciones %>% 
  group_by(Nombre_subcuenca, hydro_year) %>%
  summarize(pp_annual_mean_subcuenca=mean(pp_acc_hydro_year)) %>%
  pivot_wider(names_from = hydro_year, values_from = pp_annual_mean_subcuenca)

write.csv2(pp_hydro_year_subcuencas, "C:/Users/56984/Documents/CCGUC/SACBAD/pp/DGA+DMC/pp_hydro_year_subcuencas_SACBAD_1990_2023_rellenado.csv", row.names = FALSE)