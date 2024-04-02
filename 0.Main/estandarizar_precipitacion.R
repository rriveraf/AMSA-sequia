#Precipitacion y estandarizacion proyecto SACBAD
#Escrito por Javier Enrique Vargas Ramírez
#CCG-UC PONTIFICIA UNIVERSIDAD CATÓLICA DE CHILE

library(ggplot2)
library(ggthemes)
library(zoo)
library(tidyverse)
library(SPEI)
library(zoo)
source(paste0(getwd(), "/1.Descargar/actualizar_bbdd.R"))
source(paste0(getwd(), "/2.Promediar_formatear/promedio_mensual.R"))
source(paste0(getwd(), "/4.Calcular_indicadores/SGI/calcular_SGI.R"))
source(paste0(getwd(), "/4.Calcular_indicadores/ICE/Codigo_base.R"))
source(paste0(getwd(), "/4.Calcular_indicadores/IPE/calcular_IPE.R"))
source(paste0(getwd(), "/0.Main/funciones_sacbad.R"))


#############################################################################
#       PARTE 1: Descargar y procesar serie de precipitación                #
#############################################################################

#Descargar ultimos datos de serie de precipitación 
actualizar_datos(directorio = getwd(),
                 variable = "pp",
                 fuente = "dga")

#Cargar METADATA de estaciones seleccionadas proyecto SACBAD
estaciones_sacbad_pp<-read.csv2(paste0(getwd(), "/BBDD/metadatos/DGA+DMC/estaciones_sacbad_calidad_pp_70%.csv"))

#Cargar los ultimos datos de precipitacion
pp_DGA_2023_11<-read.csv(paste0(getwd(), "/BBDD/pp/DGA/bruto/pp_DGA_2023_2023_11.csv"))
pp_DGA_1990_2022<-read.csv2( paste0(getwd(), "/BBDD/pp/DGA/bruto/pp_DGA_1990_2022_12_crudo.csv"))
pp_DGA<-rbind(pp_DGA_1990_2022, pp_DGA_2023_11)

pp_DMC<-read.csv(paste0(getwd(), "/BBDD/pp/DMC/bruto/pp_DMC_2023_2023_11.csv"))

#Formatear datos crudos diarios a formato crudo SACBAD
sacbad_pp_DGA<-pp_bruta_a_sacbad(serie_bruta = pp_DGA, 
                                   estaciones_sacbad = estaciones_sacbad_pp, 
                                   variable = "pp_day") %>%
                filter(nchar(Codigo_nacional) > 8)

sacbad_pp_DMC<-pp_bruta_a_sacbad(serie_bruta = pp_DMC, 
                                 estaciones_sacbad = estaciones_sacbad_pp, 
                                 variable = "pp_day")  %>%
                filter(nchar(Codigo_nacional) < 8)

sacbad_pp<-rbind(sacbad_pp_DGA, sacbad_pp_DMC)

    #Output
write.csv2(sacbad_pp, paste0(getwd(), "/BBDD/pp/DGA+DMC/pp_mensual_estaciones_SACBAD_2023_2023_11_crudo.csv"), row.names = F)



#UNIR a la serie antigua: 
#Leer base de datos nueva
#pp_sacbad_2023_11<-read.csv2(paste0(getwd(), "/BBDD/pp/DGA+DMC/pp_mensual_estaciones_SACBAD_2023_2023_11_crudo.csv"))

#Leer base de datos antigua
#pp_sacbad_1990_2023<-read.csv2(paste0(getwd(), "/BBDD/pp/DGA+DMC/pp_mensual_estaciones_SACBAD_1990_2023_crudo.csv"))

#Filtrar base de datos antigua para sobreescribir con los datos más actualizados
#pp_sacbad_1990_2022<- pp_sacbad_1990_2023 %>% filter(Year <2023)

#Unir base de datos nueva con antigua
#pp_sacbad_1990_2023_11<-rbind(pp_sacbad_1990_2022, pp_sacbad_2023_11)

  #Output
#write.csv2(pp_sacbad_1990_2023_11, paste0(getwd(), "/BBDD/pp/DGA+DMC/pp_mensual_estaciones_SACBAD_1990_2023_11_crudo.csv") , row.names = F)



#RELLENAR la serie:
#serie de datos reemplazando valores NA con promedios por cuenca, año y mes
pp<-read.csv2(paste0(getwd(), "/BBDD/pp/DGA+DMC/pp_mensual_estaciones_SACBAD_1990_2023_11_crudo.csv"))

#Calcular valores promedio
pp_month_mean_subcuencas <- pp %>%
  group_by(Year, Month, Nombre_subcuenca) %>%
  summarize(pp_month_mean_subcuenca = mean(pp_month, na.rm = TRUE))

options(scipen = 999)
pp_month_mean<-pp %>%
  group_by(Year, Month) %>%
  summarize(pp_month_mean = mean(pp_month, na.rm = TRUE))
options(scipen = 0)

# Reemplazar NA's con datos promedio
pp_month_mean_subcuencas <- pp_month_mean_subcuencas  %>%
  left_join(pp_month_mean, by= c("Year", "Month")) %>%
  mutate(pp_month_mean_subcuenca = ifelse(is.na(pp_month_mean_subcuenca), pp_month_mean, pp_month_mean_subcuenca)) %>%
  select(-pp_month_mean)

pp_rellenado <- pp  %>%
  left_join(pp_month_mean_subcuencas, by= c("Year", "Month", "Nombre_subcuenca")) %>%
  mutate(pp_month = ifelse(is.na(pp_month), pp_month_mean_subcuenca, pp_month)) %>%
  select(-pp_month_mean_subcuenca)

    #Output
write.csv2(pp_rellenado, paste0(getwd(), "/BBDD/pp/DGA+DMC/pp_mensual_estaciones_SACBAD_1990_2023_11_rellenado.csv"), row.names = FALSE)




#Resumenes de datos en formato SACBAD
#Por año hidrológico
pp_hydro_year_estaciones <- pp_rellenado %>%
  group_by(hydro_year, Codigo_nacional) %>%
  summarize(pp_acc_hydro_year= sum(pp_month)) %>%
  left_join(estaciones_sacbad_pp, by="Codigo_nacional") 

write.csv2(pp_hydro_year_estaciones,  paste0(getwd(), "/BBDD/pp/DGA+DMC/pp_hydro_year_estaciones_SACBAD_1990_2023_11_rellenado.csv"), row.names = FALSE)

# Por año hidrológico y por subcuencas
pp_hydro_year_subcuencas<-pp_hydro_year_estaciones %>% 
  group_by(Nombre_subcuenca, hydro_year) %>%
  summarize(pp_annual_mean_subcuenca=mean(pp_acc_hydro_year)) %>%
  pivot_wider(names_from = hydro_year, values_from = pp_annual_mean_subcuenca)

write.csv2(pp_hydro_year_subcuencas, paste0(getwd(), "/BBDD/pp/DGA+DMC/pp_hydro_year_subcuencas_SACBAD_1990_2023_11_rellenado.csv"), row.names = FALSE)



#############################################################################
#             PARTE 2: Estandarizacion de la serie de tiempo:               #
#############################################################################

#Leer metadatos estaciones en cuencas de estudio SACBAD
estaciones_sacbad_pp<-read.csv2(paste0(getwd(),"/BBDD/metadatos/DGA+DMC/estaciones_sacbad_calidad_pp_70%.csv"))

#2) leer preciptiaciones
pp<-read.csv2(paste0(getwd(), "/BBDD/pp/DGA+DMC/pp_mensual_estaciones_SACBAD_1990_2023_11_rellenado.csv"))

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
    #print(paste0("columna ",  i , " es DMC y vamos en la colname ", k))
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


######4) Calcular IPE 3
options(scipen = 999)
IPE3<-calcular_ipe(pp_acumulada_mensual = pp_DGA, acumulacion = 3 , fuente = "dga" , directorio = getwd())

#Formatear
IPE3 <- IPE3  %>%
  mutate(date = rownames(IPE3)) %>%
  pivot_longer(cols = -date, names_to = "Codigo_nacional", values_to = "IPE3") %>%
  left_join(estaciones_sacbad_pp, by= "Codigo_nacional")

#Rellenar la serie IPE 3 (algunos Coeficientes son NA)
IPE3_mean <- IPE3 %>%
  group_by(date) %>%
  summarize(mean_IPE3 = mean(IPE3, na.rm = TRUE), .groups = "drop")

# Une el resultado con el dataframe original y reemplaza los valores NA en mean_IPE12
IPE3_rellenado <- IPE3 %>%
  left_join(IPE3_mean, by = c("date")) %>%
  mutate(IPE3 = ifelse(is.na(IPE3), mean_IPE3, IPE3)) %>%
  select(-mean_IPE3)

  #Output IPE3 Rellenado
write.csv2(IPE3_rellenado, paste0(getwd(),"/BBDD/indicadores/IPE/DGA+DMC/IPE3/IPE3_1991_2023_11_rellenado.csv", row.names = FALSE))

#Resumen
#Por año hidrologico y subcuenca
IPE3_hydro_year <- IPE3_rellenado %>%
  mutate(date = as.Date(paste("01", date, sep = "/"), format = "%d/%m/%Y")) %>%
  mutate(hydro_year = if_else(month(date) >= 4, year(date), year(date) - 1)) %>%
  group_by(Nombre_subcuenca, ID, hydro_year, date) %>%
  summarize(mean_IPE3 = mean(IPE3, na.rm = TRUE)) %>%
  filter(hydro_year>1990)

#Output
write.csv2(IPE3_hydro_year, paste0(getwd(),"/BBDD/indicadores/IPE/DGA+DMC/IPE3/IPE3_1991_2023_11_hydro_year.csv"), row.names = FALSE)

# Por año hidrológico y por subcuencas
IPE3_hydro_year_subcuencas<-IPE3_hydro_year %>% 
  group_by(Nombre_subcuenca, hydro_year) %>%
  summarize(IPE3_mean_hydro_subcuenca=mean(mean_IPE3)) %>%
  pivot_wider(names_from = hydro_year, values_from = IPE3_mean_hydro_subcuenca)

#Output
write.csv2(IPE3_hydro_year_subcuencas, paste0(getwd(),"/BBDD/indicadores/IPE/DGA+DMC/IPE3/IPE3_1991_2023_11_hydro_year_subcuencas.csv"), row.names = FALSE)


############5) Calcular IPE 6
IPE6<-calcular_ipe(pp_acumulada_mensual = pp_DGA, acumulacion = 6 , fuente = "dga" , directorio = getwd())

#Formatear
IPE6 <- IPE6  %>%
  mutate(date = rownames(IPE6)) %>%
  pivot_longer(cols = -date, names_to = "Codigo_nacional", values_to = "IPE6") %>%
  left_join(estaciones_sacbad_pp, by= "Codigo_nacional")

# Para rellenar la serie IPE: Calcula el promedio de mean_IPE12 por subcuenca y fecha
IPE6_mean <- IPE6 %>%
  group_by(date) %>%
  summarize(mean_IPE6 = mean(IPE6, na.rm = TRUE), .groups = "drop")

# Une el resultado con el dataframe original y reemplaza los valores NA en mean_IPE12
IPE6_rellenado <- IPE6 %>%
  left_join(IPE6_mean, by = c("date")) %>%
  mutate(IPE6 = ifelse(is.na(IPE6), mean_IPE6, IPE6)) %>%
  select(-mean_IPE6)

#Output
write.csv2(IPE6_rellenado, paste0(getwd(),"/BBDD/indicadores/IPE/DGA+DMC/IPE6/IPE6_1991_2023_11_rellenado.csv"), row.names = FALSE)

#Resumen
#Por año hidrologico
IPE6_hydro_year <- IPE6_rellenado %>%
  mutate(date = as.Date(paste("01", date, sep = "/"), format = "%d/%m/%Y")) %>%
  mutate(hydro_year = if_else(month(date) >= 4, year(date), year(date) - 1)) %>%
  group_by(Nombre_subcuenca, ID, hydro_year, date) %>%
  summarize(mean_IPE6 = mean(IPE6, na.rm = TRUE)) %>%
  filter(hydro_year>1990)

  #Output
write.csv2(IPE6_hydro_year, paste0(getwd(),"/BBDD/indicadores/IPE/DGA+DMC/IPE6/IPE6_1991_2023_11_hydro_year.csv"), row.names = FALSE)

#Por año hidrológico y por subcuencas
IPE6_hydro_year_subcuencas<-IPE6_hydro_year %>% 
  group_by(Nombre_subcuenca, hydro_year) %>%
  summarize(IPE6_mean_hydro_subcuenca=mean(mean_IPE6)) %>%
  pivot_wider(names_from = hydro_year, values_from = IPE6_mean_hydro_subcuenca)

#Output
write.csv2(IPE6_hydro_year_subcuencas, paste0(getwd(),"/BBDD/indicadores/IPE/DGA+DMC/IPE6/IPE6_1991_2023_11_hydro_year_subcuencas.csv"), row.names = FALSE)





##########6) Calcular IPE 12
IPE12<-calcular_ipe(pp_acumulada_mensual = pp_DGA, acumulacion = 12 , fuente = "dga" , directorio = getwd())

#Formatear
IPE12 <- IPE12  %>%
  mutate(date = rownames(IPE12)) %>%
  pivot_longer(cols = -date, names_to = "Codigo_nacional", values_to = "IPE12") %>%
  left_join(estaciones_sacbad_pp, by= "Codigo_nacional")

# Para rellenar la serie IPE
IPE12_mean <- IPE12 %>%
  group_by(date) %>%
  summarize(mean_IPE12 = mean(IPE12, na.rm = TRUE), .groups = "drop")

# Reemplaza los valores NA en mean_IPE12
IPE12_rellenado <- IPE12 %>%
  left_join(IPE12_mean, by = c("date")) %>%
  mutate(IPE12 = ifelse(is.na(IPE12), mean_IPE12, IPE12)) %>%
  select(-mean_IPE12)

#Output
write.csv2(IPE12_rellenado, paste0(getwd(),
                                   "/BBDD/indicadores/IPE/DGA+DMC/IPE12/IPE12_1991_2023_11_rellenado.csv"), row.names = FALSE)

#Promediar IPE por año hidrologico y subcuenca
IPE12_hydro_year <- IPE12_rellenado %>%
  mutate(date = as.Date(paste("01", date, sep = "/"), format = "%d/%m/%Y")) %>%
  mutate(hydro_year = if_else(month(date) >= 4, year(date), year(date) - 1)) %>%
  group_by(Nombre_subcuenca, ID, hydro_year, date) %>%
  summarize(mean_IPE12 = mean(IPE12, na.rm = TRUE)) %>%
  filter(hydro_year>1990)

#Output
write.csv2(IPE12_hydro_year, paste0(getwd(),
                                   "/BBDD/indicadores/IPE/DGA+DMC/IPE12/IPE12_1991_2023_11_hydro_year.csv"), row.names = FALSE)

#Por año hidrológico y por subcuencas
IPE12_hydro_year_subcuencas<-IPE12_hydro_year %>% 
  group_by(Nombre_subcuenca, hydro_year) %>%
  summarize(IPE12_mean_hydro_subcuenca=mean(mean_IPE12)) %>%
  pivot_wider(names_from = hydro_year, values_from = IPE12_mean_hydro_subcuenca)

#Output
write.csv2(IPE12_hydro_year_subcuencas, paste0(getwd(),"/BBDD/indicadores/IPE/DGA+DMC/IPE12/IPE12_1991_2023_11_hydro_year_subcuencas.csv"), row.names = FALSE)

