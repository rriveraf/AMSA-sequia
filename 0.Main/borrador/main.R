library(tidyverse)
library(readxl)
library(writexl)
library(lmomco)
library(SPEI)

################################################1) Descargar
setwd("C:/Users/56984/Documents/CCGUC/WebScraping")
nombres_funciones<-c("Descargar_pp_DGA.R","Descargar_caudal_DGA.R","Descargar_temp_DGA.R",
                     "Descargar_pp_DMC.R","Descargar_temp_max_DMC.R","Descargar_temp_min_DMC.R")
for(i in nombres_funciones){
source(paste0(getwd(),"/1.Descargar/",i))
}
#Definir estaciones a descargar
metadatos_DGA<-read_excel("C:/Users/56984/Documents/CCGUC/ANID_Sequias/BBDD/metadatos/DGA/estaciones_DGA.xlsx")
metadatos_DGA<-read_excel("C:/Users/56984/Documents/CCGUC/ANID_Sequias/BBDD/metadatos/DGA/pedido_eduardo.xlsx")
metadatos_DMC<-read_excel("C:/Users/56984/Documents/CCGUC/ANID_Sequias/BBDD/metadatos/DMC/estaciones_DMC.xlsx")

#Definir fechas a descargar
fecha_ini<-"2000"
fecha_fin<-"2023"

#DGA
q_1990_2023<-descargar_caudal_DGA(fecha_ini, fecha_fin, estaciones = metadatos_DGA)
pp_DGA_1990_2023<-descargar_pp_DGA(fecha_ini, fecha_fin, estaciones = metadatos_DGA)
temp_DGA_1990_2023<-descargar_temp_DGA(fecha_ini, fecha_fin, estaciones = metadatos_DGA)

#DMC
pp_DMC_2023<-descargar_pp_DMC(fecha_ini, fecha_fin, estaciones = metadatos_DMC)
temp_max_DMC_2023<-descargar_temp_max_DMC(fecha_ini, fecha_fin, estaciones = metadatos_DMC)
temp_min_DMC_2023<-descargar_temp_min_DMC(fecha_ini, fecha_fin, estaciones = metadatos_DMC)
temp_DGA_2023<-pp_DGA_2023 %>% select(Year, Month, Day, Codigo_nacional, pp_day)
#Guardar caudal DGA
write_csv(q_2023, paste0("C:/Users/56984/Documents/CCGUC/ANID_Sequias/BBDD/q/DGA/bruto/q_mean_DGA_",fecha_fin,".csv"))


################################################2) Concatenar ################################################
#Se carga la base de datos antigua
q_antigua<-read.csv("C:/Users/56984/Documents/CCGUC/ANID_Sequias/BBDD/q/DGA/bruto/q_mean_DGA.csv")
q_antigua<- q_antigua %>%
  select(Year, Month, Day, Codigo_nacional, q_mean)
#Se concatenan las bases de datos
names(q_2023)<-names(q_antigua)
q_nueva<-rbind(q_antigua, q_2023)

#Se escribe la nueva base concatenada
write.csv(q_nueva, "C:/Users/56984/Documents/CCGUC/ANID_Sequias/BBDD/q/DGA/bruto/q_mean_DGA.csv")

################################################ 3) Promediar y formatear##############
#Herramienta del proceso:
source("C:/Users/56984/Documents/CCGUC/WebScraping/2.Promediar_formatear/promedio_mensual.R")

###ICE: Caudal DGA#
q_nueva<-read.csv("C:/Users/56984/Documents/CCGUC/ANID_Sequias/BBDD/q/DGA/bruto/q_mean_DGA.csv")

q_nueva<-data.frame(q_nueva)
#Filtrar ultimos dos años para no realizar el calculo en toda la bbdd
q_nueva<-q_nueva %>%
        filter(Year>2021) %>%
        select(Year, Month, Day, Codigo_nacional, q_mean)

#Acumulado caudal mensual
q_nueva<- acumulado_mensual(q_nueva)

# Formatear a estilo matriz
q_nueva<-formatear_wider(q_nueva)

# Formatear fechas
q_nueva<-formatear_fecha(q_nueva)

#Output
write.csv(q_nueva, paste0("C:/Users/56984/Documents/CCGUC/ANID_Sequias/BBDD/q/DGA/matriz/q_mean_DGA",fecha_fin,".csv"))

###IPE, IPEE: Precipitacion DGA#
pp_1950_2023<-read.csv("C:/Users/56984/Documents/CCGUC/ANID_Sequias/BBDD/pp/DGA/bruto/pp_DGA_1950_2023.csv")
#Acumulado mensual
pp_1950_2023<- acumulado_mensual(pp_1950_2023)
# Formatear matriz
pp_1950_2023<-formatear_wider(pp_1950_2023)
#Escribir pp DGA
write.csv(pp_1950_2023, "C:/Users/56984/Documents/CCGUC/ANID_Sequias/BBDD/pp/DGA/matriz/pp_DGA_1950_2023.csv")

#IPE, IPEE: Precipitacion DMC#

pp_DMC_2020_2023<-read_excel("C:/Users/56984/Documents/CCGUC/ANID_Sequias/BBDD/pp/DMC/bruto/pp_DMC_2020_2023.xlsx")
pp_DMC_2019_2019<-read_excel("C:/Users/56984/Documents/CCGUC/ANID_Sequias/BBDD/pp/DMC/bruto/pp_DMC_2019_2019.xlsx")
pp_DMC_2019_2023<-rbind(pp_DMC_2019_2019,pp_DMC_2020_2023)
names(pp_DMC_2019_2023)<-c("Year","Month","Day","Codigo_nacional","pp")
pp_DMC_2019_2023<-acumulado_mensual(pp_DMC_2019_2023)

pp_DMC_2019_2023<-formatear_wider(pp_DMC_2019_2023)

#4) Calcular indicador

##ICE 
source("C:/Users/56984/Documents/CCGUC/WebScraping/4.Calcular_indicadores/ICE/Codigo_base.R")
##ICE 6 Y 12 DGA
#q_nueva<-read.csv(paste0("C:/Users/56984/Documents/CCGUC/ANID_Sequias/BBDD/q/DGA/matriz/q_mean_DGA",fecha_fin,".csv"))
Carac_gamma_ICE<-read.csv("C:/Users/56984/Documents/CCGUC/ANID_Sequias/BBDD/coeficientes/DGA/ICE/Coeficientes_ICE_1991_2020.csv")
Carac_gamma_ICE<-Corregir_Codigo_nacional(Carac_gamma)
q_nueva<-data.frame(q_nueva) %>%
          Corregir_Codigo_nacional()

ICE6<-calcular_ICE(q_nueva , Carac_gamma_ICE, acumulacion = 6)
ICE12<-calcular_ICE(q_nueva , Carac_gamma_ICE, acumulacion = 12)

write.csv(ICE6, paste0("C:/Users/56984/Documents/CCGUC/ANID_Sequias/BBDD/indicadores/ICE/ICE6/ICE_6_",fecha_fin,".csv"))
write.csv(ICE12, paste0("C:/Users/56984/Documents/CCGUC/ANID_Sequias/BBDD/indicadores/ICE/ICE12/ICE_12_",fecha_fin,".csv"))


###########################IPE 
source("C:/Users/56984/Documents/CCGUC/WebScraping/4.Calcular_indicadores/IPE/Codigo_base.R")

##IPE 6 Y 12 DGA 
pp_acumulada_mensual<-read.csv("C:/Users/56984/Documents/CCGUC/ANID_Sequias/BBDD/pp/DGA/matriz/pp_DGA_1950_2023.csv")
pp_acumulada_mensual<-pp_acumulada_mensual %>%
                      filter(Year>2021)
pp_acumulada_mensual<-formatear_fecha(pp_acumulada_mensual)
pp_acumulada_mensual<-Corregir_Codigo_nacional(pp_acumulada_mensual)
Carac_gamma_IPE<-read.csv("C:/Users/56984/Documents/CCGUC/ANID_Sequias/BBDD/coeficientes/DGA/IPE/Coeficientes_IPE_1991_2020_DGA.csv")
Carac_gamma_IPE<-Corregir_Codigo_nacional(Carac_gamma)

IPE6_DGA<-calcular_ipe(pp_acumulada_mensual , Carac_gamma_IPE, acumulacion = 6)
IPE12_DGA<-calcular_ipe(pp_acumulada_mensual , Carac_gamma_IPE, acumulacion = 12)

##IPE 6 Y 12 DMC 
pp_acumulada_mensual_DMC<-read.csv("C:/Users/56984/Documents/CCGUC/ANID_Sequias/BBDD/pp/DMC/matriz/pp_DMC_1989_2023.csv")
pp_acumulada_mensual_DMC<-pp_acumulada_mensual_DMC[,-1]
pp_acumulada_mensual_DMC<-formatear_fecha(pp_acumulada_mensual_DMC)
pp_acumulada_mensual_DMC<-Corregir_Codigo_nacional(pp_acumulada_mensual_DMC)
Carac_gamma_DMC<-read.csv("C:/Users/56984/Documents/CCGUC/ANID_Sequias/BBDD/coeficientes/DMC/Coeficientes_IPE_1991_2020_DMC.csv")
Carac_gamma_DMC<-Carac_gamma_DMC[,-1]
Carac_gamma_DMC<-Corregir_Codigo_nacional(Carac_gamma_DMC)

IPE6_DMC<-calcular_ipe(pp_acumulada_mensual_DMC , Carac_gamma_DMC,acumulacion = 6)
IPE12_DMC<-calcular_ipe(pp_acumulada_mensual_DMC , Carac_gamma_DMC,acumulacion = 12)

##IPE 6 DGA + DMC
IPE6_DGA <- IPE6_DGA %>% 
  mutate(date = as.character(rownames(IPE6_DGA)))

IPE6_DMC <- IPE6_DMC %>% 
  mutate(date = as.character(rownames(IPE6_DMC)))

IPE_6<-full_join(IPE6_DGA,IPE6_DMC, by = "date")  %>% 
  select(date, everything())

write.csv(IPE_6,"C:/Users/56984/Documents/CCGUC/ANID_Sequias/BBDD/indicadores/IPE/DGA+DMC/IPE6/IPE_6_DGA+DMC_1951_2023.csv")

##IPE 12 DGA + DMC
IPE12_DGA <- IPE12_DGA %>% 
  mutate(date = as.character(rownames(IPE12_DGA)))

IPE12_DMC <- IPE12_DMC %>% 
  mutate(date = as.character(rownames(IPE12_DMC)))

IPE_12<-full_join(IPE12_DGA,IPE12_DMC, by = "date")  %>% 
  select(date, everything())

write.csv(IPE_12,"C:/Users/56984/Documents/CCGUC/ANID_Sequias/BBDD/indicadores/IPE/DGA+DMC/IPE12/IPE_12_DGA+DMC_1951_2023.csv")


#5) Concatenar y formatear para extraer  ultima fecha

library(lubridate)
ultimo_mes<-paste0(substr(Sys.time() %m-% months(1),6,7),"/",substr(Sys.time() %m-% months(1),1,4))

#Cargamos base de metadatos de las estaciones
metadatos_DGA_DMC<-read.csv("C:/Users/56984/Documents/CCGUC/ANID_Sequias/BBDD/metadatos/DGA+DMC/estaciones_DGA+DMC.csv")
metadatos_DGA_DMC<-metadatos_DGA_DMC[,-1]

##ICE 6
ICE_6<-read.csv("C:/Users/56984/Documents/CCGUC/ANID_Sequias/BBDD/indicadores/ICE/ICE6/ICE_6_2023.csv")
ICE_6<-ICE6 %>% 
  mutate(date = as.character(rownames(ICE6)))  %>% 
  select(date, everything())
ICE_6<-Corregir_Codigo_nacional(ICE_6)
consolidado<-concatenar(metadatos_DGA_DMC, ICE_6, ultimo_mes)

##ICE 12
ICE_12<-read.csv("C:/Users/56984/Documents/CCGUC/ANID_Sequias/BBDD/indicadores/ICE/ICE12/ICE_12_2023.csv")
ICE_12<-ICE12 %>% 
  mutate(date = as.character(rownames(ICE12)))  %>% 
  select(date, everything())
ICE_12<-Corregir_Codigo_nacional(ICE_12)
consolidado<-concatenar(consolidado, ICE_12, ultimo_mes)

##IPE 6
IPE_6<-read.csv("C:/Users/56984/Documents/CCGUC/ANID_Sequias/BBDD/indicadores/IPE/DGA+DMC/IPE6/IPE_6_DGA+DMC_1951_2023.csv")
IPE_6<-IPE_6[,-1]
IPE_6<-Corregir_Codigo_nacional(IPE_6)
consolidado<-concatenar(metadatos_DGA_DMC, IPE_6, ultimo_mes)

#IPE 12
IPE_12<-read.csv("C:/Users/56984/Documents/CCGUC/ANID_Sequias/BBDD/indicadores/IPE/DGA+DMC/IPE12/IPE_12_DGA+DMC_1951_2023.csv")
IPE_12<-IPE_12[,-1]
IPE_12<-Corregir_Codigo_nacional(IPE_12)

consolidado<-concatenar(consolidado, IPE_12, ultimo_mes)



#IPE 6
consolidado <- merge(metadatos_DGA_DMC, IPE_6_actual, by = c("Codigo_nacional"), all.x = TRUE)%>%
  filter(Codigo_nacional != 0)

consolidado <-reclass_indicador(consolidado, "IPE6")

#IPE 12
consolidado <- merge(consolidado, IPE_12_actual,  by = c("Codigo_nacional", "date"), all.x = TRUE, sort=FALSE) %>%
  filter(Codigo_nacional != 0)
consolidado <-reclass_indicador(consolidado, "IPE12")

#ICE 6
consolidado <- merge(consolidado, ICE_6_actual, by = c("Codigo_nacional", "date"), all.x = TRUE, sort=FALSE)%>%
               filter(Codigo_nacional != 0)
consolidado <-reclass_indicador(consolidado, "ICE6")

#ICE 12
consolidado <- merge(consolidado, ICE_12_actual, by = c("Codigo_nacional", "date"), all.x = TRUE, sort=FALSE) %>%
                filter(Codigo_nacional != 0)
consolidado <-reclass_indicador(consolidado, "ICE12")

write.csv(consolidado,"C:/Users/56984/Documents/CCGUC/ANID_Sequias/BBDD/indicadores/consolidado/Consolidado.csv", na="",fileEncoding = "UTF-8")

consolidado.readz<-read.csv("C:/Users/56984/Documents/CCGUC/ANID_Sequias/BBDD/indicadores/consolidado/Consolidado.csv")
consolidado<-read.csv("C:/Users/56984/Documents/CCGUC/ANID_Sequias/BBDD/indicadores/consolidado/Consolidado_IPE_DGA+DMC.csv")