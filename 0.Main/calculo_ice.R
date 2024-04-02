library(tidyverse)
library(readxl)
library(writexl)
library(lmomco)
library(SPEI)
library(lubridate)

#0) Parametros iniciales
#Directorio de trabajo
directorio<-getwd()

#Definir fechas importantes
ultimo_mes<-as.numeric(substr(Sys.time() %m-% months(1),6,7))
ultimo_ano<-as.numeric(substr(Sys.time() %m-% months(1),1,4))
penultimo_ano<-(as.numeric(substr(Sys.time() %m-% months(1),1,4))-1)
ultima_fecha<-paste0(substr(Sys.time() %m-% months(1),6,7),"/",substr(Sys.time() %m-% months(1),1,4))

#Se cargan funciones de utilidad
source(paste0(directorio, "/1.Descargar/actualizar_bbdd.R"))
source(paste0(directorio, "/2.Promediar_formatear/promedio_mensual.R"))
source(paste0(getwd(), "/4.Calcular_indicadores/ICE/Codigo_base.R"))
source(paste0(directorio, "/5.Concatenar_final/concatenar.R"))
source("C:/Users/56984/Documents/CCGUC/WebScraping/4.Calcular_indicadores/ICE/Codigo_base.R")

#1) Actualizar bases de datos climatologicas
#fuentes_disponibles<-c("dmc", "dga")
#variables_disponibles<-c("pp")


 actualizar_datos(directorio , 
                   variable = "caudal", 
                   fuente = "dga",
                   fecha_ini= "2000",
                   fecha_fin= "2021")


#2) Cargamos la precipitacion de ambas fuentes de datos (DGA Y DMC) 
#Nos quedamos solo con los últimos dos años para mayor eficiencia de cálculo
q_DGA<- read.csv(paste0(directorio,
                         "/BBDD/q/DGA/bruto/pp_DGA_",
                         "2022",
                         "_",
                         ultimo_ano,
                         "_",
                         ultimo_mes,
                         ".csv"))
###ICE: Caudal DGA#
q_nueva<-read.csv("C:/Users/56984/Documents/CCGUC/ANID_Sequias/BBDD/q/DGA/bruto/q_mean_DGA.csv")

q_nueva<-data.frame(q_nueva)
#Filtrar ultimos dos años para no realizar el calculo en toda la bbdd
q_nueva<-q_nueva %>%
        filter(Year>1990) %>%
        select(Year, Month, Day, Codigo_nacional, q_mean)

#Acumulado caudal mensual
q_nueva<- acumulado_mensual(q_nueva)

# Formatear a estilo matriz
q_nueva<-formatear_wider(q_nueva)

# Formatear fechas
q_nueva<-formatear_fecha(q_nueva)

#Output
write.csv(q_nueva, paste0("C:/Users/56984/Documents/CCGUC/ANID_Sequias/BBDD/q/DGA/matriz/q_mean_DGA",fecha_fin,".csv"))


#4) Calcular indicador

##ICE 
q_nueva<-read.csv2("C:/Users/56984/Documents/CCGUC/SACBAD/caudales/q_mean_monthly_DGA_sacbad_1990_2023.csv")
##ICE 6 Y 12 DGA
#q_nueva<-read.csv(paste0("C:/Users/56984/Documents/CCGUC/ANID_Sequias/BBDD/q/DGA/matriz/q_mean_DGA",fecha_fin,".csv"))
Carac_gamma_ICE<-read.csv("C:/Users/56984/Documents/CCGUC/ANID_Sequias/BBDD/coeficientes/DGA/ICE/Coeficientes_ICE_1991_2020.csv")
Carac_gamma_ICE<-Corregir_Codigo_nacional(Carac_gamma_ICE)

q_nueva<-data.frame(q_nueva) %>%
          Corregir_Codigo_nacional()


ICE6<-calcular_ICE(q_nueva , Carac_gamma_ICE, acumulacion = 6)
ICE12<-calcular_ICE(q_nueva , Carac_gamma_ICE, acumulacion = 12)

write.csv(ICE6, paste0("C:/Users/56984/Documents/CCGUC/ANID_Sequias/BBDD/indicadores/ICE/ICE6/ICE_6_",fecha_fin,".csv"))
write.csv(ICE12, paste0("C:/Users/56984/Documents/CCGUC/ANID_Sequias/BBDD/indicadores/ICE/ICE12/ICE_12_",fecha_fin,".csv"))

#5) Concatenar y formatear para extraer  ultima fecha

#Cargamos base de metadatos de las estaciones
metadatos_DGA<-readxl::read_xlsx(paste0(directorio,"/BBDD/metadatos/DGA/estaciones_DGA.xlsx"))

##ICE 6
consolidado<-concatenar(metadatos_DGA_DMC, ICE_6, ultimo_mes)

##ICE 12
consolidado<-concatenar(consolidado, ICE_12, ultimo_mes)

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


#Análisis de calidad de datos para las estaciones
resultado <- q_nueva %>%
  select(-date) %>%
  summarise(across(everything(), ~ sum(!is.na(.)) / n() * 100)) %>%
  pivot_longer(everything(), names_to = "Codigo_nacional", values_to = "Calidad")

resultado <- right_join(resultado, metadatos_DGA, by = "Codigo_nacional") %>%
  select(Codigo_nacional, Nombre.estacion, Latitud, Longitud, Cuenca, Subcuenca, Calidad)

write.csv(resultado, "C:/Users/56984/Documents/CCGUC/SACBAD/SACBAD/BBDD/metadatos/DGA/calidad_caudal.csv", row.names=FALSE)
