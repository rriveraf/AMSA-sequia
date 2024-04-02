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
source(paste0(directorio, "/4.Calcular_indicadores/IPE/calcular_IPE.R"))
source(paste0(directorio, "/5.Concatenar_final/concatenar.R"))

#1) Actualizar bases de datos climatologicas
#fuentes_disponibles<-c("dmc", "dga")
#variables_disponibles<-c("pp")

for(fuente_datos in fuentes_disponibles){
actualizar_datos(directorio , 
                 variable = "temp", 
                 fuente = "dga",
                 fecha_ini= "2000",
                 fecha_fin= "2021")
}

#2) Cargamos la precipitacion de ambas fuentes de datos (DGA Y DMC) 
#Nos quedamos solo con los últimos dos años para mayor eficiencia de cálculo
pp_DGA<- read.csv(paste0(directorio,
                                   "/BBDD/pp/DGA/bruto/pp_DGA_",
                                   "2022",
                                   "_",
                                   ultimo_ano,
                                   "_",
                                   ultimo_mes,
                                   ".csv"))

pp_DMC<- read.csv(paste0(directorio,
                                     "/BBDD/pp/DMC/bruto/pp_DMC_",
                                     "2020",
                                     "_",
                                     ultimo_ano,
                                     "_",
                                     ultimo_mes,
                                     ".csv"))

#Ocupamos solamente los dos últimos años para calcular IPE
pp_DGA<- pp_DGA %>%
  filter(Year>(as.numeric(substr(Sys.time() %m-% months(1),1,4))-2)) 
pp_DMC<- pp_DMC %>%
  filter(Year>(as.numeric(substr(Sys.time() %m-% months(1),1,4))-2)) 

#3) Acumular mensualmente y formatear
pp_DGA<- acumulado_mensual(pp_DGA)
pp_DGA<- formatear_wider(pp_DGA)
pp_DGA<- formatear_fecha(pp_DGA)
pp_DGA<- as.data.frame(pp_DGA)

pp_DMC<- acumulado_mensual(pp_DMC)
pp_DMC<- formatear_wider(pp_DMC)
pp_DMC<- formatear_fecha(pp_DMC)
pp_DMC<- as.data.frame(pp_DMC)

#4) Calcular IPE
IPE6_DGA<-calcular_ipe(pp_acumulada_mensual = pp_DGA, 
                       acumulacion = 6, 
                       fuente="DGA", 
                       directorio = directorio)

IPE12_DGA<-calcular_ipe(pp_acumulada_mensual = pp_DGA, 
                       acumulacion = 12, 
                       fuente="DGA", 
                       directorio = directorio)

IPE6_DMC<-calcular_ipe(pp_acumulada_mensual = pp_DMC, 
                       acumulacion = 6, 
                       fuente="DMC", 
                       directorio = directorio)

IPE12_DMC<-calcular_ipe(pp_acumulada_mensual = pp_DMC, 
                       acumulacion = 12, 
                       fuente="DMC", 
                       directorio = directorio)

IPE6_DGA <- IPE6_DGA %>% 
  mutate(date = as.character(rownames(IPE6_DGA)))

IPE6_DMC <- IPE6_DMC %>% 
  mutate(date = as.character(rownames(IPE6_DMC)))

IPE12_DGA <- IPE12_DGA %>% 
  mutate(date = as.character(rownames(IPE12_DGA)))

IPE12_DMC <- IPE12_DMC %>% 
  mutate(date = as.character(rownames(IPE12_DMC)))


#Union de DGA y DMC
IPE6<-full_join(IPE6_DGA,IPE6_DMC, by = "date")  %>% 
  select(date, everything())

IPE12<-full_join(IPE12_DGA,IPE12_DMC, by = "date")  %>% 
  select(date, everything())

pp<-full_join(pp_DGA,pp_DMC, by = "date")  %>% 
  select(date, everything())


#5) OUTPUT
write.csv(IPE6_DGA, paste0(directorio, 
                           "/BBDD/indicadores/IPE/DGA/IPE6/IPE6_DGA_",
                           penultimo_ano,
                           "_",
                           ultimo_ano,
                           "_",
                           ultimo_mes,
                           ".csv") , row.names = FALSE)
write.csv(IPE12_DGA, paste0(directorio, 
                           "/BBDD/indicadores/IPE/DGA/IPE12/IPE12_DGA_",
                           penultimo_ano,
                           "_",
                           ultimo_ano,
                           "_",
                           ultimo_mes,
                           ".csv") , row.names = FALSE)
write.csv(IPE6_DMC, paste0(directorio, 
                            "/BBDD/indicadores/IPE/DMC/IPE6/IPE6_DMC_",
                           penultimo_ano,
                            "_",
                            ultimo_ano,
                            "_",
                            ultimo_mes,
                            ".csv") , row.names = FALSE)
write.csv(IPE12_DMC, paste0(directorio, 
                            "/BBDD/indicadores/IPE/DMC/IPE12/IPE12_DMC_",
                            penultimo_ano,
                            "_",
                            ultimo_ano,
                            "_",
                            ultimo_mes,
                            ".csv") , row.names = FALSE)

write.csv(IPE6, paste0(directorio, 
                           "/BBDD/indicadores/IPE/DGA+DMC/IPE6/IPE6_DGA_DMC_",
                           penultimo_ano,
                           "_",
                           ultimo_ano,
                           "_",
                           ultimo_mes,
                           ".csv") , row.names = FALSE)

write.csv(IPE12, paste0(directorio, 
                            "/BBDD/indicadores/IPE/DGA+DMC/IPE12/IPE12_DGA_DMC_",
                            penultimo_ano,
                            "_",
                            ultimo_ano,
                            "_",
                            ultimo_mes,
                            ".csv") , row.names = FALSE)

#6) Resumen ultima fecha del indicador incluyendo metadatos
#Cargamos base de metadatos de las estaciones
metadatos_DGA_DMC<-read.csv(paste0(directorio, "/BBDD/metadatos/DGA+DMC/estaciones_DGA+DMC.csv"))

#INDICADOR IPE6
consolidado<-concatenar(metadatos_DGA_DMC, IPE6, ultima_fecha) %>%
  drop_na(IPE6)
consolidado <-reclass_indicador(consolidado, "IPE6")

write.csv(consolidado, paste0(directorio, 
                              "/BBDD/indicadores/IPE/consolidado/IPE6/IPE_6_",
                              ultimo_ano,
                              "_",
                              ultimo_mes,
                              ".csv") , row.names = FALSE)

#INDICADOR IPE12
  
consolidado<-concatenar(metadatos_DGA_DMC, IPE12, ultima_fecha) %>%
  drop_na(IPE12)

consolidado <-reclass_indicador(consolidado, "IPE12")


write.csv(consolidado, paste0(directorio, 
                        "/BBDD/indicadores/IPE/consolidado/IPE12/IPE_12_",
                        ultimo_ano,
                        "_",
                        ultimo_mes,
                        ".csv") , row.names = FALSE)

#INDICADOR PRECIPITACIÓN
consolidado<-concatenar(metadatos_DGA_DMC, pp, ultima_fecha) %>%
  drop_na(pp)

write.csv(consolidado, paste0(directorio, 
                              "/BBDD/indicadores/pp/pp_",
                              ultimo_ano,
                              "_",
                              ultimo_mes,
                              ".csv") , row.names = FALSE)
