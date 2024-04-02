#Algoritmo para calcular IPEE
#Archivos INPUT: temp_min, temp_max, precipitación, metadatos estaciones y coeficientes estaciones.

#0)Parametros iniciales

  #Librerías
library(tidyverse)
library(readxl)
library(writexl)
library(lmomco)
library(SPEI)
library(ggplot2)
library(reshape2)
library(corrplot)

  #Directorio de trabajo
directorio<-getwd()

  #Se cargan funciones de utilidad
source(paste0(directorio, "/1.Descargar/actualizar_bbdd.R"))
source(paste0(directorio,"/4.Calcular_indicadores/IPEE/Calculo_ET0.R"))
source(paste0(directorio,"/2.Promediar_formatear/promedio_mensual.R"))
source(paste0(directorio,"/4.Calcular_indicadores/IPEE/Codigo_base.R"))


  #Definir fechas importantes
ultimo_mes<-as.numeric(substr(Sys.time() %m-% months(1),6,7))
ultimo_ano<-as.numeric(substr(Sys.time() %m-% months(1),1,4))
penultimo_ano<-(as.numeric(substr(Sys.time() %m-% months(1),1,4))-1)
ultima_fecha<-paste0(substr(Sys.time() %m-% months(1),6,7),"/",substr(Sys.time() %m-% months(1),1,4))


#1) Actualizar bases de datos climatologicas. 
#No es necesario correr si ya está actualizada al "ultimo_mes".
  #actualizar_datos(directorio , 
   #                variable = "temp", 
   #                fuente = "dga" )
  
#2) Cargar data de entrada
  
#A)Temp_min
    #Desde archivo
  temp_min_DGA_2022_2023<-read.csv(paste0(directorio,
                                        "/BBDD/temp/DGA/min/temp_DGA_min_",
                                        "2022",
                                        "_",
                                        ultimo_ano,
                                        "_",
                                        ultimo_mes,
                                        ".csv")) 

  #Formateamos
temp_min_DGA_2022_2023<-promedio_mensual(temp_min_DGA_2022_2023)
temp_min_DGA_2022_2023<-formatear_wider(temp_min_DGA_2022_2023)
temp_min_DGA_2022_2023<-formatear_fecha(temp_min_DGA_2022_2023)

#B)Temp_max
    #Desde archivo
temp_max_DGA_2022_2023<-read.csv(paste0(directorio,
                                        "/BBDD/temp/DGA/max/temp_DGA_max_",
                                        "2022",
                                        "_",
                                        ultimo_ano,
                                        "_",
                                        ultimo_mes,
                                        ".csv"))  

    #Formateamos
temp_max_DGA_2022_2023<-promedio_mensual(temp_max_DGA_2022_2023)
temp_max_DGA_2022_2023<-formatear_wider(temp_max_DGA_2022_2023)
temp_max_DGA_2022_2023<-formatear_fecha(temp_max_DGA_2022_2023)


#C)Precipitación
    #Desde archivo
pp_acum_mensual<-read.csv( paste0(directorio,
                                  "/BBDD/pp/DGA/bruto/pp_DGA_",
                                  "2022",
                                  "_",
                                  ultimo_ano,
                                  "_",
                                  ultimo_mes,
                                  ".csv"))

    #Formateamos
pp_acum_mensual<- acumulado_mensual(pp_acum_mensual)
pp_acum_mensual<- formatear_wider(pp_acum_mensual)
pp_acum_mensual<- formatear_fecha(pp_acum_mensual)



#3) Cargamos Metadata Estaciones 
pp_caract<-read.csv(paste0(directorio,"/BBDD/metadatos/DGA/Caracteristicas_estaciones.csv"))
temporal<-pp_caract[,1]
pp_caract<-Corregir_Codigo_nacional(pp_caract[,-1])
rownames(pp_caract)<-temporal

#4) Calcular evapotranspiración
BH<-calcular_evapotranspriacion(pp_acum_mensual, 
                                pp_caract, 
                                temp_max_DGA_2022_2023, 
                                temp_min_DGA_2022_2023) 

write.csv(BH, 
          paste0(directorio,
                 "/BBDD/BH/DGA/Balance_hidrico_DGA_",
                 penultimo_ano,
                 "_",
                 ultimo_ano,
                 ".csv"), 
          row.names=FALSE)


#5) Cargar coeficientes de las estaciones (ajustada log-logis)
Carac_loglogis<-read.csv(paste0(directorio,"/BBDD/coeficientes/DGA/IPEE/Coeficientes_IPEE_1991_2020_v1.csv"))
Carac_loglogis<-Corregir_Codigo_nacional(Carac_loglogis)

#6) Calculamos el IPEE
IPEE_6<-calcular_IPEE(bh_mensual = BH, 
                      Carac_loglogis = Carac_loglogis , 
                      acumulacion = 6)

IPEE_12<-calcular_IPEE(bh_mensual = BH, 
                      Carac_loglogis = Carac_loglogis , 
                      acumulacion = 12)

#7) OUTPUT
write.csv(IPEE_6, 
          paste0(directorio,"/BBDD/indicadores/IPEE/DGA/IPEE6/IPEE_6_",
          ultimo_ano,
          "_",
          ultimo_mes,
          ".csv"))


write.csv(IPEE_12, 
          paste0(directorio,"/BBDD/indicadores/IPEE/DGA/IPEE12/IPEE_12_",
          ultimo_ano,
          "_",
          ultimo_mes,
          ".csv"))















######### ANEXO: Análisis de resultados estadístico (ipee vs precipitación)##############
  #Igualamos columnas para poder generar matriz de correlación
#common_cols <- intersect(names(IPEE), names(pp_acum_mensual) )
#pp_acum_mensual_filtered <- pp_acum_mensual %>%
#    select(all_of(common_cols)) 
#
  #Calculamos la correlación promedio entre precipitación e IPEE
#cor_matrix<-mean(na.omit(diag(cor(pp_acum_mensual_filtered, IPEE))))

#corrplot(cor_matrix, method = "color", type = "upper")  # Add rectangles for readability