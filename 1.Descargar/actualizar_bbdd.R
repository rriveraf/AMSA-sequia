
actualizar_datos<-function(directorio, variable, fuente){
  
################ MANEJO DE ERRORES #############################
  
# Check if required libraries are installed and load them if not
  required_packages <- c("tidyverse", "readxl", "writexl", "lmomco", "SPEI")
  for (package in required_packages) {
    if (!requireNamespace(package, quietly = TRUE)) {
      stop(paste("The required package '", package, "' is not installed. Please install it before using this function.", sep = ""))
    }
  }
  
library(tidyverse)
library(readxl)
library(writexl)
library(lmomco)
library(SPEI)
    
# Check directorio is a string
  if (!is.character(directorio)) {
    stop("Argumento 'directorio' debe ser string.")
  }
  
# Check variable is valid
  valid_variable <- c("pp", "temp", "caudal")
  if (!(variable %in% valid_variable)) {
    stop("Argumento 'variable' debe ser 'pp', 'temp', o 'caudal'.")
  }
  
# Check fuente is valid (case-insensitive)
  valid_fuente <- c("dga", "dmc")
  if (!(tolower(fuente) %in% valid_fuente)) {
    stop("Argumento 'fuente' debe ser 'DGA' or 'DMC'")
  }
  if(variable=="caudal" & tolower(fuente)=="dmc"){stop("Error: Para 'caudal' usar solo 'DGA'")}
  if(variable=="temp" & tolower(fuente)=="dmc"){stop("Error: Para 'DMC' usar solo 'temp_max' y 'temp_min'")}
  if(variable=="temp_max" & tolower(fuente)=="dga"){stop("Error: Para 'DGA' usar solo 'temp'")}
  if(variable=="temp_min" & tolower(fuente)=="dga"){stop("Error: Para 'DGA' usar solo 'temp'")}

  
######################Cargamos los parametros de entrada########################

#Definimos fechas actuales (ultimo mes es el anterior al actual para asegurar datos mensuales completos)
ultimo_mes<-as.numeric(substr(Sys.time() %m-% months(1),6,7))
penultimo_mes<-ultimo_mes-1
ultimo_ano<-as.numeric(substr(Sys.time() %m-% months(1),1,4))

#Cargamos las funciones para descargar datos  
nombres_funciones<-c("Descargar_pp_DGA.R","Descargar_caudal_DGA.R","Descargar_temp_DGA.R",
                     "Descargar_pp_DMC.R","Descargar_temp_max_DMC.R","Descargar_temp_min_DMC.R")
for(i in nombres_funciones){source(paste0(directorio,"/1.Descargar/",i))}

#Definir estaciones a descargar
metadatos_DGA<-read_excel(paste0(directorio,"/BBDD/metadatos/DGA/estaciones_DGA.xlsx"))
metadatos_DMC<-read_excel(paste0(directorio,"/BBDD/metadatos/DMC/estaciones_DMC.xlsx"))

#Definir fechas a descargar. Cambiar estos parametros para descargar bbdd histórica
fecha_ini<-ultimo_ano
fecha_fin<-ultimo_ano


#############Procedemos a actualizar los datos solicitados######################

#PP DGA
if(variable=="pp" & tolower(fuente)=="dga"){
  #descargar
  pp_DGA_nueva<-descargar_pp_DGA(fecha_ini, fecha_fin, estaciones = metadatos_DGA)
  print("Se descargaron exitosamente todos los datos de precipitacion desde la DGA")
  
  #guardar bbdd nueva
  write.csv(pp_DGA_nueva, paste0(directorio,
                                 "/BBDD/pp/DGA/bruto/pp_DGA_",
                                 fecha_ini,
                                 "_",
                                 fecha_fin,
                                 "_",
                                 ultimo_mes,
                                 ".csv"), row.names = FALSE)
  print("Se escribió la base de datos nueva")
  
  #cargar bbdd antigua
  pp_DGA_antigua<-read.csv(paste0(directorio,
                                  "/BBDD/pp/DGA/bruto/pp_DGA_",
                                  "2022",
                                  "_",
                                  fecha_fin,
                                  "_",
                                  penultimo_mes,
                                  ".csv")) 
  pp_DGA_antigua<- pp_DGA_antigua %>%
    filter(Year<ultimo_ano)
  print("Se realizó la lectura de la base de datos antigua")
  
  pp_DGA_actualizada<-rbind(pp_DGA_antigua, pp_DGA_nueva)
  print("Se concatenaron las bases de datos")
  
  write.csv(pp_DGA_actualizada, paste0(directorio,
                                       "/BBDD/pp/DGA/bruto/pp_DGA_",
                                       "2022",
                                       "_",
                                       fecha_fin,
                                       "_",
                                       ultimo_mes,
                                       ".csv"), row.names = FALSE)
  print("Actualización exitosa")
}



#PP DMC
if(variable=="pp" & tolower(fuente)=="dmc"){
  pp_DMC_nueva<-descargar_pp_DMC(fecha_ini, fecha_fin, estaciones = metadatos_DMC)
  print("Se descargaron exitosamente todos los datos de precipitación desde la DMC")
  write.csv(pp_DMC_nueva, paste0(directorio,
                                 "/BBDD/pp/DMC/bruto/pp_DMC_",
                                 fecha_ini,
                                 "_",
                                 fecha_fin,
                                 "_",
                                 ultimo_mes,
                                 ".csv"), row.names = FALSE)
  print("Se escribió la base de datos nueva")
  
  pp_DMC_antigua<-read.csv(paste0(directorio,
                                  "/BBDD/pp/DMC/bruto/pp_DMC_",
                                  "2020",
                                  "_",
                                  fecha_fin,
                                  "_",
                                  penultimo_mes,
                                  ".csv")) 
  pp_DMC_antigua<- pp_DMC_antigua %>%
    filter(Year<ultimo_ano)
  
  print("Se realizó la lectura de la base de datos antigua")
  pp_DMC_actualizada<-rbind(pp_DMC_antigua, pp_DMC_nueva)
  print("Se concatenaron las bases de datos")
  write.csv(pp_DMC_actualizada, paste0(directorio,
                                 "/BBDD/pp/DMC/bruto/pp_DMC_",
                                 "2020",
                                 "_",
                                 fecha_fin,
                                 "_",
                                 ultimo_mes,
                                 ".csv"), row.names = FALSE)
  print("Actualización exitosa")
}

#TEMP DGA
if(variable=="temp" & tolower(fuente)=="dga"){
  temp_DGA_nueva<-descargar_temp_DGA(fecha_ini, fecha_fin, estaciones = metadatos_DGA)
  
  temp_DGA_nueva_mean<- temp_DGA_nueva %>%
                      select(Year, Month, Day, Codigo_nacional, temp_mean)
  
  temp_DGA_nueva_max<- temp_DGA_nueva %>%
                      select(Year, Month, Day, Codigo_nacional, temp_max)
  
  temp_DGA_nueva_min<- temp_DGA_nueva %>%
                      select(Year, Month, Day, Codigo_nacional, temp_min)
  
  #guardar bbdd nueva
  write.csv(temp_DGA_nueva_mean, paste0(directorio,
                                 "/BBDD/temp/DGA/mean/temp_DGA_mean_",
                                 fecha_ini,
                                 "_",
                                 fecha_fin,
                                 "_",
                                 ultimo_mes,
                                 ".csv"), row.names = FALSE)
  
  write.csv(temp_DGA_nueva_max, paste0(directorio,
                                   "/BBDD/temp/DGA/max/temp_DGA_max_",
                                   fecha_ini,
                                   "_",
                                   fecha_fin,
                                   "_",
                                   ultimo_mes,
                                   ".csv"), row.names = FALSE)
  write.csv(temp_DGA_nueva_min, paste0(directorio,
                                   "/BBDD/temp/DGA/min/temp_DGA_min_",
                                   fecha_ini,
                                   "_",
                                   fecha_fin,
                                   "_",
                                   ultimo_mes,
                                   ".csv"), row.names = FALSE)
  
  print("Se escribió la base de datos nueva")
  
  #cargar bbdd antigua
  #Mean
  temp_DGA_mean_antigua<-read.csv(paste0(directorio,
                                  "/BBDD/temp/DGA/mean/temp_DGA_mean_",
                                  "2022",
                                  "_",
                                  fecha_fin,
                                  "_",
                                  penultimo_mes,
                                  ".csv")) 
  temp_DGA_mean_antigua<- temp_DGA_mean_antigua %>%
    filter(Year<ultimo_ano)
  
  temp_DGA_mean_actualizada<-rbind(temp_DGA_mean_antigua, temp_DGA_nueva_mean)
  
  temp_DGA_mean_actualizada<- temp_DGA_mean_actualizada %>%
    filter(!(Year==ultimo_ano & Month>ultimo_mes))
  
  write.csv(temp_DGA_mean_actualizada, paste0(directorio,
                                              "/BBDD/temp/DGA/mean/temp_DGA_mean_",
                                              "2022",
                                              "_",
                                              fecha_fin,
                                              "_",
                                              ultimo_mes,
                                              ".csv"), row.names = FALSE)
  #Max
  temp_DGA_max_antigua<-read.csv(paste0(directorio,
                                         "/BBDD/temp/DGA/max/temp_DGA_max_",
                                         "2022",
                                         "_",
                                         fecha_fin,
                                         "_",
                                         penultimo_mes,
                                         ".csv")) 
  temp_DGA_max_antigua<- temp_DGA_max_antigua %>%
    filter(Year<ultimo_ano)
  
  temp_DGA_max_actualizada<-rbind(temp_DGA_max_antigua, temp_DGA_nueva_max)
  
  temp_DGA_max_actualizada<- temp_DGA_max_actualizada %>%
    filter(!(Year==ultimo_ano & Month>ultimo_mes))
  
  write.csv(temp_DGA_max_actualizada, paste0(directorio,
                                              "/BBDD/temp/DGA/max/temp_DGA_max_",
                                              "2022",
                                              "_",
                                              fecha_fin,
                                              "_",
                                              ultimo_mes,
                                              ".csv"), row.names = FALSE)
  
  #Min
  temp_DGA_min_antigua<-read.csv(paste0(directorio,
                                         "/BBDD/temp/DGA/min/temp_DGA_min_",
                                         "2022",
                                         "_",
                                         fecha_fin,
                                         "_",
                                         penultimo_mes,
                                         ".csv")) 
  temp_DGA_min_antigua<- temp_DGA_min_antigua %>%
    filter(Year<ultimo_ano)
  
  temp_DGA_min_actualizada<-rbind(temp_DGA_min_antigua, temp_DGA_nueva_min)
  
  temp_DGA_min_actualizada<- temp_DGA_min_actualizada %>%
    filter(!(Year==ultimo_ano & Month>ultimo_mes))
  
  write.csv(temp_DGA_min_actualizada, paste0(directorio,
                                              "/BBDD/temp/DGA/min/temp_DGA_min_",
                                              "2022",
                                              "_",
                                              fecha_fin,
                                              "_",
                                              ultimo_mes,
                                              ".csv"), row.names = FALSE)
  print("Actualización exitosa")
  
}


#TEMP_MAX DMC
if(variable=="temp_max" & tolower(fuente)=="dmc"){
  temp_max_DMC_nueva<-descargar_temp_max_DMC(fecha_ini, fecha_fin, estaciones = metadatos_DMC)
  print("Actualización exitosa")
}

#TEMP_MIN DMC
if(variable=="temp_min" & tolower(fuente)=="dmc"){
  temp_min_DMC_nueva<-descargar_temp_min_DMC(fecha_ini, fecha_fin, estaciones = metadatos_DMC)
  print("Actualización exitosa")
}

#Caudal DGA
if(variable=="caudal" & tolower(fuente)=="dga"){
  q_nueva<-descargar_caudal_DGA(fecha_ini, fecha_fin, estaciones = metadatos_DGA)
  print("Actualización exitosa")
  
  write.csv(q_nueva, paste0(directorio,
                                 "/BBDD/q/DGA/bruto/q_mean_DGA_",
                                 fecha_ini,
                                 "_",
                                 fecha_fin,
                                 "_",
                                 ultimo_mes,
                                 ".csv"), row.names = FALSE)
  print("Se escribió la base de datos nueva")
  
  q_antigua<-read.csv(paste0(directorio,
                                  "/BBDD/q/DGA/bruto/q_",
                                  "2020",
                                  "_",
                                  fecha_fin,
                                  "_",
                                  penultimo_mes,
                                  ".csv")) 
  
  pp_DMC_antigua<- pp_DMC_antigua %>%
    filter(Year<ultimo_ano)
  
  print("Se realizó la lectura de la base de datos antigua")
  pp_DMC_actualizada<-rbind(pp_DMC_antigua, pp_DMC_nueva)
  print("Se concatenaron las bases de datos")
  write.csv(pp_DMC_actualizada, paste0(directorio,
                                       "/BBDD/pp/DMC/bruto/pp_DMC_",
                                       "2020",
                                       "_",
                                       fecha_fin,
                                       "_",
                                       ultimo_mes,
                                       ".csv"), row.names = FALSE)
  print("Actualización exitosa")
  
}



}
