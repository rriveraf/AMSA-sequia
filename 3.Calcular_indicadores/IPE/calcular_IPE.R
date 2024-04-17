###############################################################################
####################### Codigo Marialina Nunez Grahmann #######################  
###############################################################################

#EL PRESENTE CODIGO CALCULA EL INDICE DE PRECIPITACIÓN ESTANDARIZADA A PARTIR DE
#LA PRECIPITACIÓN ACUMULADA MENSUAL. ESTE CORRESPONDE AL CÓDIGO BASE

#Limpiar variables (ctrl+l para limpiar consola)
#rm(list=ls())  

calcular_ipe<-function(pp_acumulada_mensual, acumulacion, fuente, directorio){

  ################################################################################
  #############################CÁLCULO INDICADOR IPE##############################
  ################################################################################
  
  #Check fuente is valid (case-insensitive)
  valid_acumulacion <- c(3, 6, 12)
  if (!(as.numeric(acumulacion) %in% valid_acumulacion)) {
    stop("Argumento 'acumulacion' debe ser '3', 6' o '12'")
  }
  
  #Check fuente is valid (case-insensitive)
  valid_fuente <- c("dga", "dmc")
  if (!(tolower(fuente) %in% valid_fuente)) {
    stop("Argumento 'fuente' debe ser 'DGA' o 'DMC'")
  }
  
  # Check directorio is a string
  if (!is.character(directorio)) {
    stop("Argumento 'directorio' debe ser string.")
  }
  #source(paste0(getwd(),"/","calculo_IPE_correccion.R")) #CARGAMOS LA FUNCIÓN
  
  ####Una vez cargados los parámetros de la función Gamma que caracteriza cada registro, es posible, 
  ####utilizando la función Cálculo IPE, obtener los valores para la acumulación deseada
  
  ################################################################################
  ####################################IPE#######################################
  ################################################################################
  ###############################################################################
  ################# Codigo Juan de Dios: Modificacion Marialina ################# 
  ###############################################################################
  
  calculo_ipe<-function(P_k,alpha, beta, pze){
    
    # P_k es el acumulado (promedio) de los ?ltimos "n" meses 
    # de la precipitaci?n acumulada mensual
    # Coeficientes alpha y beta de la distributci?n gamma
    # obtenidos del periodo de referencia
    # pze es la probabilidad de 0 para aplicar la gamma incompleta
    if (is.na(P_k) || is.na(alpha) || is.na(beta) || is.na(pze)){
      ind_IPE <- NA
    }
    else if (P_k<0){
      ind_IPE <- NA
    }
    else if (P_k==0){
      #Cuando no existe pze y el acumulado es 0
      P_k<-0.0001 #Valor muy pequeno
      ind_IPE<-qnorm(pgamma(P_k,shape = alpha, rate = 1/beta))
      ind_IPE<-qnorm(pze+(1-pze)*pnorm(ind_IPE))}
    else if (P_k>0){
      ind_IPE<-qnorm(pgamma(P_k,shape = alpha, rate = 1/beta))
      ind_IPE<-qnorm(pze+(1-pze)*pnorm(ind_IPE))}
    #Caso de que de -Inf
    if (is.infinite(ind_IPE) && (ind_IPE < 0)){
      ind_IPE<- -3}
    #Caso de que de Inf
    if (is.infinite(ind_IPE)){
      ind_IPE<- 3}
    
    return(ind_IPE)}
  
#Cargamos coeficientes de las estaciones IPE-DGA
  source(paste0(directorio, "/2.Promediar_formatear/promedio_mensual.R"))
  if(tolower(fuente)=="dga"){
  Carac_gamma<- read.csv(paste0(directorio,"/BBDD/coeficientes/DGA/IPE/Coeficientes_IPE_1991_2020_DGA.csv"))
  Carac_gamma<-Corregir_Codigo_nacional(Carac_gamma)
  }
  if(tolower(fuente)=="dmc"){
    Carac_gamma<- read.csv(paste0(directorio,"/BBDD/coeficientes/DMC/IPE/Coeficientes_IPE_1991_2020_DMC.csv"))
    Carac_gamma<-Corregir_Codigo_nacional(Carac_gamma)
  }

#eliminamos la columna con las fechas y la transformamos a nombres de las filas
rownames(pp_acumulada_mensual)<-pp_acumulada_mensual[,1]
pp_acumulada_mensual <- subset(pp_acumulada_mensual, select = -1) #Convertimos primera columna en nombres de filas

################################################################################
#####################Cálculo de precipitaciones acumuladas######################
################################################################################

##Acumulacion == 3

if(acumulacion==3){
  #ahora hacemos un simil para la acumulación de 12 meses (promedio 12 meses de precipitación considerando el mes de cálculo)
  pp_3meses<-pp_acumulada_mensual[13:nrow(pp_acumulada_mensual),] #usamos mismo formato para las fechas y las estaciones, empezando desde el año 1991.
  pp_3meses[,] <- NA  #generamos valores de NA en donde pondremos los promedios acumulados
  
  #ahora obtenemos los valores acumulados del promedio de precipitación para luego calcular IPE12
  for (col in c(1:ncol(pp_3meses))){ 
    for (fila in 1:nrow(pp_3meses)){
      datos_a_promediar <- pp_acumulada_mensual[(fila+10):(fila+12),col] #creamos subconjunto con los 11 meses anteriores a la fecha a analizar + el mes actual
      datos_no_na <- sum(complete.cases(datos_a_promediar)) #contamos la cantidad de datos no NA en el subconjunto
      if (datos_no_na>=2){ #usamos un 82% de filtro (deben haber al menos 10 meses para obtener el promedio), cambiar por 12 si se desea datos en todos los meses
        a_promediar <- na.omit(datos_a_promediar)
      }
      else {
        a_promediar <- datos_a_promediar #en este caso al haber NA, el promedio queda como NA
      }
      pp_3meses[fila,col] <- mean(datos_a_promediar)
    }
  }
  ################################################################################
  ####################################IPE 12######################################
  ################################################################################
  
  IPE3_MATRIZ <- pp_3meses #[1:12,] #para prueba
  IPE3_MATRIZ[,] <- NA
  
  #Limpiamos los parámetros de la función gamma por la que acumulación que nos sirve.
  Gamma_3meses <-subset(Carac_gamma,Acum==3)
  
  contador_estacion_ipe3<-1
  for (i in colnames(IPE3_MATRIZ[,])){
    contador_estacion_gamma<-3
    for (j in colnames(Gamma_3meses[,3:ncol(Gamma_3meses)])){
      if(i==j){
       #si es que coincide la estación en IPE12 y parámetros de Gamma entra
        contador_fecha<-1
        for (fecha_dato in rownames(IPE3_MATRIZ)){
          k<-as.integer(substr(fecha_dato, 1, 2))
          PP<-pp_3meses[contador_fecha,contador_estacion_ipe3]
          #print(paste0("en estacion ",i ," en fecha ", fecha_dato, " la pp es ", PP))
          IPE3_MATRIZ[contador_fecha,contador_estacion_ipe3]<-calculo_ipe(PP,Gamma_3meses[k,contador_estacion_gamma],Gamma_3meses[k+12,contador_estacion_gamma],Gamma_3meses[k+24,contador_estacion_gamma])
          #if(fecha_dato =="02/1992") {print(paste0("el valor k es ", k , " estacion es N° ", contador_estacion_gamma," el valor calculado es ", IPE3_MATRIZ[contador_fecha,contador_estacion_ipe3]))}
          contador_fecha<-contador_fecha+1
        }
      }
      contador_estacion_gamma<-contador_estacion_gamma+1
    }
    contador_estacion_ipe3<-contador_estacion_ipe3+1
  }
  return (IPE3_MATRIZ)
}

if(acumulacion == 6){
#ahora hacemos agregación de precipitación para luego obtener IPE6
pp_6meses<-pp_acumulada_mensual[13:nrow(pp_acumulada_mensual),] #usamos mismo formato para las fechas y las estaciones, empezando desde el año 1991.
pp_6meses[,] <- NA #generamos valores de NA en donde pondremos los promedios acumulados más adelante

#ahora obtenemos los valores acumulados del promedio de  precipitación de 6 meses (promedio 6 meses de precipitación considerando el mes de cálculo)
for (col in c(1:ncol(pp_6meses))){ 
  for (fila in 1:nrow(pp_6meses)){
    datos_a_promediar <- pp_acumulada_mensual[(fila+7):(fila+12),col] #creamos subconjunto con los 5 meses anteriores a la fecha a analizar + el mes actual
    datos_no_na <- sum(complete.cases(datos_a_promediar)) #contamos la cantidad de datos no NA en el subconjunto
    if (datos_no_na>=5){ #usamos un 82% de filtro (deben haber al menos 5 meses con datos para obtener el promedio), cambiar por 6 si se desea datos en todos los meses
      a_promediar <- na.omit(datos_a_promediar)
    }
    else {
      a_promediar <- datos_a_promediar #en este caso al haber NA, el promedio queda como NA
    }
    pp_6meses[fila,col] <- mean(datos_a_promediar)
  }
}
IPE6_MATRIZ <- pp_6meses #[,1:5] para prueba
IPE6_MATRIZ[,] <- NA

#Limpiamos los parámetros de la función gamma por la que acumulación que nos sirve.
Gamma_6meses <-subset(Carac_gamma,Acum==6)

contador_estacion_ipe6<-1
for (i in colnames(IPE6_MATRIZ[,])){
  contador_estacion_gamma<-3
  for (j in colnames(Gamma_6meses[,3:ncol(Gamma_6meses)])){
    if(i==j){ #si es que coincide la estación en IPE6 y parámetros de Gamma entra
      contador_fecha<-1
      for (fecha_dato in rownames(IPE6_MATRIZ)){
        k<-as.integer(substr(fecha_dato, 1, 2)) #lo queremos en formato número para llamarlo como posición
        PP<-pp_6meses[contador_fecha,contador_estacion_ipe6]
        IPE6_MATRIZ[contador_fecha,contador_estacion_ipe6]<-calculo_ipe(PP,Gamma_6meses[k,contador_estacion_gamma],Gamma_6meses[k+12,contador_estacion_gamma],Gamma_6meses[k+24,contador_estacion_gamma])
        contador_fecha<-contador_fecha+1
      }
    }
    contador_estacion_gamma<-contador_estacion_gamma+1
  }
  contador_estacion_ipe6<-contador_estacion_ipe6+1
}

return (IPE6_MATRIZ)
}

if(acumulacion==12){
#ahora hacemos un simil para la acumulación de 12 meses (promedio 12 meses de precipitación considerando el mes de cálculo)
pp_12meses<-pp_acumulada_mensual[13:nrow(pp_acumulada_mensual),] #usamos mismo formato para las fechas y las estaciones, empezando desde el año 1991.
pp_12meses[,] <- NA  #generamos valores de NA en donde pondremos los promedios acumulados

#ahora obtenemos los valores acumulados del promedio de precipitación para luego calcular IPE12
for (col in c(1:ncol(pp_12meses))){ 
  for (fila in 1:nrow(pp_12meses)){
    datos_a_promediar <- pp_acumulada_mensual[(fila+1):(fila+12),col] #creamos subconjunto con los 11 meses anteriores a la fecha a analizar + el mes actual
    datos_no_na <- sum(complete.cases(datos_a_promediar)) #contamos la cantidad de datos no NA en el subconjunto
    if (datos_no_na>=10){ #usamos un 82% de filtro (deben haber al menos 10 meses para obtener el promedio), cambiar por 12 si se desea datos en todos los meses
      a_promediar <- na.omit(datos_a_promediar)
    }
    else {
      a_promediar <- datos_a_promediar #en este caso al haber NA, el promedio queda como NA
    }
    pp_12meses[fila,col] <- mean(datos_a_promediar)
  }
}
################################################################################
####################################IPE 12######################################
################################################################################

IPE12_MATRIZ <- pp_12meses #[,1:5] para prueba
IPE12_MATRIZ[,] <- NA

#Limpiamos los parámetros de la función gamma por la que acumulación que nos sirve.
Gamma_12meses <-subset(Carac_gamma,Acum==12)

contador_estacion_ipe12<-1
for (i in colnames(IPE12_MATRIZ[,])){
  contador_estacion_gamma<-3
  for (j in colnames(Gamma_12meses[,3:ncol(Gamma_12meses)])){
    if(i==j){ #si es que coincide la estación en IPE12 y parámetros de Gamma entra
      contador_fecha<-1
      for (fecha_dato in rownames(IPE12_MATRIZ)){
        k<-as.integer(substr(fecha_dato, 1, 2))
        PP<-pp_12meses[contador_fecha,contador_estacion_ipe12]
        IPE12_MATRIZ[contador_fecha,contador_estacion_ipe12]<-calculo_ipe(PP,Gamma_12meses[k,contador_estacion_gamma],Gamma_12meses[k+12,contador_estacion_gamma],Gamma_12meses[k+24,contador_estacion_gamma])
        contador_fecha<-contador_fecha+1
      }
    }
    contador_estacion_gamma<-contador_estacion_gamma+1
  }
  contador_estacion_ipe12<-contador_estacion_ipe12+1
}
return (IPE12_MATRIZ)
}

}



##########Output##########
#write.csv(IPE12_MATRIZ, file = "IPE_12_1991_2023.csv")
#write.csv(IPE6_MATRIZ, file = "IPE_6_1991_2023.csv")
##########VAMOS A ESCRIBIR LAS CARACTERÍSTICAS DE LAS ESTACIONES##########
#prueba <- data.frame(matrix(nrow = 8, ncol = ncol(IPE12_MATRIZ)))
#prueba[1,] <- lista_nombres
#prueba[2,] <- lista_LAT
#prueba[3,] <- lista_LON
#prueba[4,] <- lista_ALTURA
#prueba[5,] <- lista_REGION
#prueba[6,] <- lista_COMUNA
#prueba[7,] <- lista_CUENCA
#prueba[8,] <- lista_SUBCUENCA
#nombres_filas <- c("Nombre estacion","Latitud","Longitud","Altira","Region","Comuna","Cuenca","Subcuenca")
#colnames(prueba) <- colnames(IPE12_MATRIZ)
#rownames(prueba) <- nombres_filas

#write.csv(prueba, file = "IPE_caracteristicas.csv")
