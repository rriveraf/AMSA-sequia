###############################################################################
####################### Codigo Marialina Nunez Grahmann #######################  
###############################################################################

#EL PRESENTE CODIGO CALCULA EL INDICE DE CAUDAL ESTANDARIZADO A PARTIR DEL CAUDAL
#MEDIO MENSUAL. ESTE CORRESPONDE AL CÓDIGO BASE

#Limpiar variables (ctrl+l para limpiar consola)
#rm(list=ls())  
#Definir carpeta de trabajo
#setwd("C:/Users/Mari/Desktop/Mari/Universidad/Magister/Sequia/Ingeniera de Proyectos/Codigo en R/Traspaso")

calcular_SGI<-function(q_medio_mensual,Carac_gamma,acumulacion){
  
  calculo_ice<-function(Q,alpha, beta, pze){
    
    # Q es la media (promedio) de los ultimos "n" meses 
    # del caudal medio mensual
    # Coeficientes alpha y beta de la distributci?n gamma
    # obtenidos del periodo de referencia
    # pze es la probabilidad de 0 para aplicar la gamma incompleta
    if (is.na(Q) || is.na(alpha) || is.na(beta) || is.na(pze)){
      ind_IPE <- NA
    }
    else if (Q<0){
      ind_IPE <- NA
    }
    else if (Q==0){
      #Cuando no existe pze y el acumulado es 0
      Q<-0.0001 #Valor muy pequeno
      ind_IPE<-qnorm(pgamma(Q,shape = alpha, rate = 1/beta))
      ind_IPE<-qnorm(pze+(1-pze)*pnorm(ind_IPE))}
    else if (Q>0){
      ind_IPE<-qnorm(pgamma(Q,shape = alpha, rate = 1/beta))
      ind_IPE<-qnorm(pze+(1-pze)*pnorm(ind_IPE))}
    #Caso de que de -Inf
    if (is.infinite(ind_IPE) && (ind_IPE < 0)){
      ind_IPE<- -3}
    #Caso de que de Inf
    if (is.infinite(ind_IPE)){
      ind_IPE<- 3}
    
    return(ind_IPE)}
  
  #eliminamos la columna con las fechas y la transformamos a nombres de las filas
  
  rownames(q_medio_mensual)<-q_medio_mensual[,1]
  q_medio_mensual <- q_medio_mensual[,-1] #Convertimos primera columna en nombres de filas
  
  #ICE 6 meses
  if(acumulacion ==6){
    q_6meses<-q_medio_mensual[13:nrow(q_medio_mensual),]#usamos mismo formato para las fechas y las estaciones, empezando desde el año 1991.
    q_6meses[,] <- NA #generamos valores de NA en donde pondremos los promedios acumulados
    
    #Promedio 6 meses
    for (col in c(1:ncol(q_6meses))){ 
      for (fila in 1:nrow(q_6meses)){
        datos_a_promediar <- q_medio_mensual[(fila+7):(fila+12),col] #creamos subconjunto con los 5 meses anteriores a la fecha a analizar + el mes actual
        datos_no_na <- sum(complete.cases(datos_a_promediar)) #contamos la cantidad de datos no NA en el subconjunto
        if (datos_no_na>=5){ #usamos un 82% de filtro (deben haber al menos 5 meses para obtener el promedio), cambiar por 6 si se desea datos en todos los meses
          a_promediar <- na.omit(datos_a_promediar)
        }
        else {
          a_promediar <- datos_a_promediar #en este caso al haber NA, el promedio queda como NA
        }
        q_6meses[fila,col] <- mean(datos_a_promediar)
      }
    }
    
    ICE6_MATRIZ <- q_6meses 
    ICE6_MATRIZ[,] <- NA
    
    #Coeficientes 6 meses
    Gamma_6meses <-subset(Carac_gamma,Acum==6)
    
    contador_estacion_ice6<-1
    for (i in colnames(ICE6_MATRIZ[,])){
      contador_estacion_gamma<-3
      for (j in colnames(Gamma_6meses[,3:ncol(Gamma_6meses)])){
        if(i==j){
          contador_fecha<-1
          for (fecha_dato in rownames(ICE6_MATRIZ)){
            k<-as.integer(substr(fecha_dato, 1, 2)) #lo queremos en formato número para llamarlo como posición
            Q<-q_6meses[contador_fecha,contador_estacion_ice6]
            ICE6_MATRIZ[contador_fecha,contador_estacion_ice6]<-calculo_ice(Q,Gamma_6meses[k,contador_estacion_gamma],Gamma_6meses[k+12,contador_estacion_gamma],Gamma_6meses[k+24,contador_estacion_gamma])
            contador_fecha<-contador_fecha+1
          }
        }
        contador_estacion_gamma<-contador_estacion_gamma+1
      }
      contador_estacion_ice6<-contador_estacion_ice6+1
    }
    
    #write.csv(ICE6_MATRIZ, file = "ICE_6_1991_2023.csv")
    return(ICE6_MATRIZ)
    
  }
  
  #ICE 12 meses
  if(acumulacion ==12){
    q_12meses<-q_medio_mensual[13:nrow(q_medio_mensual),] #usamos mismo formato para las fechas y las estaciones, empezando desde el año 1991.
    q_12meses[,] <- NA  #generamos valores de NA en donde pondremos los promedios acumulados
    
    
    ##Promedio 12 meses
    for (col in c(1:ncol(q_12meses))){ 
      for (fila in 1:nrow(q_12meses)){
        datos_a_promediar <- q_medio_mensual[(fila+1):(fila+12),col] #creamos subconjunto con los 11 meses anteriores a la fecha a analizar + el mes actual
        datos_no_na <- sum(complete.cases(datos_a_promediar)) #contamos la cantidad de datos no NA en el subconjunto
        if (datos_no_na>=10){ #usamos un 82% de filtro (deben haber al menos 5 meses para obtener el promedio), cambiar por 12 si se desea datos en todos los meses
          a_promediar <- na.omit(datos_a_promediar)
        }
        else {
          a_promediar <- datos_a_promediar
        }
        q_12meses[fila,col] <- mean(datos_a_promediar)
      }
    }
    
    ICE12_MATRIZ <- q_12meses 
    ICE12_MATRIZ[,] <- NA
    
    #Coeficientes 12 meses
    Gamma_12meses <-subset(Carac_gamma,Acum==12)
    
    contador_estacion_ice12<-1
    for (i in colnames(ICE12_MATRIZ[,])){
      contador_estacion_gamma<-3
      for (j in colnames(Gamma_12meses[,3:ncol(Gamma_12meses)])){
        if(i==j){
          contador_fecha<-1
          for (fecha_dato in rownames(ICE12_MATRIZ)){
            k<-as.integer(substr(fecha_dato, 1, 2))
            Q<-q_12meses[contador_fecha,contador_estacion_ice12]
            ICE12_MATRIZ[contador_fecha,contador_estacion_ice12]<-calculo_ice(Q,Gamma_12meses[k,contador_estacion_gamma],Gamma_12meses[k+12,contador_estacion_gamma],Gamma_12meses[k+24,contador_estacion_gamma])
            contador_fecha<-contador_fecha+1
          }
        }
        contador_estacion_gamma<-contador_estacion_gamma+1
      }
      contador_estacion_ice12<-contador_estacion_ice12+1
    }
    
    return(ICE12_MATRIZ)
  }
  
  
  
  
  ################################################################################
  #######################Fin funcion CaLCULO INDICADOR ICE########################
  ################################################################################
}
