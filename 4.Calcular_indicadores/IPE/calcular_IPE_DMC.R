###############################################################################
####################### Codigo Marialina Nunez Grahmann #######################  
###############################################################################

#EL PRESENTE CODIGO CALCULA EL INDICE DE PRECIPITACIÓN ESTANDARIZADA A PARTIR DE
#LA PRECIPITACIÓN ACUMULADA MENSUAL. ESTE CORRESPONDE AL CÓDIGO BASE

#Limpiar variables (ctrl+l para limpiar consola)
rm(list=ls())  

#Definir carpeta de trabajo
setwd("C:/Users/56984/Documents/CCGUC/WebScraping/4.Calcular_indicadores/IPE")


################################################################################
###NECESITAMOS OBTENER LA SUMA MENSUAL DE PRECIPITACIÓN: CARGAR Y GENERAR BBDD##
################################################################################

##CARGAMOS CÓDIGO QUE TOMA PP DIARIAS MENSUALES Y LAS CONVIERTE EN TOTAL MENSUAL

#filtro_datos_mensuales <- 0.3 #ACÁ DEBEMOS PONER EL FILTRO QUE NOS INTERESA EN LOS DATOS DE PRECIPITACIÓN MENSUAL (1-FILTRO)=% DE DATOS NECESARIOS
#source(paste0(getwd(),"/pp acumulada mensual","/","Codigo_calculo_total_mensual.R")) #ACÁ LLAMAMOS AL CÓDIGO QUE CALCULA LA PP MENSUAL A TRAVÉS DE LA PRECIPITACIÓN TOTAL

#SI YA TENEMOS EL TOTAL MENSUAL, CARGAMOS EL ARCHIVO, DE LO CONTRARIO DESCOMENTAMOS 
#LA LINEA ANTERIOR Y COMENTAMOS LO SIGUIENTE

#CARGAMOS EL ARCHIVO DE PRECIPITACIÓN:
pp_acumulada_mensual<-read.csv(paste0(getwd(),"/","pp_DMC_1989_2023.csv")) #USA FILTRO 70% EN PP MENSUAL
pp_acumulada_mensual<-pp_acumulada_mensual[,-1]
#### SI LAS FECHAS NO SE ENCUENTRAN ESCRITAS DE TIPO MES/AÑO CON NUMEROS, CORRER:
#source(paste0(getwd(),"/","Cambio_formato_fecha.R"))  ##para cambiar formato fecha
#nuevo_formato<-Cambio_formato_fecha(pp_acumulada_mensual[,1])
#pp_acumulada_mensual[,1]<-nuevo_formato
SO
#si estan en formato correcto, seguir:

#eliminamos la columna con las fechas y la transformamos a nombres de las filas
rownames(pp_acumulada_mensual)<-pp_acumulada_mensual[,1]
pp_acumulada_mensual <- subset(pp_acumulada_mensual, select = -1) #Convertimos primera columna en nombres de filas

#Vamos a escribir bien el nombre de estación: eliminar X inicial y cambiar . por -

#creamos vector vacío para rellenar con datos correctos de código de estación 
#Cambiamos el código de las estaciones al sistema correcto (eliminar la X que
#antepone los datos y cambiamos el punto (.) por un guión (-)
nombres_corregidos<-c() 
contador<-1
for (i in colnames(pp_acumulada_mensual)){
  codigo_provisorio_est<-gsub("\\.", "-", i)
  nombres_corregidos[contador]<-gsub("X", "", codigo_provisorio_est)
  contador=contador+1
}
names(pp_acumulada_mensual)<-nombres_corregidos


################################################################################
########################Cambiamos formato nombre estación#######################
################################################################################

#AHORA VAMOS A DESCARGAR LA BBDD de metadatos de estaciones DGA
metadatos<-read.csv(paste0(getwd(),"/","Metadatos_estaciones_pp_DGA.csv"))

#Además, vamos a hacer que los datos esten escritos de manera más cómoda e 
#universal y no con carácteres especiales (El csv Simil estaciones incluye la conversión)
metadatos_a_parms_gamma<-read.csv(paste0(getwd(),"/","Simil estaciones.csv"))

#Cambiamos el nombre en metadatos para que después se utilice siempre el mismo nombre
nombre_met_a_formato_gamma<-c()

contador<-1
for (j in metadatos[,2]){
  contador_nombre<-1
  for (i in metadatos_a_parms_gamma[,1]){
    if (i==j){
      nombre_met_a_formato_gamma[contador]<-metadatos_a_parms_gamma[contador_nombre,3]
    }
    contador_nombre<-contador_nombre+1
  }
  contador<-contador+1
}

metadatos[,3]<-nombre_met_a_formato_gamma #ahora ponemos los nombres en sistema menos "complicado"


#creamos listas vacias para almacenar las características de las estaciones
contador_nombres<-1
lista_nombres<-c()
lista_codigo<-c()
lista_LAT<-c()
lista_LON<-c()
lista_ALTURA<-c()
lista_REGION<-c()
lista_COMUNA<-c()
lista_CUENCA<-c()
lista_SUBCUENCA<-c()

# vamos a recorrer las estaciones de pp_acumulada_mensual y vamos a caracterizarlas
for (i in colnames(pp_acumulada_mensual[,1:length(pp_acumulada_mensual)])){
  contador_metadatos<-1
  for (j in metadatos[,2]){
    if (i==j){
      lista_codigo[contador_nombres]<-metadatos[contador_metadatos,2]
      lista_nombres[contador_nombres]<-metadatos[contador_metadatos,3]
      lista_LAT[contador_nombres]<-metadatos[contador_metadatos,4]
      lista_LON[contador_nombres]<-metadatos[contador_metadatos,5]
      lista_ALTURA[contador_nombres]<-metadatos[contador_metadatos,8]
      lista_REGION[contador_nombres]<-metadatos[contador_metadatos,15]
      lista_COMUNA[contador_nombres]<-metadatos[contador_metadatos,17]
      lista_CUENCA[contador_nombres]<-metadatos[contador_metadatos,18]
      lista_SUBCUENCA[contador_nombres]<-metadatos[contador_metadatos,19]
    }
    contador_metadatos<-contador_metadatos+1
  }
  contador_nombres<-contador_nombres+1
}


################################################################################
#####################Cálculo de precipitaciones acumuladas######################
################################################################################

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

#Cargamos los parámetros de la función gamma

#Para esto necesitamos exportar las características de la función Gamma para cada estación
Carac_gamma<-read.csv(paste0(getwd(),"/","Coeficientes_IPE_1991_2020_cod_est.csv"))

##en caso que el código se lea de forma XCODIGO.DIGITO en vez de CODIGO-DIGITO, lo arreglamos
nombres_corregidos<-c() #creamos vector vacío para rellenar con datos correctos de código de estación (eliminar X inicial y cambiar . por -)
contador<-1
for (i in colnames(Carac_gamma[,3:ncol(Carac_gamma)])){
  codigo_provisorio_est<-gsub("\\.", "-", i)
  nombres_corregidos[contador]<-gsub("X", "", codigo_provisorio_est)
  contador=contador+1
}

#una vez corregidos los códigos, es necesario que estos se cambien en el Data Frame
nuevas_columnas_inicio<-colnames(Carac_gamma[,1:2])
nuevas_columnas<-c(nuevas_columnas_inicio,nombres_corregidos)

names(Carac_gamma)<-nuevas_columnas #definimos nuevas columnas

################################################################################
#############################CÁLCULO INDICADOR IPE##############################
################################################################################

source(paste0(getwd(),"/","calculo_IPE_correccion.R")) #CARGAMOS LA FUNCIÓN

####Una vez cargados los parámetros de la función Gamma que caracteriza cada registro, es posible, 
####utilizando la función Cálculo IPE, obtener los valores para la acumulación deseada

################################################################################
####################################IPE 6#######################################
################################################################################

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

write.csv(IPE6_MATRIZ, file = "IPE_6_1991_2023.csv")

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

write.csv(IPE12_MATRIZ, file = "IPE_12_1991_2023.csv")

##########VAMOS A ESCRIBIR LAS CARACTERÍSTICAS DE LAS ESTACIONES##########
prueba <- data.frame(matrix(nrow = 8, ncol = ncol(IPE12_MATRIZ)))
prueba[1,] <- lista_nombres
prueba[2,] <- lista_LAT
prueba[3,] <- lista_LON
prueba[4,] <- lista_ALTURA
prueba[5,] <- lista_REGION
prueba[6,] <- lista_COMUNA
prueba[7,] <- lista_CUENCA
prueba[8,] <- lista_SUBCUENCA
nombres_filas <- c("Nombre estacion","Latitud","Longitud","Altira","Region","Comuna","Cuenca","Subcuenca")
colnames(prueba) <- colnames(IPE12_MATRIZ)
rownames(prueba) <- nombres_filas

write.csv(prueba, file = "IPE_caracteristicas.csv")
