###############################################################################
####################### Codigo Marialina Nunez Grahmann #######################  
###############################################################################

###EL PRESENTE CODIGO CALCULA LA EVAPOTRANSPIRACIÓN Y EL BALANCE HÍDRICO PARA LUEGO 
#OBTENER EL IPEE 

#Limpiar variables (ctrl+l para limpiar consola)

#las series de tiempo entregadas deben ser iguales en columna "Fecha"

calcular_evapotranspriacion<- function(pp_acum_mensual, pp_caract, Tmax_tot, Tmin_tot ){
  
#Cargamos librería
library(SPEI)
library(tidyverse)

  Tmax_tot<-as.data.frame(Tmax_tot)
  Tmin_tot<-as.data.frame(Tmin_tot)
  pp_acum_mensual<-as.data.frame(pp_acum_mensual)
  pp_caract<-as.data.frame(pp_caract)

#Sacamos la col de fecha y la ponemos como nombre de filas
rownames(Tmax_tot)<-Tmax_tot[,1]
rownames(Tmin_tot)<-Tmin_tot[,1]
rownames(pp_acum_mensual)<-pp_acum_mensual[,1]

Tmax_tot<-Tmax_tot[,-1]
Tmin_tot<-Tmin_tot[,-1]
pp_acum_mensual<-pp_acum_mensual[,-1]


###### Vamos a usar solo aquellas estaciones de PP que tengan Temperatura y PP
#para esto generamos un data frame con las mismas características que los de T°
pp_tienen_T<-data.frame(matrix(nrow = nrow(Tmax_tot), ncol = ncol(Tmax_tot)))
if(length(rownames(pp_tienen_T))==length(rownames(pp_acum_mensual))){
   rownames(pp_tienen_T)<-rownames(pp_acum_mensual)
} else { 
  print("Error: Precipitación y temperatura deben tener mismo espacio temporal")
  }
colnames(pp_tienen_T)<-colnames(Tmax_tot)
latitudes<-c()

contador<-1
for (i in colnames(pp_tienen_T)){
  if (i %in% colnames(pp_acum_mensual)){ #llenamos las estaciones con PP disponible
    pp_tienen_T[i]<-pp_acum_mensual[i]
    latitudes[contador]<-as.numeric(pp_caract[i][2,]) #se genera vector con latitudes en el orden de las estaciones a utilizar
    contador<-contador+1
  } else {print(paste0("Sin datos de precipitación para estación : ", i))}
}


######## UNA VEZ QUE LAS BBDD ESTAN IGUALES, CALCULAMOS EL ET0 ###########
ET0<-pp_tienen_T
ET0[,]<-NA

for (i in c(1:length(latitudes))){ 
    param_tmin<-Tmin_tot[,i]
    param_tmax<-Tmax_tot[,i]
    param_lat<-latitudes[i]
    param_pp<-pp_tienen_T[,i]
    time_series_fechas<-ts(param_tmin, start = c(1990, 1), frequency = 12)
    ET0[,i]<-hargreaves(Tmin=time_series_fechas,Tmax=param_tmax,lat=param_lat,Pre=param_pp,na.rm=TRUE)
}

BH<-pp_tienen_T-ET0

return (BH)
}

##write.csv(BH,file="Balance_hidrico_bbdd_total.csv")

