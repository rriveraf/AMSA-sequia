#cambio fecha tipo ene-90 a 01/1990

Cambio_formato_fecha<-function(fechas){

#fechas corresponde a una lista (o columna de dataframe)
  
lista_fechas<-c()
contador<-1
for (i in fechas){
  fecha1 <-substr(i,1,3)
  if (fecha1=="ene"){
    mes<-"01"
  }
  else if (fecha1=="feb"){
    mes<-"02"
  }
  else if (fecha1=="mar"){
    mes<-"03"
  }
  else if (fecha1=="abr"){
    mes<-"04"
  }
  else if (fecha1=="may"){
    mes<-"05"
  }
  else if (fecha1=="jun"){
    mes<-"06"
  }
  else if (fecha1=="jul"){
    mes<-"07"
  }
  else if (fecha1=="ago"){
    mes<-"08"
  }
  else if (fecha1=="sep"){
    mes<-"09"
  }
  else if (fecha1=="oct"){
    mes<-"10"
  }
  else if (fecha1=="nov"){
    mes<-"11"
  }
  else if (fecha1=="dic"){
    mes<-"12"
  }
  fecha_con_numero<-paste0("01","/",mes,"/",substr(i, nchar(i) - 1, nchar(i)))
  fecha_transformada<-as.Date(fecha_con_numero,format = "%d/%m/%y")
  fecha_formateada <- format(fecha_transformada, "%m/%Y")
  lista_fechas[contador]<-fecha_formateada
  contador<-contador+1
}
return(lista_fechas)
}
