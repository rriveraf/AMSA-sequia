#Extraer datos de la Direccion Meteorologica de Chile
descargar_caudal_DGA<-function(fecha_ini, fecha_fin, estaciones){

library(rvest)
library(writexl)
library(readxl)
library(dplyr)
library(stringr)
library(lubridate)
library(tidyr)
library(purrr)
library(beepr)
library(rjson)
require(RCurl)
require(XML)
library(rvest)
require(rjson)
library(RJSONIO)


##################### Definimos funciones de utilidad #####################

#####1) Función para concatenar URL's #####

getUrls2 <- function(year,cod_est){
  #Definir URL raíz del servidor (debe ser estática)
  root1<-"https://snia.mop.gob.cl/dgasat/pages/dgasat_param/dgasat_param_tablas.jsp?estacion1="
  #cod_est<-"07321005-4"
  root2<-"&estacion2=-1&estacion3=-1&parametros="
  root3<-"_12_Caudal+%28m3%2Fseg%29&accion=refresca&param=&tipo=ANO&fechaFinGrafico=04%2F05%2F2023&hora_fin=0&tiporep=S&period=rango&fechaInicioTabla=01%2F01%2F"
  #year<-"2018"
  root4<-"&fechaFinTabla=31%2F12%2F"
  root5<-"&UserID=nobody&EsDL1=&EsDL2=&EsDL3="
  #url<-paste0(root1,cod_est,root2,cod_est,root3,year,root4,year,root5)
  
  #Se genera el url completo
  urls <- NULL
  urls <- c(urls,paste0(root1,cod_est,root2,cod_est,root3,year,root4,year,root5))
  return(urls)
}




####2) Función para descargar data desde Link ########

getData<-function(url){
  #INTENTAR descargar HTML, con recepción de error si no conecta.
  
  tryCatch({
    html <- read_html(url)
    # Additional code you want to execute if successful
    #Extraer datos del html
    tabla<-html %>%
      html_elements("#datos") %>%
      html_table() %>%
      data.frame() 
    colnames(tabla)<-c("id", "fecha", "caudal_min","caudal_max","caudal_mean")
   
    #Resumir los datos mensualmente:
    tabla$fecha <- as.Date(tabla$fecha, format = "%d/%m/%Y")
    tabla$Month <- month(tabla$fecha)
    tabla$Year <- year(tabla$fecha)
    tabla$Day <- day(tabla$fecha)
    tabla <- tabla %>%
      #group_by(Year, Month) %>%
      #summarize(caudal_month =  mean(caudal_mean)) %>%
      mutate(Codigo_nacional= substring(url, 85, 94)) %>%
      select(Day, Month,Year, caudal_mean, Codigo_nacional)
      
    
    #Rellenamos las consultas vacías o con error con NA:
    
    if (nrow(tabla) > 0) {
      return(tabla)
    } else { return(data.frame(Year=as.numeric(substring(url, 322, 325)),
                               Day=NA,
                               Month=NA,
                               caudal_mean=NA,
                               Codigo_nacional=substring(url, 85, 94))) }
    
  }, error = function(e) {
    # En caso de que la conexión tire ERROR:
    return(data.frame(Year=as.numeric(substring(url, 322, 325)),
                      Day=NA,
                      Month=NA,
                      caudal_mean=NA,
                      Codigo_nacional=substring(url, 85, 94)))
    message("An error occurred: ", conditionMessage(e))
  })
  
}  



######################## Comienza rutina ################################

#Ingresar data de las estaciones
#direccion_catastro_estaciones <- paste0(getwd(),"/BBDD/estaciones/estaciones_BBDD_original.xlsx")
#estaciones <- read_excel(direccion_catastro_estaciones, sheet = "Sheet1")

#Definir fecha de inicio y fecha de fin para extraer datos
urls<-NULL
#fecha_ini<-"2020"
#fecha_fin<-"2023"
for(year in fecha_ini:fecha_fin){
  for(i in 1:length(estaciones$Codigo_nacional)){
    urls<-c(urls,getUrls2(year, estaciones$Codigo_nacional[[i]]))
  }
}

#Main loop
temp<-NULL
for (i in 1:length(urls)){
  
  temp<-rbind(temp, getData(urls[i]))
  print(paste0("Voy en el enlace web número: ",i, "de ",length(urls)))
  
}

#Filtrar valores NA, negativos outliers >1000.
result <- temp %>% 
  filter(caudal_mean >= 0, caudal_mean <= 10000) %>% 
  select(Year, Month, Day, Codigo_nacional, caudal_mean)

return(result)

##Debería ser full_join para conservar todas las estaciones
#result <- inner_join(estaciones, temp, by = "Codigo_nacional") %>%
#         distinct() 

#OUTPUT TO EXCEL
#write_xlsx(
#  result,
#  path =paste0("caudal_", fecha_ini ,"_", fecha_fin,".xlsx"),
#  col_names = TRUE,
#  format_headers = TRUE
#)

}

