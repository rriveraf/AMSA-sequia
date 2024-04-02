#Extraer datos de la Direccion Meteorologica de Chile
descargar_pp_DMC<-function(fecha_ini, fecha_fin, estaciones){
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
library(RCurl)
library(XML)
library(rvest)
library(rjson)
library(RJSONIO)

######################################################
                    
############### Funcion: Crear direcciones URL ############
getUrls2 <- function(year,cod_est){
  
  
  #Definir URL raíz del servidor (debe ser estática)
  root1<-"https://climatologia.meteochile.gob.cl/application/anual/aguaCaidaAnual/"
  #cod_est<-330020
  #root2<-"&estacion2=-1&estacion3=-1&parametros="
  #root3<-"_3_Pptaci%C3%B3n+Acum.+%28mm%29&accion=refresca&param=&tipo=ANO&fechaFinGrafico=04%2F05%2F2023&hora_fin=0&tiporep=S&period=rango&fechaInicioTabla=01%2F01%2F"
  #year<-"2023"
  #root4<-"&fechaFinTabla=31%2F12%2F"
  #root5<-"&UserID=nobody&EsDL1=&EsDL2=&EsDL3="
  #url<-paste0(root1,cod_est,root2,cod_est,root3,year,root4,year,root5)
  
  #Se genera el url completo
  urls <- NULL
  urls <- c(urls,paste0(root1,cod_est,"/",year))
  return(urls)
}

############### Función: Extraer data  #########################
getData<-function(url){
  #Descargar html
  tryCatch({
    html <- read_html(url)
    # Additional code you want to execute if successful
    #Extraer datos del html
    tabla<-html %>%
      html_elements("#excel") %>%
      html_table() %>%
      data.frame()
    
    names(tabla)<-tabla[1,]
    names(tabla)[1]<-"Day"
    tabla<-tabla[-1,]  %>% 
      pivot_longer(cols = -Day,
                   names_to = "Month",
                   values_to = "pp_day") %>%
      mutate(pp_day = str_replace(pp_day, "s/p", "0"))  %>%
      mutate(pp_day = as.numeric(pp_day))  %>%
      mutate(Day = as.numeric(Day)) %>%
      mutate(Month = case_when(
        Month == "Ene" ~ "01",
        Month == "Feb" ~ "02",
        Month == "Mar" ~ "03",
        Month == "Abr" ~ "04",
        Month == "May" ~ "05",
        Month == "Jun" ~ "06",
        Month == "Jul" ~ "07",
        Month == "Ago" ~ "08",
        Month == "Sep" ~ "09",
        Month == "Oct" ~ "10",
        Month == "Nov" ~ "11",
        Month == "Dic" ~ "12",
        TRUE ~ Month  # Keep original value if it doesn't match any case
      ))
      celda_vacia<-which(is.na(tabla$Day)==TRUE)[1]
      tabla<-tabla[-c(celda_vacia:nrow(tabla)),]  %>%
        mutate(Year = as.numeric(substring(url, nchar(url) - 3))) %>% 
        mutate(Codigo_nacional=substring(url, 73, 78)) %>% 
        arrange(Month, Day, Year)
    
    
    #En caso de que no haya informacion disponible:
    if (nrow(tabla) > 0) {
      return(tabla)
    } else { return(data.frame(Day=NA,
                               Month=NA,
                               pp_day=NA,
                               Year=as.numeric(substring(url, nchar(url) - 3)),
                               Codigo_nacional=substring(url, 73, 78))) }
  
  }, error = function(e) {
    # En caso de que la conexión tire ERROR:
    return(data.frame(Day=NA,
                      Month=NA,
                      pp_day=NA,
                      Year=as.numeric(substring(url, nchar(url) - 3)),
                      Codigo_nacional=substring(url, 73, 78))) 
    message("No se pudo descargar la página: ", conditionMessage(e))
  })
 
}  

#Definir fecha de inicio y fecha de fin para extraer datos
urls<-NULL
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

#Filtramos outlier y ordenamos columnas
result <- temp %>% 
  filter(pp_day>= 0 | is.na(pp_day) , pp_day <= 900 | is.na(pp_day)) %>%
  select(Year, Month, Day, Codigo_nacional, pp_day) 

#En el caso de que la descarga sea del ultimo ano, eliminar observaciones
#que aun no ocurren (Month>ultimo_mes)

ultimo_mes<-as.numeric(substr(Sys.time() %m-% months(1),6,7))
ultimo_ano<-as.numeric(substr(Sys.time() %m-% months(1),1,4))

result$Month<-as.numeric(result$Month)

if(fecha_fin==ultimo_ano){
result <- result %>% 
  filter(!(Year == ultimo_ano & Month > as.numeric(ultimo_mes) ))
}

return(result)

beepr::beep(8)

}
