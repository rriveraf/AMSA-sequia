#Extraer datos de la Direccion Meteorologica de Chile
descargar_pp_DGA<-function(fecha_ini, fecha_fin, estaciones){
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
  root1<-"https://snia.mop.gob.cl/dgasat/pages/dgasat_param/dgasat_param_tablas.jsp?estacion1="
  #cod_est<-"01000005-K"
  root2<-"&estacion2=-1&estacion3=-1&parametros="
  root3<-"_3_Pptaci%C3%B3n+Acum.+%28mm%29&accion=refresca&param=&tipo=ANO&fechaFinGrafico=04%2F05%2F2023&hora_fin=0&tiporep=S&period=rango&fechaInicioTabla=01%2F01%2F"
  #year<-"2018"
  root4<-"&fechaFinTabla=31%2F12%2F"
  root5<-"&UserID=nobody&EsDL1=&EsDL2=&EsDL3="
  #url<-paste0(root1,cod_est,root2,cod_est,root3,year,root4,year,root5)
  
  #Se genera el url completo
  urls <- NULL
  urls <- c(urls,paste0(root1,cod_est,root2,cod_est,root3,year,root4,year,root5))
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
      html_elements("#datos") %>%
      html_table() %>%
      data.frame() 
    colnames(tabla)<-c("id", "fecha", "pp_min","pp_max","pp_mean")
    
      #Resumir los datos mensualmente:
      tabla$fecha <- as.Date(tabla$fecha, format = "%d/%m/%Y")
      tabla$Month <- month(tabla$fecha)
      tabla$Year <- year(tabla$fecha)
      tabla$Day <- day(tabla$fecha)
      
      tabla <- tabla %>%
        #group_by(Year, Month) %>%
        mutate(pp_day = pp_min - lag(pp_min, default = 0)) %>%
        mutate(Codigo_nacional= substring(url, 85, 94))%>% 
        select(Day, Month,Year, pp_day, Codigo_nacional)
    
    #En caso de que no haya informacion disponible:
    if (nrow(tabla) > 0) {
      return(tabla)
    } else { return(data.frame(Year=as.numeric(substring(url, 328, 331)),
                               Day=NA,
                               Month=NA,
                               pp_day=NA,
                               Codigo_nacional=substring(url, 85, 94))) }
  
  }, error = function(e) {
    # En caso de que la conexión tire ERROR:
    return(data.frame(Year=as.numeric(substring(url, 328, 331)),
                      Day=NA,
                      Month=NA,
                      pp_day=NA,
                      Codigo_nacional=substring(url, 85, 94)))
    message("An error occurred: ", conditionMessage(e))
  })
 
}  
 
  

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
  print(paste0("Voy en el enlace web número: ",i, " de ",length(urls)))
  
}

#temp[temp$pp_day > 900 | temp$pp_day < 0] <- NA
result <- temp %>% 
  filter(pp_day< 900 | is.na(pp_day), pp_day >= 0 | is.na(pp_day)) %>% 
  select(Year, Month, Day, Codigo_nacional, pp_day)

ultimo_mes<-as.numeric(substr(Sys.time() %m-% months(1),6,7))
ultimo_ano<-as.numeric(substr(Sys.time() %m-% months(1),1,4))

result$Month<-as.numeric(result$Month)

if(fecha_fin==ultimo_ano){
  result <- result %>% 
    filter(!(Year == ultimo_ano & Month > as.numeric(ultimo_mes) ))
}

#OUTPUT TO CSV
#write.csv2(temp, paste0("pp_DGA_", fecha_ini ,"_", fecha_fin,".csv"))

#OUTPUT TO EXCEL
#write_xlsx(
#  result,
#  path =paste0("pp_", fecha_ini ,"_", fecha_fin,".xlsx"),
#  col_names = TRUE,
#  format_headers = TRUE
#)


beepr::beep(8)
return(result)
}
