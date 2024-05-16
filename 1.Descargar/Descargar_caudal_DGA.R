#Extraer datos de la Direccion Meteorologica de Chile
#Es necesario correr archivo de Librerias.R para poder correr esta función

descargar_caudal_DGA<-function(ano_inicio, ano_actual, mes_ultimo, estaciones){
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
      html <- read_html(GET(url, add_headers("User-Agent" = "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/97.0.4692.71 Safari/537.36")))
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
        mutate(Codigo_nacional = substring(url, 85, 94)) %>%
        select(Day, Month,Year, caudal_mean, Codigo_nacional)

      #print(tabla)
        


      #Rellenamos las consultas vacías o con error con NA:

      if (nrow(tabla) > 0) {
        return(tabla)
      } else { return(data.frame(Year=as.numeric(substring(url, 322, 325)),
                                 Day=NA,
                                 Month=NA,
                                 caudal_mean=NA,
                                 Codigo_nacional=substring(url, 85, 94))) }

    }, error = function(e) {
      message("An error occurred: ", conditionMessage(e))
      # En caso de que la conexión tire ERROR:
      return(data.frame(Year=as.numeric(substring(url, 322, 325)),
                        Day=NA,
                        Month=NA,
                        caudal_mean=NA,
                        Codigo_nacional=substring(url, 85, 94)))
      
    })

  }  
  ######################## Comienza rutina ################################

  #Definir fecha de inicio y fecha de fin para extraer datos
  urls<-NULL
  for(year in ano_inicio:ano_actual){
    for(i in 1:length(estaciones$Codigo_nacional)){
      urls<-c(urls,getUrls2(year, estaciones$Codigo_nacional[[i]]))
    }
  }

  #Main loop
  test <- NULL
  temp<-NULL
  for (i in 1:length(urls)){

    temp<-rbind(temp, getData(urls[i]))
    Sys.sleep(0.5)
    print(paste0("Voy en el enlace web número: ",i, "de ",length(urls)))

  }

  #Filtrar valores NA, negativos outliers >1000.
  result <- temp %>% 
  filter(caudal_mean >= 0, caudal_mean <= 10000) %>% 
    select(Year, Month, Day, Codigo_nacional, caudal_mean)

  result <- result %>% 
    filter(!(Year == ano_actual & Month > as.numeric(mes_ultimo) ))

  test <- result

  return(result)

}

q = descargar_caudal_DGA(2020, 2024, 4, metadata_DGA_caudal)
write.csv(q, "caudal_DGA_2020_2024_4.csv")

