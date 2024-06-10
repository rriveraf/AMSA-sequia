#Extraer datos de la Direccion Meteorologica de Chile o bien Direccion general de Aguas. Depende de la variable.
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
      message("Error de conexión: ", conditionMessage(e))
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
    Sys.sleep(0.1)
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
descargar_temp_min_DMC<-function(ano_inicio, ano_actual, mes_ultimo, estaciones){
                    
############### Funcion: Crear direcciones URL ############
  getUrls2 <- function(year,cod_est){


    #Definir URL raíz del servidor (debe ser estática)
    root1<-"https://climatologia.meteochile.gob.cl/application/anual/temperaturaMinimaAnual/"
    #Se genera el url completo
    urls <- NULL
    urls <- c(urls,paste0(root1,cod_est,"/",year))
    return(urls)
  }

  ############### Función: Extraer data  #########################
  getData<-function(url){
    #Descargar html
    tryCatch({
      html <- read_html(GET(url, add_headers("User-Agent" = "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/97.0.4692.71 Safari/537.36")))
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
                     values_to = "temp_min") %>%
        mutate(temp_min = as.numeric(temp_min))  %>%
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
        celda_fin<-which(is.na(tabla$Day))[1]
        tabla<-tabla[-c(celda_fin:nrow(tabla)),]  %>%
          mutate(Year = as.numeric(substring(url, nchar(url) - 3))) %>% 
          mutate(Codigo_nacional=substring(url, 81, 86)) %>% 
          arrange(Month, Day, Year) %>% 
          select(Year, Month, Day, Codigo_nacional, temp_min)


      #En caso de que no haya informacion disponible:
      if (nrow(tabla) > 0) {
        return(tabla)
      } else { return(data.frame(Year=as.numeric(substring(url, nchar(url) - 3)),
                                 Month=NA,
                                 Day=NA,
                                 Codigo_nacional=substring(url, 81, 86),
                                 temp_min=NA  ))  }

    }, error = function(e) {
      message("Error de conexión: ", conditionMessage(e))
      # En caso de que la conexión tire ERROR:
      return(data.frame(Year=as.numeric(substring(url, nchar(url) - 3)),
                        Month=NA,
                        Day=NA,
                        Codigo_nacional=substring(url, 81, 86),
                        temp_min=NA)) 
      
    })
  
  }  
  urls<-NULL
  #ano_inicio<-"1967"
  #ano_actual<-"2023"
  for(year in ano_inicio:ano_actual){
    for(i in 1:length(estaciones$Codigo_nacional)){

      urls<-c(urls,getUrls2(year, estaciones$Codigo_nacional[[i]]))

      #urls<-c(urls,getUrls2(year, "330020"))
    }
  }

  #Main loop
  temp<-NULL
  for (i in 1:length(urls)){

    temp<-rbind(temp, getData(urls[i]))
    Sys.sleep(0.1)
    print(paste0("Voy en el enlace web número: ",i, "de ",length(urls)))

  }
  result <- temp %>% 
    filter(temp_min>= -50 | is.na(temp_min), temp_min <= 50 | is.na(temp_min))

  result$Month <- as.numeric(result$Month)
  result$Year <- as.numeric(result$Year)

  result <- result %>% 
    filter(!(Year == ano_actual & Month > as.numeric(mes_ultimo)))

  

  return(result)
  beepr::beep(8)
}
descargar_temp_max_DMC<-function(ano_inicio, ano_actual, mes_ultimo, estaciones){
  ############### Funcion: Crear direcciones URL ############
  getUrls2 <- function(year,cod_est){

    #Definir URL raíz del servidor (debe ser estática)
    root1<-"https://climatologia.meteochile.gob.cl/application/anual/temperaturaMaximaAnual/"

    #Se genera el url completo
    urls <- NULL
    urls <- c(urls,paste0(root1,cod_est,"/",year))
    return(urls)
  }

  ############### Función: Extraer data  #########################
  getData<-function(url){
    #Descargar html
    tryCatch({
      html <- read_html(GET(url, add_headers("User-Agent" = "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/97.0.4692.71 Safari/537.36")))
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
                     values_to = "temp_max") %>%
        mutate(temp_max = as.numeric(temp_max))  %>%
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
        celda_fin<-which(is.na(tabla$Day))[1]
        tabla<-tabla[-c(celda_fin:nrow(tabla)),]  %>%
          mutate(Year = as.numeric(substring(url, nchar(url) - 3))) %>% 
          mutate(Codigo_nacional=substring(url, 81, 86)) %>% 
          arrange(Month, Day, Year) %>% 
          select(Year, Month, Day, Codigo_nacional, temp_max)


      #En caso de que no haya informacion disponible:
      if (nrow(tabla) > 0) {
        return(tabla)
      } else { return(data.frame(Year=as.numeric(substring(url, nchar(url) - 3)),
                                 Month=NA,
                                 Day=NA,
                                 Codigo_nacional=substring(url, 81, 86),
                                 temp_max=NA  ))  }

    }, error = function(e) {
      
      # En caso de que la conexión tire ERROR:
      return(data.frame(Year=as.numeric(substring(url, nchar(url) - 3)),
                        Month=NA,
                        Day=NA,
                        Codigo_nacional=substring(url, 81, 86),
                        temp_max=NA)) 

      message("Error de conexión: ", conditionMessage(e))
      
    })
  
  }  
  urls<-NULL
  #ano_inicio<-"1989"
  #ano_actual<-"2023"
  for(year in ano_inicio:ano_actual){
    for(i in 1:length(estaciones$Codigo_nacional)){

      urls<-c(urls,getUrls2(year, estaciones$Codigo_nacional[[i]]))

      #urls<-c(urls,getUrls2(year, "330020"))
    }
  }

  #Main loop
  temp<-NULL
  for (i in 1:length(urls)){

    temp<-rbind(temp, getData(urls[i]))
    Sys.sleep(0.1)
    print(paste0("Voy en el enlace web número: ",i, "de ",length(urls)))

  }

  result <- temp %>% 
    filter(temp_max>= -50 | is.na(temp_max), temp_max <= 50 | is.na(temp_max))


  result$Month<-as.numeric(result$Month)

  result <- result %>% 
    filter(!(Year == ano_actual & Month > as.numeric(mes_ultimo)))

  
  return(result)
  beepr::beep(8)

}
descargar_temp_DGA<-function(ano_inicio, ano_actual, mes_ultimo, estaciones){

  ##################### Definimos funciones de utilidad #####################

  #####1) Función para concatenar URL's #####

  getUrls2 <- function(year,cod_est){
    #Definir URL raíz del servidor (debe ser estática)
    root1<-"https://snia.mop.gob.cl/dgasat/pages/dgasat_param/dgasat_param_tablas.jsp?estacion1="
    #cod_est<-"07321005-4"
    root2<-"&estacion2=-1&estacion3=-1&parametros="
    root3<-"_5_Temp.del+Aire+%28%C2%BAC%29&accion=refresca&param=&tipo=ANO&fechaFinGrafico=18%2F05%2F2023&hora_fin=0&tiporep=S&period=rango&fechaInicioTabla=01%2F01%2F"
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
      colnames(tabla)<-c("id", "fecha", "temp_min","temp_max","temp_mean")

      #Resumir los datos mensualmente:
      tabla$fecha <- as.Date(tabla$fecha, format = "%d/%m/%Y")
      tabla$Month <- month(tabla$fecha)
      tabla$Year <- year(tabla$fecha)
      tabla$Day <- day(tabla$fecha)
      tabla <- tabla %>%
        #group_by(Year, Month) %>%
        #summarize(temp_month =  mean(temp_mean)) %>%
        mutate(Codigo_nacional= substring(url, 85, 94)) %>% 
      select(Day, Month,Year, Codigo_nacional, temp_min, temp_max, temp_mean)


      #Rellenamos las consultas vacías o con error con NA:

      if (nrow(tabla) > 0) {
        return(tabla)
      } else { return(data.frame(Year=as.numeric(substring(url, 327, 330)),
                                 Day=NA,
                                 Month=NA,
                                 temp_mean=NA,
                                 temp_max=NA,
                                 temp_min=NA,
                                 Codigo_nacional=substring(url, 85, 94))) }

    }, error = function(e) {
      # En caso de que la conexión tire ERROR:
      message("Error de conexión: ", conditionMessage(e))
      return(data.frame(Year=as.numeric(substring(url, 327, 330)),
                        Day=NA,
                        Month=NA,
                        temp_mean=NA,
                        temp_max=NA,
                        temp_min=NA,
                        Codigo_nacional=substring(url, 85, 94)))
      
    })

  }  

  
  #Definir fecha de inicio y fecha de fin para extraer datos
  urls<-NULL
  #ano_ini<-"1990"
  #ano_fin<-"2023"
  for(year in ano_inicio:ano_actual){
    for(i in 1:length(estaciones$Codigo_nacional)){
      urls<-c(urls,getUrls2(year, estaciones$Codigo_nacional[[i]]))
    }
  }

  #Main loop
  temp<-NULL
  for (i in 1:length(urls)){

    temp<-rbind(temp, getData(urls[i]))
    Sys.sleep(0.1)
    print(paste0("Voy en el enlace web número: ",i, "de ",length(urls)))

  }
  result <- temp %>%
    filter(
      temp_max >= -50 & temp_max <= 50,
      temp_min >= -50 & temp_min <= 50,
      temp_mean >= -50 & temp_mean <= 50
    )

  result <- result %>% 
    filter(!(Year == ano_actual & Month > as.numeric(mes_ultimo) ))

  return(result)
  beepr::beep(8)

}
descargar_pp_DMC<-function(ano_inicio, ano_actual, mes_ultimo, estaciones){

######################################################               
############### Funcion: Crear direcciones URL ############
  getUrls2 <- function(year,cod_est){


    #Definir URL raíz del servidor (debe ser estática)
    root1<-"https://climatologia.meteochile.gob.cl/application/anual/aguaCaidaAnual/"
    #Se genera el url completo
    urls <- NULL
    urls <- c(urls,paste0(root1,cod_est,"/",year))
    return(urls)
  }

  ############### Función: Extraer data  #########################
  getData<-function(url){
    #Descargar html
    tryCatch({
      html <- read_html(GET(url, add_headers("User-Agent" = "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/97.0.4692.71 Safari/537.36")))
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
      #message("Error de conexión: ", conditionMessage(e))
      # En caso de que la conexión tire ERROR:
      return(data.frame(Day=NA,
                        Month=NA,
                        pp_day=NA,
                        Year=as.numeric(substring(url, nchar(url) - 3)),
                        Codigo_nacional=substring(url, 73, 78))) 
      
    })

  }  

  #Definir fecha de inicio y fecha de fin para extraer datos
  urls<-NULL
  for(year in ano_inicio:ano_actual){
    for(i in 1:length(estaciones$Codigo_nacional)){

      urls<-c(urls,getUrls2(year, estaciones$Codigo_nacional[[i]]))

    }
  }

  #Main loop
  temp<-NULL
  for (i in 1:length(urls)){

    temp<-rbind(temp, getData(urls[i]))
    Sys.sleep(0.1)
    print(paste0("Voy en el enlace web número: ",i, "de ",length(urls)))

  }

  #Filtramos outlier y ordenamos columnas
  result <- temp %>% 
    filter(pp_day>= 0 | is.na(pp_day) , pp_day <= 900 | is.na(pp_day)) %>%
      select(Year, Month, Day, Codigo_nacional, pp_day) 

  result$Month<-as.numeric(result$Month)

  result <- result %>% 
    filter(!(Year == ano_actual & Month > as.numeric(mes_ultimo) ))

  




  beepr::beep(8)
  return(result)


}
descargar_pp_DGA<-function(ano_inicio, ano_actual, mes_ultimo, estaciones){
                    
############### Funcion: Crear direcciones URL ############
  getUrls2 <- function(year,cod_est){


    #Definir URL raíz del servidor (debe ser estática)
    root1<-"https://snia.mop.gob.cl/dgasat/pages/dgasat_param/dgasat_param_tablas.jsp?estacion1="
    #cod_est<-"01000005-K"
    root2<-"&estacion2=-1&estacion3=-1&parametros="
    root3<-"_3_Pptaci%C3%B3n+Acum.+%28mm%29&accion=refresca&param=&tipo=ANO&fechaFinGrafico=04%2F05%2F2023&hora_fin=0&tiporep=S&period=rango&fechaInicioTabla=01%2F01%2F"
    
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
     html <- read_html(GET(url, add_headers("User-Agent" = "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/97.0.4692.71 Safari/537.36")))
      
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
          mutate(pp_day = ifelse(lag(pp_min, default = 0) >= 0, pp_min - lag(pp_min, default = 0), pp_min)) %>%
          mutate(Codigo_nacional = substring(url, 85, 94)) %>%
          select(Day, Month, Year, pp_day, Codigo_nacional)

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
      message("Error de conexión: ", conditionMessage(e))
      return(data.frame(Year=as.numeric(substring(url, 328, 331)),
                        Day=NA,
                        Month=NA,
                        pp_day=NA,
                        Codigo_nacional=substring(url, 85, 94)))
      
    })
 
}  

  urls<-NULL
  for(year in ano_inicio:ano_actual){
    for(i in 1:length(estaciones$Codigo_nacional)){
    urls<-c(urls,getUrls2(year, estaciones$Codigo_nacional[[i]]))
    }
  }

  #Main loop
  temp<-NULL
  for (i in 1:length(urls)){

    temp<-rbind(temp, getData(urls[i]))
    Sys.sleep(0.1)
    print(paste0("Voy en el enlace web número: ",i, " de ",length(urls)))

  }

  #temp[temp$pp_day > 900 | temp$pp_day < 0] <- NA
  result <- temp %>% 
  filter(pp_day< 900 | is.na(pp_day), pp_day >= 0 | is.na(pp_day)) %>% 
  select(Year, Month, Day, Codigo_nacional, pp_day)

  result$Month<-as.numeric(result$Month)

  result <- result %>% 
    filter(!(Year == ano_actual & Month > as.numeric(mes_ultimo) ))


  beepr::beep(8)
  return(result)
}


