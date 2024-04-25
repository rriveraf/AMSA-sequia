#Extraer datos de la Direccion Meteorologica de Chile
#Es necesario correr archivo de Librerias.R para poder correr esta función
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
      html <- read_html(GET(url, add_headers("User-Agent" = "Mozilla/5.0")))
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
      # En caso de que la conexión tire ERROR:
      return(data.frame(Year=as.numeric(substring(url, nchar(url) - 3)),
                        Month=NA,
                        Day=NA,
                        Codigo_nacional=substring(url, 81, 86),
                        temp_min=NA)) 
      message("No se pudo descargar la página: ", conditionMessage(e))
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



