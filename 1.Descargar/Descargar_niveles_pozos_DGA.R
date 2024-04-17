#Extraer datos de la Direccion Meteorologica de Chile
#Es necesario correr archivo de Librerias.R para poder correr esta función
descargar_niveles_DGA<-function(ano_inicio, ano_actual, mes_ultimo, estaciones){

######################################################
                    
############### Funcion: Crear direcciones URL ############
  getUrls2 <- function(year,cod_est){


    #Definir URL raíz del servidor (debe ser estática)
    root1<-"https://snia.mop.gob.cl/dgasat/pages/dgasat_param/dgasat_param_tablas.jsp?estacion1="
    #cod_est<-"05100001-3"
    root2<-"&estacion2=-1&estacion3=-1&parametros="
    root3<-"_127_Nivel+de+Pozo+%28m%29&accion=refresca&param=&tipo=ANO&fechaFinGrafico=04%2F05%2F2023&hora_fin=0&tiporep=S&period=rango&fechaInicioTabla=01%2F01%2F"
    
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
      html <- read_html(GET(url, add_headers("User-Agent" = "Mozilla/5.0")))
      # Additional code you want to execute if successful
      #Extraer datos del html
      tabla<-html %>%
        html_elements("#datos") %>%
        html_table() %>%
        data.frame() 
      colnames(tabla)<-c("id", "fecha", "nivel_min","nivel_max","nivel_mean")

        #Resumir los datos mensualmente:
        tabla$fecha <- as.Date(tabla$fecha, format = "%d/%m/%Y")
        tabla$Month <- month(tabla$fecha)
        tabla$Year <- year(tabla$fecha)
        tabla$Day <- day(tabla$fecha)

        tabla <- tabla %>%
          #group_by(Year, Month) %>%
          mutate(Codigo_nacional= substring(url, 85, 94))%>% 
          select(Day, Month,Year, nivel_mean, Codigo_nacional)

      #En caso de que no haya informacion disponible:
      if (nrow(tabla) > 0) {
        return(tabla)
      } else { return(data.frame(Year=as.numeric(substring(url, 328, 331)),
                                 Day=NA,
                                 Month=NA,
                                 nivel_mean=NA,
                                 Codigo_nacional=substring(url, 85, 94))) }

    }, error = function(e) {
      # En caso de que la conexión tire ERROR:
      return(data.frame(Year=as.numeric(substring(url, 328, 331)),
                        Day=NA,
                        Month=NA,
                        nivel_mean=NA,
                        Codigo_nacional=substring(url, 85, 94)))
      message("An error occurred: ", conditionMessage(e))
    })
  
  }


  #Ingresar data de las estaciones
  #direccion_catastro_estaciones <- paste0(getwd(),"/BBDD/estaciones/estaciones_BBDD_original.xlsx")
  #estaciones <- read_excel(direccion_catastro_estaciones, sheet = "Sheet1")

  #Definir fecha de inicio y fecha de fin para extraer datos
  urls<-NULL
  #ano_inicio<-"2020"
  #ano_actual<-"2023"
  for(year in ano_inicio:ano_actual){
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

  result <- temp
  #Meses completos: eliminar último mes de la serie descargada
  
  result$Month<-as.numeric(result$Month)
    result <- result %>% 
      filter(!(Year == ano_actual & Month > as.numeric(mes_ultimo) ))
  

  beepr::beep(8)
  return(result)
}


  #OUTPUT TO CSV
  #write.csv2(temp, paste0("pp_DGA_", ano_inicio ,"_", ano_actual,".csv"))

  #OUTPUT TO EXCEL
  #write_xlsx(
  #  result,
  #  path =paste0("pp_", ano_inicio ,"_", ano_actual,".xlsx"),
  #  col_names = TRUE,
  #  format_headers = TRUE
  #)