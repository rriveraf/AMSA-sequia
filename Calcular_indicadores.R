calcular_ICE_DGA <- function(directorio_base, ruta_archivo_caudal_concatenado) {
  print("Calculando indicadores ICE para caudal DGA")
  print("Tiempo estimado de espera para esta etapa: 1 minuto")
  tryCatch({  
    #lectura de datos
    caudal_mensual_completo = read.csv(ruta_archivo_caudal_concatenado)

    # Ordenar por código nacional, año y mes: CLAVE PARA CALCULAR ICE
    caudal_mensual_completo <- caudal_mensual_completo %>%
      arrange(Codigo_nacional, Year, Month)

    # Corrección de caudales nulos
    caudal_mensual_completo$q_month[caudal_mensual_completo$q_month == 0] <- 0.0001

    # Calcular promedios móviles
    caudal_mensual_completo <- caudal_mensual_completo %>%
      mutate(Date = as.Date(paste(Year, Month, "01", sep="-"))) %>%
      arrange(Codigo_nacional, Date) %>%
      group_by(Codigo_nacional) %>%
      mutate(
        q_3month = rollapply(q_month, 3, mean, partial = FALSE, fill = NA, align = "right"),
        q_6month = rollapply(q_month, 6, mean, partial = FALSE, fill = NA, align = "right"),
        q_12month = rollapply(q_month, 12, mean, partial = FALSE, fill = NA, align = "right"),
        q_24month = rollapply(q_month, 24, mean, partial = FALSE, fill = NA, align = "right")
      ) %>%
      ungroup() %>%
      select(-Date)

    # Calcular ICE, fijamos mínimo valor -5 y máximo valor 5
    ice_dataframe <- caudal_mensual_completo %>%
      group_by(Codigo_nacional) %>%
      mutate(
        ice_1 = spi(q_month, scale = 1, distribution = "Gamma", na.rm = TRUE, verbose = FALSE)$fitted,
        ice_3 = spi(q_3month, scale = 1, distribution = "Gamma", na.rm = TRUE, verbose = FALSE)$fitted,
        ice_6 = spi(q_6month, scale = 1, distribution = "Gamma", na.rm = TRUE, verbose = FALSE)$fitted,
        ice_12 = spi(q_12month, scale = 1, distribution = "Gamma", na.rm = TRUE, verbose = FALSE)$fitted,
        ice_24 = spi(q_24month, scale = 1, distribution = "Gamma", na.rm = TRUE, verbose = FALSE)$fitted
      ) %>%
      ungroup() %>%
      mutate(across(starts_with("ice"), ~pmax(pmin(., 5), -5)))
    

      fechas_archivo = obtener_fechas_archivo(ruta_archivo_caudal_concatenado)
      # Guardar el dataframe de ICE
      write.csv(ice_dataframe, paste0(directorio_base, "/BBDD/indicadores/ICE/DGA/ICE_DGA_1989_", fechas_archivo$ano_fin,"_", fechas_archivo$mes_fin,".csv"))
      
      print("Indicadores ICE calculados con éxito")
    
      return(TRUE)

  }, error = function(e) {
    message("Error al calcular los indicadores ICE: ", e$message)
    return(FALSE)
  })
}

calcular_IPE_DGA_DMC <- function(directorio_base, ruta_archivo_pp_concatenado, fuente) {
  #source(paste0(directorio_base, "/funciones_de_utilidad.R"))
  # Verificación de la fuente de datos
  
  print(paste0("Calculando indicadores IPE para ", fuente))
  print("Tiempo estimado de espera para esta etapa: 3 minutos")
  if(tolower(fuente) != "dga" && tolower(fuente) != "dmc") {
    stop("La fuente debe ser 'DGA' o 'DMC'")
  }

  tryCatch({
    pp_mensual_completo = read.csv(ruta_archivo_pp_concatenado)
    # Ordenar datos por código nacional, año y mes: CLAVE PARA CALCULAR SPI
    pp_mensual_completo <- pp_mensual_completo %>%
      arrange(Codigo_nacional, Year, Month)

    # Calcular SPI para varios períodos, fijando minimo valor en -5 y maximo valor en 5
    spi_dataframe <- pp_mensual_completo %>%
      group_by(Codigo_nacional) %>%
      mutate(
        spi_1 = spi(pp_month, scale = 1, distribution = "Gamma", na.rm = TRUE, verbose = FALSE)$fitted,
        spi_3 = spi(pp_month, scale = 3, distribution = "Gamma", na.rm = TRUE, verbose = FALSE)$fitted,
        spi_6 = spi(pp_month, scale = 6, distribution = "Gamma", na.rm = TRUE, verbose = FALSE)$fitted,
        spi_12 = spi(pp_month, scale = 12, distribution = "Gamma", na.rm = TRUE, verbose = FALSE)$fitted,
        spi_24 = spi(pp_month, scale = 24, distribution = "Gamma", na.rm = TRUE, verbose = FALSE)$fitted
      ) %>%
      ungroup() %>% 
      mutate(across(starts_with("spi"), ~pmax(pmin(., 5), -5)))

    fechas_archivo = obtener_fechas_archivo(ruta_archivo_pp_concatenado)
  
   #Determinar el directorio de salida basado en la fuente
    if(tolower(fuente) == "dga") {
      write.csv(spi_dataframe, paste0(directorio_base, "/BBDD/indicadores/IPE/DGA/IPE_DGA_1979_", fechas_archivo$ano_fin,"_", fechas_archivo$mes_fin,".csv"))
    } else if (tolower(fuente) == "dmc"){
      write.csv(spi_dataframe, paste0(directorio_base, "/BBDD/indicadores/IPE/DMC/IPE_DMC_1979_", fechas_archivo$ano_fin,"_", fechas_archivo$mes_fin,".csv"))
    }
    print(paste0("Indicadores IPE para ", fuente, " calculados con éxito"))
    
    return(TRUE)
  }, error = function(e) {
    message(paste0("Error al calcular los indicadores IPE para ", fuente, ": ", e$message))
    return(FALSE)
  })
}

calcular_IPEE_DGA_DMC <- function(directorio_base, ruta_archivo_pp_concatenado, ruta_archivo_temp_concatenado, fuente) {
  #source(paste0(directorio_base, "/funciones_de_utilidad.R"))
  print(paste0("Calculando indicadores IPEE para ", fuente))
  print("Tiempo estimado de espera para esta etapa: 5 minutos")
  if(tolower(fuente) != "dga" && tolower(fuente) != "dmc") {
    stop("La fuente debe ser 'DGA' o 'DMC'")
  }
  
  tryCatch({
    pp_mensual_completo = read.csv(ruta_archivo_pp_concatenado)
    temp_mensual_completo = read.csv(ruta_archivo_temp_concatenado)

    # Juntar todos los archivos en un solo dataframe
    pp_temp_mensual_completo <- merge(pp_mensual_completo, temp_mensual_completo, by = c("Year", "Month", "Codigo_nacional"))

    # ordenar por codigo nacional, año y mes: CLAVE PARA CALCULAR SPEI
    pp_temp_mensual_completo <- pp_temp_mensual_completo %>%
      arrange(Codigo_nacional, Year, Month)

    # Calcular evapotranspiración y Balance Hídrico
    # Para eso necesitamos la latitud de cada estación
    if(tolower(fuente) == "dga") {
      metadata_path = paste0(directorio_base, "/BBDD/metadatos/DGA/pp_y_temp/estaciones_DGA_pp_y_temp.csv")
    } else if (tolower(fuente) == "dmc") {
      metadata_path = paste0(directorio_base, "/BBDD/metadatos/DMC/estaciones_DMC.csv")
    }
    metadata = read.csv(metadata_path)
    #encontrar la latitud de cada estación
    pp_temp_mensual_completo$lat <- metadata$LAT[match(pp_temp_mensual_completo$Codigo_nacional, metadata$Codigo_nacional)]
    #Calcular ETP y BH
    pp_temp_mensual_completo <- pp_temp_mensual_completo %>%
      group_by(Codigo_nacional) %>%
      rowwise() %>%
      mutate(ETP = hargreaves(Tmin = temp_min, Tmax = temp_max, lat = lat, Pre = pp_month, verbose = FALSE),
             BH = pp_month - ETP) %>%
      ungroup()

    # Calcular IPEE usando distribución Log-Logistic
    spei_dataframe <- pp_temp_mensual_completo %>%
      group_by(Codigo_nacional) %>%
      mutate(
        spei_1 = spei(BH, scale = 1, distribution = "log-Logistic", na.rm = TRUE, verbose = FALSE)$fitted,
        spei_3 = spei(BH, scale = 3, distribution = "log-Logistic", na.rm = TRUE, verbose = FALSE)$fitted,
        spei_6 = spei(BH, scale = 6, distribution = "log-Logistic", na.rm = TRUE, verbose = FALSE)$fitted,
        spei_12 = spei(BH, scale = 12, distribution = "log-Logistic", na.rm = TRUE, verbose = FALSE)$fitted,
        spei_24 = spei(BH, scale = 24, distribution = "log-Logistic", na.rm = TRUE, verbose = FALSE)$fitted
      ) %>%
      ungroup() %>%
      mutate(across(starts_with("spei"), ~pmax(pmin(., 5), -5)))

    fechas_archivo = obtener_fechas_archivo(ruta_archivo_pp_concatenado)
    # Guardar el resultado en el directorio correspondiente
    if(tolower(fuente) == "dga") {
      write.csv(spei_dataframe, paste0(directorio_base, "/BBDD/indicadores/IPEE/DGA/IPEE_DGA_1979_", fechas_archivo$ano_fin,"_", fechas_archivo$mes_fin,".csv"))
    } else if (tolower(fuente) == "dmc") {
      write.csv(spei_dataframe, paste0(directorio_base, "/BBDD/indicadores/IPEE/DMC/IPEE_DMC_1979_", fechas_archivo$ano_fin,"_", fechas_archivo$mes_fin,".csv"))
    }
    print(paste0("Indicadores IPEE para ", fuente, " calculados con éxito"))
    
    return(TRUE)

  }, error = function(e) {
    message(paste0("Error al calcular los indicadores IPEE para ", fuente, ": ", e$message))
    return(FALSE)
  })
  
  # Cargar archivos concatenados y completos
} 

calcular_indicador <- function(directorio_base, fuente, indicador, ano_actual, mes_ultimo) {
  if(tolower(indicador) == "ice" && tolower(fuente) == "dga"){
    archivo_caudal = paste0(directorio_base, "/BBDD/q/DGA/concatenado/q_DGA_mensual_1989_", ano_actual, "_", mes_ultimo, ".csv")
    calcular_ICE_DGA(directorio_base, archivo_caudal)
    eliminar_archivos_antiguos(paste0(directorio_base, "/BBDD/indicadores/ICE/DGA/"), ano_actual, mes_ultimo)
  }
  else if(tolower(indicador) == "ipe" && tolower(fuente) == "dga"){
    archivo_pp = paste0(directorio_base, "/BBDD/pp/DGA/concatenado/pp_DGA_mensual_1979_", ano_actual, "_", mes_ultimo, ".csv")
    calcular_IPE_DGA_DMC(directorio_base, archivo_pp, fuente)
    eliminar_archivos_antiguos(paste0(directorio_base, "/BBDD/indicadores/IPE/DGA/"), ano_actual, mes_ultimo)
  }
  else if(tolower(indicador) == "ipe" && tolower(fuente) == "dmc"){
    archivo_pp = paste0(directorio_base, "/BBDD/pp/DMC/concatenado/pp_DMC_mensual_1979_", ano_actual, "_", mes_ultimo, ".csv")
    calcular_IPE_DGA_DMC(directorio_base, archivo_pp, fuente)
    eliminar_archivos_antiguos(paste0(directorio_base, "/BBDD/indicadores/IPE/DMC/"), ano_actual, mes_ultimo)
  }
  else if(tolower(indicador) == "ipee" && tolower(fuente) == "dga"){
    archivo_pp = paste0(directorio_base, "/BBDD/pp/DGA/concatenado/pp_DGA_mensual_1979_", ano_actual, "_", mes_ultimo, ".csv")
    archivo_temp = paste0(directorio_base, "/BBDD/temp/DGA/concatenado/temp_DGA_mensual_1979_", ano_actual, "_", mes_ultimo, ".csv")
    calcular_IPEE_DGA_DMC(directorio_base, archivo_pp, archivo_temp, fuente)
    eliminar_archivos_antiguos(paste0(directorio_base, "/BBDD/indicadores/IPEE/DGA/"), ano_actual, mes_ultimo)
  }
  else if(tolower(indicador) == "ipee" && tolower(fuente) == "dmc"){
    archivo_pp = paste0(directorio_base, "/BBDD/pp/DMC/concatenado/pp_DMC_mensual_1979_", ano_actual, "_", mes_ultimo, ".csv")
    archivo_temp = paste0(directorio_base, "/BBDD/temp/DMC/concatenado/temp_DMC_mensual_1979_", ano_actual, "_", mes_ultimo, ".csv")
    calcular_IPEE_DGA_DMC(directorio_base, archivo_pp, archivo_temp, fuente)
    eliminar_archivos_antiguos(paste0(directorio_base, "/BBDD/indicadores/IPEE/DMC/"), ano_actual, mes_ultimo)
  }
}
