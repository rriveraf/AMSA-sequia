
procesar_caudal_DGA <- function(directorio_base, ruta_archivo_datos, ruta_archivo_metadatos, completitud = 0.7) {
 
    print("Inicio de depuración y rellenado de datos de caudal para fuente DGA") 
  # try catch Cargar datos
    tryCatch({
        caudal_DGA <- read.csv(ruta_archivo_datos)
        metadatos_caudal <- read.csv(ruta_archivo_metadatos)
    }, error = function(e) {
        cat("Error al cargar los archivos: ", e$message, "\n")
        return(NULL)
    })
  # Procesamiento de datos
  caudal_DGA$caudal_mean[caudal_DGA$caudal_mean < 0] <- NA
  caudal_DGA <- na.omit(caudal_DGA)
  caudal_DGA <- unique(caudal_DGA)

  # Conteo de mediciones por estación
  count_estaciones <- caudal_DGA %>% group_by(Codigo_nacional) %>% summarise(n = n()) %>% arrange(desc(n))

  # Fechas y filtrado por completitud
  fechas_archivo <- obtener_fechas_archivo(ruta_archivo_datos)
  n_minimo_datos <- ((fechas_archivo$ano_fin - fechas_archivo$ano_ini) * 365 + fechas_archivo$mes_fin * 30) * completitud
  estaciones_validas <- count_estaciones %>% filter(n >= n_minimo_datos)
  caudal_DGA_validas <- caudal_DGA %>% filter(Codigo_nacional %in% estaciones_validas$Codigo_nacional)

  # Relleno de fechas y datos faltantes
  caudal_DGA_relleno <- rellenar_fechas(caudal_DGA_validas) %>%
    select(Year, Month, Day, Codigo_nacional, caudal_mean)

  # ########### COMIENZA RELLENADO DE DATOS FALTANTES ###########
  # PASO 1: rellenamos los valores faltantes con la mediana mensual de la estación
  caudal_DGA_relleno <- caudal_DGA_relleno %>%
    group_by(Codigo_nacional, Year, Month) %>%
    mutate(caudal_mean = ifelse(is.na(caudal_mean), ave(caudal_mean, FUN = function(x) median(x, na.rm = TRUE)), caudal_mean)) %>%
    ungroup()

  # PASO 2: rellenamos con mediana anual de la estación
  caudal_DGA_relleno <- caudal_DGA_relleno %>%
    group_by(Codigo_nacional, Month) %>%
    mutate(caudal_mean = ifelse(is.na(caudal_mean), ave(caudal_mean, FUN = function(x) median(x, na.rm = TRUE)), caudal_mean)) %>%
    ungroup()

  # PASO 3: rellenamos con la mediana global, por seguridad si existe algún valor NA
  caudal_DGA_relleno <- caudal_DGA_relleno %>%
    group_by(Codigo_nacional) %>%
    mutate(caudal_mean = ifelse(is.na(caudal_mean), median(caudal_mean, na.rm = TRUE), caudal_mean)) %>%
    ungroup()

  # Guardar datos depurados
  tryCatch({
    write.csv(caudal_DGA_relleno, paste0(directorio_base, "/BBDD/q/DGA/depurado/q_mean_DGA_", fechas_archivo$ano_ini, "_", fechas_archivo$ano_fin, "_", fechas_archivo$mes_fin, ".csv"), row.names = FALSE)
    print("Fin de depuración y rellenado de datos de caudal para fuente DGA")
    return(TRUE)
  }, error = function(e) {
    cat("Error al guardar los archivos: ", e$message, "\n")
    return(FALSE)
  })
  
}
procesar_pp_DGA_DMC <- function(directorio_base, ruta_archivo_datos, ruta_archivo_metadatos, completitud = 0.7, tolerancia_km = 50, fuente) {
  
  print(paste0("Inicio de depuración y rellenado de datos de precipitación para fuente ", fuente))
  print(paste0("Tiempo de espera estimado para esta etapa: 1 minuto"))
  
  if(tolower(fuente) != "dga" && tolower(fuente) != "dmc") {
    stop("La fuente debe ser 'DGA' o 'DMC'")
  }
  # Intentar cargar los datos y manejar errores potenciale
  tryCatch({
     pp_DGA_DMC <- read.csv(ruta_archivo_datos)
     metadatos_pp <- read.csv(ruta_archivo_metadatos)
   }, error = function(e) {
     cat("Error al cargar los archivos: ", e$message, "\n")
     return(NULL)
   })

  # Marcar y limpiar outliers e inválidos
  pp_DGA_DMC$pp_day[pp_DGA_DMC$pp_day < 0 | pp_DGA_DMC$pp_day >= 500] <- NA
  pp_DGA_DMC <- drop_na(pp_DGA_DMC)
  pp_DGA_DMC <- unique(pp_DGA_DMC)

  # Conteo de mediciones por estación
  count_estaciones <- pp_DGA_DMC %>% group_by(Codigo_nacional) %>% summarise(n = n()) %>% arrange(desc(n))

  # Fechas y filtrado por completitud
  fechas_archivo <- obtener_fechas_archivo(ruta_archivo_datos)
  n_minimo_datos = ((fechas_archivo$ano_fin - fechas_archivo$ano_ini) * 365 + fechas_archivo$mes_fin * 30) * completitud
  estaciones_validas <- count_estaciones %>% filter(n >= n_minimo_datos)
  pp_DGA_DMC_validas <- pp_DGA_DMC %>% filter(Codigo_nacional %in% estaciones_validas$Codigo_nacional)



  # Relleno de fechas y datos faltantes
  pp_DGA_DMC_relleno <- rellenar_fechas(pp_DGA_DMC_validas) %>%
    select(Year, Month, Day, Codigo_nacional, pp_day)


  # PASO 1: Imputacion de vecinos cercanos
  # Creación de tuplas para la imputación de vecinos cercanos
  tuplas_estaciones <- data.frame(Codigo_nacional = unique(pp_DGA_DMC_relleno$Codigo_nacional), mas_cercana = NA)
  
  #si la fuente de datos es dmc, acotamos los metadatos a las estaciones validas, para asi no tener problemas con la imputacion
  if(tolower(fuente) == "dmc"){
    metadatos_pp <- metadatos_pp %>% filter(Codigo_nacional %in% estaciones_validas$Codigo_nacional)
  }
  tuplas_estaciones$mas_cercana <- sapply(tuplas_estaciones$Codigo_nacional, function(x) estacion_mas_cercana(x, metadatos_pp, tolerancia_km))

  # Imputación de vecinos cercanos
  for (i in 1:nrow(pp_DGA_DMC_relleno)) {
    if (is.na(pp_DGA_DMC_relleno$pp_day[i])) {
      registro <- pp_DGA_DMC_relleno[i,]
      estacion_mas_cercana <- tuplas_estaciones %>% filter(Codigo_nacional == registro$Codigo_nacional)
      imputacion_pp <- obtener_valor(pp_DGA_DMC_relleno, estacion_mas_cercana$mas_cercana, registro$Year, registro$Month, registro$Day, "pp_day")
      pp_DGA_DMC_relleno$pp_day[i] <- imputacion_pp
    }
  }
  # PASO 2: rellenamos los valores faltantes con la mediana mensual de la estación
  pp_DGA_DMC_relleno <- pp_DGA_DMC_relleno %>%
    group_by(Codigo_nacional, Year, Month) %>%
    mutate(pp_day = ifelse(is.na(pp_day), ave(pp_day, FUN = function(x) median(x, na.rm = TRUE)), pp_day)) %>%
    ungroup()

  # PASO 3: ahora rellenamos los valores faltantes con la mediana anual de la estación
  pp_DGA_DMC_relleno <- pp_DGA_DMC_relleno %>%
    group_by(Codigo_nacional, Year) %>%
    mutate(pp_day = ifelse(is.na(pp_day), median(pp_day, na.rm = TRUE), pp_day)) %>%
    ungroup()

  # PASO 4: Rellenar la mediana global de la estacion, por seguridad si existe algun valor NA
  pp_DGA_DMC_relleno <- pp_DGA_DMC_relleno %>%
    group_by(Codigo_nacional) %>%
    mutate(pp_day = ifelse(is.na(pp_day), median(pp_day, na.rm = TRUE), pp_day)) %>%
    ungroup()

  print(summary(pp_DGA_DMC_relleno))

  tryCatch({
    if(tolower(fuente) == "dga") {
        write.csv(pp_DGA_DMC_relleno, paste0(directorio_base, "/BBDD/pp/DGA/depurado/pp_DGA_", fechas_archivo$ano_ini, "_", fechas_archivo$ano_fin, "_", fechas_archivo$mes_fin, ".csv"), row.names = FALSE)
    } else if(tolower(fuente) == "dmc") {
        write.csv(pp_DGA_DMC_relleno, paste0(directorio_base, "/BBDD/pp/DMC/depurado/pp_DMC_", fechas_archivo$ano_ini, "_", fechas_archivo$ano_fin, "_", fechas_archivo$mes_fin, ".csv"), row.names = FALSE)
    }
    print(paste0("Fin de depuración y rellenado de datos de precipitación para fuente ", fuente))
    
    return(TRUE)
}, error = function(e) {
    cat("Error al guardar los archivos: ", e$message, "\n")
    return(FALSE)
})
}
procesar_temp_DGA_DMC <- function(directorio_base, ruta_archivo_datos, ruta_archivo_metadatos, completitud = 0.7, tolerancia_km = 50, fuente) {
  print(paste0("Inicio de depuración y rellenado de datos de temperatura para fuente ", fuente))
  print(paste0("Tiempo de espera estimado para esta etapa: 10 minutos"))
  # try catch Cargar datos
  tryCatch({
      temp_DGA_DMC <- read.csv(ruta_archivo_datos)
      metadatos_temp <- read.csv(ruta_archivo_metadatos)
  }, error = function(e) {
      cat("Error al cargar los archivos: ", e$message, "\n")
      return(NULL)
  })

  # Marcar outliers
  temp_DGA_DMC$temp_min[temp_DGA_DMC$temp_min <= -20 | temp_DGA_DMC$temp_min >= 42] <- NA
  temp_DGA_DMC$temp_max[temp_DGA_DMC$temp_max <= -20 | temp_DGA_DMC$temp_max >= 42] <- NA

  # Limpiar datos
  temp_DGA_DMC <- drop_na(temp_DGA_DMC)
  temp_DGA_DMC <- unique(temp_DGA_DMC)

  # Conteo de mediciones por estación
  count_estaciones <- temp_DGA_DMC %>% group_by(Codigo_nacional) %>% summarise(n = n())
  
  # Fechas y filtrado por completitud
  fechas_archivo <- obtener_fechas_archivo(ruta_archivo_datos)
  n_minimo_datos = ((fechas_archivo$ano_fin - fechas_archivo$ano_ini) * 365 + fechas_archivo$mes_fin*30) * completitud
  estaciones_validas <- count_estaciones %>% filter(n >= n_minimo_datos)
  temp_DGA_DMC_validas <- temp_DGA_DMC %>% filter(Codigo_nacional %in% estaciones_validas$Codigo_nacional)
  
  print("llegue hasta aqui")
  # Relleno de fechas faltantes
  temp_DGA_DMC_relleno <- rellenar_fechas(temp_DGA_DMC_validas)
  temp_DGA_DMC_relleno <- temp_DGA_DMC_relleno %>%
    select(Year, Month, Day, Codigo_nacional, temp_min, temp_max)
  
  #print("llegue hasta aqui")

  # PASO 1: Imputación de vecinos cercanos
  tuplas_estaciones <- data.frame(Codigo_nacional = unique(temp_DGA_DMC_relleno$Codigo_nacional), mas_cercana = NA)
  tuplas_estaciones$mas_cercana <- sapply(tuplas_estaciones$Codigo_nacional, function(x) estacion_mas_cercana(x, metadatos_temp, tolerancia_km))
  for (i in 1:nrow(temp_DGA_DMC_relleno)) {
    if (is.na(temp_DGA_DMC_relleno$temp_min[i])) {
      registro <- temp_DGA_DMC_relleno[i,]
      estacion_mas_cercana <- tuplas_estaciones %>% filter(Codigo_nacional == registro$Codigo_nacional)
      imputacion_temp_min <- obtener_valor(temp_DGA_DMC_relleno, estacion_mas_cercana$mas_cercana, registro$Year, registro$Month, registro$Day, "temp_min")
      imputacion_temp_max <- obtener_valor(temp_DGA_DMC_relleno, estacion_mas_cercana$mas_cercana, registro$Year, registro$Month, registro$Day, "temp_max")
      temp_DGA_DMC_relleno$temp_min[i] <- imputacion_temp_min
      temp_DGA_DMC_relleno$temp_max[i] <- imputacion_temp_max
    }
  }

  # PASO 2: Rellenar con el promedio mensual de la estación
  temp_DGA_DMC_relleno <- temp_DGA_DMC_relleno %>%
    group_by(Codigo_nacional, Year, Month) %>%
    mutate(
      temp_min = ifelse(is.na(temp_min), ave(temp_min, FUN = function(x) mean(x, na.rm = TRUE)), temp_min),
      temp_max = ifelse(is.na(temp_max), ave(temp_max, FUN = function(x) mean(x, na.rm = TRUE)), temp_max)
    ) %>%
    ungroup()

  # PASO 3: Rellenar con el promedio anual de la estación
  temp_DGA_DMC_relleno <- temp_DGA_DMC_relleno %>%
    group_by(Codigo_nacional, Year) %>%
    mutate(
      temp_min = ifelse(is.na(temp_min), mean(temp_min, na.rm = TRUE), temp_min),
      temp_max = ifelse(is.na(temp_max), mean(temp_max, na.rm = TRUE), temp_max)
    ) %>%
    ungroup()

  # PASO 4: Rellenar con el promedio global de la estacion, por seguridad si algun existiese algun valor NA
  temp_DGA_DMC_relleno <- temp_DGA_DMC_relleno %>%
    group_by(Codigo_nacional) %>%
    mutate(
      temp_min = ifelse(is.na(temp_min), mean(temp_min, na.rm = TRUE), temp_min),
      temp_max = ifelse(is.na(temp_max), mean(temp_max, na.rm = TRUE), temp_max)
    ) %>%
    ungroup()

  # Guardar datos depurados
  tryCatch({
    if(tolower(fuente) == "dga") {
        write.csv(temp_DGA_DMC_relleno, paste0(directorio_base, "/BBDD/temp/DGA/depurado/temp_DGA_", fechas_archivo$ano_ini, "_", fechas_archivo$ano_fin, "_", fechas_archivo$mes_fin, ".csv"), row.names = FALSE)
    } else if(tolower(fuente) == "dmc") {
        write.csv(temp_DGA_DMC_relleno, paste0(directorio_base, "/BBDD/temp/DMC/depurado/temp_DMC_", fechas_archivo$ano_ini, "_", fechas_archivo$ano_fin, "_", fechas_archivo$mes_fin, ".csv"), row.names = FALSE)
    }
    print(paste0("Fin de depuración y rellenado de datos de temperatura para fuente ", fuente))
    return(TRUE)
}, error = function(e) {
    cat("Error al guardar los archivos: ", e$message, "\n")
    return(FALSE)
})
}

