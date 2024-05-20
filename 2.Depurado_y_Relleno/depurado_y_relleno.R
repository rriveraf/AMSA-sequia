
procesar_caudal_DGA <- function(directorio_base, archivo_datos, archivo_metadatos, completitud = 0.7) {
  source(paste0(directorio_base, "/funciones_de_utilidad.R"))
  
  # try catch Cargar datos
    tryCatch({
        caudal_DGA <- read.csv(paste0(directorio_base, archivo_datos))
        metadatos_caudal <- read.csv(paste0(directorio_base, archivo_metadatos))
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
  fechas_archivo <- obtener_fechas_archivo(paste0(directorio_base, archivo_datos))
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
  write.csv(caudal_DGA_relleno, paste0(directorio_base, "/BBDD/q/DGA/depurado/q_mean_DGA_", fechas_archivo$ano_ini, "_", fechas_archivo$ano_fin, "_", fechas_archivo$mes_fin, ".csv"), row.names = FALSE)
}
procesar_pp_DGA_DMC <- function(directorio_base, archivo_datos, archivo_metadatos, completitud = 0.7, tolerancia_km = 50, fuente) {
  source(paste0(directorio_base, "/funciones_de_utilidad.R"))
  
  if(tolower(fuente) != "dga" && tolower(fuente) != "dmc") {
    stop("La fuente debe ser 'DGA' o 'DMC'")
  }
  # Intentar cargar los datos y manejar errores potenciale
  tryCatch({
     pp_DGA_DMC <- read.csv(paste0(directorio_base, archivo_datos))
     metadatos_pp <- read.csv(paste0(directorio_base, archivo_metadatos))
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
  fechas_archivo <- obtener_fechas_archivo(paste0(directorio_base, archivo_datos))
  n_minimo_datos = ((fechas_archivo$ano_fin - fechas_archivo$ano_ini) * 365 + fechas_archivo$mes_fin * 30) * completitud
  estaciones_validas <- count_estaciones %>% filter(n >= n_minimo_datos)
  pp_DGA_DMC_validas <- pp_DGA_DMC %>% filter(Codigo_nacional %in% estaciones_validas$Codigo_nacional)

  # Relleno de fechas y datos faltantes
  pp_DGA_DMC_relleno <- rellenar_fechas(pp_DGA_DMC_validas) %>%
    select(Year, Month, Day, Codigo_nacional, pp_day)

  # PASO 1: Imputacion de vecinos cercanos
  # Creación de tuplas para la imputación de vecinos cercanos
  tuplas_estaciones <- data.frame(Codigo_nacional = unique(pp_DGA_DMC_relleno$Codigo_nacional), mas_cercana = NA)
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

  if(tolower(fuente) == "dga") {
    # Guardar datos depurados
    write.csv(pp_DGA_DMC_relleno, paste0(directorio_base, "/BBDD/pp/DGA/depurado/pp_DGA_", fechas_archivo$ano_ini, "_", fechas_archivo$ano_fin, "_", fechas_archivo$mes_fin, ".csv"), row.names = FALSE)
  } else if (tolower(fuente) == "dmc"){
    # Guardar datos depurados
    write.csv(pp_DGA_DMC_relleno, paste0(directorio_base, "/BBDD/pp/DMC/depurado/pp_DMC_", fechas_archivo$ano_ini, "_", fechas_archivo$ano_fin, "_", fechas_archivo$mes_fin, ".csv"), row.names = FALSE)
  }
  # Guardar datos depurados
}
procesar_temp_DGA <- function(directorio_base, archivo_datos, archivo_metadatos, completitud = 0.7, tolerancia_km = 50) {
  source(paste0(directorio_base, "/funciones_de_utilidad.R"))

  # try catch Cargar datos
  tryCatch({
      temp_DGA <- read.csv(paste0(directorio_base, archivo_datos))
      metadatos_temp <- read.csv(paste0(directorio_base, archivo_metadatos))
  }, error = function(e) {
      cat("Error al cargar los archivos: ", e$message, "\n")
      return(NULL)
  })

  # Marcar outliers
  temp_DGA$temp_min[temp_DGA$temp_min <= -20 | temp_DGA$temp_min >= 42] <- NA
  temp_DGA$temp_max[temp_DGA$temp_max <= -20 | temp_DGA$temp_max >= 42] <- NA

  # Limpiar datos
  temp_DGA <- drop_na(temp_DGA)
  temp_DGA <- unique(temp_DGA)

  # Conteo de mediciones por estación
  count_estaciones <- temp_DGA %>% group_by(Codigo_nacional) %>% summarise(n = n())

  # Fechas y filtrado por completitud
  fechas_archivo <- obtener_fechas_archivo(paste0(directorio_base, archivo_datos))
  n_minimo_datos = ((fechas_archivo$ano_fin - fechas_archivo$ano_ini) * 365 + fechas_archivo$mes_fin*30) * completitud
  estaciones_validas <- count_estaciones %>% filter(n >= n_minimo_datos)
  temp_DGA_validas <- temp_DGA %>% filter(Codigo_nacional %in% estaciones_validas$Codigo_nacional)

  # Relleno de fechas faltantes
  temp_DGA_relleno <- rellenar_fechas(temp_DGA_validas)
  temp_DGA_relleno <- temp_DGA_relleno %>%
    select(Year, Month, Day, Codigo_nacional, temp_min, temp_max)

  # PASO 1: Imputación de vecinos cercanos
  tuplas_estaciones <- data.frame(Codigo_nacional = unique(temp_DGA_relleno$Codigo_nacional), mas_cercana = NA)
  tuplas_estaciones$mas_cercana <- sapply(tuplas_estaciones$Codigo_nacional, function(x) estacion_mas_cercana(x, metadatos_temp, tolerancia_km))
  for (i in 1:nrow(temp_DGA_relleno)) {
    if (is.na(temp_DGA_relleno$temp_min[i])) {
      registro <- temp_DGA_relleno[i,]
      estacion_mas_cercana <- tuplas_estaciones %>% filter(Codigo_nacional == registro$Codigo_nacional)
      imputacion_temp_min <- obtener_valor(temp_DGA_relleno, estacion_mas_cercana$mas_cercana, registro$Year, registro$Month, registro$Day, "temp_min")
      imputacion_temp_max <- obtener_valor(temp_DGA_relleno, estacion_mas_cercana$mas_cercana, registro$Year, registro$Month, registro$Day, "temp_max")
      temp_DGA_relleno$temp_min[i] <- imputacion_temp_min
      temp_DGA_relleno$temp_max[i] <- imputacion_temp_max
    }
  }

  # PASO 2: Rellenar con el promedio mensual de la estación
  temp_DGA_relleno <- temp_DGA_relleno %>%
    group_by(Codigo_nacional, Year, Month) %>%
    mutate(
      temp_min = ifelse(is.na(temp_min), ave(temp_min, FUN = function(x) mean(x, na.rm = TRUE)), temp_min),
      temp_max = ifelse(is.na(temp_max), ave(temp_max, FUN = function(x) mean(x, na.rm = TRUE)), temp_max)
    ) %>%
    ungroup()

  # PASO 3: Rellenar con el promedio anual de la estación
  temp_DGA_relleno <- temp_DGA_relleno %>%
    group_by(Codigo_nacional, Year) %>%
    mutate(
      temp_min = ifelse(is.na(temp_min), mean(temp_min, na.rm = TRUE), temp_min),
      temp_max = ifelse(is.na(temp_max), mean(temp_max, na.rm = TRUE), temp_max)
    ) %>%
    ungroup()

  # PASO 4: Rellenar con el promedio global de la estacion, por seguridad si algun existiese algun valor NA
  temp_DGA_relleno <- temp_DGA_relleno %>%
    group_by(Codigo_nacional) %>%
    mutate(
      temp_min = ifelse(is.na(temp_min), mean(temp_min, na.rm = TRUE), temp_min),
      temp_max = ifelse(is.na(temp_max), mean(temp_max, na.rm = TRUE), temp_max)
    ) %>%
    ungroup()

  # Guardar datos depurados
  write.csv(temp_DGA_relleno, paste0(directorio_base, "/BBDD/temp/DGA/depurado/temp_DGA_", fechas_archivo$ano_ini, "_", fechas_archivo$ano_fin, "_", fechas_archivo$mes_fin, ".csv"), row.names = FALSE)
}
procesar_temp_DMC <- function(directorio_base, archivo_datos_min, archivo_datos_max, archivo_metadatos, completitud = 0.7, tolerancia_km = 50) {
  source(paste0(directorio_base, "/funciones_de_utilidad.R"))

  # Intentar cargar los datos y manejar errores potenciales
  tryCatch({
    temp_DMC_min <- read.csv(paste0(directorio_base, archivo_datos_min))
    temp_DMC_max <- read.csv(paste0(directorio_base, archivo_datos_max))
    metadatos_temp <- read.csv(paste0(directorio_base, archivo_metadatos))
  }, error = function(e) {
    cat("Error al cargar los archivos: ", e$message, "\n")
    return(NULL)  # Detiene la ejecución y devuelve NULL si hay error
  })

  # Combinar archivos de temperatura mínima y máxima
  temp_DMC <- merge(temp_DMC_min, temp_DMC_max, by = c("Year", "Month", "Day", "Codigo_nacional"))

  # Marcar outliers
  temp_DMC$temp_min[temp_DMC$temp_min <= -20 | temp_DMC$temp_min >= 42] <- NA
  temp_DMC$temp_max[temp_DMC$temp_max <= -20 | temp_DMC$temp_max >= 42] <- NA

  # Limpiar datos
  temp_DMC <- drop_na(temp_DMC)
  temp_DMC <- unique(temp_DMC)

  # Conteo de mediciones por estación y filtrado por completitud
  count_estaciones <- temp_DMC %>% group_by(Codigo_nacional) %>% summarise(n = n())
  fechas_archivo <- obtener_fechas_archivo(paste0(directorio_base, archivo_datos_min))  # Asumiendo la misma estructura de fecha para ambos archivos
  n_minimo_datos = ((fechas_archivo$ano_fin - fechas_archivo$ano_ini) * 365 + fechas_archivo$mes_fin * 30) * completitud
  estaciones_validas <- count_estaciones %>% filter(n >= n_minimo_datos)
  temp_DMC_validas <- temp_DMC %>% filter(Codigo_nacional %in% estaciones_validas$Codigo_nacional)

  # Rellenar fechas faltantes
  temp_DMC_relleno <- rellenar_fechas(temp_DMC_validas)
  temp_DMC_relleno <- temp_DMC_relleno %>% select(Year, Month, Day, Codigo_nacional, temp_min, temp_max)

  # PASO 1: Imputación de vecinos cercanos
  tuplas_estaciones <- data.frame(Codigo_nacional = unique(temp_DMC_relleno$Codigo_nacional), mas_cercana = NA)
  tuplas_estaciones$mas_cercana <- sapply(tuplas_estaciones$Codigo_nacional, function(x) estacion_mas_cercana(x, metadatos_temp, tolerancia_km))
  for (i in 1:nrow(temp_DMC_relleno)) {
    if (is.na(temp_DMC_relleno$temp_min[i]) | is.na(temp_DMC_relleno$temp_max[i])) {
      registro <- temp_DMC_relleno[i,]
      estacion_mas_cercana <- tuplas_estaciones$mas_cercana[tuplas_estaciones$Codigo_nacional == registro$Codigo_nacional]
      temp_DMC_relleno$temp_min[i] <- obtener_valor(temp_DMC_relleno, estacion_mas_cercana, registro$Year, registro$Month, registro$Day, "temp_min")
      temp_DMC_relleno$temp_max[i] <- obtener_valor(temp_DMC_relleno, estacion_mas_cercana, registro$Year, registro$Month, registro$Day, "temp_max")
    }
  }

  # PASO 2: Imputación de media mensual de la estación
  temp_DMC_relleno <- temp_DMC_relleno %>%
    group_by(Codigo_nacional, Year, Month) %>%
    mutate(
      temp_min = ifelse(is.na(temp_min), mean(temp_min, na.rm = TRUE), temp_min),
      temp_max = ifelse(is.na(temp_max), mean(temp_max, na.rm = TRUE), temp_max)
    ) %>%
    ungroup()

  # PASO 3: Imputación con promedio anual de la estación
  temp_DMC_relleno <- temp_DMC_relleno %>%
    group_by(Codigo_nacional, Year) %>%
    mutate(
      temp_min = ifelse(is.na(temp_min), mean(temp_min, na.rm = TRUE), temp_min),
      temp_max = ifelse(is.na(temp_max), mean(temp_max, na.rm = TRUE), temp_max)
    ) %>%
    ungroup()

  # PASO 4: Imputación con promedio general de la estacion, por seguridad si existe algun valor NA
  temp_DMC_relleno <- temp_DMC_relleno %>%
    group_by(Codigo_nacional) %>%
    mutate(
      temp_min = ifelse(is.na(temp_min), mean(temp_min, na.rm = TRUE), temp_min),
      temp_max = ifelse(is.na(temp_max), mean(temp_max, na.rm = TRUE), temp_max)
    ) %>%
    ungroup()

  # Guardar los datos procesados
  write.csv(temp_DMC_relleno, paste0(directorio_base, "/BBDD/temp/DMC/depurado/temp_DMC_", fechas_archivo$ano_ini, "_", fechas_archivo$ano_fin, "_", fechas_archivo$mes_fin, ".csv"), row.names = FALSE)
}
