calcular_ICE_DGA <- function(directorio_base, archivo_caudal_depurado, archivo_metadatos) {
  source(paste0(directorio_base, "/funciones_de_utilidad.R"))
  # Cargar datos históricos fijos y datos actuales
  archivo_historico <- "/BBDD/q/DGA/periodo_historico/q_DGA_monthly_1989_2019.csv"
  caudal_mensual_1989_2019 <- read.csv(paste0(directorio_base, archivo_historico))
  caudal_day_2020_actual <- read.csv(paste0(directorio_base, archivo_caudal_depurado))
  metadatos_temp <- read.csv(paste0(directorio_base, archivo_metadatos))  # Si es necesario

  # Calcular caudal mensual para los datos actuales
  caudal_mensual_2020_actual <- caudal_day_2020_actual %>%
    group_by(Year, Month, Codigo_nacional) %>%
    summarise(q_month = mean(caudal_mean, na.rm = TRUE)) %>%
    ungroup()

  # Filtrar estaciones que existen en los datos históricos
  estaciones_historicas <- unique(caudal_mensual_1989_2019$Codigo_nacional)
  caudal_mensual_2020_actual <- caudal_mensual_2020_actual[caudal_mensual_2020_actual$Codigo_nacional %in% estaciones_historicas, ]

  # Combinar datos históricos y actuales
  caudal_mensual_completo <- rbind(caudal_mensual_1989_2019, caudal_mensual_2020_actual)

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

  # Calcular ICE
  ice_dataframe <- caudal_mensual_completo %>%
    group_by(Codigo_nacional) %>%
    mutate(
      ice_1 = spi(q_month, scale = 1, distribution = "Gamma", na.rm = TRUE, verbose = FALSE)$fitted,
      ice_3 = spi(q_3month, scale = 1, distribution = "Gamma", na.rm = TRUE, verbose = FALSE)$fitted,
      ice_6 = spi(q_6month, scale = 1, distribution = "Gamma", na.rm = TRUE, verbose = FALSE)$fitted,
      ice_12 = spi(q_12month, scale = 1, distribution = "Gamma", na.rm = TRUE, verbose = FALSE)$fitted,
      ice_24 = spi(q_24month, scale = 1, distribution = "Gamma", na.rm = TRUE, verbose = FALSE)$fitted
    ) %>%
    ungroup()
  
  fechas_archivo = obtener_fechas_archivo(archivo_caudal_depurado)
  # Guardar el dataframe de ICE
  write.csv(ice_dataframe, paste0(directorio_base, "/BBDD/indicadores/ICE/DGA/ICE_DGA_1989_", fechas_archivo$ano_fin,"_", fechas_archivo$mes_fin,".csv"))

  # Retorna el dataframe para verificación en consola o para manipulación adicional
  return(ice_dataframe)
}
calcular_IPE_DGA_DMC <- function(directorio_base, archivo_pp_depurado, fuente) {
  source(paste0(directorio_base, "/funciones_de_utilidad.R"))
  # Verificación de la fuente de datos
  if(tolower(fuente) != "dga" && tolower(fuente) != "dmc") {
    stop("La fuente debe ser 'DGA' o 'DMC'")
  }
  # Cargar archivo histórico según la fuente
  archivo_historico <- if(tolower(fuente) == "dga") {
    "/BBDD/pp/DGA/periodo_historico/pp_DGA_monthly_1979_2019_cr2met.csv"
  } else {
    "/BBDD/pp/DMC/periodo_historico/pp_DMC_monthly_1979_2019_cr2met.csv"
  }

  # Cargar datos históricos (1979-2019) y actuales(2020-hoy)
  pp_mensual_historico <- read.csv(paste0(directorio_base, archivo_historico))
  pp_day_actual <- read.csv(paste0(directorio_base, archivo_pp_depurado))
  
  # Renombrar la columna de precipitación si es necesario
  names(pp_mensual_historico)[names(pp_mensual_historico) == "precipitacion_mensual"] <- "pp_month"
  
  # Calcular precipitación mensual para los datos actuales
  pp_mensual_actual <- pp_day_actual %>%
    group_by(Year, Month, Codigo_nacional) %>%
    summarise(pp_month = sum(pp_day, na.rm = TRUE)) %>%
    ungroup()

  # Combinar datos históricos y actuales
  pp_mensual_completo <- rbind(pp_mensual_historico, pp_mensual_actual)

  # Corrección para cero precipitaciones
  pp_mensual_completo$pp_month[pp_mensual_completo$pp_month == 0] <- 0.0001
  
  # Ordenar datos por código nacional, año y mes: CLAVE PARA CALCULAR SPI
  pp_mensual_completo <- pp_mensual_completo %>%
    arrange(Codigo_nacional, Year, Month)

  # Calcular SPI para varios períodos
  spi_dataframe <- pp_mensual_completo %>%
    group_by(Codigo_nacional) %>%
    mutate(
      spi_1 = spi(pp_month, scale = 1, distribution = "Gamma", na.rm = TRUE, verbose = FALSE)$fitted,
      spi_3 = spi(pp_month, scale = 3, distribution = "Gamma", na.rm = TRUE, verbose = FALSE)$fitted,
      spi_6 = spi(pp_month, scale = 6, distribution = "Gamma", na.rm = TRUE, verbose = FALSE)$fitted,
      spi_12 = spi(pp_month, scale = 12, distribution = "Gamma", na.rm = TRUE, verbose = FALSE)$fitted,
      spi_24 = spi(pp_month, scale = 24, distribution = "Gamma", na.rm = TRUE, verbose = FALSE)$fitted
    ) %>%
    ungroup()
  
  fechas_archivo = obtener_fechas_archivo(archivo_pp_depurado)
  # Determinar el directorio de salida basado en la fuente
  if(tolower(fuente) == "dga") {
    write.csv(spi_dataframe, paste0(directorio_base, "/BBDD/indicadores/IPE/DGA/IPE_DGA_1979_", fechas_archivo$ano_fin,"_", fechas_archivo$mes_fin,".csv"))
  } else if (tolower(fuente) == "dmc"){
    write.csv(spi_dataframe, paste0(directorio_base, "/BBDD/indicadores/IPE/DMC/IPE_DMC_1979_", fechas_archivo$ano_fin,"_", fechas_archivo$mes_fin,".csv"))
  }
  
  # Retorna el dataframe para verificación en consola o manipulación adicional
  return(spi_dataframe)
}
calcular_IPEE_DGA_DMC <- function(directorio_base, archivo_pp_depurado, archivo_temp_depurado, fuente) {
  source(paste0(directorio_base, "/funciones_de_utilidad.R"))

  if(tolower(fuente) != "dga" && tolower(fuente) != "dmc") {
    stop("La fuente debe ser 'DGA' o 'DMC'")
  }
  if(tolower(fuente) == "dga"){
    archivo_historico_pp <- "/BBDD/pp/DGA/periodo_historico/pp_DGA_monthly_1979_2019_cr2met.csv"
    metadata_path <- "/BBDD/metadatos/DGA/pp_y_temp/estaciones_DGA_pp_y_temp.csv"
  }
  else if (tolower(fuente) == "dmc"){
    archivo_historico_pp <- "/BBDD/pp/DMC/periodo_historico/pp_DMC_monthly_1979_2019_cr2met.csv"
    metadata_path <- "/BBDD/metadatos/DMC/estaciones_DMC.csv"
  }

  # Cargar archivos históricos basados en la fuente
  pp_mensual_historico <- read.csv(archivo_historico_pp)
  temp_mensual_historico <- read.csv(archivo_historico_temp)

  # Cargar datos actuales de precipitación y temperatura
  pp_day_actual <- read.csv(paste0(directorio_base, archivo_pp_depurado))
  temp_day_actual <- read.csv(paste0(directorio_base, archivo_temp_depurado))
  
  # Calcular precipitación mensual
  pp_mensual_actual <- pp_day_actual %>%
    group_by(Year, Month, Codigo_nacional) %>%
    summarise(pp_month = sum(pp_day, na.rm = TRUE)) %>%
    ungroup()
  #calcular temperaturas extremas mensuales
  temp_mensual_actual <- temp_day_actual %>%
    group_by(Year, Month, Codigo_nacional) %>%
    summarise(temp_min = mean(temp_min, na.rm = TRUE), temp_max = mean(temp_max, na.rm = TRUE)) %>%
    ungroup()

  # Combinar datos históricos y actuales: obtener series completas de precipitacion y temperatura
  pp_mensual_completo <- rbind(pp_mensual_historico, pp_mensual_actual)
  temp_mensual_completo <- rbind(temp_mensual_historico, temp_mensual_actual)

  # Juntar todos los archivos en un solo dataframe
  pp_temp_mensual_completo <- merge(pp_mensual_completo, temp_mensual_completo, by = c("Year", "Month", "Codigo_nacional"))
  
  # ordenar por codigo nacional, año y mes: CLAVE PARA CALCULAR SPEI
  pp_temp_mensual_completo <- pp_temp_mensual_completo %>%
    arrange(Codigo_nacional, Year, Month)

  # Calcular evapotranspiración y Balance Hídrico
  # Para eso necesitamos la latitud de cada estación
  metadata <- read.csv(metadata_path)
  pp_temp_mensual_completo$lat <- metadata$LAT[match(pp_temp_mensual_completo$Codigo_nacional, metadata$Codigo_nacional)]

  pp_temp_mensual_completo <- pp_temp_mensual_completo %>%
    group_by(Codigo_nacional) %>%
    rowwise() %>%
    mutate(ETP = hargreaves(Tmin = temp_min, Tmax = temp_max, lat = lat, Pre = pp_month, verbose = FALSE),
           BH = pp_month - ETP) %>%
    ungroup()

  # Calcular IPEE
  spei_dataframe <- pp_temp_mensual_completo %>%
    group_by(Codigo_nacional) %>%
    mutate(
      spei_1 = spei(BH, scale = 1, distribution = "log-Logistic", na.rm = TRUE, verbose = FALSE)$fitted,
      spei_3 = spei(BH, scale = 3, distribution = "log-Logistic", na.rm = TRUE, verbose = FALSE)$fitted,
      spei_6 = spei(BH, scale = 6, distribution = "log-Logistic", na.rm = TRUE, verbose = FALSE)$fitted,
      spei_12 = spei(BH, scale = 12, distribution = "log-Logistic", na.rm = TRUE, verbose = FALSE)$fitted,
      spei_24 = spei(BH, scale = 24, distribution = "log-Logistic", na.rm = TRUE, verbose = FALSE)$fitted
    ) %>%
    ungroup()
  
  fechas_archivo = obtener_fechas_archivo(archivo_pp_depurado)
  # Guardar el resultado en el directorio correspondiente
  if(tolower(fuente) == "dga") {
    write.csv(spei_dataframe, paste0(directorio_base, "/BBDD/indicadores/IPEE/DGA/IPEE_DGA_1979_", fechas_archivo$ano_fin,"_", fechas_archivo$mes_fin,".csv"))
  } else if (tolower(fuente) == "dmc") {
    write.csv(spei_dataframe, paste0(directorio_base, "/BBDD/indicadores/IPEE/DMC/IPEE_DMC_1979_", fechas_archivo$ano_fin,"_", fechas_archivo$mes_fin,".csv"))
  }

  return(spei_dataframe)
} 