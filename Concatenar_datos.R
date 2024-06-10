#Los objetuvos de este script son:
# 1. Concatenar los datos históricos (1979 - 2019) con los datos actuales (2019-hoy) de caudal, precipitación y temperatura
# 2. Guardar/retornar los datos concatenados en un archivo CSV

concatenar_caudal_DGA <- function(directorio_base, ruta_archivo_caudal_depurado, ano_actual, mes_ultimo){
  print(paste0("Concatenando datos de caudal DGA desde 1989 hasta ", ano_actual, " mes ", mes_ultimo))
  # Cargar datos históricos fijos y datos actuales
  archivo_historico <- "/BBDD/q/DGA/periodo_historico/q_DGA_monthly_1989_2019.csv"
  tryCatch({
    caudal_mensual_1989_2019 <- read.csv(paste0(directorio_base, archivo_historico))
    caudal_day_2020_actual <- read.csv(ruta_archivo_caudal_depurado)
  }, error = function(e) {
    stop("Error al cargar los datos históricos o actuales")
  })
  # Calcular caudal mensual para los datos actuales
  caudal_mensual_2020_actual <- caudal_day_2020_actual %>%
    group_by(Year, Month, Codigo_nacional) %>%
    summarise(q_month = mean(caudal_mean, na.rm = TRUE)) %>%
    ungroup()

  # Filtrar estaciones que existen en los datos históricos 
  # Así se asgura que datos nuevos tengan una data histórica
  estaciones_historicas <- unique(caudal_mensual_1989_2019$Codigo_nacional)
  caudal_mensual_2020_actual <- caudal_mensual_2020_actual[caudal_mensual_2020_actual$Codigo_nacional %in% estaciones_historicas, ]

  # Verificar si la columna 'X' existe y luego eliminarla si es necesario
  if ("X" %in% names(caudal_mensual_1989_2019)) {
    caudal_mensual_1989_2019 <- caudal_mensual_1989_2019 %>% select(-X)
  }
  # Combinar datos históricos y actuales
  caudal_mensual_completo <- rbind(caudal_mensual_1989_2019, caudal_mensual_2020_actual)

  tryCatch({
    # Guardar datos concatenados
    write.csv(caudal_mensual_completo, paste0(directorio_base, "/BBDD/q/DGA/concatenado/q_DGA_mensual_1989_", ano_actual, "_", mes_ultimo, ".csv"), row.names = FALSE)
    print("Datos de caudal DGA concatenados con éxito")
    return(TRUE)
  }, error = function(e) {
    message("Error al guardar los datos concatenados: ", e$message)
    return(FALSE)
  })
}
concatenar_pp_DGA_DMC <- function(directorio_base, ruta_archivo_pp_depurado, fuente, ano_actual, mes_ultimo){
  print(paste0("Concatenando datos de precipitación ", fuente, " desde 1979 hasta ", ano_actual, " mes ", mes_ultimo))
  if(tolower(fuente) != "dga" && tolower(fuente) != "dmc") {
    stop("La fuente debe ser 'DGA' o 'DMC'")
  }
  # Cargar archivo histórico según la fuente
  archivo_historico <- if(tolower(fuente) == "dga") {
    "/BBDD/pp/DGA/periodo_historico/pp_DGA_monthly_1979_2019_cr2met.csv"
  } else if(tolower(fuente) == "dmc"){
    "/BBDD/pp/DMC/periodo_historico/pp_DMC_monthly_1979_2019_cr2met.csv"
  }
  tryCatch({
    # Cargar datos históricos (1979-2019) y actuales(2020-hoy)
    pp_mensual_historico <- read.csv(paste0(directorio_base, archivo_historico))
    pp_day_actual <- read.csv(ruta_archivo_pp_depurado)
  }, error = function(e) {
    stop("Error al cargar los datos históricos o actuales")
  })
  
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

  tryCatch({
    if(tolower(fuente) == "dga") {
      # Guardar datos concatenados
      write.csv(pp_mensual_completo, paste0(directorio_base, "/BBDD/pp/DGA/concatenado/pp_DGA_mensual_1979_", ano_actual, "_", mes_ultimo, ".csv"), row.names = FALSE)
    } else if(tolower(fuente) == "dmc") {
      # Guardar datos concatenados
      write.csv(pp_mensual_completo, paste0(directorio_base, "/BBDD/pp/DMC/concatenado/pp_DMC_mensual_1979_", ano_actual, "_", mes_ultimo, ".csv"), row.names = FALSE)
    }
    print(paste0("Datos de precipitación ", fuente, " concatenados con éxito"))
    return(TRUE)
    }, error = function(e) {
      message("Error al guardar los datos concatenados: ", e$message)
      return(FALSE)
    }
  )
}

concatenar_temp_DGA_DMC <- function(directorio_base, ruta_archivo_temp_depurado, fuente, ano_actual, mes_ultimo){

  print(paste0("Concatenando datos de temperatura ", fuente, " desde 1979 hasta ", ano_actual, " mes ", mes_ultimo))
  if(tolower(fuente) != "dga" && tolower(fuente) != "dmc") {
    stop("La fuente debe ser 'DGA' o 'DMC'")
  }
  # Cargar archivo histórico según la fuente
  archivo_historico <- if(tolower(fuente) == "dga") {
    "/BBDD/temp/DGA/periodo_historico/temp_DGA_monthly_1979_2019_cr2met.csv"
  } else if (tolower(fuente) == "dmc"){
    "/BBDD/temp/DMC/periodo_historico/temp_DMC_monthly_1979_2019_cr2met.csv"
  }
  tryCatch({
    # Cargar datos históricos (1979-2019) y actuales(2020-hoy)
    temp_mensual_historico <- read.csv(paste0(directorio_base, archivo_historico))
    temp_day_actual <- read.csv(ruta_archivo_temp_depurado)
  }, error = function(e) {
    stop("Error al cargar los datos históricos o actuales")
  })

  # Calcular temperaturas mensuales para los datos actuales
  temp_mensual_actual <- temp_day_actual %>%
    group_by(Year, Month, Codigo_nacional) %>%
    summarise(temp_min = mean(temp_min, na.rm = TRUE), temp_max = mean(temp_max, na.rm = TRUE)) %>%
    ungroup()

  # Combinar datos históricos y actuales
  temp_mensual_completo <- rbind(temp_mensual_historico, temp_mensual_actual)

  tryCatch({
    if(tolower(fuente) == "dga") {
      # Guardar datos concatenados
      write.csv(temp_mensual_completo, paste0(directorio_base, "/BBDD/temp/DGA/concatenado/temp_DGA_mensual_1979_", ano_actual, "_", mes_ultimo, ".csv"), row.names = FALSE)
    } else if(tolower(fuente) == "dmc") {
      # Guardar datos concatenados
      write.csv(temp_mensual_completo, paste0(directorio_base, "/BBDD/temp/DMC/concatenado/temp_DMC_mensual_1979_", ano_actual, "_", mes_ultimo, ".csv"), row.names = FALSE)
    }
    print(paste0("Datos de temperatura ", fuente, " concatenados con éxito"))
    return(TRUE)
  }, error = function(e) {
    message("Error al guardar los datos concatenados: ", e$message)
    return(FALSE)

  })
}
