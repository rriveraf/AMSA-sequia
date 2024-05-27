
rellenar_fechas <- function(dataframe){
    #cambio de formato de fechas
    dataframe$Date <- as.Date(with(dataframe, paste(Year, Month, Day, sep = "-")), "%Y-%m-%d")
    #eliminar columnas de fecha
    dataframe <- subset(dataframe, select = -c(Year, Month, Day))
    #rango de fechas
    start_date <- min(dataframe$Date)
    end_date <- max(dataframe$Date)
    #vector de fechas
    full_dates <- seq.Date(start_date, end_date, by = "day")
    #dataframe de todas las combinaciones posibles de fecha y código nacional (PRODUCTO CRUZ)
    all_combinations <- expand.grid(Date = full_dates, Codigo_nacional = unique(dataframe$Codigo_nacional))
    #fusionar con el dataframe original para obtener las entradas faltantes como NA
    complete_df <- merge(all_combinations, dataframe, by = c("Date", "Codigo_nacional"), all.x = TRUE)
    #ordenar el dataframe por fecha y código
    complete_df <- complete_df %>% arrange(Codigo_nacional, Date)
    #volver al formato inicial de fechas 
    complete_df$Year <- year(complete_df$Date)
    complete_df$Month <- month(complete_df$Date)
    complete_df$Day <- day(complete_df$Date)
    complete_df <- subset(complete_df, select = -c(Date))
    return(complete_df)
}


estacion_mas_cercana <- function(codigo_estacion, metadata, tolerancia_km) {
  # Obtener coordenadas de la estación dada por su código
  coords <- obtener_coordenadas(codigo_estacion, metadata)
  R <- 6371  # Radio de la Tierra en kilómetros
  # Convertir grados en radianes
  lon1 <- coords$LONG * pi / 180
  lat1 <- coords$LAT * pi / 180
  lon2 <- metadata$LONG * pi / 180
  lat2 <- metadata$LAT * pi / 180
  # Diferencia de las coordenadas
  delta_lon <- lon2 - lon1
  delta_lat <- lat2 - lat1
  # Fórmula de Haversine
  a <- sin(delta_lat / 2)^2 + cos(lat1) * cos(lat2) * sin(delta_lon / 2)^2
  c <- 2 * atan2(sqrt(a), sqrt(1-a))
  distance <- R * c  # Calcula la distancia en kilómetros
  # Filtra para evitar seleccionar la misma estación
  is_same_station <- metadata$Codigo_nacional == codigo_estacion
  distance[is_same_station] <- Inf  # Asigna Infinito a la distancia de la misma estación
  # Encuentra el índice de la estación más cercana si está dentro de la tolerancia
  if (any(distance <= tolerancia_km)) {
    nearest_station_index <- which.min(distance)
    if (distance[nearest_station_index] <= tolerancia_km) {
      return(as.character(metadata$Codigo_nacional[nearest_station_index]))
    }
  }
  return(NA)  # Retorna NA si no hay ninguna estación dentro de la tolerancia
}

obtener_coordenadas <- function(codigo, metadata) {
  # Encuentra el índice de la estación
  index <- which(metadata$Codigo_nacional == codigo)
  # Devuelve las coordenadas de la estación
  return(metadata[index, c("LAT", "LONG")])
}

obtener_valor <- function(df, codigo_nacional, year, month, day, columna_valor) {
  resultado <- df %>%
    filter(Codigo_nacional == codigo_nacional, Year == year, Month == month, Day == day)
  
  if (nrow(resultado) == 1) {
    return(resultado[[columna_valor]])
  } else {
    return(NA)  # Retorna NA si no hay resultados o hay más de uno
  }
}

obtener_fechas_archivo <- function(ruta_archivo) {
  # Expresión regular para encontrar los anos y el mes
    patron_regex <- "_(\\d{4})_(\\d{4})_(\\d{1,2})\\.csv$"
    # Aplicar regexpr() para encontrar las coincidencias en la ruta del archivo
    match <- regexpr(patron_regex, ruta_archivo, perl = TRUE)
    # Verificar si se encontró alguna coincidencia
    if(match == -1) {
        return(character(0))
    }
    # Extraer las coincidencias utilizando regmatches()
    coincidencias <- regmatches(ruta_archivo, match)
    # Extraer los grupos de captura usando gsub()
    partes <- gsub(patron_regex, "\\1 \\2 \\3", coincidencias)
    # Separar los anos y el mes en variables separadas

    partes <- strsplit(partes, " ")[[1]]
    ano_ini <- as.integer(partes[1])
    ano_fin <- as.integer(partes[2])
    mes_fin <- as.integer(partes[3])
    # Devolver los resultados como una lista
    #variables ano_ini y mes_fin servirán para luego actualizar los archivos descargados en la bddd
    #variable ano_fin servirá para definir el rango de fechas a descargar, es el año al cual esta actualizada la bdd de la variable
    return(list(ano_ini = ano_ini, ano_fin = ano_fin, mes_fin = mes_fin))
}  

