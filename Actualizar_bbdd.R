

actualizar_datos <- function(directorio_base, variable, fuente) {
  ################ MANEJO DE ERRORES #############################
  source(paste0(directorio_base, "/Descargar.R"))
  source(paste0(directorio_base, "/Depurado_y_relleno.R"))
  source(paste0(directorio_base, "/Concatenar_datos.R"))
  
  # Check directorio_base is a string
  if (!is.character(directorio_base)) {
    stop("Argumento 'directorio_base' debe ser string.")
  }
  
  # Check variable is valid
  valid_variable <- c("pp", "temp", "caudal")
  if (!(variable %in% valid_variable)) {
    stop("Argumento 'variable' debe ser 'pp', 'temp', 'caudal'")
  }
  
  # Check fuente is valid (case-insensitive)
  valid_fuente <- c("dga", "dmc")
  if (!(tolower(fuente) %in% valid_fuente)) {
    stop("Argumento 'fuente' debe ser 'DGA' or 'DMC'")
  }
  if(variable=="caudal" & tolower(fuente)=="dmc"){stop("Error: Para 'caudal' usar solo 'DGA'")}
  
  # Definimos fechas actuales (ultimo mes es el anterior al actual para asegurar datos mensuales completos)
  mes_ultimo <- as.numeric(substr(Sys.time() %m-% months(1), 6, 7))
  ano_actual <- as.numeric(substr(Sys.time() %m-% months(1), 1, 4))
  
  # Definir estaciones a descargar
  metadatos_DMC_path = paste0(directorio_base, "/BBDD/metadatos/DMC/estaciones_DMC.xlsx")
  csv_metadata_pozos_path = paste0(directorio_base, "/BBDD/metadatos/DGA/pozos/estaciones_DGA_pozos.csv")
  csv_metadata_caudal_path = paste0(directorio_base, "/BBDD/metadatos/DGA/caudal/estaciones_DGA_caudal.csv")
  csv_metadata_pp_y_temp_path = paste0(directorio_base, "/BBDD/metadatos/DGA/pp_y_temp/estaciones_DGA_pp_y_temp.csv")
  
  # Definir columnas de los archivos de metadatos, con su tipo de dato específico
  tipos <- cols(
    Codigo_nacional = col_character(),
    Nombre = col_character(),
    LAT = col_double(),
    LONG = col_double(),
    Altura = col_integer(),
    COD_CUEN = col_integer(),
    COD_SUBC = col_integer(),
    COD_SSUBC = col_integer(),
    NOM_CUEN = col_character(),
    NOM_SUBC = col_character(),
    NOM_SSUBC = col_character(),
    COD_REG = col_integer(),
    COD_PROV = col_integer(),
    COD_COM = col_integer(),
    NOM_REG = col_character(),
    NOM_PROV = col_character(),
    NOM_COM = col_character()
  )
  
  #DESCARGA DE DATOS
  #metadata_DGA_pozos <- read_csv(csv_metadata_pozos_path, col_types = tipos)
  metadata_DGA_caudal <- read_csv(csv_metadata_caudal_path, col_types = tipos)
  metadata_DGA_pp_y_temp <- read_csv(csv_metadata_pp_y_temp_path, col_types = tipos)
  metadatos_DMC <- read_excel(metadatos_DMC_path)
  
  directorio_archivo = obtener_directorio_descargas(variable, fuente)
  archivo_mas_grande = buscar_archivo_mas_grande(directorio_archivo)
  
  # Si hay archivos en la base de datos, se descarga desde el año de la última fecha en la base de datos (ano_fin)
  if(length(archivo_mas_grande) != 0 && length(obtener_fechas_archivo(archivo_mas_grande)) != 0) {
    list_fechas = obtener_fechas_archivo(archivo_mas_grande)
    ano_ini = list_fechas$ano_ini
    ano_fin = list_fechas$ano_fin
    mes_fin = as.numeric(list_fechas$mes_fin)
    
    if(ano_fin == ano_actual && mes_fin == mes_ultimo) {
      print(paste("La base de datos bruta de", variable, fuente, "está actualizada, no es necesario descargar datos"))
    } else { 
      print(paste("La base de datos de", variable, fuente, "contiene datos desde", ano_ini, "hasta", ano_fin, "mes", mes_fin))
      print(paste("Descargando datos desde", ano_fin, "hasta", ano_actual, "mes", mes_ultimo, "variable", variable, fuente))
      data_nueva = descargar_variable(variable, fuente, ano_fin, ano_actual, mes_ultimo, metadata_DGA_pozos, metadata_DGA_caudal, metadata_DGA_pp_y_temp, metadatos_DMC)
      resultado = concatenar_datos_descargados(data_nueva, list_fechas, ano_actual, mes_ultimo, variable, fuente, directorio_archivo)
    }
  } else {
    # si la base de datos está vacía, se descarga desde el año 2020. Periodos historicos están hasta 2019
    print(paste("La base de datos de", variable, fuente, "está vacía"))
    ano_ini = 2020
    print(paste0("Se necesitan datos desde el año ", ano_ini, " para concantenar correctamente con periodos históricos que van hasta 2019"))
    print(paste("Descargando datos desde", ano_ini, "hasta", ano_actual, "mes", mes_ultimo, "variable", variable, fuente))
    data_nueva = descargar_variable(variable, fuente, ano_ini, ano_actual, mes_ultimo, metadata_DGA_pozos, metadata_DGA_caudal, metadata_DGA_pp_y_temp, metadatos_DMC)
    # Guardar datos en la base de datos
    guardar_datos(data_nueva, ano_ini, ano_actual, mes_ultimo, variable, fuente, directorio_archivo)
    print(paste("Datos de", variable, fuente, "actualizados"))
  }

  #FIN DESCARGA DE DATOS
  #INICIO DE DEPURACION Y RELLENO DE DATOS

  directorio_depuracion <- gsub("bruto$", "depurado", directorio_archivo)
  archivo_mas_grande_depurado <- buscar_archivo_mas_grande(directorio_depuracion)
  #Si el archivo depurado alojado en bdd tiene la misma fecha que la ultima descargada, no se hace nada
  if(length(archivo_mas_grande_depurado) != 0 && length(obtener_fechas_archivo(archivo_mas_grande_depurado)) != 0) {
    list_fechas_depurado = obtener_fechas_archivo(archivo_mas_grande_depurado)
    ano_ini_depurado = list_fechas_depurado$ano_ini
    ano_fin_depurado = list_fechas_depurado$ano_fin
    mes_fin_depurado = as.numeric(list_fechas_depurado$mes_fin)
    
    if(ano_fin_depurado == ano_actual && mes_fin_depurado == mes_ultimo && ano_ini_depurado == 2020) {
      print(paste("La base de datos de", variable, fuente, "depurada está actualizada, no es necesario depurar datos"))
    }
    else {
      print(paste("La base de datos de", variable, fuente, "depurada está incompleta"))
       depurar_variable(directorio_base, directorio_depuracion, variable, fuente, ano_actual, mes_ultimo)
    }
  } 
  # se depuran los datos desde 2020 (ultimo archivo de descargas)
  else {
    # si la base de datos depurada está vacía, se depura desde el año 2020. Periodos historicos están hasta 2019
    print(paste("La base de datos de", variable, fuente, "depurada está vacía"))
    depurar_variable(directorio_base, directorio_depuracion, variable, fuente, ano_actual, mes_ultimo)
  }
  #FIN DE DEPURACION Y RELLENO DE DATOS
  #INICIO Concatenación de datos de periodo histórico con datos depurados
  directorio_concatenado <- gsub("depurado$", "concatenado", directorio_depuracion)
  archivo_mas_grande_concatenado <- buscar_archivo_mas_grande(directorio_concatenado)
  #Si el archivo concatenado alojado esta actualizado a la fecha, no se hace nada
  if(length(archivo_mas_grande_concatenado) != 0 && length(obtener_fechas_archivo(archivo_mas_grande_concatenado)) != 0) {
    list_fechas_concatenado = obtener_fechas_archivo(archivo_mas_grande_concatenado)
    ano_ini_concatenado = list_fechas_concatenado$ano_ini
    ano_fin_concatenado = list_fechas_concatenado$ano_fin
    mes_fin_concatenado = as.numeric(list_fechas_concatenado$mes_fin)
    
    #se verifica con 1979 y 1989 porque pp y temp inician en 1979 y caudal en 1989.
    if(ano_fin_concatenado == ano_actual && mes_fin_concatenado == mes_ultimo && (ano_ini_concatenado == 1979 || ano_ini_concatenado == 1989)) {
      print(paste("La base de datos de", variable, fuente, "concatenada está actualizada, no es necesario concatenar datos"))
    }
    else {
      print(paste("La base de datos de", variable, fuente, "concatenada está incompleta"))
      concatenar_variable(directorio_base, directorio_concatenado, variable, fuente, ano_actual, mes_ultimo)
    }
  }
  # se concatenan los datos desde 2020 (ultimo archivo de descargas)
  else {
    # si la base de datos concatenada está vacía, se concatenan desde el año 2020. Periodos historicos están hasta 2019
    print(paste("La base de datos de", variable, fuente, "concatenada está vacía"))
    concatenar_variable(directorio_base, directorio_concatenado, variable, fuente, ano_actual, mes_ultimo)
  }
  #FIN Concatenación de datos de periodo histórico con datos depurados
}
  

