actualizar_datos(getwd(), "niveles_pozos", "DGA")

actualizar_datos(getwd(), "pp", "DGA")

actualizar_datos(getwd(), "caudal", "DGA")

actualizar_datos<-function(directorio_base, variable, fuente){
################ MANEJO DE ERRORES #############################

  source(paste0(directorio_base, "/1.Descargar/Librerias.R"))
# Check directorio_base is a string
  if (!is.character(directorio_base)) {
    stop("Argumento 'directorio_base' debe ser string.")
  }
# Check variable is valid
  valid_variable <- c("pp", "temp", "caudal", "temp_max", "temp_min", "niveles_pozos")
  if (!(variable %in% valid_variable)) {
    stop("Argumento 'variable' debe ser 'pp', 'temp', 'caudal', 'temp_max', 'temp_min' o 'niveles_pozos'")
  }
  
# Check fuente is valid (case-insensitive)
  valid_fuente <- c("dga", "dmc")
  if (!(tolower(fuente) %in% valid_fuente)) {
    stop("Argumento 'fuente' debe ser 'DGA' or 'DMC'")
  }
  if(variable=="caudal" & tolower(fuente)=="dmc"){stop("Error: Para 'caudal' usar solo 'DGA'")}
  if(variable=="temp" & tolower(fuente)=="dmc"){stop("Er: Parorra 'DMC' usar solo 'temp_max' y 'temp_min'")}
  if(variable=="temp_max" & tolower(fuente)=="dga"){stop("Error: Para 'DGA' usar solo 'temp'")}
  if(variable=="temp_min" & tolower(fuente)=="dga"){stop("Error: Para 'DGA' usar solo 'temp'")}
  if(variable=="niveles_pozos" & tolower(fuente)=="dmc"){stop("Error: Para 'niveles_pozos' usar solo 'DGA'")}


  #Definimos fechas actuales (ultimo mes es el anterior al actual para asegurar datos mensuales completos)
  mes_ultimo<-as.numeric(substr(Sys.time() %m-% months(1),6,7))
  ano_actual<-as.numeric(substr(Sys.time() %m-% months(1),1,4))

  source(paste0(directorio_base, "/1.Descargar/Manejo_BDD.R"))

  #Definir estaciones a descargar
  #metadatos_DGA<-read_excel(paste0(directorio_base,"/BBDD/metadatos/DGA/estaciones_DGA.xlsx"))
  metadatos_DMC<-read_excel(paste0(directorio_base,"/BBDD/metadatos/DMC/estaciones_DMC.xlsx"))

  csv_metadata_pozos_path = paste0(directorio_base, "/BBDD/metadatos/DGA/pozos/estaciones_DGA_pozos.csv")
  csv_metadata_caudal_path = paste0(directorio_base, "/BBDD/metadatos/DGA/caudal/estaciones_DGA_caudal.csv")
  csv_metadata_pp_y_temp_path = paste0(directorio_base, "/BBDD/metadatos/DGA/pp_y_temp/estaciones_DGA_pp_y_temp.csv")

  #Definir columnas de los archivos de metadatos, con su tipo de dato específico
  tipos <- cols(
  Codigo_nacional = col_character(),
  Nombre = col_character(),
  LAT = col_double(),
  LONG = col_double(),
  Altura = col_integer(),
  TIPO = col_character(),
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
  metadata_DGA_pozos <- read_csv(csv_metadata_pozos_path, col_types = tipos)
  metadata_DGA_caudal <- read_csv(csv_metadata_caudal_path, col_types = tipos)
  metadata_DGA_pp_y_temp <- read_csv(csv_metadata_pp_y_temp_path, col_types = tipos)

  
  #metadatos_pozos_dga <- read.csv(paste0(directorio_base, "/otros/capas a csv/pozos.csv"))

  directorio_archivo = obtener_directorio(variable, fuente)
  archivo_mas_grande = buscar_archivo_mas_grande(directorio_archivo)
  #Si hay archivos en la base de datos, se descarga desde el año de la última fecha en la base de datos (ano_fin)
  #variables ano_ini y mes_fin servirán para luego actualizar los archivos descargados en la bddd
  #variable ano_fin servirá para definir el rango de fechas a descargar, es el año al cual esta actualizada la bdd de la variable
  if(length(archivo_mas_grande) != 0 && length(obtener_fechas_archivo(archivo_mas_grande)) != 0){
    list_fechas = obtener_fechas_archivo(archivo_mas_grande)
    ano_ini = list_fechas$ano_ini
    ano_fin = list_fechas$ano_fin
    mes_fin = as.numeric(list_fechas$mes_fin)
    if(ano_fin == ano_actual && mes_fin == mes_ultimo){
      print(paste("La base de datos de", variable, fuente, "está actualizada"))
    }
    else{ 
      print(paste("La base de datos de", variable, fuente, "contiene datos desde", ano_ini, "hasta", ano_fin, "mes", mes_fin))
      print(paste("Descargando datos desde", ano_fin, "hasta", ano_actual, "mes", mes_ultimo, "variable", variable, fuente))
      data_nueva = descargar_variable(variable, fuente, ano_fin, ano_actual, mes_ultimo, metadata_DGA_pozos, 
        metadata_DGA_caudal, metadata_DGA_pp_y_temp, metadatos_DMC)


      concatenar_datos(data_nueva, list_fechas, ano_actual, mes_ultimo, variable, fuente, directorio_archivo)
      print(paste("Datos de", variable, fuente, "actualizados"))
      }
    }
  else {
    #si la base de datos está vacía, se le pregunta al usuario desde que año desea descargar
    print(paste("La base de datos de", variable, fuente, "está vacía"))
    ano_ini = solicitar_ano()
    print(paste("Descargando datos desde", ano_ini, "hasta", ano_actual, "mes", mes_ultimo, "variable", variable, fuente))
    data_nueva = descargar_variable(variable, fuente, ano_ini, ano_actual, mes_ultimo, metadata_DGA_pozos, 
      metadata_DGA_caudal, metadata_DGA_pp_y_temp, metadatos_DMC)
    #Guardar datos en la base de datos
    guardar_datos(data_nueva, ano_ini, ano_actual, mes_ultimo, variable, fuente, directorio_archivo)
    print(paste("Datos de", variable, fuente, "actualizados"))
  }

}

  

