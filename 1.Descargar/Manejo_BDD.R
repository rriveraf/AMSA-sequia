# Description: Este script contiene las funciones necesarias para descargar y manejar la base de datos de precipitación, caudal, temperatura y niveles de pozos.

obtener_directorio<-function(variable, fuente){
    directorio_base = getwd()

    if (variable=="pp" & tolower(fuente)=="dga"){
        directorio = file.path(directorio_base, "BBDD", "pp", "DGA", "bruto")
    }
    else if (variable == "pp" & tolower(fuente) == "dmc") {
        directorio = file.path(directorio_base, "BBDD", "pp", "DMC", "bruto")
    }
    else if (variable == "temp" & tolower(fuente) == "dga") {
        directorio = file.path(directorio_base, "BBDD", "temp", "DGA", "bruto")
    }
    else if(variable== "caudal" & tolower(fuente) == "dga") {
        directorio = file.path(directorio_base, "BBDD", "q", "DGA", "bruto")
    }
    else if(variable == "temp_max" & tolower(fuente) == "dmc") {
        directorio = file.path(directorio_base, "BBDD", "temp", "DMC", "max")
    }
    else if(variable == "temp_min" & tolower(fuente) == "dmc") {
        directorio = file.path(directorio_base, "BBDD", "temp", "DMC", "min")
    }
    else if(variable == "niveles_pozos" & tolower(fuente) == "dga") {
        directorio = file.path(directorio_base, "BBDD", "niveles_pozos", "bruto")
    }
    
    else{
        return(character(0))
        stop("No se ha encontrado la combinación de variable y fuente")
    }
    return(directorio)
}

buscar_archivo_mas_grande<-function(directorio){
    archivos = list.files(directorio, pattern = ".csv", full.names = TRUE)
    if(length(archivos) == 0){
        return(character(0))
    }
    tamanos = sapply(archivos, function(x) file.info(x)$size)
    archivo_mas_pesado = archivos[which.max(tamanos)]
    return(archivo_mas_pesado)
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

descargar_variable <- function(variable, fuente, ano_inicio, ano_actual, mes_ultimo, metadata_DGA_pozos,
 metadata_DGA_caudal, metadata_DGA_pp_y_temp, estaciones_dmc) {
    if(variable == "pp" & tolower(fuente) == "dga"){
        return(descargar_pp_DGA(ano_inicio, ano_actual, mes_ultimo, metadata_DGA_pp_y_temp))
    }
    else if(variable == "caudal" & tolower(fuente) == "dga"){
        return(descargar_caudal_DGA(ano_inicio, ano_actual, mes_ultimo, metadata_DGA_caudal))
    }
    else if(variable == "temp" & tolower(fuente) == "dga"){
        return(descargar_temp_DGA(ano_inicio, ano_actual, mes_ultimo, metadata_DGA_pp_y_temp))
    }
    else if(variable == "niveles_pozos" & tolower(fuente) == "dga"){
        return(descargar_niveles_DGA(ano_inicio, ano_actual, mes_ultimo, metadata_DGA_pozos))
    }
    else if(variable == "pp" & tolower(fuente) == "dmc"){
        return(descargar_pp_DMC(ano_inicio, ano_actual, mes_ultimo, estaciones_dmc))
    }
    else if(variable == "temp_max" & tolower(fuente) == "dmc"){
        return(descargar_temp_max_DMC(ano_inicio, ano_actual, mes_ultimo, estaciones_dmc))
    }
    else if(variable == "temp_min" & tolower(fuente) == "dmc"){
        return(descargar_temp_min_DMC(ano_inicio, ano_actual, mes_ultimo, estaciones_dmc))
    }
    else{
        stop("No se ha encontrado la combinación de variable y fuente")
    }
}

#solicitar_ano <- function() {
#  ano_valido <- FALSE
#  ano <- NA  # Inicializar `ano` con NA (Not Available)
#  
#  while(!ano_valido) {
#    # Solicitar al usuario que ingrese el año
#    entrada <- readline(prompt = "Ingrese el año del que desea iniciar a descargar: ")
#    # encontrar el año actual y convertir la entrada a número (integer)
#    ano_actual <- as.integer(format(Sys.Date(), "%Y"))
#    ano <- as.integer(entrada)
#    
#    # Verificar si la entrada es un año válido
#    if(!is.na(ano) && ano > 0 && ano <= ano_actual && ano >= 1979) {
#      ano_valido <- TRUE
#    } else {
#      cat("Entrada no válida. Por favor, ingrese un año entre 1979 y el año actual.\n")
#    }
#  }
#  return(ano)
#}

guardar_datos <- function(data_nueva, ano_inicio, ano_actual, mes_ultimo, variable, fuente, directorio_archivo){
    # esta función se ejecuta sólo si la bdd está vacía al descargar los datos
    # de lo contrario, se ejecuta función concatenar_datos()
    if(variable == "pp" & tolower(fuente) == "dga"){
        write.csv(data_nueva, file = paste0(directorio_archivo, "/pp_DGA_", ano_inicio, "_", ano_actual, "_", mes_ultimo, ".csv"), row.names = FALSE)
    }
    else if(variable == "caudal" & tolower(fuente) == "dga"){
        write.csv(data_nueva, file = paste0(directorio_archivo, "/q_mean_DGA_", ano_inicio, "_", ano_actual, "_", mes_ultimo, ".csv"), row.names = FALSE)
    }
    else if(variable == "temp" & tolower(fuente) == "dga"){
        write.csv(data_nueva, file = paste0(directorio_archivo, "/temp_DGA_", ano_inicio, "_", ano_actual, "_", mes_ultimo, ".csv"), row.names = FALSE)
    }
    else if(variable == "niveles_pozos" & tolower(fuente) == "dga"){
        write.csv(data_nueva, file = paste0(directorio_archivo, "/niveles_pozos_DGA_", ano_inicio, "_", ano_actual, "_", mes_ultimo, ".csv"), row.names = FALSE)
    }
    else if(variable == "pp" & tolower(fuente) == "dmc"){
        write.csv(data_nueva, file = paste0(directorio_archivo, "/pp_DMC_", ano_inicio, "_", ano_actual, "_", mes_ultimo, ".csv"), row.names = FALSE)
    }
    else if(variable == "temp_max" & tolower(fuente) == "dmc"){
        write.csv(data_nueva, file = paste0(directorio_archivo, "/temp_max_DMC_", ano_inicio, "_", ano_actual, "_", mes_ultimo, ".csv"), row.names = FALSE)
    }
    else if(variable == "temp_min" & tolower(fuente) == "dmc"){
        write.csv(data_nueva, file = paste0(directorio_archivo, "/temp_min_DMC_", ano_inicio, "_", ano_actual, "_", mes_ultimo, ".csv"), row.names = FALSE)
    }
    else{
        stop("No se ha encontrado la combinación de variable y fuente")
    }

}

concatenar_datos <- function(data_nueva, list_fechas, ano_actual, mes_ultimo, variable, fuente, directorio_archivo){
    #ano_ini_bdd es el año de la primera fecha en la bdd
    #ano_fin_bdd es el año desde el cual se descargaron los datos
    #mes_fin es el mes de la última fecha en la bdd
    # esta función se ejecuta sólo si la bdd no está vacía al descargar los datos
    ano_ini_bdd = list_fechas$ano_ini
    ano_fin_bdd = list_fechas$ano_fin
    mes_fin = list_fechas$mes_fin

    #filtramos quitando eliminando las entradas del año en que se comenzó a descargar los datos, para luego concatenar

    if(variable == "pp" && tolower(fuente) == "dga"){
       archivo_antiguo = paste0(directorio_archivo, "/pp_DGA_", ano_ini_bdd, "_", ano_fin_bdd, "_", mes_fin, ".csv")
       data_vieja = read.csv(archivo_antiguo)
       data_vieja = data_vieja<- data_vieja %>% 
        filter(Year<ano_fin_bdd)
       pp_actualizada = rbind(data_vieja, data_nueva)
       archivo_nuevo = paste0(directorio_archivo, "/pp_DGA_", ano_ini_bdd, "_", ano_actual, "_", mes_ultimo, ".csv")
       write.csv(pp_actualizada, file = archivo_nuevo, row.names = FALSE)
       if(file.exists(archivo_nuevo)){
            print(paste0("La base de datos de ", variable, " ", fuente, " ha sido actualizada correctamente."))
            file.remove(archivo_antiguo)
        }
        else{
            print(paste0("El archivo nuevo de ", variable, " ", fuente, " no se ha creado correctamente."))
        }
    }   
    else if(variable == "caudal" && tolower(fuente) == "dga"){
        archivo_antiguo = paste0(directorio_archivo, "/q_mean_DGA_", ano_ini_bdd, "_", ano_fin_bdd, "_", mes_fin, ".csv")
        data_vieja = read.csv(archivo_antiguo)
        data_vieja = data_vieja<- data_vieja %>% 
        filter(Year<ano_fin_bdd)|
        q_actualizada = rbind(data_vieja, data_nueva)

        archivo_nuevo = paste0(directorio_archivo, "/q_mean_DGA_", ano_ini_bdd, "_", ano_actual, "_", mes_ultimo, ".csv")
        write.csv(q_actualizada, file = archivo_nuevo, row.names = FALSE)

        if(file.exists(archivo_nuevo)){
            print(paste0("La base de datos de ", variable, " ", fuente, " ha sido actualizada correctamente."))
            file.remove(archivo_antiguo)
        }
        else{
            print(paste0("El archivo nuevo de ", variable, " ", fuente, " no se ha creado correctamente."))
        }
        
    }
    else if(variable == "temp" && tolower(fuente) == "dga"){
        archivo_antiguo = paste0(directorio_archivo, "/temp_DGA_", ano_ini_bdd, "_", ano_fin_bdd, "_", mes_fin, ".csv")
        data_vieja = read.csv(archivo_antiguo)
        data_vieja = data_vieja<- data_vieja %>% 
            filter(Year<ano_fin_bdd)
        #eliminar temp_mean de la data nueva, no la necesitamos
        data_nueva <- data_nueva %>% select(-temp_mean)
        temp_actualizada = rbind(data_vieja, data_nueva)
        archivo_nuevo <- paste0(directorio_archivo, "/temp_DGA_", ano_ini_bdd, "_", ano_actual, "_", mes_ultimo, ".csv")
        # Guardar los datos actualizados en un nuevo archivo
        write.csv(temp_actualizada, file = archivo_nuevo, row.names = FALSE)
        # Verificar que el archivo nuevo existe
        if (file.exists(archivo_nuevo)) {
            print(paste0("La base de datos de ", variable, " ", fuente, " ha sido actualizada correctamente."))
            file.remove(archivo_antiguo)
        } 
        else {
            print(paste0("El archivo nuevo de ", variable, " ", fuente, " no se ha creado correctamente."))
        }

    
    }
    else if(variable == "pp" && tolower(fuente) == "dmc"){
        archivo_antiguo = paste0(directorio_archivo, "/pp_DMC_", ano_ini_bdd, "_", ano_fin_bdd, "_", mes_fin, ".csv")
        data_vieja = read.csv(archivo_antiguo)
        data_vieja = data_vieja<- data_vieja %>% 
            filter(Year<ano_fin_bdd)
        pp_actualizada = rbind(data_vieja, data_nueva)
        archivo_nuevo = paste0(directorio_archivo, "/pp_DMC_", ano_ini_bdd, "_", ano_actual, "_", mes_ultimo, ".csv")
        write.csv(pp_actualizada, file = archivo_nuevo, row.names = FALSE)

        if(file.exists(archivo_nuevo)){
            print(paste0("La base de datos de ", variable, " ", fuente, " ha sido actualizada correctamente."))
            file.remove(archivo_antiguo)
        }
        else{
            print(paste0("El archivo nuevo de ", variable, " ", fuente, " no se ha creado correctamente."))
        }
    }
    else if(variable == "temp_max" && tolower(fuente) == "dmc"){
        data_vieja = read.csv(paste0(directorio_archivo, "/temp_max_DMC_", ano_ini_bdd, "_", ano_fin_bdd, "_", mes_fin, ".csv"))
        data_vieja = data_vieja<- data_vieja %>% 
            filter(Year<ano_fin_bdd)
        temp_max_actualizada = rbind(data_vieja, data_nueva)
        write.csv(temp_max_actualizada, file = paste0(directorio_archivo, "/temp_max_DMC_", ano_ini_bdd, "_", ano_actual, "_", mes_ultimo, ".csv"), row.names = FALSE)
    }
    else if(variable == "temp_min" && tolower(fuente) == "dmc"){
        data_vieja = read.csv(paste0(directorio_archivo, "/temp_min_DMC_", ano_ini_bdd, "_", ano_fin_bdd, "_", mes_fin, ".csv"))
        data_vieja = data_vieja<- data_vieja %>% 
            filter(Year<ano_fin_bdd)
        temp_min_actualizada = rbind(data_vieja, data_nueva)
        write.csv(temp_min_actualizada, file = paste0(directorio_archivo, "/temp_min_DMC_", ano_ini_bdd, "_", ano_actual, "_", mes_ultimo, ".csv"), row.names = FALSE)
    }
    #else if(variable == "niveles_pozos" && tolower(fuente) == "dga"){
    #    data_vieja = read.csv(paste0(directorio_archivo, "/niveles_pozos_DGA_", ano_ini_bdd, "_", ano_fin_bdd, "_", mes_fin, ".csv"))
    #    data_vieja = data_vieja<- data_vieja %>% 
    #        filter(Year<ano_fin_bdd)
    #    niveles_pozos_actualizada = rbind(data_vieja, data_nueva)
    #    write.csv(niveles_pozos_actualizada, file = paste0(directorio_archivo, "/niveles_pozos_DGA_", ano_ini_bdd, "_", ano_actual, "_", mes_ultimo, ".csv"), row.names = FALSE)
    #}
    else{
        stop("No se ha encontrado la combinación de variable y fuente")
    }
    return(TRUE)
}