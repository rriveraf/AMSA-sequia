# Description: Este script contiene las funciones necesarias para descargar y manejar la base de datos de precipitación, caudal y temperatura.

obtener_directorio_descargas<-function(variable, fuente){
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
    else if(variable == "temp" & tolower(fuente) == "dmc") {
        directorio = file.path(directorio_base, "BBDD", "temp", "DMC", "bruto")
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
    else if(variable == "temp" & tolower(fuente) == "dmc"){

        print("Descargando temperatura máxima")
        temp_max = descargar_temp_max_DMC(ano_inicio, ano_actual, mes_ultimo, estaciones_dmc)
        print("Descargando temperatura mínima")
        temp_min = descargar_temp_min_DMC(ano_inicio, ano_actual, mes_ultimo, estaciones_dmc)
        temp_dmc = merge(temp_max, temp_min, by = c("Year", "Month", "Day",  "Codigo_nacional"))
        return(temp_dmc)
    }   
    else{
        stop("No se ha encontrado la combinación de variable y fuente")
    }
}

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
    else if(variable == "temp" & tolower(fuente) == "dmc"){
        write.csv(data_nueva, file = paste0(directorio_archivo, "/temp_DMC_", ano_inicio, "_", ano_actual, "_", mes_ultimo, ".csv"), row.names = FALSE)
    }
    else{
        stop("No se ha encontrado la combinación de variable y fuente")
    }

}

concatenar_datos_descargados <- function(data_nueva, list_fechas, ano_actual, mes_ultimo, variable, fuente, directorio_archivo){
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
        data_vieja = data_vieja <- data_vieja %>% 
          filter(Year<ano_fin_bdd)
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
    else if(variable == "temp" && tolower(fuente) == "dmc"){
        archivo_antiguo = paste0(directorio_archivo, "/temp_DMC_", ano_ini_bdd, "_", ano_fin_bdd, "_", mes_fin, ".csv")
        data_vieja = read.csv(archivo_antiguo)
        data_vieja = data_vieja<- data_vieja %>% 
            filter(Year<ano_fin_bdd)
        temp_actualizada = rbind(data_vieja, data_nueva)
        archivo_nuevo = paste0(directorio_archivo, "/temp_DMC_", ano_ini_bdd, "_", ano_actual, "_", mes_ultimo, ".csv")
        write.csv(temp_actualizada, file = archivo_nuevo, row.names = FALSE)

        if(file.exists(archivo_nuevo)){
            print(paste0("La base de datos de ", variable, " ", fuente, " ha sido actualizada correctamente."))
            file.remove(archivo_antiguo)
        }
        else{
            print(paste0("El archivo nuevo de ", variable, " ", fuente, " no se ha creado correctamente."))
        }
    }

    else{
        stop("No se ha encontrado la combinación de variable y fuente")
    }
    return(TRUE)
}

#Función para eliminar archivos antiguos, solo se aplicará a la BDD de archivos depurados y concatenados.
#No así a la bdd de archivos descargados.
#Se eliminarán los archivos que tengan una fecha de fin menor a la fecha actual
eliminar_archivos_antiguos <- function(directorio, ano_actual, mes_ultimo){
    archivos = list.files(directorio, pattern = ".csv", full.names = TRUE)
    for(archivo in archivos){
        fecha = obtener_fechas_archivo(archivo)
        if(length(fecha) != 0){
            if(fecha$ano_fin < ano_actual | (fecha$ano_fin == ano_actual & fecha$mes_fin < mes_ultimo)){
                file.remove(archivo)
            }
        }
    }
}

depurar_variable <- function(directorio_base, directorio_depurado, variable, fuente, ano_actual, mes_ultimo){
    directorio_descargas = obtener_directorio_descargas(variable, fuente)
    if(variable == "pp" && tolower(fuente) == "dga"){
        ruta_archivo_descargado = paste0(directorio_descargas, "/pp_DGA_", 2020, "_", ano_actual, "_", mes_ultimo, ".csv")
        ruta_archivo_metadata = paste0(directorio_base, "/BBDD/metadatos/DGA/pp_y_temp/estaciones_DGA_pp_y_temp.csv")
        resultado = procesar_pp_DGA_DMC(directorio_base, ruta_archivo_descargado, ruta_archivo_metadata, fuente = "DGA")
        if(resultado == FALSE){
            stop("Error al procesar los datos de precipitación")
        }
    }
    else if(variable == "pp" && tolower(fuente) == "dmc")
    {
        ruta_archivo_descargado = paste0(directorio_descargas, "/pp_DMC_", 2020, "_", ano_actual, "_", mes_ultimo, ".csv")
        ruta_archivo_metadata = paste0(directorio_base, "/BBDD/metadatos/DMC/estaciones_DMC.xlsx")
        resultado = procesar_pp_DGA_DMC(directorio_base, ruta_archivo_descargado, ruta_archivo_metadata, fuente = "DMC")
        if(resultado == FALSE){
            stop("Error al procesar los datos de precipitación")
        }
    }
    else if(variable == "temp" && tolower(fuente) == "dga")
    {
        ruta_archivo_descargado = paste0(directorio_descargas, "/temp_DGA_", 2020, "_", ano_actual, "_", mes_ultimo, ".csv")
        ruta_archivo_metadata = paste0(directorio_base, "/BBDD/metadatos/DGA/pp_y_temp/estaciones_DGA_pp_y_temp.csv")
        resultado = procesar_temp_DGA_DMC(directorio_base, ruta_archivo_descargado, ruta_archivo_metadata, fuente = "DGA")
        if(resultado == FALSE){
            stop("Error al procesar los datos de temperatura")
        }
    }
    else if(variable == "temp" && tolower(fuente) == "dmc")
    {
        ruta_archivo_descargado = paste0(directorio_descargas, "/temp_DMC_", 2020, "_", ano_actual, "_", mes_ultimo, ".csv")
        ruta_archivo_metadata = paste0(directorio_base, "/BBDD/metadatos/DMC/estaciones_DMC.xlsx")
        resultado = procesar_temp_DMC_DMC(directorio_base, ruta_archivo_descargado, ruta_archivo_metadata, fuente = "DMC")
        if(resultado == FALSE){
            stop("Error al procesar los datos de temperatura")
        }
    }
    else if(variable == "caudal" && tolower(fuente) == "dga")
    {
        ruta_archivo_descargado = paste0(directorio_descargas, "/q_mean_DGA_", 2020, "_", ano_actual, "_", mes_ultimo, ".csv")
        ruta_archivo_metadata = paste0(directorio_base, "/BBDD/metadatos/DGA/caudal/estaciones_DGA_caudal.csv")
        resultado = procesar_caudal_DGA(directorio_base, ruta_archivo_descargado, ruta_archivo_metadata)
        if(resultado == FALSE){
            stop("Error al procesar los datos de caudal")
        }
    }
    else{
        stop("No se ha encontrado la combinación de variable y fuente")
    }
    # Eliminar archivos antiguos del directorio depurado
    eliminar_archivos_antiguos(directorio_depurado, ano_actual, mes_ultimo)
}

concatenar_variable <- function(directorio_base, directorio_concatenado, variable, fuente, ano_actual, mes_ultimo){
    if(variable == "pp" && tolower(fuente) == "dga"){
        ruta_archivo_pp_depurado = paste0(directorio_base, "/BBDD/pp/DGA/depurado/pp_DGA_2020_", ano_actual, "_", mes_ultimo, ".csv")
        resultado = concatenar_pp_DGA_DMC(directorio_base, ruta_archivo_pp_depurado, fuente = "DGA", ano_actual, mes_ultimo)
        if(resultado == FALSE){
            stop("Error al concatenar los datos de precipitación")
        }
    }
    else if(variable == "pp" && tolower(fuente) == "dmc"){
        ruta_archivo_pp_depurado = paste0(directorio_base, "/BBDD/pp/DMC/depurado/pp_DMC_2020_", ano_actual, "_", mes_ultimo, ".csv")
        resultado = concatenar_pp_DGA_DMC(directorio_base, ruta_archivo_pp_depurado, fuente = "DMC", ano_actual, mes_ultimo)
        if(resultado == FALSE){
            stop("Error al concatenar los datos de precipitación")
        }
    }
    else if(variable == "temp" && tolower(fuente) == "dga"){
        ruta_archivo_temp_depurado = paste0(directorio_base, "/BBDD/temp/DGA/depurado/temp_DGA_2020_", ano_actual, "_", mes_ultimo, ".csv")
        resultado = concatenar_temp_DGA_DMC(directorio_base, ruta_archivo_temp_depurado, fuente = "DGA", ano_actual, mes_ultimo)
        if(resultado == FALSE){
            stop("Error al concatenar los datos de temperatura")
        }
    }
    else if(variable == "temp" && tolower(fuente) == "dmc"){
        ruta_archivo_temp_depurado = paste0(directorio_base, "/BBDD/temp/DMC/depurado/temp_DMC_2020_", ano_actual, "_", mes_ultimo, ".csv")
        resultado = concatenar_temp_DGA_DMC(directorio_base, ruta_archivo_temp_depurado, fuente = "DMC", ano_actual, mes_ultimo)
        if(resultado == FALSE){
            stop("Error al concatenar los datos de temperatura")
        }
    }
    else if(variable == "caudal" && tolower(fuente) == "dga"){
        ruta_archivo_caudal_depurado = paste0(directorio_base, "/BBDD/q/DGA/depurado/q_mean_DGA_2020_", ano_actual, "_", mes_ultimo, ".csv")
        ruta_archivo_metadatos = paste0(directorio_base, "/BBDD/metadatos/DGA/caudal/estaciones_DGA_caudal.csv")
        resultado = concatenar_caudal_DGA(directorio_base, ruta_archivo_caudal_depurado, ruta_archivo_metadatos, ano_actual, mes_ultimo)
        if(resultado == FALSE){
            stop("Error al concatenar los datos de caudal")
        }
    }
    else{
        stop("No se ha encontrado la combinación de variable y fuente")
    }
    # Eliminar archivos antiguos del directorio concatenado
    eliminar_archivos_antiguos(directorio_concatenado, ano_actual, mes_ultimo)
}

obtener_directorio_indicadores <- function(directorio_base, fuente, indicador){
    if(tolower(indicador) == "ipe" && tolower(fuente) == "dga"){
        directorio = file.path(directorio_base, "BBDD", "indicadores", "IPE", "DGA")
    }
    else if(tolower(indicador) == "ipe" && tolower(fuente) == "dmc"){
        directorio = file.path(directorio_base, "BBDD", "indicadores", "IPE", "DMC")
    }
    else if(tolower(indicador) == "ipee" && tolower(fuente) == "dga"){
        directorio = file.path(directorio_base, "BBDD", "indicadores", "IPEE", "DGA")
    }
    else if(tolower(indicador) == "ipee" && tolower(fuente) == "dmc"){
        directorio = file.path(directorio_base, "BBDD", "indicadores", "IPEE", "DMC")
    }
    else if(tolower(indicador) == "ice" && tolower(fuente) == "dga"){
        directorio = file.path(directorio_base, "BBDD", "indicadores", "ICE", "DGA")
    }
    else{
        stop("No se ha encontrado la combinación de fuente e indicador")
    }
    return(directorio)
}

verificar_datos_para_indicadores <- function(directorio_base, indicador, fuente, ano_actual, mes_ultimo){
    if(tolower(indicador) == "ice" && tolower(fuente) == "dga"){
        archivo_caudal = paste0(directorio_base, "/BBDD/q/DGA/concatenado/q_DGA_mensual_1989_", ano_actual, "_", mes_ultimo, ".csv")
        if(file.exists(archivo_caudal)){
            return(TRUE)
        }
        else{
            return(FALSE)
        }
    }
    if(tolower(indicador) == "ipe" && tolower(fuente) == "dga"){
        archivo_pp = paste0(directorio_base, "/BBDD/pp/DGA/concatenado/pp_DGA_mensual_1979_", ano_actual, "_", mes_ultimo, ".csv")
        if(file.exists(archivo_pp)){
            return(TRUE)
        }
        else{
            return(FALSE)
        }
    }
    if(tolower(indicador) == "ipe" && tolower(fuente) == "dmc"){
        archivo_pp = paste0(directorio_base, "/BBDD/pp/DMC/concatenado/pp_DMC_mensual_1979_", ano_actual, "_", mes_ultimo, ".csv")
        if(file.exists(archivo_pp)){
            return(TRUE)
        }
        else{
            return(FALSE)
        }
    }
    if(tolower(indicador) == "ipee" && tolower(fuente) == "dga"){
        archivo_pp = paste0(directorio_base, "/BBDD/pp/DGA/concatenado/pp_DGA_mensual_1979_", ano_actual, "_", mes_ultimo, ".csv")
        archivo_temp = paste0(directorio_base, "/BBDD/temp/DGA/concatenado/temp_DGA_mensual_1979_", ano_actual, "_", mes_ultimo, ".csv")
        if(file.exists(archivo_pp) && file.exists(archivo_temp)){
            return(TRUE)
        }
        else{
            return(FALSE)
        }
    }
    if(tolower(indicador) == "ipee" && tolower(fuente) == "dmc"){
        archivo_pp = paste0(directorio_base, "/BBDD/pp/DMC/concatenado/pp_DMC_mensual_1979_", ano_actual, "_", mes_ultimo, ".csv")
        archivo_temp = paste0(directorio_base, "/BBDD/temp/DMC/concatenado/temp_DMC_mensual_1979_", ano_actual, "_", mes_ultimo, ".csv")
        if(file.exists(archivo_pp) && file.exists(archivo_temp)){
            return(TRUE)
        }
        else{
            return(FALSE)
        }
    }
    else{
        return(FALSE)
    }
}
