actualizar_mapas <- function(directorio_base){
    
    source(paste0(directorio_base, "/Interpolar_indicadores.R"))
    # Definimos fechas actuales (ultimo mes es el anterior al actual para asegurar datos mensuales completos)
    
    mes_ultimo <- 4
    #mes_ultimo <- as.numeric(substr(Sys.time() %m-% months(1), 6, 7))
    ano_actual <- as.numeric(substr(Sys.time() %m-% months(1), 1, 4))

    #Lo primero es verificar si existen los datos necesarios para realizar los mapas (indicadores consolidados).
    resultado = verificar_si_indicadores_consolidados(directorio_base, ano_actual, mes_ultimo)
    if(resultado != FALSE){
        print("Se encontraron los datos necesarios para realizar los mapas.")
        #Borramos los mapas anteriores
        borrar_directorios_mapas_indicadores(directorio_base)
        #generamos los nuevos mapas
        interpolar_y_actualizar_shapefiles(mes_ultimo, ano_actual)

    }
    else{
        print("No se encontraron los datos necesarios para poder actualizar los mapas. ")
        print("Porvafor actualice todos los Indicadores de Sequía para poder continuar.")
    }


}