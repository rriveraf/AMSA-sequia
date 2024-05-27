actualizar_indicador<- function(directorio_base, fuente, indicador){

  source(paste0(directorio_base, "/Manejo_BDD.R"))
  source(paste0(directorio_base, "/Calcular_indicadores.R"))

  valid_indicador <- c("ipe", "ipee", "ice")
  if (!(indicador %in% valid_indicador)) {
    stop("Argumento 'indicador' debe ser 'ipe', 'ipee' o 'ice'")
  }
  
  valid_fuente <- c("dga", "dmc")
  if (!(tolower(fuente) %in% valid_fuente)) {
    stop("Argumento 'fuente' debe ser 'DGA' or 'DMC'")
  }

  # Definimos fechas actuales (ultimo mes es el anterior al actual para asegurar datos mensuales completos)
  mes_ultimo <- as.numeric(substr(Sys.time() %m-% months(1), 6, 7))
  ano_actual <- as.numeric(substr(Sys.time() %m-% months(1), 1, 4))
    
  directorio_indicador <- obtener_directorio_indicadores(directorio_base, fuente, indicador)
  archivo_mas_grande <- buscar_archivo_mas_grande(directorio_indicador)

  #verificar estado de los archivos de indicadores
  #si no hay archivos de indicadores, verificar si hay datos para calcularlos y calcular
  #si archivos de indicadores están incompletos, verificar si hay datos para completarlos y completar
  if(length(archivo_mas_grande) != 0 && length(obtener_fechas_archivo(archivo_mas_grande)) != 0) {
    list_fechas <- obtener_fechas_archivo(archivo_mas_grande)
    ano_fin <- list_fechas$ano_fin
    mes_fin <- list_fechas$mes_fin

    if(ano_fin == ano_actual && mes_fin == mes_ultimo){
        print(paste0("Indicador ", indicador, " para ", fuente, " ya están actualizados, no es necesario calcularlos."))
    }
    else{
        print(paste0("Indicador ", indicador, " para ", fuente, " está incompleto, se procederá a actualizar."))
        resultado <- verificar_datos_para_indicadores(directorio_base, indicador, fuente, ano_actual, mes_ultimo)
        if(resultado == TRUE){
            calcular_indicador(directorio_base, fuente, indicador, ano_actual, mes_ultimo)
        }
        else{
            print(paste0("La base de datos necesaria para completar el indicador ", indicador, fuente, " está desactualizada", ". Porfavor asegurese de actualizar los datos necesarios.")) 
        } 
    }
  }
  else{
      print(paste0("No hay archivos de indicadores para ", indicador, " de ", fuente, ". Se procederá a calcularlos."))
      resultado <- verificar_datos_para_indicadores(directorio_base, indicador, fuente, ano_actual, mes_ultimo)
      if(resultado == TRUE){
          calcular_indicador(directorio_base, fuente, indicador, ano_actual, mes_ultimo)
      }
      else{
          print(paste0("La base de datos necesaria para completar el indicador ", indicador," ", fuente, " está desactualizada", ". Porfavor actualice los datos necesarios.")) 
      }
  }
}
