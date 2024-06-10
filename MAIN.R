#definir directorio base
directorio_base = getwd()
#LLamar a scripts
source(paste0(directorio_base, "/Librerias.R"))
source(paste0(directorio_base, "/Manejo_BDD.R"))
source(paste0(directorio_base, "/Actualizar_bbdd.R"))
source(paste0(directorio_base, "/Actualizar_indicadores.R"))
source(paste0(directorio_base, "/Actualizar_mapas.R"))
source(paste0(directorio_base, "/Graficar_series_indicadores.R"))
source(paste0(directorio_base, "/Funciones_de_utilidad.R"))
source(paste0(directorio_base, "/Actualizar_mapas.R"))
source(paste0(directorio_base, "/Publicar_arcgis_online.R"))

actualizar_datos(directorio_base, "pp", "DGA") #ok
actualizar_datos(directorio_base, "pp", "DMC") #ok
actualizar_datos(directorio_base, "temp", "DGA") #ok
actualizar_datos(directorio_base, "caudal", "DGA") 
actualizar_datos(directorio_base, "temp", "DMC")

#actualizar indicadores
actualizar_indicador(directorio_base, "DGA", "ipe")
actualizar_indicador(directorio_base, "dmc", "ipe") 
actualizar_indicador(directorio_base, "DGA", "ice")
actualizar_indicador(directorio_base, "dmc", "ipee")
actualizar_indicador(directorio_base, "DGA", "ipee")

actualizar_mapas(directorio_base)
subir_mapas(directorio_base)


#raster_list <- actualizar_mapas(directorio_base, "ice")
#generar raster de indicadores


