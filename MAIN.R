#definir directorio base
directorio_base = getwd()

#LLamar a scripts
source(paste0(directorio_base, "/Librerias.R"))
source(paste0(directorio_base, "/Manejo_BDD.R"))
source(paste0(directorio_base, "/Actualizar_bbdd.R"))
source(paste0(directorio_base, "/Actualizar_indicadores.R"))
source(paste0(directorio_base, "/Graficar_series_indicadores.R"))
source(paste0(directorio_base, "/Funciones_de_utilidad.R"))

actualizar_datos(directorio_base, "pp", "DGA")
actualizar_datos(directorio_base, "pp", "DMC")

actualizar_datos(directorio_base, "temp", "DGA")
actualizar_datos(directorio_base, "caudal", "DGA")
#actualizar_datos(directorio_base, "niveles_pozos", "DGA")
actualizar_datos(directorio_base, "temp", "DMC")


actualizar_indicador(directorio_base, "DGA", "ipe")
actualizar_indicador(directorio_base, "dmc", "ipe") 
actualizar_indicador(directorio_base, "DGA", "ice")
actualizar_indicador(directorio_base, "dmc", "ipee")
actualizar_indicador(directorio_base, "DGA", "ipee")
# Calcular indicadores

