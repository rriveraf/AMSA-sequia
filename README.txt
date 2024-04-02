README del Programa de Cálculo de Indicadores Meteorológicos
Este programa ha sido desarrollado por Javier Enrique Vargas Ramírez y Marialina Ñuñez con financiamiento del Centro de Cambio Global UC. El programa está escrito en lenguaje de programación R, versión 4.3.0. El objetivo de esta aplicación es calcular indicadores meteorológicos estandarizados a partir de datos de precipitación, temperatura y caudal. El programa está organizado en varias carpetas para facilitar la comprensión y el mantenimiento del código.

Para ejecutar el programa y calcular el Indicador de Precipitación Estandarizado (IPE), es necesario descomprimir la carpeta del programa en un proyecto nuevo de R. Luego, abrir la carpeta Main y ejecutar el script "calcular_ipe.R".
Para calcular otro indicador, como el Indicador de Precipitación Estandarizado Extendido (IPEE), proceder de manera similar, pero abrir el script correspondiente, por ejemplo, "calcular_ipee.R".
Asegúrese de que el programa esté leyendo correctamente el directorio de trabajo para evitar posibles errores.

Contacto y Soporte:
Si tiene preguntas, problemas o sugerencias sobre este programa, no dude en ponerse en contacto con el desarrollador, Javier Enrique Vargas Ramírez, a través del siguiente correo electrónico: jqvargas@uc.cl

¡Gracias por utilizar este programa! Esperamos que sea útil para sus necesidades de cálculo de indicadores meteorológicos.

*********Estructura del Programa*********
El programa está dividido en las siguientes carpetas:

Main:

Contiene los scripts principales para ejecutar el programa y controlar el algoritmo en su totalidad.
Descargar:

Contiene algoritmos para realizar la descarga automática de datos de precipitación, temperatura y caudal desde fuentes de estaciones meteorológicas automáticas de la DGA y la DMC. Los datos descargados son depurados superficialmente para eliminar outliers y datos imposibles.
Promediar_formatear:

Contiene funciones de utilidad general utilizadas para manipular las bases de datos. Estas funciones son esenciales para el manejo eficiente de los datos.
Concatenar:

Contiene scripts y funciones para concatenar y combinar diferentes conjuntos de datos. Esta carpeta se utiliza para organizar y estructurar los datos de manera adecuada antes de realizar los cálculos.
Calcular_indicadores:

Contiene algoritmos para calcular los indicadores meteorológicos estandarizados. Aquí se llevan a cabo los cálculos que generan los resultados finales.
Concatenar_final:

Incluye scripts adicionales para la concatenación final de los datos procesados, si es necesario.
BBDD:

Se utiliza para administrar toda la información utilizada por el programa. Aquí se almacenan y gestionan las bases de datos y otros archivos necesarios para el funcionamiento del programa.
Consideraciones Importantes
Depuración de Datos:

El programa realiza una depuración superficial de los datos, eliminando outliers y valores imposibles. Sin embargo, se reconoce que existe una brecha en la identificación de estaciones que marcan precipitación nula en días en que otras estaciones cercanas registran precipitación abundante. Esta área representa una oportunidad de mejora para futuras versiones del programa.
Ejecución del Programa:

