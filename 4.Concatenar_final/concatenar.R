
concatenar<-function(base_entrada, indicador_unir, fecha){

library(tidyverse)

  ##base_entrada tiene que tener la fecha en el formato correcto
  ## los Codigos_nacional en formato correcto, primera columna se llama date
  nombre_col <- as.character(deparse(substitute(indicador_unir)))
  indicador_unir<-indicador_unir %>%
    filter(date==fecha)
  
  indicador_unir<-pivot_longer(indicador_unir, cols = -date, names_to = "Codigo_nacional", values_to = nombre_col)
  
  consolidado <- left_join(base_entrada, indicador_unir, 
                       by = c("Codigo_nacional"))%>%
    filter(Codigo_nacional != 0)
  
  if ("date.y" %in% names(consolidado)) {
    consolidado <- consolidado %>%
      select(-date.y)
  }
  if ("date.x" %in% names(consolidado)) {
    consolidado <- consolidado %>%
      rename(date=date.x)
  }
  consolidado$date[is.na(consolidado$date)] <- fecha
  return (consolidado)
}