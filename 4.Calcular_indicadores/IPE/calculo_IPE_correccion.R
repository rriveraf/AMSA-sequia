###############################################################################
################# Codigo Juan de Dios: Modificacion Marialina ################# 
###############################################################################

calculo_ipe<-function(P_k,alpha, beta, pze){
  
  # P_k es el acumulado (promedio) de los ?ltimos "n" meses 
  # de la precipitaci?n acumulada mensual
  # Coeficientes alpha y beta de la distributci?n gamma
  # obtenidos del periodo de referencia
  # pze es la probabilidad de 0 para aplicar la gamma incompleta
  if (is.na(P_k) || is.na(alpha) || is.na(beta) || is.na(pze)){
    ind_IPE <- NA
  }
  else if (P_k<0){
    ind_IPE <- NA
  }
  else if (P_k==0){
    #Cuando no existe pze y el acumulado es 0
    P_k<-0.0001 #Valor muy pequeno
    ind_IPE<-qnorm(pgamma(P_k,shape = alpha, rate = 1/beta))
    ind_IPE<-qnorm(pze+(1-pze)*pnorm(ind_IPE))}
  else if (P_k>0){
    ind_IPE<-qnorm(pgamma(P_k,shape = alpha, rate = 1/beta))
    ind_IPE<-qnorm(pze+(1-pze)*pnorm(ind_IPE))}
  #Caso de que de -Inf
  if (is.infinite(ind_IPE) && (ind_IPE < 0)){
    ind_IPE<- -3}
  #Caso de que de Inf
  if (is.infinite(ind_IPE)){
    ind_IPE<- 3}
  
  return(ind_IPE)}
