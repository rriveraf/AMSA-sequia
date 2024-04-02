###############################################################################
################# Codigo Juan de Dios: Modificacion Marialina ################# 
###############################################################################

calculo_ice<-function(Q,alpha, beta, pze){
  
  # Q es la media (promedio) de los ultimos "n" meses 
  # del caudal medio mensual
  # Coeficientes alpha y beta de la distributci?n gamma
  # obtenidos del periodo de referencia
  # pze es la probabilidad de 0 para aplicar la gamma incompleta
  if (is.na(Q) || is.na(alpha) || is.na(beta) || is.na(pze)){
    ind_IPE <- NA
  }
  else if (Q<0){
    ind_IPE <- NA
  }
  else if (Q==0){
    #Cuando no existe pze y el acumulado es 0
    P_k<-0.0001 #Valor muy pequeno
    ind_IPE<-qnorm(pgamma(Q,shape = alpha, rate = 1/beta))
    ind_IPE<-qnorm(pze+(1-pze)*pnorm(ind_IPE))}
  else if (Q>0){
    ind_IPE<-qnorm(pgamma(Q,shape = alpha, rate = 1/beta))
    ind_IPE<-qnorm(pze+(1-pze)*pnorm(ind_IPE))}
  #Caso de que de -Inf
  if (is.infinite(ind_IPE) && (ind_IPE < 0)){
    ind_IPE<- -3}
  #Caso de que de Inf
  if (is.infinite(ind_IPE)){
    ind_IPE<- 3}
  
  return(ind_IPE)}
