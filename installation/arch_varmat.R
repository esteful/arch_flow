
"arch_varmat"<-  function(x)
  #returns the variation matrix 
  #prints the less varying variable to use as divisor in alr transformation
  #from Buxeda i Garrigós evariation.matrix2 R function
  
{
  #create the matrix 
  p <- dim(x)[2]
  varmat <- matrix(0, p, p)
  varmat2<-matrix(0,p+4,p)
  
  #add values to the matrix (the log of the x/)
  for(i in 1:p) {
    varmat[, i] <- diag(var(log(x/x[, i])))
  }
  varsum <- apply(varmat, 2, sum) #sumatorio
  totvar <- sum(varmat)/(2 * p)
  varprop <- totvar/varsum
  varcor<-vector(mode="numeric",length=p)
  for(i in 1:p) {
    varcor[i]<-cor(varmat[-c(i),i],varsum[-i])
  }
  hola<-as.list(dimnames(x)[[2]])
  par(mar=c(5,5,4,2)+0.1,mgp=c(3,1,0))
  
  #add the total variation, etc
  for(i in 1:p) varmat2[i,]<- varmat[i,]
  varmat2[p+1,]<- varsum
  varmat2[p+2,]<- varprop
  varmat2[p+3,]<- varcor
  varmat2[p+4,1]<- totvar
  dimnames(varmat2)<-list(c(dimnames(x)[[2]],"t.i","vt/t.i","r v,t","vt"),c(dimnames(x)[[2]]))
  cat("%Values of vt/t.i (in Buxeda's notation).\n\n
  %Maximum determines variable to use as divisor in subsequent log-ratio transformation.\n\n\n", "%", varprop, "\n\n")
  ord <- order(varprop)
  cat("%Maximum value : ", max(varprop), "\n")
  cat("%Variable : ", ord[p], "\n")
  
  #lvar: less variying element
  assign(".lvar", ord[p],.GlobalEnv)
  
  #return a the variation matrix
  return(varmat2)
}





















