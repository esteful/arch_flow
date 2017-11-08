"entropia02"<-  function(x)
  {
    # calcula líentropia díun fitxer donat fila per fila
    # fet per Jaume Buxeda i GarrigÛs
    # versiÛ normalitzada a marÁ de 2015      
    n<-dim(x)[1]
    d<-dim(x)[2]
    varmat<-matrix(0, n,d+2)
    varmat2<-matrix(0,n,d)
    {
      # normalitza a 100% els valors de la matriu x
      varmat2<-as.matrix(sweep(x,1,apply(x,1,sum),FUN="/"))
    }
    for (j in 1:n)
    {
      for (k in 1:d)
      {
        if (varmat2[j,k]==0) varmat[j,k]<-0 else varmat[j,k]<-log((1/varmat2[j,k]))/log(2)
        varmat[j,d+1]<-varmat[j,d+1]+(varmat[j,k]*varmat2[j,k])
      }
    }
    varmat[,d+2]<-varmat[,d+1]/(log(d)/log(2))
    dimnames(varmat)<-list(c(dimnames(x)[[1]]),c(dimnames(x)[[2]], "H2", "H2%"))
    list(Entropia=as.data.frame(varmat),Probabilitat= as.data.frame(varmat2))
  }
