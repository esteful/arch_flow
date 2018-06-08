
arch_mahala <- function(group_list, test_sample, rm_vars =rm_vars){

  #group list contains the list of df for each compositional groups (as dataframes)
  #test samples contains contains only one sample, e.g. for "GA1" define test_sample <- GR_NF["GA1",-c(rm_vars)]
  #rm_vars are the variables to be removed (normally categorical ones and undesired elements)


z = 1
for (i in group_list){
  print(names(group_list)[z])
  maha.prob(i[,-c(rm_vars)], test_sample[,-c(rm_vars)]) -> maha
  print(maha$varmat)
  z = z+1#
}
}




maha.prob<-
  function(x,y)
  {
    # Calcula el centroide d’un grup donat x i amb això calcula les distàncies 
    # de Mahalanobis dels individus del segon fitxer y a el grup inicial.
    # Posteriorment, calcula la T quadrada de Hotelling i d’aquí ho passa a
    # un test F d’igualtat multivariant, calculant-ne la probabilitat.
    s<-dim(y)[2]
    na<-dim(y)[1]
    nb<-dim(x)[1]
    varmat <- matrix(0, na, 4)
    varmat[,1]<-mahalanobis(y,colMeans(x),var(x))
    varmat[,2]<-varmat[,1]*(nb/(nb+1))
    varmat[,3]<-varmat[,2]*((1+nb-s-1)/((1+nb-2)*s))
    varmat[,4]<-round(as.single(1-pf(varmat[,3],s,(1+nb-s-1))),digits=6)
    dimnames(varmat)<-list(c(dimnames(y)[[1]]),c("Dist.Maha.","T2","F","Prob."))
    list(varmat=varmat)
  }




