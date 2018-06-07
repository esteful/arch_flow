"classifica"<-
  function(x)
  {
    # Aquesta rutina fa una matriu signary per fer balances a partir díun dendrograma
    # de les variables en un procÈs díuniformitat 
    n<-length(x[[4]])
    sortida<-matrix(0,n-1,n)
    signary<-matrix(-2,n-1,n) #-2 per no crear confusions amb valors possibles
    zeros<-matrix(-2,n-1,n) #cal una matriu a part de signary pels zeros de leafs
    # ara mirem com sÛn les fusions
    for (i in 1:n-1) {
      sortida[i,]<-cutree(x,k=i+1)
    }
    # ara resolem lÌnia per lÌnia. La primera i lí˙ltima sÛn especials. 
    # La primera nomÈs tÈ dos grups. Lí˙ltima nomÈs tÈ dos individus
    for (i in 1:n-1){
      if (i==1) {
        posar<-which(sortida[i,]==1)
        signary[i,posar]<-1
        # ara cal veure si Ès leaf i posar-hi zeros a sota
        if (length(posar)==1) {
          signary[c((i+1):(n-1)),posar]<-0
          zeros[c((i+1):(n-1)),posar]<-0
        }
        posar<-which(sortida[i,]==2)
        signary[i,posar]<--1
        if (length(posar)==1) {
          signary[c((i+1):(n-1)),posar]<-0
          zeros[c((i+1):(n-1)),posar]<-0
        }
      }
      if (i>1) {
        if (i<(n-1)) {
          # prepara un vector f1 on poso els zeros per ser un factor
          # de la fila i-1 i un vector f2 per ser un factor de la fila i
          f1<-sortida[(i-1),]
          f2<-sortida[i,]
          posar<-which(zeros[(i-1),]==0)
          if (length(posar)>0) {f1[posar]<-0}
          posar<-which(zeros[i,]==0)
          if (length(posar)>0) {f2[posar]<-0}
          # ara converteixo f1 i f2 en factors, miro i faig signary per levels
          f1<-as.factor(t(f1))
          f2<-as.factor(t(f2))
          # faig una matriu per avaluar si els grups sÛn diferents
          nf1<-nlevels(f1)
          nf2<-nlevels(f2)
          m<-matrix(TRUE,nf1,nf2)
          if (levels(f2)[1]== "0") {des<-2} else {des<-1}
          for (j in 1:nf1) {
            for (jj in des:nf2) {
              posarf1<-which(f1==levels(f1)[j])
              posarf2<-which(f2==levels(f2)[jj])
              m[j,jj]<-identical(posarf1,posarf2)
            }
          }
          # ara faig un comptador per buscar dues columnes de F de 1 i -1
          comptador<-1
          for (jj in 1:nf2) {
            if (any(m[,jj])==FALSE) {
              if (levels(f2)[jj]!= "0") {
                posarf2<-which(f2==levels(f2)[jj])
                signary[i,posarf2]<-comptador
                comptador<--1
              }
            }
            if (any(m[,jj])==TRUE) {
              posarf2<-which(f2==levels(f2)[jj])
              signary[i,posarf2]<-0
            }
          }
          # ara cal posar 0 a les columnes de leafs
          posar<-which(signary[i,]==1)
          if (length(posar)==1) {
            signary[c((i+1):(n-1)),posar]<-0
            zeros[c((i+1):(n-1)),posar]<-0
          }
          posar<-which(signary[i,]==-1)
          if (length(posar)==1) {
            signary[c((i+1):(n-1)),posar]<-0
            zeros[c((i+1):(n-1)),posar]<-0
          }
        }
        if (i==(n-1)) {
          f2<-sortida[i,]
          posar<-which(zeros[i,]==0)
          if (length(posar)>0) {f2[posar]<-0}
          comptador=1
          for (jj in 1:n) {
            if (f2[jj]!=0) {
              f2[jj]<-comptador
              comptador<--1
            }
          }
          signary[i,]<-f2
        }
      } 
    }
    dimnames(sortida)[[2]] <-x[[4]]
    list(sortida=as.data.frame(sortida), signary=as.data.frame(signary), f1=f1, f2=f2, m=as.data.frame(m), des=des, zeros=as.data.frame(zeros))
  }

