"evariation.matrix2"<-
  function(x, reportfile = "VMreport.srp")
  {
    # Given a compositional data set as an input, x, this function
    # returns statistics based on the variation matrix described in
    # Buxeda, 1998, 11. The list returns values of vt/t.i (in Buxeda's
    # notation), the maximum of which determines the variable to use
    # as divisor in the subsequent log-ratio transformation. varmat can
    # be listed if the full variation matrix is needed.
    # Inicialment, fet per M.J. Baxter i adaptat per Jaume Buxeda i GarrigÛs
    # versiÛ normalitzada a marÁ de 2015 
    if(min(x) < 0) {
      cat("Negative data - terminating.")
      return()
    }
    p <- dim(x)[2]
    varmat <- matrix(0, p, p)
    varmat2<-matrix(0,p+4,p)
    for(i in 1:p) {
      varmat[, i] <- diag(var(log(x/x[, i])))
    }
    varsum <- apply(varmat, 2, sum)
    totvar <- sum(varmat)/(2 * p)
    varprop <- totvar/varsum
    varcor<-vector(mode="numeric",length=p)
    for(i in 1:p) {
      varcor[i]<-cor(varmat[-c(i),i],varsum[-i])
    }
    hola<-as.list(dimnames(x)[[2]])
    par(mar=c(5,5,4,2)+0.1,mgp=c(3,1,0))
    for(i in 1:p) {
      plot(varsum[-i],varmat[-c(i),i], xlab=list(expression(tau[.i]),font=2,cex=1.5),ylab=list( paste("var(ln(x/", dimnames(x[i])[[2]],"))",sep=""),cex=1,font=2),type="n",cex.axis=0.8)
      text(varsum[-i],varmat[-c(i),i], labels=dimnames(x[-i])[[2]],cex=0.5)
      rvarcor<-round(varcor[i],dig=6)
      text(min(varsum[-i]),max(varmat[-c(i),i]),label=bquote("r"[v*tau]*" = "*.(rvarcor)),cex=0.7,adj=0)
    }
    for(i in 1:p) varmat2[i,]<- varmat[i,]
    varmat2[p+1,]<- varsum
    varmat2[p+2,]<- varprop
    varmat2[p+3,]<- varcor
    varmat2[p+4,1]<- totvar
    dimnames(varmat2)<-list(c(dimnames(x)[[2]],"t.i","vt/t.i","r v,t","vt"),c(dimnames(x)[[2]]))
    cat("Values of vt/t.i (in Buxeda's notation).\n\nMaximum determines variable to use as divisor in subsequent log-ratio transformation.\n\n\n",
        varprop, "\n\n")
    ord <- order(varprop)
    cat("Maximum value : ", max(varprop), "\n")
    cat("Variable : ", ord[p], "\n")
    list(varmat2=varmat2)
    
    ###uniformitat
    talls=c(0.3, 0.5, 0.9)
    nom <- "Data"
    mida.boxplot=10
    rang.boxplot=c(-4,4)
    
    aaa<-length(talls)            # 3
    bbb<- matrix(0,aaa,1)         # (0,3,1)
    pop <- dim(x)[2]+1            # le suma 1 al total de variables (columnas)
    mat <- matrix(0, pop,1)       # matriz con valores 0 de tantas filas como variables y 1 columna
    
    #MVC<-evariation.matrix2(x)    # crea una matriz de variacion de la que se puede extraer varias cosas
    #no hace falta porque hemos extraido la matriz
    
    
    #MVC<-as.data.frame(MVC$varmat)#segundo paso para extraer solo la matriz de variacion
    
    MVC<-as.data.frame(varmat2)    #como hemos sacado la función evariation matrix podemos utiliar varmat
    tMVC<-as.data.frame(t(MVC))   #matriz transpuesta
    ordre<-order(tMVC[,c(pop)],decreasing=T)  #ordena las variables de la matriz transpuesta, la ultima columna?
    tMVC<-as.data.frame(tMVC[ordre,])
    
    for(i in 1:(pop-1)) {
      mat[i,1] <- tMVC[i, pop]
    }
    mat[pop,1] <- MVC[pop+3,1]
    
    dimnames(mat)[[1]]<-c(dimnames(tMVC)[[1]], "vt")
    
    tmat<-t(mat[c(1:pop-1),])
    
    tmatentro<-entropia02(tmat-mat[pop,1])
    
    par(mar=c(5,5,4,2)+0.1,mgp=c(3,1,0))
    
    if (nom=="NA") {
      
      plot(mat[,1], type="p",pch=16,ylab=list(expression(tau[.i]),font=2,cex=2),xlab="",xaxt="n",ylim=c(0, trunc(mat[1,1]) +1),cex.axis=0.8)      
    }
    else {
      plot(mat[,1], type="p",pch=16,ylab=list(expression(tau[.i]),font=2,cex=2),xlab="",xaxt="n",ylim=c(0, trunc(mat[1,1]) +1),cex.axis=0.8,main=bquote(paste(.(nom)," (n = ",.(dim(x)[1]),")")))
    }
    
    
    eti<-etiquetes.elements(mat)
    #axis(1, at=1:pop, labels=dimnames(mat)[[1]],font=2, las=2)
    axis(1, at=1:pop, labels=eti, font=2, las=2)
    lines(mat[c(1:pop-1),1])
    lines(c(0,pop),c(mat[pop,1],mat[pop,1]),lty=3)
    for (i in 1:aaa) {
      bbb[i,1]<-length(which(MVC[pop+1,]<talls[i]))
      if (bbb[i,1]>0) {
        segments(bbb[i,1]+0.5, par("usr")[3], bbb[i,1]+0.5, (mat[pop,1]*2.5)+(mat[pop,1]/talls[i]),lty=3)
        boxed.labels(bbb[i,1]+0.5,mat[pop,1]/2,talls[i],cex=0.6, col="black", bg="white",border=NA)
      }
    }
    h2<-round(tmatentro$Entropia[1,pop],dig=2)
    h2p<- round(tmatentro$Entropia[1,pop+1]*100,dig=2)
    vt<-round(mat[pop,1],dig=2)
    text(pop-5, (trunc(mat[1,1]) +1)-((trunc(mat[1,1]) +1)/14),label=bquote("    "*"H"[2]*" = "*.(h2)*" Sh"),cex=0.8,adj=0)
    text(pop-5, (trunc(mat[1,1]) +1)- ((trunc(mat[1,1]) +1)/8.5),label=bquote("H"[2]*" % = "*.(h2p)),cex=0.8,adj=0)
    text(pop-5, (trunc(mat[1,1]) +1)- ((trunc(mat[1,1]) +1)/6.2),label=bquote("     "*"vt = "*.(vt)),cex=0.8,adj=0)
    xxx<-recordPlot()
    #ara fem el dendrograma de la MVC
    dendroMVC<-hclust(as.dist(MVC[c(1:(dim(MVC)[2])),]),"ave")
    for (i in 1:(dim(MVC)[2])) {
      if (regexpr ("^Fe2O3$",as.character(dendroMVC[[4]][i]))==T) dendroMVC[[4]][i]<-expression("Fe"["2"]*"O"["3"])
      if (regexpr ("^Al2O3$", as.character(dendroMVC[[4]][i]))==T) dendroMVC[[4]][i]<-expression("Al"["2"]*"O"["3"])
      if (regexpr ("^P2O5$", as.character(dendroMVC[[4]][i]))==T) dendroMVC[[4]][i]<-expression("P"["2"]*"O"["5"])
      if (regexpr ("^TiO2$", as.character(dendroMVC[[4]][i]))==T) dendroMVC[[4]][i]<-expression("TiO"["2"])
      if (regexpr ("^Na2O$", as.character(dendroMVC[[4]][i]))==T) dendroMVC[[4]][i]<-expression("Na"["2"]*"O")
      if (regexpr ("^K2O$", as.character(dendroMVC[[4]][i]))==T) dendroMVC[[4]][i]<-expression("K"["2"]*"O")
      if (regexpr ("^SiO2$", as.character(dendroMVC[[4]][i]))==T) dendroMVC[[4]][i]<-expression("SiO"["2"])
    }
    plot(as.dendrogram(dendroMVC))
    yyy<-recordPlot()
    # ara fem el signary
    classi<-classifica(dendroMVC)
    Signary<-as.data.frame(t(classi$signary))
    dimnames(Signary)[[1]]<-dimnames(x)[[2]]
    CoDaDendrogram(acomp(x),signary=Signary, border="red4", col="goldenrod3", type="boxplot", box.space=mida.boxplot, range=rang.boxplot)
    zzz<-recordPlot()
    emf("uniformitat.emf")
    replayPlot(xxx)
    dev.off()
    pdf("uniformitat.pdf")
    replayPlot(xxx)
    dev.off()
    emf("dendroMVC.emf")
    replayPlot(yyy)
    dev.off()
    pdf("dendroMVC.pdf")
    replayPlot(yyy)
    dev.off()
    emf("CoDaDendro.emf")
    replayPlot(zzz)
    dev.off()
    pdf("CoDaDendro.pdf")
    replayPlot(zzz)
    dev.off()
    bases<-gsi.buildilrBase(Signary)
    Fitxer_ilr<-ilr(x,bases)
    par(mar=c(5,4,4,2)+0.1,mgp=c(3,1,0))
    list(MVC=MVC,Entropia=tmatentro$Entropia,Probabilitat= tmatentro$Probabilitat, Signary= Signary, Bases= bases, Fitxer_ilr= Fitxer_ilr)
  
  }

