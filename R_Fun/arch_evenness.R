
"arch_evenness" <- function(df_chem)
#compositions plotrix and devEMF
  {
  
#Code modified from the original code from J.Buxeda i Garrigos 
#Based on the observations from M.Baxter and Buxeda on compositional data
#It serves to asses the uniformity of a dataset 


  df_chem -> x
 
#create the matrix with the corresponding dimensions
      p <- dim(x)[2] #number of columns
      varmat <- matrix(0, p, p) # create a matrix with column number and equal row number
      varmat2<-matrix(0,p+4,p) # same matrix with 4 more rows 
  
      
#add values in the matrix
       for(i in 1:p) {# for each variable
        varmat[, i] <- diag(var(log(x/x[, i]))) #add the values to the matrix
        #diagonal matrix with the varianzes of the log x divided by the value of the corresponding variable
    }
  
  
#add values in the last 4 rows
  
#"t.i": the sum of the individual variabilities for each value in a given column
       varsum  <- apply(varmat, 2, sum)   
  
#vt is the total variation: sum of every "t.i" from all columns (a single value)
       totvar  <- sum(varmat)/(2 * p)                
  
# vt/t.i (ratio of total variation and individual variation)
       varprop <- totvar/varsum                     
  
#"r v,t" calculates the correlation between individual and total variation
       varcor  <- vector(mode="numeric",length=p)    
  
 #Correlation matrix
      for(i in 1:p) {
         varcor[i]<-cor(varmat[-c(i),i],varsum[-i])
         }
  
 #list of variables 
       hola<-as.list(dimnames(x)[[2]]) 
  
#add values to the matrix
  for(i in 1:p) 
      varmat2[i,]  <-  varmat[i,]  #add previously calculated values
      varmat2[p+1,]<-  varsum      #add the sum of all variabilities 
      varmat2[p+2,]<-  varprop     #add the ration of total variation and individual variation
      varmat2[p+3,]<-  varcor      #add correlation values  
      varmat2[p+4,1]<- totvar      #add the total variation, only one value in the first column
  
  
#set the names
      dimnames(varmat2)<-list(c(dimnames(x)[[2]],"t.i","vt/t.i","r v,t","vt"),c(dimnames(x)[[2]])) 
  
#vt/t.i values by order
     ord <- order(varprop)
     lvar <- ord[p]
     
#### Plot graphics
  
#Set the dimensions for the graph of MVC
  
    talls=c(0.3, 0.5, 0.9) #vertical doted axis 
    main_title <- "Data"
    mida.boxplot=10
    rang.boxplot=c(-4,4)
  
    aaa<-length(talls)            # 3
    bbb<- matrix(0,aaa,1)         # (0,3,1)
    pop <- dim(x)[2]+1            # all variables + 1
    mat <- matrix(0, pop,1)       # a matrix with all variables + 1 
  
 
#Create a dataframe
   
    MVC<-as.data.frame(varmat2)               #MVC (Compositional variation matrix) varmat previously created
    tMVC<-as.data.frame(t(MVC))               #transpose the matrix
    ordre<-order(tMVC[,c(pop)],decreasing=T)  #order the variables 
    tMVC<-as.data.frame(tMVC[ordre,])         #transpose the matrix
  
#MVC plot
    for(i in 1:(pop-1)) {
    mat[i,1] <- tMVC[i, pop]
      }
   
    mat[pop,1] <- MVC[pop+3,1]
    dimnames(mat)[[1]]<-c(dimnames(tMVC)[[1]], "vt")
    tmat<-t(mat[c(1:pop-1),])
    tmatentro<-entropia02(tmat-mat[pop,1])
    par(mar=c(5,5,4,2)+0.1,mgp=c(3,1,0))
  
    
#Add title and y axis on the graph 1
    
  if (main_title=="NA") {
      plot(mat[,1], type="p",pch=16,ylab=list(expression(tau[.i]),font=2,cex=2),xlab="",xaxt="n",ylim=c(0, trunc(mat[1,1]) +1),cex.axis=0.8)      
  }
  else {
    plot(mat[,1], type="p",pch=16,ylab=list(expression(tau[.i]),font=2,cex=2),xlab="",xaxt="n",ylim=c(0, trunc(mat[1,1]) +1),cex.axis=0.8,main=bquote(paste(.( main_title)," (n = ",.(dim(x)[1]),")")))
  }
  
#Create the labels for the variables and lines to add on the plot
    
  axis(1, at=1:pop, labels=dimnames(mat)[[1]],font=2, las=2) #add chemical variables in the x axis
  lines(mat[c(1:pop-1),1]) # add lines between points 
  lines(c(0,pop),c(mat[pop,1],mat[pop,1]),lty=3) #add horizontal line of total variation
  
  
#add the numeric value of the given segment in the vertical lines (0.3, 0.5, 0.9)
  
     for (i in 1:aaa) {
       bbb[i,1]<-length(which(MVC[pop+1,]<talls[i]))
       if (bbb[i,1]>0) {
         segments(bbb[i,1]+0.5, par("usr")[3], bbb[i,1]+0.5, (mat[pop,1]*2.5)+(mat[pop,1]/talls[i]),lty=3)
         boxed.labels(bbb[i,1]+0.5,mat[pop,1]/2,talls[i],cex=0.6, col="black", bg="white",border=NA)
       }
      }
  
# add the notations on entropy
     
      #calculate values (external function called "entropia" used)
         h2<-round(tmatentro$Entropia[1,pop],dig=2)
         h2p<- round(tmatentro$Entropia[1,pop+1]*100,dig=2)
         vt<-round(mat[pop,1],dig=2)
  
      #add the strings
         text(pop-5, (trunc(mat[1,1]) +1)-((trunc(mat[1,1]) +1)/14),label=bquote(""*"H"[2]*" = "*.(h2)*" Sh"), cex=0.8, adj=0)
         text(pop-5, (trunc(mat[1,1]) +1)- ((trunc(mat[1,1]) +1)/8.5),label=bquote("H"[2]*" % = "*.(h2p)),cex=0.8,adj=0)
         text(pop-5, (trunc(mat[1,1]) +1)- ((trunc(mat[1,1]) +1)/6.2),label=bquote("     "*"vt = "*.(vt)),cex=0.8,adj=0)
  
       #save the plot of MVC  
       xxx<-recordPlot()
         
         
#Dendrogram of MVC
       
  dendroMVC<-hclust(as.dist(MVC[c(1:(dim(MVC)[2])),]),"ave") #hclust with average method
 
   for (i in 1:(dim(MVC)[2])) {
    if (regexpr ("^Fe2O3$",as.character(dendroMVC[[4]][i]))==T) dendroMVC[[4]][i]<-expression("Fe"["2"]*"O"["3"])
    if (regexpr ("^Al2O3$", as.character(dendroMVC[[4]][i]))==T) dendroMVC[[4]][i]<-expression("Al"["2"]*"O"["3"])
    if (regexpr ("^P2O5$", as.character(dendroMVC[[4]][i]))==T) dendroMVC[[4]][i]<-expression("P"["2"]*"O"["5"])
    if (regexpr ("^TiO2$", as.character(dendroMVC[[4]][i]))==T) dendroMVC[[4]][i]<-expression("TiO"["2"])
    if (regexpr ("^Na2O$", as.character(dendroMVC[[4]][i]))==T) dendroMVC[[4]][i]<-expression("Na"["2"]*"O")
    if (regexpr ("^K2O$", as.character(dendroMVC[[4]][i]))==T) dendroMVC[[4]][i]<-expression("K"["2"]*"O")
    if (regexpr ("^SiO2$", as.character(dendroMVC[[4]][i]))==T) dendroMVC[[4]][i]<-expression("SiO"["2"])
  }
 
  
  plot(as.dendrogram(dendroMVC), main= "MVC Dendrogram")
  
     #save the plot of MVC dendrogram
      yyy<-recordPlot()

##CoDadendrogram
  
  classi<-classifica(dendroMVC)
  Signary<-as.data.frame(t(classi$signary))
  dimnames(Signary)[[1]]<-dimnames(x)[[2]]
  CoDaDendrogram(acomp(x),signary=Signary, border="red4", col="goldenrod3", type="boxplot", box.space=mida.boxplot, range=rang.boxplot)

  #save the plot of CoDaDendrogram
  zzz<-recordPlot()
  
  
##Print plots  
  
 #Uniformity
  emf("uniformity.emf")
  replayPlot(xxx)
  dev.off()
  pdf("uniformity.pdf")
  replayPlot(xxx)
  dev.off()
  
 #MVC dendrogram 
  emf("dendroMVC.emf")
  replayPlot(yyy)
  dev.off()
  pdf("dendroMVC.pdf")
  replayPlot(yyy)
  dev.off()
  
 #CoDaDendrogram
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
  assign(".lvar", lvar,.GlobalEnv)
  
}
  