#All the packages required to run ArchFlow

####   packages ######:
#archdata
#plyr
#dplyr
#RcmdrMisc
#Compositions
#ggplot2
#ggthemes
#plotrix
#dendextend
#d3heatmap
#stargazer
#devEMF
#devtools
#rmarkdown



### Functions #####
#arch_dendro
#arch_PCA
#arch_heatmap
#arch_triangles
#arch_varmat
#arch_uniformity
#arch_latex
#arch_pie_plot






#Example data

packages <- c("archdata",
                        "plyr",
                        "dplyr",
                        "RcmdrMisc",
                        "compositions",
                        "ggplot2",
                        "ggfortify",
                        "ggthemes",
                        "plotrix",
                        "dendextend",
                        "d3heatmap",
                        "stargazer",
                        "devEMF",
                        "devtools",
                        "rmarkdown")


new <- packages[!(packages %in% installed.packages()[,"Package"])]

if(length(new)) install.packages(new)
a=lapply(packages, require, character.only=TRUE)


rm(list=ls()) #clear the environment 




####################################FUNCTIONS########################################################


##########################################################
###################### DENDRO   ##############################
##########################################################


"arch_dendro" <- function(df_chem, nplot, printDendro = TRUE, df_raw){

#This code is to plot hierarchical clustering of clr transformed data, using centroid algorithm and squared euclidean distance

  df_chem -> x
  
# transform the numeric variables to log cetered ratio  
   
  x.clr <- logcenter.tran(x) 

# perform the cluster analysis
  
  HClust <-  hclust(d = dist(x.clr)^2, method = "cen") 

#save in global environment for using cut tree

  assign(".HClust", HClust,.GlobalEnv)

#save the dendrogram 
  
  my_dend <- as.dendrogram(HClust) #format for the dendrogram

#nplot vector indicates the chosen categorical columns to plot
  
  for (i in nplot)
    { 
      #plot options  
        my_dend <- set(my_dend, "labels_cex", 0.6) #labels text size
        par(mar = c(2,2,2,2)) #set legend scale
  
     #add colors to labels
        labels_colors(my_dend) <-rainbow(nlevels(as.factor(df_raw[,i])))[as.factor(df_raw[,i])][order.dendrogram(my_dend)]
      
        #MORE COLOR PALETTES: heat.colors, rainbow, terrain.colors, cm.colors, topo.colors
        
     #create the plot
        plot(my_dend, main= "Hierarchical Clustering", horiz =  FALSE)
        legend("topright",cex = 1, legend = unique(as.factor(df_raw[,i])), 
        fill = unique(rainbow(nlevels(as.factor(df_raw[,i])))[as.factor(df_raw[,i])]))

     #save the plot
        dendro <-recordPlot(my_dend)
     
     if (printDendro == TRUE){
       emf("Dendrogram.emf")
       replayPlot(dendro)
       dev.off()
       pdf("Dendrogram.pdf")
       replayPlot(dendro)
       dev.off()
     }
    }         
  
  assign("my_dend", my_dend,.GlobalEnv)
  
     
}


##########################################################
######################   P C A   ##############################
##########################################################


"arch_PCA" <- function(df_chem, df_raw, lvar=.lvar, nplot=nplot, alr = TRUE, printPCA = FALSE, labels= FALSE, shape_cat_number = 15, frame = TRUE){
  
  #Plot PCAs on alr transformed data (if desired) showing colored categories according to "nplot" given number
  
  #alr tranformation (only if alr = TRUE)
  if (alr == TRUE){
    df_chem <-  alr(df_chem,lvar) #lvar is the less variable element in the dataset. Saved to the global environment previously by function "uniformity" 
  }
  
  
  
for (i in nplot){
    
  
    pca <- paste("pca_cat", i, sep = "_")               #generate names (eg. pca_cat_1, pca_cat_2,...)
    
    assign(pca, autoplot(prcomp(df_chem),               #compute PCA on alr transformed matrix (only if alr = TRUE)
                         
                         data = df_raw,                 #set the df with the categorical data for labeling
                         
                         #labels
                         colour = colnames(df_raw[i]),  #set the categories to be displayed  
                         label = labels,                #TRUE if the name of the ANIDs are to be displayed
                         label.size = 5,                #set the label sizes (not shapes)
                         
                         #shapes      
                         shape = colnames(df_raw)[shape_cat_number],        #set the category to display by shapes, or chose from 1-25 shapes to display all equally, 15 by default       
                         size = 2,                        #set the size of the shapes
                         
                         #loadings        
                         loadings = TRUE,
                         loadings.label = TRUE, 
                         loadings.colour = 'gray', 
                         loadings.label.colour = "black",
                         
                         #eLlipse   
                         frame = frame,   #true by default, if "norm" is removed the shaope of the frame is geometrical
                         frame.type = "norm") + ggtitle(label = "PCA") + theme(plot.title = element_text(hjust = 0.5)) + labs(caption= paste("PCA using:", paste(noquote(colnames(df_chem)), collapse = ", "))) + theme(plot.caption = element_text(size=7, hjust=0.5, margin=margin(t=1))))
    
    
    
    plot(eval(parse(text =paste("pca_cat", i, sep = "_")))) #plot PCAs according to npca vector
    
    
    
    pcaplot <-recordPlot(pca)
    
    if (printPCA == TRUE){
      emf("PCA.emf")
      replayPlot(pcaplot)
      dev.off()
      pdf("PCA.pdf")
      replayPlot(pcaplot)
      dev.off()
    }
  }
}





##########################################################
######################D3 HEATMAP###########################
##########################################################



"arch_heatmap" <- function(df_chem){
  #based on d3heatmap package
  
  #### Heatmap of the chemical composition ####
  mat_div <- t(t(df_chem)/colMeans(df_chem))
  #get the relative values of each composition
  d3heatmap(mat_div,         #colors are based on these values
            cellnote=df_chem,       #show the raw chemical data
            labRow= row.names(df_chem),width = 1050, height = 900)   #the label names
}












##########################################################
######################SCATTER MATRIX   ##########################
##########################################################



arch_scatter_matrix <- function(df_raw, vars, group, title){
##based on ggplot and ggthems   
  
 
  ggthemes_data <- ggthemes::ggthemes_data
  require("ggplot2")
        .df <- df_raw[c(vars)]
        .grid <- expand.grid(x = 1:ncol(.df), y = 1:ncol(.df))
        .grid <- subset(.grid, x != y)
        .all <- do.call("rbind", lapply(1:nrow(.grid), function(i) {
          xcol <- .grid[i, "x"]; 
          ycol <- .grid[i, "y"]; 
          data.frame(xvar = names(.df)[ycol], yvar = names(.df)[xcol],
                     x = .df[, xcol], y = .df[, ycol], .df)
        }))
            
      .all$xvar  <- factor(.all$xvar, levels = names(.df))
      .all$yvar  <- factor(.all$yvar, levels = names(.df))
      .densities <- do.call("rbind", lapply(1:ncol(.df), function(i) {
        .tmp <- as.data.frame(density(x = .df[, i])[c("x", "y")]); 
        .tmp$y <- .tmp$y/max(.tmp$y)*diff(range(.tmp$x)) + min(.tmp$x); 
        data.frame(xvar = names(.df)[i], yvar = names(.df)[i],
                   x = .tmp$x, y = .tmp$y)
      }))
      
      .all <- data.frame(.all, z = rep(df_raw[,eval(parse(text = "group"))], length = nrow(.all)))
      .densities$z <- NA
      .plot <- ggplot(.all, aes(x = x, y = y, colour = z, shape = z)) + 
        facet_grid(xvar ~ yvar, scales = "free") + 
        geom_point(size=0.5) + ##change the point size here 
        geom_line(aes(x = x, y = y), data = .densities, colour = "grey") + 
        scale_y_continuous(expand = c(0.01, 0)) + 
        xlab(NULL) + 
        ylab(NULL) + 
        labs(colour = "Groups", shape = "Groups") + 
        labs(title = title) + 
        ggthemes::theme_base(base_size = 10, base_family = "sans") + 
        theme(legend.position = "right")

  print(.plot)
  
  ggsave(filename = "scatter_plot.pdf", 
         
         plot = .plot)
  
  rm(ggthemes_data, .grid, .all, .densities)
  
}





##########################################################
#####################TRIANGLES     ##########################
##########################################################

"arch_triangles"<-
  function(x,grup=1,idioma=3,paleta=1,ordenar=2,llegenda=grup, encerclat=1)
  {
    # change the labels language language: 1- catala, 2- spanish, 3- english
    # paleta: color palette, 0-arqub en blanc i negre, 1-arqub (default), 2-rainbow, 3-heat.colors, 4-terrain.colors, 5- topo.colors, 6- cm.colors
    # ordenar:order, 1-no. When 2 the file is odered according to the given factor of the group
    # llegenda: legend, group as.factor or group as.character
    # encerclat: 1-circle around the points, 2-only dots, withouth circle 
    
    
    
    #the column with groups
    x[,grup]<-as.factor(x[,grup])
    
    #order
    if (ordenar==2) {x<-x[ order(x[,grup]), ]}
    
    #define color palettes  
    arqub<-vector(length=15)
    arqub<-c("gray75", "grey9", "cyan", "red", "goldenrod1", "dodgerblue", "darkgoldenrod4", "chartreuse1",  "darkgreen", "indianred1", "blue", "darkmagenta", "maroon1", "aquamarine", "lightpink")
    arqubBN<-vector(length=15)
    arqubBN<-c("white","grey90", "grey70", "grey50", "grey30", "grey10", "dodgerblue", "darkgoldenrod4", "chartreuse1",  "darkgreen", "indianred1", "blue", "darkmagenta", "maroon1", "aquamarine")
    
    idllegenda<-vector(length=nlevels(x[,grup]))
    
    for (ll in 1:nlevels(x[,grup])) {idllegenda[ll]<-ll}
    if (llegenda!=grup) {
      for (ll in 1:nlevels(x[,grup])) {
        z<-which(x[,grup]==levels(x[,grup])[ll])
        idllegenda[ll]<-x[z[1],llegenda]
      }
    }
    
    
    #color palettes
    if (paleta==2) {
      colx<-c(rainbow(nlevels(x[,grup])))[as.factor(x[,grup])]
    }
    if (paleta==3) {
      colx<-c(heat.colors(nlevels(x[,grup])))[as.factor(x[,grup])]
    }
    if (paleta==4) {
      colx<-c(terrain.colors(nlevels(x[,grup])))[as.factor(x[,grup])]
    }
    if (paleta==5) {
      colx<-c(topo.colors(nlevels(x[,grup])))[as.factor(x[,grup])]
    }
    if (paleta==6) {
      colx<-c(cm.colors(nlevels(x[,grup])))[as.factor(x[,grup])]
    }
    #windows(record=T)
    par(mar=c(0,0,0,0)+0.1,mgp=c(3,1,0))
    n<-dim(x)[1]
    punts<-matrix(0,n,4)
    
    #el primer triangle es el sialcap
    
    #text language
    plot(c(0,100,50,0),c(0,0,100,0),xlab="",ylab="",axes=F,type="n",xlim=c(-20,120),ylim=c(-20,120))
    if (idioma==1) {
      text(50,115,labels=expression(bold("Sistema CaO-Al"["2"]*"O"["3"]*"-SiO"["2"])),cex=1.4)
      text(50,108,labels="(% en pes)",font=2)
    }
    if (idioma==2) {
      text(50,115,labels=expression(bold("Sistema CaO-Al"["2"]*"O"["3"]*"-SiO"["2"])),cex=1.4)
      text(50,108,labels="(% en peso)",font=2)
    }
    
    if (idioma==3) {
      text(50,115,labels=expression(bold("CaO-Al"["2"]*"O"["3"]*"-SiO"["2"]~"System")),cex=1.4)
      text(50,108,labels="(wt %)",font=2)}
    
    
    
    ###First triangle
    
    lines(c(0,100,50,0),c(0,0,86.60254,0))
    #lines(c(0,40,20,0),c(60,60,100,60))
    lines(c(25,62.5),c(0,64.951905),lty=4,col="grey")
    lines(c(50,75),c(0,43.30127),lty=4,col="grey")
    lines(c(75,87.5),c(0,21.650635),lty=4,col="grey")
    lines(c(12.5,87.5),c(21.650635, 21.650635),lty=3,col="grey")
    lines(c(25,75),c(43.30127, 43.30127),lty=3,col="grey")
    lines(c(37.5,62.5),c(64.951905, 64.951905),lty=3,col="grey")
    lines(c(25,12.5),c(0, 21.650635),lty=2,col="grey")
    lines(c(50,25),c(0, 43.30127),lty=2,col="grey")
    lines(c(75,37.5),c(0, 64.951905),lty=2,col="grey")
    lines(c(0,100,50,0),c(0,0, 86.60254,0))
    lines(c(50,58.245),c(86.60254,37.403637))
    lines(c(58.245,25.86),c(37.403637,44.7908337))
    lines(c(58.245,48.135),c(37.403637,18.9746165))
    lines(c(58.245,85.9),c(37.403637,24.4219163))
    lines(c(25.86,48.135),c(44.7908337, 18.9746165))
    
    text(100,-10, labels=expression(bold("Al"["2"]*"O"["3"])),pos=2,cex=1.4)
    text(60,82.272413, labels=expression(bold("SiO"["2"])),pos=4,cex=1.4)
    
    
    
    text(-5,5, labels=expression(bold("CaO")),pos=2,cex=1.4)
    text(c(1,25,50,75,98),c(-2,-2,-2,-2,-2),labels=c("0","25","50","75","100"),cex=0.8)
    text(c(102,90.5,78,65.5,54),c(1.7320508,21.650635, 43.30127, 64.951905,85.7365146),labels=c("0","25","50","75","100"),cex=0.8)
    text(c(48,34.5,22,9.5,-3),c(85.7365146, 64.951905, 43.30127, 21.650635, 1.7320508),labels=c("0","25","50","75","100"),cex=0.8)
    text(c(50,30.3,59.5,82.5,48.135),c(89.2006162,45.8993462,35.0740287,23.25278199,17.320508), labels=c("Qz","Wo","An","Mul","Gh"),cex=1,font=2)
    
    
    
    punts[,1]<-x$"Al2O3"
    punts[,2]<-x$"SiO2" 
    punts[,3]<-x$"CaO"
    punts[,c(1:3)]<-sweep(punts[,c(1:3)]/0.01,1,apply(punts[,c(1:3)],1,sum),FUN="/")
    
    #La coordenada x es el valor de l’eix inferior mes el catet del triangle rectangle. La seva hipotenusa
    #es el valor del eix dret i es multiplica pel sinus de 30 graus, que es 0.5
    punts[,4]<-punts[,1]+(punts[,2]/2)
    
    
    
    
    
    #color palettes first triangle
    if (paleta==0) {
      if (encerclat==2) {
        for (w in 1:n) {
          points(punts[w,4],(punts[w,2]*0.8660254),pch=19,cex=0.8,col=arqubBN[x[w,grup]])
        }
        legend(-15,91, bty="n",pch=19, legend = unique(x[,grup]), col = arqubBN[1:nlevels(x[,grup])])
      }
      if (encerclat==1) {
        for (w in 1:n) {
          points(punts[w,4],(punts[w,2]*0.8660254),pch=19,cex=0.8,col=arqubBN[x[w,grup]])
          points(punts[w,4],(punts[w,2]*0.8660254),pch=21,cex=0.8)
        }
        legend(-15,91, bty="n",pch=19, legend =  unique(x[,grup]), col = arqubBN[1:nlevels(x[,grup])])
        legend(-15,91, bty="n",pch=21, legend = rep("",nlevels(x[,grup])))
      }
    }
    
    if (paleta==1) {
      if (encerclat==2) {
        for (w in 1:n) {
          points(punts[w,4],(punts[w,2]*0.8660254),pch=19,cex=0.8,col=arqub[x[w,grup]])
        }
        legend(-15,91, bty="n",pch=19, legend = unique(x[,grup]), col = arqub[1:nlevels(x[,grup])])
      }
      if (encerclat==1) {
        for (w in 1:n) {
          points(punts[w,4],(punts[w,2]*0.8660254),pch=19,cex=0.8,col=arqub[x[w,grup]])
          points(punts[w,4],(punts[w,2]*0.8660254),pch=21,cex=0.8)
        }
        legend(-15,91, bty="n",pch=19, legend = unique(x[,grup]), col = arqub[1:nlevels(x[,grup])])
        legend(-15,91, bty="n",pch=21, legend = rep("",nlevels(x[,grup])))
      }
    }
    if (paleta==2) {
      if (encerclat==2) {
        points(punts[,4],(punts[,2]*0.8660254),pch=19,cex=0.8,col=colx)
        legend(-15,91, bty="n",pch=19, legend =  unique(x[,grup]), col = rainbow(nlevels(x[,grup])))
      }
      if (encerclat==1) {
        points(punts[,4],(punts[,2]*0.8660254),pch=21,cex=0.8,bg=colx)
        legend(-15,91, bty="n",pch=19, legend =  unique(x[,grup]), col = rainbow(nlevels(x[,grup])))
        legend(-15,91, bty="n",pch=21, legend = rep("",nlevels(x[,grup])))
      }
    }
    if (paleta==3) {
      if (encerclat==2) {
        points(punts[,4],(punts[,2]*0.8660254),pch=19,cex=0.8,col=colx)
        legend(-15,91, bty="n",pch=19, legend =  unique(x[,grup]), col = heat.colors(nlevels(x[,grup])))
      }
      if (encerclat==1) {
        points(punts[,4],(punts[,2]*0.8660254),pch=21,cex=0.8,bg=colx)
        legend(-15,91, bty="n",pch=19, legend =  unique(x[,grup]), col = heat.colors(nlevels(x[,grup])))
        legend(-15,91, bty="n",pch=21, legend = rep("",nlevels(x[,grup])))
      }
    }
    if (paleta==4) {
      if (encerclat==2) {
        points(punts[,4],(punts[,2]*0.8660254),pch=19,cex=0.8,col=colx)
        legend(-15,91, bty="n",pch=19, legend =  unique(x[,grup]), col = terrain.colors(nlevels(x[,grup])))
      }
      if (encerclat==1) {
        points(punts[,4],(punts[,2]*0.8660254),pch=21,cex=0.8,bg=colx)
        legend(-15,91, bty="n",pch=19, legend =  unique(x[,grup]), col = terrain.colors(nlevels(x[,grup])))
        legend(-15,91, bty="n",pch=21, legend = rep("",nlevels(x[,grup])))
      }
    }
    if (paleta==5) {
      if (encerclat==2) {
        points(punts[,4],(punts[,2]*0.8660254),pch=19,cex=0.8,col=colx)
        legend(-15,91, bty="n",pch=19, legend =  unique(x[,grup]), col = topo.colors(nlevels(x[,grup])))
      }
      if (encerclat==1) {
        points(punts[,4],(punts[,2]*0.8660254),pch=21,cex=0.8,bg=colx)
        legend(-15,91, bty="n",pch=19, legend =  unique(x[,grup]), col = topo.colors(nlevels(x[,grup])))
        legend(-15,91, bty="n",pch=21, legend = rep("",nlevels(x[,grup])))
      }
    }
    if (paleta==6) {
      if (encerclat==2) {
        points(punts[,4],(punts[,2]*0.8660254),pch=19,cex=0.8,col=colx)
        legend(-15,91, bty="n",pch=19, legend =  unique(x[,grup]), col = cm.colors(nlevels(x[,grup])))
      }
      if (encerclat==1) {
        points(punts[,4],(punts[,2]*0.8660254),pch=21,cex=0.8,bg=colx)
        legend(-15,91, bty="n",pch=19, legend =  unique(x[,grup]), col = cm.colors(nlevels(x[,grup])))
        legend(-15,91, bty="n",pch=21, legend = rep("",nlevels(x[,grup])))
      }
    }
    
    #plot first triangle
    xxx<-recordPlot()
    emf("triangle1.emf")
    replayPlot(xxx)
    dev.off()
    pdf("triangle1.pdf")
    replayPlot(xxx)
    dev.off()
    
    
    
    ##Second triangle 
    par(mar=c(0,0,0,0)+0.1,mgp=c(3,1,0))
    n<-dim(x)[1]
    punts<-matrix(0,n,4)
    plot(c(0,100,50,0),c(0,0,100,0),xlab="",ylab="",axes=F,type="n",xlim=c(-20,120),ylim=c(-20,120))
    if (idioma==1) {
      text(50,115,labels=expression(bold("Triangle ceràmic")),cex=1.4)
      text(50,108,labels="(% en pes)",font=2)
    }
    if (idioma==2) {
      text(50,115,labels=expression(bold("Triángulo cerámico")),cex=1.4)
      text(50,108,labels="(% en peso)",font=2)
    }
    if (idioma==3) {
      text(50,115,labels=expression(bold("Ceramic triangle")),cex=1.4)
      text(50,108,labels="(wt %)",font=2)
    }
    lines(c(0,100,50,0),c(0,0,86.60254,0))
    #lines(c(0,40,20,0),c(60,60,100,60))
    lines(c(25,62.5),c(0,64.951905),lty=4,col="grey")
    lines(c(50,75),c(0,43.30127),lty=4,col="grey")
    lines(c(75,87.5),c(0,21.650635),lty=4,col="grey")
    lines(c(12.5,87.5),c(21.650635, 21.650635),lty=3,col="grey")
    lines(c(25,75),c(43.30127, 43.30127),lty=3,col="grey")
    lines(c(37.5,62.5),c(64.951905, 64.951905),lty=3,col="grey")
    lines(c(25,12.5),c(0, 21.650635),lty=2,col="grey")
    lines(c(50,25),c(0, 43.30127),lty=2,col="grey")
    lines(c(75,37.5),c(0, 64.951905),lty=2,col="grey")
    lines(c(0,100,50,0),c(0,0, 86.60254,0))
    lines(c(50,58.245),c(86.60254,37.403637))
    lines(c(58.245,25.86),c(37.403637,44.7908337))
    lines(c(58.245,48.135),c(37.403637,18.9746165))
    lines(c(58.245,85.9),c(37.403637,24.4219163))
    lines(c(25.86,48.135),c(44.7908337, 18.9746165))
    text(100,-10, labels=expression(bold("Al"["2"]*"O"["3"])),pos=2,cex=1.4)
    text(60,82.272413, labels=expression(bold("SiO"["2"])),pos=4,cex=1.4)
    
    
    
    text(2.8,14.9, labels=expression(bold("Fe"["2"]*"O"["3"]~"+")),pos=2,cex=1.4)
    text(0.2,9.2, labels=expression(bold("MgO +")),pos=2,cex=1.4)
    text(-5,5, labels=expression(bold("CaO")),pos=2,cex=1.4)
    text(c(1,25,50,75,98),c(-2,-2,-2,-2,-2),labels=c("0","25","50","75","100"),cex=0.8)
    text(c(102,90.5,78,65.5,54),c(1.7320508,21.650635, 43.30127, 64.951905,85.7365146),labels=c("0","25","50","75","100"),cex=0.8)
    text(c(48,34.5,22,9.5,-3),c(85.7365146, 64.951905, 43.30127, 21.650635, 1.7320508),labels=c("0","25","50","75","100"),cex=0.8)
    text(c(50,30.3,59.5,82.5,48.135),c(89.2006162,45.8993462,35.0740287,23.25278199,17.320508), labels=c("Qz","Wo","An","Mul","Gh"),cex=1,font=2)
    
    
    
    punts[,1]<-x[,"Al2O3"]
    punts[,2]<-x[,"SiO2"] 
    punts[,3]<-x[,"CaO"]+ x[,"MgO"]+ x[,"Fe2O3"]
    punts[,c(1:3)]<-sweep(punts[,c(1:3)]/0.01,1,apply(punts[,c(1:3)],1,sum),FUN="/")
    
    
    
    #La coordenada x es el valor de l’eix inferior mes el catet del triangle rectangle. La seva hipotenusa
    #es el valor del eix dret i es multiplica pel sinus de 30 graus, que es 0.5
    punts[,4]<-punts[,1]+(punts[,2]/2)
    if (paleta==0) {
      if (encerclat==2) {
        for (w in 1:n) {
          points(punts[w,4],(punts[w,2]*0.8660254),pch=19,cex=0.8,col=arqubBN[x[w,grup]])
        }
        legend(-15,91, bty="n",pch=19, legend =  unique(x[,grup]), col = arqubBN[1:nlevels(x[,grup])])
      }
      if (encerclat==1) {
        for (w in 1:n) {
          points(punts[w,4],(punts[w,2]*0.8660254),pch=19,cex=0.8,col=arqubBN[x[w,grup]])
          points(punts[w,4],(punts[w,2]*0.8660254),pch=21,cex=0.8)
        }
        legend(-15,91, bty="n",pch=19, legend =  unique(x[,grup]), col = arqubBN[1:nlevels(x[,grup])])
        legend(-15,91, bty="n",pch=21, legend = rep("",nlevels(x[,grup])))
      }
    }
    if (paleta==1) {
      if (encerclat==2) {
        for (w in 1:n) {
          points(punts[w,4],(punts[w,2]*0.8660254),pch=19,cex=0.8,col=arqub[x[w,grup]])
        }
        legend(-15,91, bty="n",pch=19, legend =  unique(x[,grup]), col = arqub[1:nlevels(x[,grup])])
      }
      if (encerclat==1) {
        for (w in 1:n) {
          points(punts[w,4],(punts[w,2]*0.8660254),pch=19,cex=0.8,col=arqub[x[w,grup]])
          points(punts[w,4],(punts[w,2]*0.8660254),pch=21,cex=0.8)
        }
        legend(-15,91, bty="n",pch=19, legend =  unique(x[,grup]), col = arqub[1:nlevels(x[,grup])])
        legend(-15,91, bty="n",pch=21, legend = rep("",nlevels(x[,grup])))
      }
    }
    if (paleta==2) {
      if (encerclat==2) {
        points(punts[,4],(punts[,2]*0.8660254),pch=19,cex=0.8,col=colx)
        legend(-15,91, bty="n",pch=19, legend =  unique(x[,grup]), col = rainbow(nlevels(x[,grup])))
      }
      if (encerclat==1) {
        points(punts[,4],(punts[,2]*0.8660254),pch=21,cex=0.8,bg=colx)
        legend(-15,91, bty="n",pch=19, legend =  unique(x[,grup]), col = rainbow(nlevels(x[,grup])))
        legend(-15,91, bty="n",pch=21, legend = rep("",nlevels(x[,grup])))
      }
    }
    if (paleta==3) {
      if (encerclat==2) {
        points(punts[,4],(punts[,2]*0.8660254),pch=19,cex=0.8,col=colx)
        legend(-15,91, bty="n",pch=19, legend =  unique(x[,grup]), col = heat.colors(nlevels(x[,grup])))
      }
      if (encerclat==1) {
        points(punts[,4],(punts[,2]*0.8660254),pch=21,cex=0.8,bg=colx)
        legend(-15,91, bty="n",pch=19, legend =  unique(x[,grup]), col = heat.colors(nlevels(x[,grup])))
        legend(-15,91, bty="n",pch=21, legend = rep("",nlevels(x[,grup])))
      }
    }
    if (paleta==4) {
      if (encerclat==2) {
        points(punts[,4],(punts[,2]*0.8660254),pch=19,cex=0.8,col=colx)
        legend(-15,91, bty="n",pch=19, legend =  unique(x[,grup]), col = terrain.colors(nlevels(x[,grup])))
      }
      if (encerclat==1) {
        points(punts[,4],(punts[,2]*0.8660254),pch=21,cex=0.8,bg=colx)
        legend(-15,91, bty="n",pch=19, legend =  unique(x[,grup]), col = terrain.colors(nlevels(x[,grup])))
        legend(-15,91, bty="n",pch=21, legend = rep("",nlevels(x[,grup])))
      }
    }
    if (paleta==5) {
      if (encerclat==2) {
        points(punts[,4],(punts[,2]*0.8660254),pch=19,cex=0.8,col=colx)
        legend(-15,91, bty="n",pch=19, legend =  unique(x[,grup]), col = topo.colors(nlevels(x[,grup])))
      }
      if (encerclat==1) {
        points(punts[,4],(punts[,2]*0.8660254),pch=21,cex=0.8,bg=colx)
        legend(-15,91, bty="n",pch=19, legend =  unique(x[,grup]), col = topo.colors(nlevels(x[,grup])))
        legend(-15,91, bty="n",pch=21, legend = rep("",nlevels(x[,grup])))
      }
    }
    if (paleta==6) {
      if (encerclat==2) {
        points(punts[,4],(punts[,2]*0.8660254),pch=19,cex=0.8,col=colx)
        legend(-15,91, bty="n",pch=19, legend =  unique(x[,grup]), col = cm.colors(nlevels(x[,grup])))
      }
      if (encerclat==1) {
        points(punts[,4],(punts[,2]*0.8660254),pch=21,cex=0.8,bg=colx)
        legend(-15,91, bty="n",pch=19, legend =  unique(x[,grup]), col = cm.colors(nlevels(x[,grup])))
        legend(-15,91, bty="n",pch=21, legend = rep("",nlevels(x[,grup])))
      }
    }
    xxx<-recordPlot()
    emf("triangle2.emf")
    replayPlot(xxx)
    dev.off()
    pdf("triangle2.pdf")
    replayPlot(xxx)
    dev.off()
    
    
    #third trianle 
    
    par(mar=c(0,0,0,0)+0.1,mgp=c(3,1,0))
    n<-dim(x)[1]
    punts<-matrix(0,n,4)
    plot(c(0,100,50,0),c(0,0,100,0),xlab="",ylab="",axes=F,type="n",xlim=c(-20,120),ylim=c(-20,120))
    
    
    if (idioma==1) {
      text(50,115,labels=expression(bold("Sistema CaO-MgO-SiO"["2"])),cex=1.4)
      text(50,108,labels="(% en pes)",font=2)
    }
    if (idioma==2) {
      text(50,115,labels=expression(bold("Sistema CaO-MgO-SiO"["2"])),cex=1.4)
      text(50,108,labels="(% en peso)",font=2)
    }
    
    if (idioma==3) {
      text(50,115,labels=expression(bold("CaO-MgO-SiO"["2"]~"System")),cex=1.4)
      text(50,108,labels="(wt %)",font=2)
    }
    
    
    
    
    lines(c(0,100,50,0),c(0,0,86.60254,0))
    #lines(c(0,40,20,0),c(60,60,100,60))
    #es dibuixa el grid
    lines(c(25,62.5),c(0,64.951905),lty=4,col="grey")
    lines(c(50,75),c(0,43.30127),lty=4,col="grey")
    lines(c(75,87.5),c(0,21.650635),lty=4,col="grey")
    lines(c(12.5,87.5),c(21.650635, 21.650635),lty=3,col="grey")
    lines(c(25,75),c(43.30127, 43.30127),lty=3,col="grey")
    lines(c(37.5,62.5),c(64.951905, 64.951905),lty=3,col="grey")
    lines(c(25,12.5),c(0, 21.650635),lty=2,col="grey")
    lines(c(50,25),c(0, 43.30127),lty=2,col="grey")
    lines(c(75,37.5),c(0, 64.951905),lty=2,col="grey")
    #es dibuixa el triangle
    lines(c(0,100,50,0),c(0,0, 86.60254,0))
    #es dibuixen els triangles d’equilibri termodinamic
    lines(c(50,46.355),c(86.60254,48.0557494))
    lines(c(25.86, 46.355),c(44.7908337,48.0557494))
    lines(c(70.075, 46.355),c(51.8316202, 48.0557494))
    lines(c(36.825, 46.355),c(38.1657394, 48.0557494))
    lines(c(78.65, 46.355),c(36.9792846, 48.0557494))
    lines(c(36.825, 25.86),c(38.1657394, 44.7908337))
    lines(c(36.825, 78.65),c(38.1657394, 36.9792846))
    lines(c(36.825, 44.96),c(38.1657394, 33.2553754))
    lines(c(78.65, 44.96),c(36.9792846, 33.2553754))
    
    
    #etiquetes del triangle
    text(100,-10, labels=expression(bold("MgO")),pos=2,cex=1.4)
    text(60,82.272413, labels=expression(bold("SiO"["2"])),pos=4,cex=1.4)
    text(-5,5, labels=expression(bold("CaO")),pos=2,cex=1.4)
    
    
    
    #etiquetes dels eixos
    text(c(1,25,50,75,98),c(-2,-2,-2,-2,-2),labels=c("0","25","50","75","100"),cex=0.8)
    text(c(102,90.5,78,65.5,54),c(1.7320508,21.650635, 43.30127, 64.951905,85.7365146),labels=c("0","25","50","75","100"),cex=0.8)
    text(c(48,34.5,22,9.5,-3),c(85.7365146, 64.951905, 43.30127, 21.650635, 1.7320508),labels=c("0","25","50","75","100"),cex=0.8)
    
    #etiquetes de les fases
    text(c(50,27.2,46.9,69.4,35.4,77.6,44.96),c(89.2006162, 41.6,45.2, 49.8, 37,35,31), labels=c("Qz","Wo","Di","En","Ak","Fo","Mtc"),cex=1,font=2)
    
    punts[,1]<-x[,"MgO"]
    punts[,2]<-x[,"SiO2"] 
    punts[,3]<-x[,"CaO"]
    punts[,c(1:3)]<-sweep(punts[,c(1:3)]/0.01,1,apply(punts[,c(1:3)],1,sum),FUN="/")
    
    #La coordenada x es el valor de l’eix inferior mes el catet del triangle rectangle. La seva hipotenusa
    #es el valor del eix dret i es multiplica pel sinus de 30 graus, que es 0.5
    punts[,4]<-punts[,1]+(punts[,2]/2)
    
    
    ###different color palettes
    if (paleta==0) {
      if (encerclat==2) {
        for (w in 1:n) {
          points(punts[w,4],(punts[w,2]*0.8660254),pch=19,cex=0.8,col=arqubBN[x[w,grup]])
        }
        legend(-15,91, bty="n",pch=19, legend =  unique(x[,grup]), col = arqubBN[1:nlevels(x[,grup])])
      }
      if (encerclat==1) {
        for (w in 1:n) {
          points(punts[w,4],(punts[w,2]*0.8660254),pch=19,cex=0.8,col=arqubBN[x[w,grup]])
          points(punts[w,4],(punts[w,2]*0.8660254),pch=21,cex=0.8)
        }
        legend(-15,91, bty="n",pch=19, legend =  unique(x[,grup]), col = arqubBN[1:nlevels(x[,grup])])
        legend(-15,91, bty="n",pch=21, legend = rep("",nlevels(x[,grup])))
      }
    }
    if (paleta==1) {
      if (encerclat==2) {
        for (w in 1:n) {
          points(punts[w,4],(punts[w,2]*0.8660254),pch=19,cex=0.8,col=arqub[x[w,grup]])
        }
        legend(-15,91, bty="n",pch=19, legend =  unique(x[,grup]), col = arqub[1:nlevels(x[,grup])])
      }
      if (encerclat==1) {
        for (w in 1:n) {
          points(punts[w,4],(punts[w,2]*0.8660254),pch=19,cex=0.8,col=arqub[x[w,grup]])
          points(punts[w,4],(punts[w,2]*0.8660254),pch=21,cex=0.8)
        }
        legend(-15,91, bty="n",pch=19, legend =  unique(x[,grup]), col = arqub[1:nlevels(x[,grup])])
        legend(-15,91, bty="n",pch=21, legend = rep("",nlevels(x[,grup])))
      }
    }
    if (paleta==2) {
      if (encerclat==2) {
        points(punts[,4],(punts[,2]*0.8660254),pch=19,cex=0.8,col=colx)
        legend(-15,91, bty="n",pch=19, legend =  unique(x[,grup]), col = rainbow(nlevels(x[,grup])))
      }
      if (encerclat==1) {
        points(punts[,4],(punts[,2]*0.8660254),pch=21,cex=0.8,bg=colx)
        legend(-15,91, bty="n",pch=19, legend =  unique(x[,grup]), col = rainbow(nlevels(x[,grup])))
        legend(-15,91, bty="n",pch=21, legend = rep("",nlevels(x[,grup])))
      }
    }
    if (paleta==3) {
      if (encerclat==2) {
        points(punts[,4],(punts[,2]*0.8660254),pch=19,cex=0.8,col=colx)
        legend(-15,91, bty="n",pch=19, legend =  unique(x[,grup]), col = heat.colors(nlevels(x[,grup])))
      }
      if (encerclat==1) {
        points(punts[,4],(punts[,2]*0.8660254),pch=21,cex=0.8,bg=colx)
        legend(-15,91, bty="n",pch=19, legend =  unique(x[,grup]), col = heat.colors(nlevels(x[,grup])))
        legend(-15,91, bty="n",pch=21, legend = rep("",nlevels(x[,grup])))
      }
    }
    if (paleta==4) {
      if (encerclat==2) {
        points(punts[,4],(punts[,2]*0.8660254),pch=19,cex=0.8,col=colx)
        legend(-15,91, bty="n",pch=19, legend =  unique(x[,grup]), col = terrain.colors(nlevels(x[,grup])))
      }
      if (encerclat==1) {
        points(punts[,4],(punts[,2]*0.8660254),pch=21,cex=0.8,bg=colx)
        legend(-15,91, bty="n",pch=19, legend =  unique(x[,grup]), col = terrain.colors(nlevels(x[,grup])))
        legend(-15,91, bty="n",pch=21, legend = rep("",nlevels(x[,grup])))
      }
    }
    if (paleta==5) {
      if (encerclat==2) {
        points(punts[,4],(punts[,2]*0.8660254),pch=19,cex=0.8,col=colx)
        legend(-15,91, bty="n",pch=19, legend =  unique(x[,grup]), col = topo.colors(nlevels(x[,grup])))
      }
      if (encerclat==1) {
        points(punts[,4],(punts[,2]*0.8660254),pch=21,cex=0.8,bg=colx)
        legend(-15,91, bty="n",pch=19, legend =  unique(x[,grup]), col = topo.colors(nlevels(x[,grup])))
        legend(-15,91, bty="n",pch=21, legend = rep("",nlevels(x[,grup])))
      }
    }
    if (paleta==6) {
      if (encerclat==2) {
        points(punts[,4],(punts[,2]*0.8660254),pch=19,cex=0.8,col=colx)
        legend(-15,91, bty="n",pch=19, legend =  unique(x[,grup]), col = cm.colors(nlevels(x[,grup])))
      }
      if (encerclat==1) {
        points(punts[,4],(punts[,2]*0.8660254),pch=21,cex=0.8,bg=colx)
        legend(-15,91, bty="n",pch=19, legend =  unique(x[,grup]), col = cm.colors(nlevels(x[,grup])))
        legend(-15,91, bty="n",pch=21, legend = rep("",nlevels(x[,grup])))
      }
    }
    
    ###output plots 
    xxx<-recordPlot()
    emf("triangle3.emf")
    replayPlot(xxx)
    dev.off()
    pdf("triangle3.pdf")
    replayPlot(xxx)
    dev.off()
    #fi i posem els marges per defecte
    par(mar=c(5,4,4,2)+0.1,mgp=c(3,1,0))
    palette("default")
    
    
  }







##########################################################
######################UNIFORMITY ##########################
##########################################################





"arch_uniformity" <- function(df_chem)
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
  
       







##########################################################
######################VARIATION MATRIX ##########################
##########################################################       





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





##########################################################
######################VARIATION MATRIX ##########################
##########################################################       
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





##########################################################
######################ENTROPIA ##########################
##########################################################       

"entropia02" <-   function(x)
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





##########################################################
######################ENTROPIA ##########################
##########################################################       



"etiquetes.elements"<-
  function(x)
  {
    # fa les etiquetes per un plot fent els subscripts. Han díestar una en cada fila
    # fet per Jaume Buxeda i GarrigÛs
    # versiÛ normalitzada a marÁ de 2015      
    n<-dim(x)[1]
    etiquetes<-vector(length=n)
    etiquetes[]<-dimnames(x)[[1]]
    for(i in 1:n) {
      if (regexpr ("^Fe2O3$",as.character(etiquetes[i]))==T) etiquetes[i]<-expression(bold("Fe"["2"]*"O"["3"]))
      if (regexpr ("^Al2O3$", as.character(etiquetes[i]))==T) etiquetes[i]<-expression(bold("Al"["2"]*"O"["3"]))
      if (regexpr ("^P2O5$", as.character(etiquetes[i]))==T) etiquetes[i]<-expression(bold("P"["2"]*"O"["5"]))
      if (regexpr ("^TiO2$", as.character(etiquetes[i]))==T) etiquetes[i]<-expression(bold("TiO"["2"]))
      if (regexpr ("^Na2O$", as.character(etiquetes[i]))==T) etiquetes[i]<-expression(bold("Na"["2"]*"O"))
      if (regexpr ("^K2O$", as.character(etiquetes[i]))==T) etiquetes[i]<-expression(bold("K"["2"]*"O"))
      if (regexpr ("^SiO2$", as.character(etiquetes[i]))==T) etiquetes[i]<-expression(bold("SiO"["2"]))
      if (regexpr ("^MnO$", as.character(etiquetes[i]))==T) etiquetes[i]<-expression(bold("MnO"))
      if (regexpr ("^MgO$", as.character(etiquetes[i]))==T) etiquetes[i]<-expression(bold("MgO"))
      if (regexpr ("^CaO$", as.character(etiquetes[i]))==T) etiquetes[i]<-expression(bold("CaO"))
      if (regexpr ("^Ba$", as.character(etiquetes[i]))==T) etiquetes[i]<-expression(bold("Ba"))
      if (regexpr ("^BaO$", as.character(etiquetes[i]))==T) etiquetes[i]<-expression(bold("BaO"))
      if (regexpr ("^Rb$", as.character(etiquetes[i]))==T) etiquetes[i]<-expression(bold("Rb"))
      if (regexpr ("^Nb$", as.character(etiquetes[i]))==T) etiquetes[i]<-expression(bold("Nb"))
      if (regexpr ("^Pb$", as.character(etiquetes[i]))==T) etiquetes[i]<-expression(bold("Pb"))
      if (regexpr ("^Zr$", as.character(etiquetes[i]))==T) etiquetes[i]<-expression(bold("Zr"))
      if (regexpr ("^Y$", as.character(etiquetes[i]))==T) etiquetes[i]<-expression(bold("Y"))
      if (regexpr ("^Sr$", as.character(etiquetes[i]))==T) etiquetes[i]<-expression(bold("Sr"))
      if (regexpr ("^Ce$", as.character(etiquetes[i]))==T) etiquetes[i]<-expression(bold("Ce"))
      if (regexpr ("^Ga$", as.character(etiquetes[i]))==T) etiquetes[i]<-expression(bold("Ga"))
      if (regexpr ("^V$", as.character(etiquetes[i]))==T) etiquetes[i]<-expression(bold("V"))
      if (regexpr ("^Zn$", as.character(etiquetes[i]))==T) etiquetes[i]<-expression(bold("Zn"))
      if (regexpr ("^Cu$", as.character(etiquetes[i]))==T) etiquetes[i]<-expression(bold("Cu"))
      if (regexpr ("^Ni$", as.character(etiquetes[i]))==T) etiquetes[i]<-expression(bold("Ni"))
      if (regexpr ("^Cr$", as.character(etiquetes[i]))==T) etiquetes[i]<-expression(bold("Cr"))
      if (regexpr ("^Th$", as.character(etiquetes[i]))==T) etiquetes[i]<-expression(bold("Th"))
      if (regexpr ("^Mo$", as.character(etiquetes[i]))==T) etiquetes[i]<-expression(bold("Mo"))
      if (regexpr ("^Sn$", as.character(etiquetes[i]))==T) etiquetes[i]<-expression(bold("Sn"))
      if (regexpr ("^W$", as.character(etiquetes[i]))==T) etiquetes[i]<-expression(bold("W"))
      if (regexpr ("^Co$", as.character(etiquetes[i]))==T) etiquetes[i]<-expression(bold("Co"))
      if (regexpr ("^LOI$", as.character(etiquetes[i]))==T) etiquetes[i]<-expression(bold("LOI"))
      if (regexpr ("vt", as.character(etiquetes[i]))==T) etiquetes[i]<-expression(bold("vt"))
      if (regexpr ("^As$", as.character(etiquetes[i]))==T) etiquetes[i]<-expression(bold("As"))
      if (regexpr ("^Ca$", as.character(etiquetes[i]))==T) etiquetes[i]<-expression(bold("Ca"))
      if (regexpr ("^La$", as.character(etiquetes[i]))==T) etiquetes[i]<-expression(bold("La"))
      if (regexpr ("^Lu$", as.character(etiquetes[i]))==T) etiquetes[i]<-expression(bold("Lu"))
      if (regexpr ("^Na$", as.character(etiquetes[i]))==T) etiquetes[i]<-expression(bold("Na"))
      if (regexpr ("^Nd$", as.character(etiquetes[i]))==T) etiquetes[i]<-expression(bold("Nd"))
      if (regexpr ("^Sm$", as.character(etiquetes[i]))==T) etiquetes[i]<-expression(bold("Sm"))
      if (regexpr ("^U$", as.character(etiquetes[i]))==T) etiquetes[i]<-expression(bold("U"))
      if (regexpr ("^Yb$", as.character(etiquetes[i]))==T) etiquetes[i]<-expression(bold("Yb"))
      if (regexpr ("^Cs$", as.character(etiquetes[i]))==T) etiquetes[i]<-expression(bold("Cs"))
      if (regexpr ("^Eu$", as.character(etiquetes[i]))==T) etiquetes[i]<-expression(bold("Eu"))
      if (regexpr ("^Fe$", as.character(etiquetes[i]))==T) etiquetes[i]<-expression(bold("Fe"))
      if (regexpr ("^Hf$", as.character(etiquetes[i]))==T) etiquetes[i]<-expression(bold("Hf"))
      if (regexpr ("^Sb$", as.character(etiquetes[i]))==T) etiquetes[i]<-expression(bold("Sb"))
      if (regexpr ("^Sc$", as.character(etiquetes[i]))==T) etiquetes[i]<-expression(bold("Sc"))
      if (regexpr ("^Ta$", as.character(etiquetes[i]))==T) etiquetes[i]<-expression(bold("Ta"))
      if (regexpr ("^Tb$", as.character(etiquetes[i]))==T) etiquetes[i]<-expression(bold("Tb"))
      if (regexpr ("^Al$", as.character(etiquetes[i]))==T) etiquetes[i]<-expression(bold("Al"))
      if (regexpr ("^Gd$", as.character(etiquetes[i]))==T) etiquetes[i]<-expression(bold("Gd"))
      if (regexpr ("^K$", as.character(etiquetes[i]))==T) etiquetes[i]<-expression(bold("K"))
      if (regexpr ("^Dy$", as.character(etiquetes[i]))==T) etiquetes[i]<-expression(bold("Dy"))
      if (regexpr ("^Ho$", as.character(etiquetes[i]))==T) etiquetes[i]<-expression(bold("Ho"))
      if (regexpr ("^Mg$", as.character(etiquetes[i]))==T) etiquetes[i]<-expression(bold("Mg"))
      if (regexpr ("^Er$", as.character(etiquetes[i]))==T) etiquetes[i]<-expression(bold("Er"))
      if (regexpr ("^Mn$", as.character(etiquetes[i]))==T) etiquetes[i]<-expression(bold("Mn"))
      if (regexpr ("^Pr$", as.character(etiquetes[i]))==T) etiquetes[i]<-expression(bold("Pr"))
      if (regexpr ("^Ti$", as.character(etiquetes[i]))==T) etiquetes[i]<-expression(bold("Ti"))
      if (regexpr ("^Tm$", as.character(etiquetes[i]))==T) etiquetes[i]<-expression(bold("Tm"))
      if (regexpr ("^Ppc$", as.character(etiquetes[i]))==T) etiquetes[i]<-expression(bold("Ppc"))
      if (regexpr ("^SO3$", as.character(etiquetes[i]))==T) etiquetes[i]<-expression(bold("SO"["3"]))
    }
    as.vector(etiquetes)
  }







##########################################################
######################ENTROPIA ##########################
##########################################################       

"logcenter.tran"<-
  function(x)
  {
    # This transforms a data matrix, x, to centred log-ratios 
    # Fet per M.J. Baxter
    # versiÛ normalitzada a marÁ de 2015
    if(min(x) < 0) {
      cat("Negative data - terminating.")
      return()
    }
    as.data.frame(sweep(log(x),1,apply(log(x),1,mean),FUN="-"))
  }



##########################################################
######################SUM TABLE##########################
##########################################################       


  "sum_table" <- function(x) {

	sum <- #calculate the values using numSummary 
	numSummary(x, 
			statistics=c("mean", "sd", "cv") #specify the stats to display
 				)
	
	table	<- as.data.frame(sum$table)  	#extract only the table
  
	table$cv <- table$cv*100  #the cv  in % (RSD)
		
	table <- round(table,2)

	table$N <- sum$n

	table$izenak  <-  row.names(table)  	#keep the row.names(because arrange(){dplyr} removes them) 
      
      "element"  ->  table$what[nchar(row.names(table)) <= 2] #create a new column with elements
      "compound" ->  table$what[nchar(row.names(table)) > 2] #add the compounds to this column 

 
 	table <- arrange(table, what, izenak) ## arrange! first compounds and then elements!

 	row.names(table) <- table$izenak 	#put it back the names of elements

    colnames(table)[1]<-  "Mean"			#names of columns
	colnames(table)[2] <- "St. Dev"
	colnames(table)[3] <- "RSD"
	colnames(table)[4] <- "N"
 

table <- table[,-c(5,6)] #remove the undesired columns 


table <- table[,c(4,1,2,3)] # Change the order or columns to put the first N

return(table)

}

  
  arch_latex<- function(group_list, landscape = FALSE, MVC = FALSEM, rm_vars= rm_vars){
    
    cat(paste("%Copy and paste this code in the Latex File", "\n\n"))  
    j <- 1
    
    
    for (i in group_list){
      names(group_list)[j] -> i_name
      j+1 -> j
      
      message("%",i_name)
      stargazer(sum_table(i[,-c(rm_vars)]), title = paste("Summary table of", i_name, sep = " "), summary = FALSE, digits = 1, label = paste("Sum_table", i_name, sep = "_"))
      
      
      
      if (MVC == TRUE){
        
        #Print the MVC in landscape mode
        if (landscape == TRUE){
          message("\\begin{landscape}")
        }
        stargazer(arch_varmat(i[,-c(rm_vars)]), title = paste("MVC of", i_name, sep = " "), summary= FALSE,  digits = 2, label = paste("MVC of", i_name, sep = "-"), font.size = "tiny")
        
        if (landscape == TRUE) {
          message("\\end{landscape}")} 
        
      }  
    }
  }
  
  
  
##################### PIE CHARTS    ################

    
  arch_pie_plot <- function(pies,cols,rows){
    
    y <-df_raw[,1:3]
    
    .df <- data.frame(y = pies , #pie charts
                      s = rows, #rows 
                      t = cols) #columns
    
    .df <- as.data.frame(with(.df, prop.table(table(y, s, t), margin = 2:3)))
    
    .plot <-  ggplot(data = .df, aes(x = factor(1), y = Freq, fill = y)) + 
      geom_bar(width = 1, stat = "identity") + 
      scale_y_continuous(labels = scales::percent_format(), limits = c(0, 1)) + 
      coord_polar(theta = "y") + 
      facet_grid(s ~ t) + 
      xlab(NULL) + 
      labs(fill = "ID") + 
      theme_bw(base_size = 8, base_family = "sans") + 
      theme(panel.spacing = unit(0.3, "lines"), legend.position = "right", 
            axis.title.x = element_blank(), axis.title.y = element_blank(), axis.text.y 
            = element_blank(), axis.ticks = element_blank())
    print(.plot)
    
  }
  
  
  
  
  
  
###BUGS: WHEN DOING STARGAZER FOR LATEX THE DIGITS ARE NOT ROUNDED IN RSD!!!
message("          # All the packages were installed and loaded succesfully!
          # All the functions were loaded to the system succesfully!
          # The ArchFlow rmd files are ready to Run!")









