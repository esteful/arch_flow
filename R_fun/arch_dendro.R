"arch_dendro" <- function(df_chem, nplot, printDendro = TRUE, df_raw = df_raw)
{

  if (!require("ggbiplot")) {
    install.packages("ggbiplot", dependencies = TRUE)
    require(ggbiplot)
  }
  
  if (!require("dendextend")) {
    install.packages("dendextend", dependencies = TRUE)
    require(dendextend)
  }
  if (!require("devtools")) {
    install.packages("devtools", dependencies = TRUE)
    require(devtools)
  }
  
  if (!require("devEMF")) {
    install.packages("devEMF", dependencies = TRUE)
    require(devEMF)
  }
  
  
    #Plot hierarchical clustering of clr transformed data, using centroid algorithm and squared euclidean distance

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
     labels_colors(my_dend) <- rainbow(nlevels(as.factor(df_raw[,i])))[as.factor(df_raw[,i])][order.dendrogram(my_dend)]
    
  #create the plot
     plot(my_dend, main= "Hierarchical Clustering", horiz =  FALSE)
     legend("topright",cex = 0.5, legend = unique(as.factor(df_raw[,i])), 
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
     
}


