"arch_PCA" <- function(df_chem,df_raw, lvar=lvar, nplot=nplot, alr = TRUE, printPCA = FALSE, labels= TRUE)
  {
  
#Plot PCAs on alr transformed data (if desired) showing coloured categories according to nplot given values.
  
#load ggbiplot
   require(ggbiplot)

#alr tranformation (only if alr = TRUE)
  if (alr == TRUE){
    require(compositions) 
    df_chem <-  alr(df_chem,lvar) #lvar is the less variable element in the dataset. Calculated previously.
      }

#show label names or only points   
  if (labels == TRUE){
    labels_names = row.names(df_chem)
  }
  
  if (labels == FALSE){
    labels_names = NULL
  }

    
#loop for generating PCAs 
  for (i in nplot){
    
    pca <- paste("pca_cat", i, sep = "_") #generate names (eg. pca_cat_1, pca_cat_2,...)
    assign(pca,
           ggbiplot(prcomp
                    (df_chem),
                    #choices == 2,  #principal components number to include 
                    labels= labels_names,
                    center= TRUE, 
                    scale.= TRUE,
                    obs.scale = 1, 
                    var.scale = 1, 
                    groups = as.factor(df_raw[,i]), #e.g. df_raw[,2] (Site: Gloucester, New Forest, Wales) 
                    ellipse = FALSE, 
                    circle = FALSE,
                    varname.size= 3, 
                    varname.adjust= TRUE) 
    
           + theme(legend.direction = 'vertical', 
                      legend.position = 'right',
                      plot.caption=element_text(size=8, hjust=0.5, margin=margin(t=1))
                      )
           + labs(caption= paste("PCA using:", paste(noquote(colnames(df_chem)), collapse = ",")))
                  
           + scale_color_discrete(name = as.character(paste(colnames(df_raw[i]))))
           + ggtitle(label = "PCA")
           )

      plot(eval(parse(text =paste("pca_cat", i, sep = "_")))) #plot PCAs according to ncpa vector
      
      
  }
  
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



