"arch_PCA" <- function(df_chem, df_raw, lvar=.lvar, nplot=nplot, alr = TRUE, printPCA = FALSE, labels= FALSE, shape_cat_number = 15, frame = TRUE, PCx = 1, PCy =2, nshapes=10, label.size = 3){
  
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
                         label.size = label.size,                 #set the label sizes (not shapes)
                                            
                         #shapes      
                         shape = colnames(df_raw)[shape_cat_number],  #set the category to display by shapes     
                         size = 1,                        #set the size of the shapes
                         
                         #loadings        
                         loadings = TRUE,
                         loadings.label = TRUE, 
                         loadings.colour = 'gray', 
                         loadings.label.colour = "black",
                         
                         #eLlipse   
                         frame = frame, #true by default
                         frame.type = "norm")   # if "norm" is removed the shaope of the frame is geometrical
                         
               
                 + scale_shape_manual(values=c(1:nshapes)) # or chose from 1-25 shapes to display all equally, 15 by default
                 + ggtitle(label = "PCA") 
                 + theme(plot.title = element_text(hjust = 0.5))  
                 + labs(caption= paste("PCA using:", paste(noquote(colnames(df_chem)), collapse = ", "))) 
                 + theme(plot.caption = element_text(size=7, hjust=0.5, margin=margin(t=1))))
   
    
    
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