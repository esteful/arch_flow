"arch_heatmap" <- function(df_chem){
  #based on d3heatmap package
  
  #### Heatmap of the chemical composition ####
  mat_div <- t(t(df_chem)/colMeans(df_chem))
  #get the relative values of each composition
  d3heatmap(mat_div,         #colors are based on these values
            cellnote=df_chem,       #show the raw chemical data
            labRow= row.names(df_chem),width = 1050, height = 900)   #the label names
}