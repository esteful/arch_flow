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
  