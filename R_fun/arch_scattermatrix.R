arch_scatter_matrix <- function(df_raw, vars, group, title){
  
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
    ggthemes::theme_few(base_size = 10, base_family = "sans") + 
    theme(legend.position = "right")

  print(.plot)
  
  ggsave(filename = "scatter_plot.pdf", 
         
         plot = .plot)
  
  rm(ggthemes_data, .grid, .all, .densities)
  
}
