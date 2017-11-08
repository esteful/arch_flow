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
