

"chemLabels"<-
  function(labels_vector)
  {
    # fa les chemLabels per un plot fent els subscripts. Han díestar una en cada fila
    # fet per Jaume Buxeda i GarrigÛs
    # versiÛ normalitzada a marÁ de 2015   
    
    #varsum_ordered_vec
    n<-length(labels_vector)
    
    chemLabels<-vector(length=n)
    
    chemLabels[]<- labels_vector

    for(i in 1:n) {
      if (regexpr ("^Fe2O3$",as.character(chemLabels[i]))==T) chemLabels[i] <-  expression(bold("Fe"["2"]*"O"["3"]))
      if (regexpr ("^Al2O3$", as.character(chemLabels[i]))==T) chemLabels[i]<- expression(bold("Al"["2"]*"O"["3"]))
      if (regexpr ("^P2O5$", as.character(chemLabels[i]))==T) chemLabels[i]<-expression(bold("P"["2"]*"O"["5"]))
      if (regexpr ("^TiO2$", as.character(chemLabels[i]))==T) chemLabels[i]<-expression(bold("TiO"["2"]))
      if (regexpr ("^Na2O$", as.character(chemLabels[i]))==T) chemLabels[i]<-expression(bold("Na"["2"]*"O"))
      if (regexpr ("^K2O$", as.character(chemLabels[i]))==T) chemLabels[i]<-expression(bold("K"["2"]*"O"))
      if (regexpr ("^SiO2$", as.character(chemLabels[i]))==T) chemLabels[i]<-expression(bold("SiO"["2"]))
      if (regexpr ("^MnO$", as.character(chemLabels[i]))==T) chemLabels[i]<-expression(bold("MnO"))
      if (regexpr ("^MgO$", as.character(chemLabels[i]))==T) chemLabels[i]<-expression(bold("MgO"))
      if (regexpr ("^CaO$", as.character(chemLabels[i]))==T) chemLabels[i]<-expression(bold("CaO"))
      if (regexpr ("^Ba$", as.character(chemLabels[i]))==T) chemLabels[i]<-expression(bold("Ba"))
      if (regexpr ("^BaO$", as.character(chemLabels[i]))==T) chemLabels[i]<-expression(bold("BaO"))
      if (regexpr ("^Rb$", as.character(chemLabels[i]))==T) chemLabels[i]<-expression(bold("Rb"))
      if (regexpr ("^Nb$", as.character(chemLabels[i]))==T) chemLabels[i]<-expression(bold("Nb"))
      if (regexpr ("^Pb$", as.character(chemLabels[i]))==T) chemLabels[i]<-expression(bold("Pb"))
      if (regexpr ("^Zr$", as.character(chemLabels[i]))==T) chemLabels[i]<-expression(bold("Zr"))
      if (regexpr ("^Y$", as.character(chemLabels[i]))==T) chemLabels[i]<-expression(bold("Y"))
      if (regexpr ("^Sr$", as.character(chemLabels[i]))==T) chemLabels[i]<-expression(bold("Sr"))
      if (regexpr ("^Ce$", as.character(chemLabels[i]))==T) chemLabels[i]<-expression(bold("Ce"))
      if (regexpr ("^Ga$", as.character(chemLabels[i]))==T) chemLabels[i]<-expression(bold("Ga"))
      if (regexpr ("^V$", as.character(chemLabels[i]))==T) chemLabels[i]<-expression(bold("V"))
      if (regexpr ("^Zn$", as.character(chemLabels[i]))==T) chemLabels[i]<-expression(bold("Zn"))
      if (regexpr ("^Cu$", as.character(chemLabels[i]))==T) chemLabels[i]<-expression(bold("Cu"))
      if (regexpr ("^Ni$", as.character(chemLabels[i]))==T) chemLabels[i]<-expression(bold("Ni"))
      if (regexpr ("^Cr$", as.character(chemLabels[i]))==T) chemLabels[i]<-expression(bold("Cr"))
      if (regexpr ("^Th$", as.character(chemLabels[i]))==T) chemLabels[i]<-expression(bold("Th"))
      if (regexpr ("^Mo$", as.character(chemLabels[i]))==T) chemLabels[i]<-expression(bold("Mo"))
      if (regexpr ("^Sn$", as.character(chemLabels[i]))==T) chemLabels[i]<-expression(bold("Sn"))
      if (regexpr ("^W$", as.character(chemLabels[i]))==T) chemLabels[i]<-expression(bold("W"))
      if (regexpr ("^Co$", as.character(chemLabels[i]))==T) chemLabels[i]<-expression(bold("Co"))
      if (regexpr ("^LOI$", as.character(chemLabels[i]))==T) chemLabels[i]<-expression(bold("LOI"))
      if (regexpr ("vt", as.character(chemLabels[i]))==T) chemLabels[i]<-expression(bold("vt"))
      if (regexpr ("^As$", as.character(chemLabels[i]))==T) chemLabels[i]<-expression(bold("As"))
      if (regexpr ("^Ca$", as.character(chemLabels[i]))==T) chemLabels[i]<-expression(bold("Ca"))
      if (regexpr ("^La$", as.character(chemLabels[i]))==T) chemLabels[i]<-expression(bold("La"))
      if (regexpr ("^Lu$", as.character(chemLabels[i]))==T) chemLabels[i]<-expression(bold("Lu"))
      if (regexpr ("^Na$", as.character(chemLabels[i]))==T) chemLabels[i]<-expression(bold("Na"))
      if (regexpr ("^Nd$", as.character(chemLabels[i]))==T) chemLabels[i]<-expression(bold("Nd"))
      if (regexpr ("^Sm$", as.character(chemLabels[i]))==T) chemLabels[i]<-expression(bold("Sm"))
      if (regexpr ("^U$", as.character(chemLabels[i]))==T) chemLabels[i]<-expression(bold("U"))
      if (regexpr ("^Yb$", as.character(chemLabels[i]))==T) chemLabels[i]<-expression(bold("Yb"))
      if (regexpr ("^Cs$", as.character(chemLabels[i]))==T) chemLabels[i]<-expression(bold("Cs"))
      if (regexpr ("^Eu$", as.character(chemLabels[i]))==T) chemLabels[i]<-expression(bold("Eu"))
      if (regexpr ("^Fe$", as.character(chemLabels[i]))==T) chemLabels[i]<-expression(bold("Fe"))
      if (regexpr ("^Hf$", as.character(chemLabels[i]))==T) chemLabels[i]<-expression(bold("Hf"))
      if (regexpr ("^Sb$", as.character(chemLabels[i]))==T) chemLabels[i]<-expression(bold("Sb"))
      if (regexpr ("^Sc$", as.character(chemLabels[i]))==T) chemLabels[i]<-expression(bold("Sc"))
      if (regexpr ("^Ta$", as.character(chemLabels[i]))==T) chemLabels[i]<-expression(bold("Ta"))
      if (regexpr ("^Tb$", as.character(chemLabels[i]))==T) chemLabels[i]<-expression(bold("Tb"))
      if (regexpr ("^Al$", as.character(chemLabels[i]))==T) chemLabels[i]<-expression(bold("Al"))
      if (regexpr ("^Gd$", as.character(chemLabels[i]))==T) chemLabels[i]<-expression(bold("Gd"))
      if (regexpr ("^K$", as.character(chemLabels[i]))==T) chemLabels[i]<-expression(bold("K"))
      if (regexpr ("^Dy$", as.character(chemLabels[i]))==T) chemLabels[i]<-expression(bold("Dy"))
      if (regexpr ("^Ho$", as.character(chemLabels[i]))==T) chemLabels[i]<-expression(bold("Ho"))
      if (regexpr ("^Mg$", as.character(chemLabels[i]))==T) chemLabels[i]<-expression(bold("Mg"))
      if (regexpr ("^Er$", as.character(chemLabels[i]))==T) chemLabels[i]<-expression(bold("Er"))
      if (regexpr ("^Mn$", as.character(chemLabels[i]))==T) chemLabels[i]<-expression(bold("Mn"))
      if (regexpr ("^Pr$", as.character(chemLabels[i]))==T) chemLabels[i]<-expression(bold("Pr"))
      if (regexpr ("^Ti$", as.character(chemLabels[i]))==T) chemLabels[i]<-expression(bold("Ti"))
      if (regexpr ("^Tm$", as.character(chemLabels[i]))==T) chemLabels[i]<-expression(bold("Tm"))
      if (regexpr ("^Ppc$", as.character(chemLabels[i]))==T) chemLabels[i]<-expression(bold("Ppc"))
      if (regexpr ("^SO3$", as.character(chemLabels[i]))==T) chemLabels[i]<-expression(bold("SO"["3"]))
    }
    as.vector(chemLabels)
  }
