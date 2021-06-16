source("DEuclidean.r")
NiarPredictionAtt <- function(data,NiarTreeModel){
  Results = list()
  for(i in 1:nrow(data)){
    Results[i] <- FindTheBest(data[i,],NiarTreeModel)
  }
  
  
  #  root <- NiarTreeModel[[1]]
  # rootL <- NiarTreeModel$SonL
  # rootR <- NiarTreeModel$SonR
  # rootcase <- root$root
  # DiscValue <- rootcase[root$AttDiscriminant]
  # CasePrediction <- rootcase[nrow(rootcase)]
  # 
  return(Results)
  #return(lapply(X=CaseToCheck, FindTheBest,NiarTreeModel))
}

FindTheBest <- function(CaseToCheck,NiarTreeModel){
  root <- NiarTreeModel[[1]]
  rootcase <- root$root
  rootL <- NiarTreeModel$SonL
  rootR <- NiarTreeModel$SonR
  if (CaseToCheck[root$AttDiscriminant] <= rootcase[root$AttDiscriminant])
  {
    if(CaseToCheck[root$AttDiscriminant] == rootcase[root$AttDiscriminant])
    {
      c1 <- CaseToCheck[1:ncol(CaseToCheck)-1]
      c2 <- rootcase[1:ncol(rootcase)-1]
      if(DEuclidean1x1(c1,c2) == 0.0){
        return(rootcase[ncol(rootcase)])
      }else{
        if (is.null(rootL$SonL) == FALSE) 
          root <- FindTheBest(CaseToCheck,rootL)
        else
          return(rootcase[ncol(rootcase)])
      }
    }
    else{
      if (is.null(rootL$SonL) == FALSE) 
        root <- FindTheBest(CaseToCheck,rootL)
      else
        return(rootcase[ncol(rootcase)])
    }
  }
  else
  {
    if (is.null(rootR$SonR) == FALSE) 
      root <- FindTheBest(CaseToCheck,rootR)
    else 
      return(rootcase[ncol(rootcase)])
  }
}
