
#source("findRoot.r")
#source("AttDicriminant.r")
#' @export
NiarTreeAttDisc <- function(data){
  GeneralList <- list()
  AttDisc <- 0
  if(nrow(data) == 1)
    return()#list(left = NULL, right = NULL, caso = NULL, AttDiscriminant = NULL))
  else{
    if(nrow(data) > 1){
      tdata <- data[,1:ncol(data)-1]
      #AttDisc <- AttDicriminant(tdata)
      AttDisc <- sample(37:42, 1)
      break_at <- findRoot(data,AttDisc)
      dataord<- order(data[,AttDisc])
      data <- data[dataord,]
      if(break_at > 1)
      {
        dataizq = data[1:break_at-1,]
        datader = data[-seq(break_at),]
        breakcase = data[break_at,]
      }
      else{
        dataizq = data[1:break_at,]
        datader = data[-seq(break_at),]
        breakcase = data[break_at,]
      }
      #GeneralList <- list(list(left = dataizq, SonL = list(), right = datader, SonR = list(), caso = breakcase, AttDiscriminant = AttDisc))
      #GeneralList <- list(list(left = dataizq, right = datader, caso = breakcase, AttDiscriminant = AttDisc))
      GeneralList <- list(list(totalleft = nrow(dataizq), totalright = nrow(datader),root = breakcase, AttDiscriminant = AttDisc))
      GeneralList$SonL <- append(GeneralList$SonL,NiarTreeAttDisc(dataizq))
      GeneralList$SonR <- append(GeneralList$SonR,NiarTreeAttDisc(datader))
    }
  }
  return(GeneralList)
}
