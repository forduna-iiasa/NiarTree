AttDicriminant <- function(data){
  val <- 0
  AttDis <- 0
  #for(i in 1:length(data)){
  #valc <- sum(diff(data[,i]))
  for(i in 37:42){ #consider only WET_1 to WET_6
    valc <- sum(diff(data[,i]))
    if(i==37){
      val <- valc
      AttDis <- 1
    }else{
      if(valc < val){
        val <- valc
        AttDis <- i
      }
    }
  }
  return(AttDis)
}