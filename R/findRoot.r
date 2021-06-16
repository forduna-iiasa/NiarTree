#' @export
findRoot <- function(data,att){
  g <- data[,att]
  dataord<- order(data[,att])
  data <- data[dataord,]
  med <- mean(data[,att])
  caso <- MinEuclidean1xn(med,data[,att])
  return(caso)
}
