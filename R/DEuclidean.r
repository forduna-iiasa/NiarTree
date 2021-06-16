#' @export
MinEuclidean1xn <- function(a,b){
  for(i in 1:length(b)){
    b[i] <- DEuclidean1x1(a,b[i])
  }
  return(which.min(b))
}

#' @export
MinEuclidean1x1 <- function(a,b){
  for(i in 1:length(b)){
    b[i] <- DEuclidean1x1(a,b[i])
    print(b[i])
  }
  return(which.min(b))
}

#' @export
DEuclidean1x1 <- function(a,b){
  return(sqrt(sum((a - b)^2)))
}

#' @export
DEuclidean1xn <- function(a,b){
  Dist<-0.0
  s<-0.0
  for(i in 1:length(b)){
    s <- s + (a - b[i])^2
  }
  return(sqrt(s))
}
