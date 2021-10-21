partial_digest_problem <- function(L){
  width <- max(L)
  L <- L[-which.max(L)]
  X <- c(0,width)
  return(place(L, X, width))
}
place <- function(L, X, width){
  if(length(L) == 0){
    print(X)
    return(X)
  }
  y <- max(L)
  
  y_X <- y - X
  if(prod(L %in% y_X)){
    X <- c(X, y)
    L <- which(!(L %in% y_X))
    place(L, X, width)
    X <- X[1:length(X)-1] # spoliham na to, ze y zustane poslednim prvkem X
    L <- c(L, y_X)
  }
  w_y_X <- width - y - X
  if(prod(L %in% w_y_X)){
    X <- c(X, width - y)
    L <- which(!(L %in% w_y_X))
    place(L, X, width)
    X <- X[1:length(X)-1]
    L <- c(L, w_y_X)
  }
  return()
  
}