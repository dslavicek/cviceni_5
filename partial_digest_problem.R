partial_digest_problem <- function(L){
  width <- max(L)
  L <- L[-which.max(L)]
  X <- c(0,width)
  return(place(L, X, width))
}
place <- function(L, X, width){
  print("place")
  print("L:")
  print(L)
  print("X:")
  print(X)
  if(length(L) == 0){
    print("result:")
    print(X)
    return(X)
  }
  y <- max(L)
  
  y_X <- abs(y - X)
  print("y_x:")
  print(y_X)
  if(prod(y_X %in% L)){
    print("prvni if")
    X <- c(X, y)
    L <- which(!(L %in% y_X))
    place(L, X, width)
    X <- X[1:length(X)-1] # spoliham na to, ze y zustane poslednim prvkem X
    L <- c(L, y_X)
  }
  w_y_X <- abs(width - y - X)
  if(prod(w_y_X %in% L)){
    print("druhej if")
    X <- c(X, width - y)
    L <- which(!(L %in% w_y_X))
    place(L, X, width)
    X <- X[1:length(X)-1]
    L <- c(L, w_y_X)
  }
  return()
  
}