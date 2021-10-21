double_digest_brute = function(fragmentsA, fragmentsB, fragmentsAB){
  # 1. sort fragments
  #fragmentsA <- sort(fragmentsA)
  #fragmentsB <- sort(fragmentsB)
  fragmentsAB <- sort(fragmentsAB)
  
  # 2. order fragments
  # iteration through all permutations needed
  
  # 3. create fragments map
  restriction_mapA <- c(0, cumsum(fragmentsA))
  restriction_mapB <- c(0, cumsum(fragmentsB))
  print(restriction_mapA)
  print(restriction_mapB)
  # 4. merge positions
  restriction_map_merged <- c(restriction_mapA, restriction_mapB)
  restriction_map_merged <- sort(restriction_map_merged)
  restriction_map_merged <- unique(restriction_map_merged)
  
  # while(pointerA <= length(restriction_mapA) &&
  #       pointerB <= length(restriction_mapB)){
  #   print(restriction_mapA[pointerA])
  #   print(restriction_mapB[pointerB])
  #   print(restriction_mapA[pointerA] >= restriction_mapB[pointerB])
  #   if(restriction_mapA[pointerA] >= restriction_mapB[pointerB]){
  #     restriction_map_merged <- c(restriction_map_merged, restriction_mapA[pointerA])
  #     pointerA <- pointerA + 1
  #   }else{
  #     restriction_map_merged <- c(restriction_map_merged, restriction_mapB[pointerB])
  #     pointerB <- pointerB + 1
  #   }
  # }
  
  # 5. difference
  fragments_merged <- diff(restriction_map_merged)
  # 6. sort, compare with fragmentsAB
  fragments_merged <- sort(fragments_merged)
  
  if(prod(fragments_merged == fragmentsAB)){
    print("Reseni nalezeno")
    return(restriction_map_merged)
    
    # 7. Reverse solution
  }
  
  
}

perm <- function(v) {
  n <- length(v)
  if (n == 1) v
  else {
    X <- NULL
    for (i in 1:n) X <- rbind(X, cbind(v[i], perm(v[-i])))
    X
  }
}