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