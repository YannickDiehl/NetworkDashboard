# Generate Network
generate_network = function(n, p) {
  
  # Calculate number of possible edges
  L <- n * (n-1)/2
  
  # Generate matrix values, sampling 0 or 1 with given probabilities
  matvals <- sample(c(0, 1), L, replace = TRUE, prob = c(1 - p,p))
  
  # From the values above, generate a symmetric matrix
  networkmat <- matrix(rep(0, n * n), ncol = n)
  mv <- 1
  for (i in 1:n) {
    for (j in 1:n) {
      if (i > j) {
        networkmat[i, j] <- matvals[mv]
        networkmat[j, i] <- matvals[mv]
        mv <- mv + 1
      }
    }
  }
  return(networkmat)
}


# Breadth-First Search Algorithm
bfs <- function(graph, start){
  
  # A Queue to manage the nodes that have yet to be visited, intialized with the start node
  queue <- c(start)
  
  # A boolean array indicating whether we have already visited a node
  visited <- rep(F, nrow(graph))
  
  # (The start node is already visited)
  visited[start] <- T
  
  # Keeping the distances (might not be necessary depending on your use case)
  distances <- rep(Inf, nrow(graph))  
  
  # (the distance to the start node is 0)
  distances[start] <- 0
  
  # While there are nodes left to visit...
  while(length(queue) > 0) {
    
    node = queue[1] # get...
    queue = queue[-1] # ...and remove next node
    
    for(i in seq_along(graph[node,])) {
      
      if(graph[node,i] && !visited[i]){
        
        visited[i] = TRUE
        distances[i] = distances[node] + 1
        queue = c(queue, i)
        
      }
    }
  }
  
  return (distances)
  
}





