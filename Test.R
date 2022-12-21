graph <- generate_network(20, .1)

qgraph(graph)

colSums(graph)

centralityPlot(graph, include = c("Degree","Strength", "Closeness","Betweenness"))
a <- clusteringPlot(graph)

y <- bfs(graph, 2)

a
x <- map_dfr(set_names(1:10), ~ bfs(graph, .x))

flatten_dbl(x) %>% is.infinite() %>% any()

k3 <- graph[3,] %>% set_names(1:20)
neigbors <- k3[k3 == 1] %>% names() %>% as.numeric()
graph[c(3,neigbors),c(3,neigbors)] %>% graph_from_adjacency_matrix %>% gsize()

sum(graph == 1) # ecount/gsize number of edges

colSums(graph) # degrees (k_i)

test <- graph %>% graph_from_adjacency_matrix() 

ecount(induced_subgraph(test, neighbors(test, 1)))

transitivity(test, type = "localundirected")

degree(test, 1)

betweenness(test, c(1:20))

centrality(graph)$Betweenness
u <- centralityPlot(graph, include = c("Degree","Strength", "Closeness","Betweenness"))
u$data %>% filter(measure == "Betweenness")


max(flatten_dbl(x)[is.finite(flatten_dbl(x))])

max(is.finite(flatten_dbl(x)))

pull(x, 1)

qgraph(graph, groups = y)

