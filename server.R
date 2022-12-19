library(shiny)
library(tidyverse)
library(igraph)
library(qgraph)
source("helpers.R")

# Define server logic required to draw a histogram
shinyServer(
  
  function(input, output) {
    
    observeEvent(input$reset_input, {
      updateSliderInput(inputId = "n_vertices", value = 10)
      updateSliderInput(inputId = "edge_prob", value = .3)
    })
    
    N <- reactive(input$n_vertices)
    
    output$select_var1 <- renderUI({
    selectInput(
      inputId = "var_select1",
      label = "Starting node:",
      choices = 1:N(),
      selected = 1
    )
    })
    
    output$select_var2 <- renderUI({
      selectInput(
        inputId = "var_select2",
        label = "Ending node (optional):",
        choices = 1:N()
      )
    })
    
    network_matrix <- reactive({
      input$update
      generate_network(
        n = N(),
        p = input$edge_prob
      ) 
    })
    
    k <- reactive(colSums(network_matrix()))
    
    L <- reactive(sum(k())/2)
    
    k_mean <- reactive((2*L())/N())
    
    var_1 <- reactive(input$var_select1 %>% as.numeric())
    
    var_2 <- reactive(input$var_select2 %>% as.numeric())
    
    bfs_df <- reactive(map_dfr(set_names(1:N()), ~ bfs(network_matrix(), .x)))
    
    bfs_vec <- reactive(flatten_dbl(bfs_df())[is.finite(flatten_dbl(bfs_df()))])
    
    d_mean <- reactive(sum(bfs_vec())/(N()*(N()-1)))
    
    Li <- reactive(c())
    
    
    
    igraph <- reactive(graph_from_adjacency_matrix(network_matrix()))
    
    
    output$networkPlot <- renderPlot({
      
      if (input$bfs_check == T) {
        qgraph(
          network_matrix(), 
          groups = as.factor(bfs_df()[[var_1()]]),
          theme = "colorblind")
        
      } else {
        qgraph(network_matrix())
        
      }
    })
    
    
    output$legend <- renderUI({fluidPage(
      h5(HTML("<strong> Legend </strong>")), 
      HTML("<p> N = overall number of nodes </p>"),
      HTML("<p> L = overall number of links </p>"),
      HTML("<p> k<sub>i</sub> = number of links from one node to other nodes (degree) </p>"),
      HTML("<p> \u27E8k\u27E9 = average degree </p>"),
      HTML("<p> p<sub>k</sub> = probability that a randomly selected node in the network has degree k </p>"),
      HTML("<p> BFS = Breadth-First Search Algorithm </p>"),
      HTML("<p> d<sub>max</sub> = longest shortest path between two nodes (diameter)"),
      HTML("<p> d<sub>ij</sub> = shortest path between nodes i and j"),
      HTML("<p> \u27E8d\u27E9 = average Path Length"),
      HTML("<p> C<sub>Bi</sub> = how far a node is from every other node in the network (closeness after Bavelas)"),
      HTML("<p> B<sub>i</sub> = how often a node lies on the shortest path connecting any two other nodes (betweenness) </p>"),
      HTML("<p> L<sub>i</sub> = number of links between the k<sub>i</sub> neighbors of node i </p>"),
      HTML("<p> C<sub>i</sub> = degree to which the neighbors of a given node link to each other (local cluster) </p>"),
      HTML("<p> \u27E8C\u27E9 = average clustering coefficient"),
      HTML("<p> C<sub>\u0394</sub> = global clustering coefficient </p>")
    )})
    

    output$global_prop <- renderUI({fluidPage(
      h5(HTML("<strong> Global properties </strong>")),
      withMathJax("$$N=", N(), "$$"),
      withMathJax("$$L_{max}=\\frac{N(N-1)}{2}=", (N()*(N()-1))/2, "$$"),
      withMathJax("$$L=\\frac{1}{2}\\sum_{i=1}^{N}k_i=", L(), "$$"),
      withMathJax("$$\\langle k \\rangle=\\frac{1}{N}\\sum_{i=1}^{N}k_i=\\frac{2L}{N}=", round(k_mean(), 3), "$$"),
      withMathJax("$$p_k=\\frac{N_k}{N};~", "e.g.~p_1=", round(length(k()[k()==1])/N(), 3), "$$"),
      withMathJax("$$d_{max}=", max(bfs_vec()), "$$"),
      withMathJax("$$\\langle d \\rangle=\\frac{1}{N(N-1)}\\sum_{i \\neq j}d(i,j)=", round(d_mean(),3), "$$"),
      withMathJax("$$\\langle C \\rangle=\\frac{1}{N}\\sum_{i=1}^{N}C_1=", round(transitivity(igraph(), type = "localaverageundirected"), 3), "$$"),
      withMathJax("$$C_{\\Delta}=", round(transitivity(igraph(), type = "globalundirected"), 3), "$$")
    )})
    
    
    output$local_prop <- renderUI({fluidPage(
      h5(HTML("<strong>Local properties</strong>")),
      withMathJax("$$k_i=", k()[var_1()], "$$"),
      withMathJax("$$d_{ij}=", bfs_df()[var_1(), var_2()], "$$"),
      withMathJax("$$C_{Bi}=\\frac{1}{\\sum_{j=1}^{N}d_{ij}}=", round(1/sum(bfs_df()[var_1()]),3),"$$"),
      withMathJax("$$B_i=\\sum_{<j,k>}\\frac{d_{jk}~through~i}{d_{jk}}=", round(betweenness(igraph(), var_1(), directed = F),3), "$$"),
      withMathJax("$$C_i=\\frac{2L_i}{k_i(k_i-1)}=", round(transitivity(igraph(), type = "localundirected")[var_1()], 3), "$$")
    )})
    
    
    output$components <- renderUI({
      inf <- any(is.infinite(flatten_dbl(bfs_df())))
      fluidPage(
        h5(HTML("<strong> Network conditions </strong>")),
        HTML(ifelse(inf == T, 
                    paste("<span style='color:red'>&#10008;</span> Network is disconnected and has", 
                          count_components(igraph()), 
                          "different components"),
                    paste("<span style='color:green'>&#10004;</span> Network is full connected") 
        ))
      )
    })
    
    
    output$degree_distribution <- renderPlot({
      as_tibble(k()) %>% 
        ggplot(aes(x = value)) +
        geom_bar(aes(x = value, y=..prop..), stat="count", fill = "steelblue") +
        # ggtitle("Degree Distribution") +
        xlab("degree (k)") +
        ylab(bquote('probability of k ('*~p[k]*')')) +
        theme_bw() 
        # theme(plot.title = element_text(face="bold"))
    })
    
    output$distance_distribution <- renderPlot({
      as_tibble(bfs_vec()) %>% 
        ggplot(aes(x = value)) +
        # geom_bar(aes(x = value, y=..prop..), stat="count", fill = "steelblue") +
        geom_density(adjust = 2, color = "steelblue") +
        geom_vline(xintercept = d_mean(), linetype = "dashed") +
        # ggtitle("Distance Distribution") +
        xlab(bquote('distance ('*~d[ij]*')')) +
        ylab(bquote('probability of d ('*~p[d]*')')) +
        theme_bw() 
        # theme(plot.title = element_text(face="bold"))
    })
    
    
    output$centrality <- renderPlot({
      centralityPlot(network_matrix(), include = c("Degree","Strength", "Closeness","Betweenness"))
    })
    
    
    output$clustering <- renderPlot({
      clusteringPlot(network_matrix())
    })
    
    output$adjacency_matrix <- renderTable(
      digits = 0, colnames = T, rownames = T, width = "100%", bordered = T,
      align = "c", stripped = T,{
      network_matrix() %>% 
          as.data.frame() %>% 
          rename_with(~ str_replace(.x, "V", "N"), everything())
    })

  }


)
