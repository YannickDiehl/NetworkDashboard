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
      label = "Starting Variable:",
      choices = 1:N(),
      selected = 1
    )
    })
    
    output$select_var2 <- renderUI({
      selectInput(
        inputId = "var_select2",
        label = "Ending Variable (optional):",
        choices = 1:N()
      )
    })
    
    network_matrix <- reactive({
      input$update
      generate.network(
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
    
    
    output$ledgend <- renderUI({fluidPage(
      h5(HTML("<strong> Legend </strong>")), 
      HTML("<p> N = overall number of nodes </p>"),
      HTML("<p> L = overall number of links </p>"),
      HTML("<p> k = number of links from one node to other nodes (degree) </p>"),
      HTML("<p> \u27E8k\u27E9 = average degree </p>"),
      HTML("<p> p<sub>k</sub> = probability that a randomly selected node in the network has degree k </p>"),
      HTML("<p> BFS = Breadth-First Search Algorithm </p>"),
      HTML("<p> d<sub>max</sub> = longest shortest path between two nodes (diameter)"),
      HTML("<p> d<sub>ij</sub> = shortest path between nodes i and j"),
      HTML("<p> \u27E8d\u27E9 = average Path Length")
    )})
    
    
    output$global_prop <- renderUI({fluidPage(
      h5(HTML("<strong>Global properties </strong>")),
      withMathJax("$$N=", N(), "$$"),
      withMathJax("$$L_{max}=\\frac{N(N-1)}{2}=", (N()*(N()-1))/2, "$$"),
      withMathJax("$$L=\\frac{1}{2}\\sum_{i=1}^{N}k_i=", L(), "$$"),
      withMathJax("$$\\langle k \\rangle=\\frac{1}{N}\\sum_{i=1}^{N}k_i=\\frac{2L}{N}=", k_mean(), "$$"),
      withMathJax("$$p_k=\\frac{N_k}{N};~", "e.g.~p_1=", length(k()[k()==1])/N(), "$$"),
      withMathJax("$$d_{max}~=~", max(bfs_vec()), "$$"),
      withMathJax("$$\\langle d \\rangle~=~\\frac{\\sum_{i \\neq j}d(i,j)}{N(N-1)}=", round(sum(bfs_vec())/(N()*(N()-1)),3), "$$")
    )})
    
    output$local_prop <- renderUI({fluidPage(
      h5(HTML("<strong>Local properties</strong>")),
      withMathJax("$$d_{ij}=", bfs_df()[var_1(), var_2()], "$$")
    )})
    
    output$degree_distribution <- renderPlot({
      as_tibble(k()) %>% 
        ggplot(aes(x = value)) +
        geom_bar(aes(x = value, y=..prop..), stat="count", fill = "steelblue") +
        ggtitle("Degree Distribution") +
        xlab("k (degree)") +
        ylab(bquote('probability of k ('*~p[k]*')')) +
        theme(plot.title = element_text(face="bold"))
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
