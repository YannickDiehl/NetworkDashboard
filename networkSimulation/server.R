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
    
    N <- reactive({input$n_vertices})
    
    network_matrix <- reactive({
      input$update
      generate.network(
        n = N(),
        p = input$edge_prob
      ) 
    })
    
    k <- reactive({
      colSums(network_matrix())
    })
    
    L <- reactive({
      sum(k())/2
    })
    
    k_mean <- reactive({
      (2*L())/N()
    })
    
    output$networkPlot <- renderPlot({
      qgraph(network_matrix())
    })
    
    
    output$ledgend <- renderUI({fluidPage(
      h5(strong("Legend")),
      paste("$$N~=~overall~number~of~nodes$$"),
      paste("$$L~=~overall~number~of~links$$"),
      paste("$$k~(degree)~=~number~of~links~from~one~node~to~other~nodes$$"),
      paste("$$\\langle k \\rangle~=~average~degree$$"),
      paste("$$p_k~=~probability~that~a~randomly~selected~node~in~the~network~has~degree~k$$")
    )})
    
    
    output$global_prop <- renderUI({fluidPage(
      h5(strong("Global properties")),
      withMathJax("$$N=", N(), "$$"),
      withMathJax("$$L_{max}=\\frac{N(N-1)}{2}=", (N()*(N()-1))/2, "$$"),
      withMathJax("$$L=\\frac{1}{2}\\sum_{i=1}^{N}k_i=", L(), "$$"),
      withMathJax("$$\\langle k \\rangle=\\frac{1}{N}\\sum_{i=1}^{N}k_i=\\frac{2L}{N}=", k_mean(), "$$"),
      withMathJax("$$p_k=\\frac{N_k}{N};~", "e.g.~p_1=", length(k()[k()==1])/N(), "$$")
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
          as_tibble() %>% 
          rename_with(~ str_replace(.x, "V", "N"), everything())
    })

  }


)
