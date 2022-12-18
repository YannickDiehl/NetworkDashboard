library(shiny)

shinyUI(
  
  fluidPage(
    
    # includeCSS('styles.css'),
    
    tags$head(
      tags$style(
      HTML("div.MathJax_Display{
      text-align: left !important;}")
      )),
    
    withMathJax(),
    
    # Application title
    titlePanel("Random undirected network model simulation"),
    
    # Sidebar with a slider input
    fluidRow(
      
      column(3, 
             
      wellPanel(
        
        sliderInput(
          inputId = "n_vertices",
          label = "Nodes:",
          min = 0,
          max = 20,
          value = 10,
          animate = T,
          ticks = T
        ),
        
        sliderInput(
          inputId = "edge_prob",
          label = "Link probability:",
          min = 0,
          max = 1,
          value = 0.3,
          step = 0.025,
          animate = T,
          ticks = T
        ),
        
        fluidRow(
          
          actionButton(
            inputId = "update",
            label = "New Network", 
            icon("arrows-rotate"), 
            class = "btn btn-primary"
          ),
          
          actionButton(
            inputId = "reset_input", 
            label = "Reset inputs"
          )
        )
        
      ),
      
      wellPanel(
        
        uiOutput("select_var1"),
        
        uiOutput("select_var2"),
        
        checkboxInput(
          inputId = "bfs_check", 
          label = "Show BFS", 
          value = T
          )
        ),
        
      wellPanel(
        
        uiOutput("local_prop")
        
      )
        
      ),
      
      # Show a plot of the generated network
      column(6,
             
        plotOutput("networkPlot", width = "100%", height = 600)
          
          ),
      
      # Descriptives
      column(3, 
             
        wellPanel(
          
          uiOutput("ledgend"),
        
        ),
        
        wellPanel(
          uiOutput("global_prop")
        
        ))
      ),
    
    fluidRow(
      
      column(9,
        
        headerPanel(h5("Adjacency matrix")),
        wellPanel(
          tableOutput("adjacency_matrix")
        )
        
       ),
      
      column(3,
             
         plotOutput("degree_distribution")
             
        )
      
      
    )
    )
  )
    

