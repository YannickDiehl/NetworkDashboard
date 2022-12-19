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
    titlePanel("Random network simulation"),
    
    # Sidebar with a slider input
    fluidRow(
      
      column(3, 
             
      wellPanel(
        
        sliderInput(
          inputId = "n_vertices",
          label = "Nodes:",
          min = 0,
          max = 30,
          value = 10,
          animate = animationOptions(interval = 3000),
          ticks = T
        ),
        
        sliderInput(
          inputId = "edge_prob",
          label = "Link probability:",
          min = 0,
          max = 1,
          value = 0.3,
          step = 0.025,
          animate = animationOptions(interval = 3000),
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
      ),
      
      wellPanel(
        
        uiOutput("components")
        
      )
        
      ),
      
      # Show a plot of the generated network
      column(6,
             
        plotOutput("networkPlot", width = "100%", height = 1200)
          
          ),
      
      # Descriptives
      column(3, 
             
       wellPanel(
         uiOutput("legend"),
       ),
        
        wellPanel(
          uiOutput("global_prop")
        ))
      ),
    
    fluidRow(
      
      column(3,
             
        plotOutput("degree_distribution")
        
       ),
      
      column(3,
             
         plotOutput("centrality")
             
      ),
      
      column(3,
             
          plotOutput("distance_distribution")
             
      ),
      
      column(3,
             
          plotOutput("clustering")
             
      )
      
      
    ),
    
    fluidRow(
      
      column(12,
             
         headerPanel(h5("Adjacency matrix")),
         wellPanel(
           tableOutput("adjacency_matrix")
         ) 
             
             
      )
      
    )
    )
  )
    

