# NetworkDashboard

This app creates an interactive environment in simulating radom networks. 

This enables teachers and students to use specific examples to evaluate **undirected, unweighted random networks** and to discuss the effect of the number of nodes involved and the associated link probability. 

This app is therefore primarily used for interactive exploration.

## Technical notes on using the app

To use the app, follow these steps:

1. Download the repository
2. Open the corresponding R project file NetworkDashboard.Rproj
3. Open the file server.R
4. Click Run App in the top right corner (I recommend selecting Run external from the drop-down menu to open the app in the browser)

Alternatively, you can also use the following link (http://yannickdiehl.shinyapps.io/networksimulation/), but the usage time per month for the course is limited here and you cannot view the code.

A third option is to install the app locally and use it over its local R version. To do this, run the following code:

shiny::runGitHub(username = "YannickDiehl", repo = "NetworkDashboard")
