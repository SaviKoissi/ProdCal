This Shiny app is designed to estimate the number of seedlings, the time required for their production, and the number of personnel needed for in vitro culture production. All calculations incorporate uncertainty around key production parameters, ensuring a more realistic and flexible model. Detailed calculations can be reviewed in the accompanying PDF document. Additionally, the R file titled "BayProdCal.R" demonstrates how the calculations were implemented using a Bayesian approach, offering further insights into the underlying methodology.

This application is built using Shiny and several other R packages, including tools from the tidyverse collection. To ensure smooth functionality, you'll need to install the necessary packages and set up the environment correctly. 

```r
# Required packages
install.packages(c("shiny", "shinyWidgets", "shinydashboard", "tidyverse"))

```

Then load the library Shiny and execute the following command

```r
sink("/dev/null") 
shiny::runGitHub("ProdCal", "SaviKoissi")
sink() 
```

