#' @import shiny
app_ui <- function() {
    fluidPage(
        
        # Application title
        titlePanel("Monitoring du taux de positivité au test PCR du SARS-Cov-2 à l'Université de Bordeaux"),
        
        # Sidebar with a slider input for number of bins 
        sidebarLayout(
            sidebarPanel(
                h3("Données"),
                numericInput("nsuccess",
                             label = "Nombres de cas",
                             value = 153),
                numericInput("ntrials",
                             label = "Population totale",
                             value = 100000),
                # numericInput("scalingfactor",
                #              label = "Nombre de tests",
                #              value = NULL),
                numericInput("incid_ref",
                             label = "Incidence de référence",
                             value = 153/100000),
                h3("Prior"),
                sliderInput("alpha",
                            "Alpha",
                            min = 0,
                            max = 10,
                            value = 1),
                
                sliderInput("beta",
                            "Beta",
                            min = 0,
                            max = 10,
                            value = 1),
                h3("Intervalle de crédibilité"),
                sliderInput("credibility_mass",
                            "Niveau de crédibilité",
                            min = 0.01,
                            max = 0.99,
                            value = 0.8),
                h3("Options de représentation"),
                checkboxInput("logscale", 
                              label = "Échelle log10", 
                              value = TRUE)
            ),
            
            # Show a plot of the generated distribution
            mainPanel(
                plotOutput("distPlot"),
                h5(""),
                tableOutput("resTable")
            )
        )
    )
}
