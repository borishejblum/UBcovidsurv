#' @import shiny
app_ui <- function() {
    fluidPage(
        
        # Application title
        titlePanel("Monitoring du taux de positivité au test PCR du SARS-Cov-2 à l'Université de Bordeaux"),
        
        # Sidebar with a slider input for number of bins 
        sidebarLayout(
            sidebarPanel(
                h3("Modèle"),
                radioButtons("model_type",
                             label="",
                             choices = c("Vraisemblance Binomiale" = "binom", 
                                         "Vraisemblance Hypergéométrique" = "hyper"),
                             selected = "hyper"
                ),
                h3("Données"),
                numericInput("ncases",
                             label = "Nombre de cas",
                             value = 1),
                numericInput("ntests",
                             label = "Nombre de tests",
                             value = 10),
                numericInput("ntotal",
                             label = "Nombre total d'étudiants",
                             value = 500),
                # numericInput("scalingfactor",
                #              label = "Nombre de tests",
                #              value = NULL),
                numericInput("incid_ref",
                             label = "Incidence de référence",
                             value = 123/100000),
                h3("Prior"),
                sliderInput("alpha",
                            "Alpha",
                            min = 0.2,
                            max = 3,
                            step = 0.2,
                            value = 1),
                
                sliderInput("beta",
                            "Beta",
                            min = 0.2,
                            max = 3,
                            step = 0.2,
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
