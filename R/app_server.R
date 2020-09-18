#' @import shiny
app_server <- function(input, output, session) {

    # reactive(if(is.null(input$scalingfactor)){
    #     scalingfactor <- 1
    # })
    
    output$distPlot <- renderPlot({
        plot_prior_post(alpha = input$alpha, beta = input$beta, 
                        nsuccess = input$nsuccess, ntrials = input$ntrials,
                        credibility_mass = input$credibility_mass,
                        logscale = input$logscale,
                        incid_ref = input$incid_ref)
    })
    
    output$resTable <- renderTable({
        table_res(alpha = input$alpha, beta = input$beta, 
                  nsuccess = input$nsuccess, ntrials = input$ntrials,
                  credibility_mass = input$credibility_mass,
                  ref_incid = input$incid_ref)
    })
}
