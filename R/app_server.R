#' @import shiny
app_server <- function(input, output, session) {
    
    # reactive(if(is.null(input$scalingfactor)){
    #     scalingfactor <- 1
    # })
    
    observeEvent({input$model_type; 
        input$alpha; input$beta; 
        input$ncases; input$ntotal; input$ntests; input$incid_ref;
        input$credibility_mass;
        input$logscale},{
        if(input$model_type == "binom"){
            dprior <- function(theta, log=FALSE){
                dprior_beta(theta, alpha = input$alpha, beta = input$beta, log=FALSE)
            }
            dposterior <- function(theta, log=FALSE){
                dposterior_betabinomial(theta, alpha = input$alpha, beta = input$beta, 
                                        nsuccess = input$ncases, ntrials = input$ntotal,
                                        log=FALSE)
            }
            rposterior <- function(n){
                rposterior_betabinomial(n, alpha = input$alpha, beta = input$beta, 
                                        nsuccess = input$ncases, ntrials = input$ntotal)
            }
            MAP_fun <- function(){
                MAP_betabinomial(alpha = input$alpha, beta = input$beta, 
                                 nsuccess = input$ncases, ntrials = input$ntotal)
            }
            posterior_mean_fun <- function(){
                posterior_mean_betabinomial(alpha = input$alpha, beta = input$beta, 
                                            nsuccess = input$ncases, ntrials = input$ntotal)
            }
            
        }else if(input$model_type == "hyper"){
            dprior <- function(theta, log=FALSE){
                dprior_betabinom(theta, alpha = input$alpha, beta = input$beta, N = input$ntotal, log=FALSE)
            }
            dposterior <- function(theta, log=FALSE){
                dposterior_hypergeom(theta, alpha = input$alpha, beta = input$beta, N = input$ntotal,
                                     nsuccess = input$ncases, ntrials = input$ntests,
                                     log=FALSE)
            }
            rposterior <- function(n){
                rposterior_hypergeom(n, alpha = input$alpha, beta = input$beta, N = input$ntotal,
                                     nsuccess = input$ncases, ntrials = input$ntests)
            }
            
            MAP_fun <- function(){
                MAP_hypergeom(alpha = input$alpha, beta = input$beta, N = input$ntotal,
                              nsuccess = input$ncases, ntrials = input$ntests)
            }
            posterior_mean_fun <- function(){
                posterior_mean_hypergeom(alpha = input$alpha, beta = input$beta, N = input$ntotal, 
                                         nsuccess = input$ncases, ntrials = input$ntests)
            }
        }
        plot2render <- plot_prior_post(alpha = input$alpha, beta = input$beta, 
                                       ncases = input$ncases, ntrials = input$ntrials,
                                       credibility_mass = input$credibility_mass,
                                       logscale = input$logscale,
                                       incid_ref = input$incid_ref,
                                       dprior = dprior, 
                                       dposterior = dposterior,
                                       rposterior = rposterior)
        output$distPlot <- renderPlot({plot2render})
        table2render <- table_res(credibility_mass = input$credibility_mass,
                                  ref_incid = input$incid_ref,
                                  dprior = dprior,
                                  dposterior = dposterior,
                                  rposterior = rposterior,
                                  MAP_fun = MAP_fun,
                                  posterior_mean_fun = posterior_mean_fun)
        output$resTable <- renderTable({table2render})
    })
    
}
