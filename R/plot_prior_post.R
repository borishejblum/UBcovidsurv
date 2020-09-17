#' @import ggplot2
#' @import dplyr

plot_prior_post <- function(alpha, beta, nsuccess, ntrials, credibility_mass, logscale,
                            incid_ref = NULL){
  
  my_xseq <- 10^(seq(from=-5, to=0, length.out = 1000))
  data2plot <- rbind.data.frame(
    cbind.data.frame("value" = my_xseq, 
                     "pdf" = dprior(theta = my_xseq, 
                                    alpha = alpha, beta = beta), 
                     "dist"="a priori"),
    cbind.data.frame("value" = my_xseq, 
                     "pdf" = dposterior(theta = my_xseq, 
                                        alpha = alpha, beta = beta,
                                        nsuccess = nsuccess, ntrials = ntrials), 
                     "dist"="a posteriori")
  )
  CI <- cred_int(credibility_mass = credibility_mass, alpha = alpha, beta = beta, nsuccess = nsuccess, ntrials = ntrials)
  
  p <- ggplot(data2plot, aes(x=value)) + 
    geom_line(aes(y=pdf, col=dist)) + 
    theme_classic() + 
    ylab("Densité de probabilité") + 
    xlab("Incidence") + #expression(theta)) + 
    geom_ribbon(data = data2plot %>% filter(dist == "a posteriori", value >= CI["lower"], value <= CI["upper"]),
                aes(x=value, ymin=0, ymax=pdf, fill=dist), alpha=0.2) +
    guides(colour=guide_legend(title="Distribution")) +
    NULL
  
  if(!is.null(incid_ref)){
    p <- p + geom_vline(xintercept = incid_ref, aes(color="Incidence de réference"), linetype = "dashed")
  }
  
  
  if(logscale){
    p <- p + 
      geom_segment(x = log10(CI["lower"]), xend = log10(CI["lower"]), y = 0, 
                   yend = dposterior(theta = CI["lower"], 
                                     alpha = alpha, beta = beta,
                                     nsuccess = nsuccess, ntrials = ntrials),
                   aes(color = "a posteriori"), linetype = "dotted") +
      geom_segment(x = log10(CI["upper"]), xend = log10(CI["upper"]), y = 0, 
                   yend = dposterior(theta = CI["upper"], 
                                     alpha = alpha, beta = beta,
                                     nsuccess = nsuccess, ntrials = ntrials),
                   aes(color = "a posteriori"), linetype = "dotted") +
      scale_x_log10() + expand_limits(x=10^-5)
  }else{
    p <- p +  
      geom_segment(x = CI["lower"], xend = CI["lower"], y = 0, 
                   yend = dposterior(theta = CI["lower"], 
                                     alpha = alpha, beta = beta,
                                     nsuccess = nsuccess, ntrials = ntrials),
                   aes(color = "a posteriori"), linetype = "dotted") +
      geom_segment(x = CI["upper"], xend = CI["upper"], y = 0, 
                   yend = dposterior(theta = CI["upper"], 
                                     alpha = alpha, beta = beta,
                                     nsuccess = nsuccess, ntrials = ntrials),
                   aes(color = "a posteriori"), linetype = "dotted")
  }
  
  
  
  p <- p + guides(fill=guide_legend(title=paste0("Interval de crédibilité à ", credibility_mass*100, "%"), 
                                    #override.aes = list(linetype="dotted")
  ))
  
  return(p)
}
