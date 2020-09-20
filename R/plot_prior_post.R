#' @import ggplot2
#' @import dplyr

plot_prior_post <- function(alpha, beta, ncases, ntrials, credibility_mass, logscale,
                            incid_ref = NULL, dprior, dposterior, rposterior){
  

  my_xseq <- 10^(seq(from=-5, to=0, length.out = 1000))
  data2plot_prior <- cbind.data.frame("value" = my_xseq, 
                                      "pdf" = dprior(theta = my_xseq), 
                                      "dist"="a priori")
  data2plot_post <- cbind.data.frame("value" = my_xseq, 
                   "pdf" = dposterior(theta = my_xseq), 
                   "dist"="a posteriori")
  max_post <- max(data2plot_post$pdf)
  data2plot_prior$pdf <- data2plot_prior$pdf/max(data2plot_prior$pdf)
  data2plot_post$pdf <- data2plot_post$pdf/max_post
  data2plot <- rbind.data.frame(data2plot_prior, data2plot_post)
  
  CI <- cred_int(credibility_mass = credibility_mass, rposterior = rposterior)
  
  p <- ggplot(data2plot, aes(x=value)) + 
    geom_line(aes(y=pdf, col=dist)) +
    theme_classic() + 
    ylab("Densité de probabilité (normalisée)") 
  
  if(!is.null(incid_ref)){
    p <- p + geom_vline(aes(linetype = "Incidence de réference", xintercept = incid_ref), color="grey20") +
      scale_linetype_manual("", values="dashed")
  }
  
  p <- p + geom_ribbon(data = data2plot %>% filter(dist == "a posteriori", value >= CI["lower"], value <= CI["upper"]),
                       aes(x=value, ymin=0, ymax=pdf, fill=paste0("Interval de crédibilité à ", credibility_mass*100, "%")), alpha=0.2) +
    scale_color_manual("Distribution", breaks = c("a priori", "a posteriori"), 
                       values=c("dodgerblue", "red3")) +
    scale_fill_manual("", values=c("red3")) 
  
  
  if(logscale){
    p <- p + 
      geom_segment(x = log10(CI["lower"]), xend = log10(CI["lower"]), y = 0, 
                   yend = dposterior(theta = CI["lower"])/max_post,
                   aes(color = "a posteriori"), linetype = "dotted") +
      geom_segment(x = log10(CI["upper"]), xend = log10(CI["upper"]), y = 0, 
                   yend = dposterior(theta = CI["upper"])/max_post,
                   aes(color = "a posteriori"), linetype = "dotted") +
      scale_x_log10() + expand_limits(x=10^-5) + 
      xlab("Incidence (échelle log10)") #expression(theta)) + 
  }else{
    p <- p +  
      geom_segment(x = CI["lower"], xend = CI["lower"], y = 0, 
                   yend = dposterior(theta = CI["lower"])/max_post,
                   aes(color = "a posteriori"), linetype = "dotted") +
      geom_segment(x = CI["upper"], xend = CI["upper"], y = 0, 
                   yend = dposterior(theta = CI["upper"])/max_post,
                   aes(color = "a posteriori"), linetype = "dotted") + 
      xlab("Incidence") #expression(theta)) + 
  }
  
  p <- p + guides(fill=guide_legend(override.aes = list(linetype="dotted", color="red3")),
                  color=guide_legend(override.aes = list(linetype="solid"))
  )
  
  return(p)
}
