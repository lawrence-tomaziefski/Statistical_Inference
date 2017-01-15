#Statistical Inference Course Project
#compare_distros.R
#by Lawrence Tomaziefski
#2017-01-08
#_______________________________________________________________________________
#Script Begins
#-------------------------------------------------------------------------------------
#Getting the Data

require(dplyr)
require(ggthemes)
require(ggplot2)
require(xlsx)

#Establish a color pallete that will be used for making plots.  Note that to reproduce the graphs one will have to use thier own color palette.  

color_frame = read.xlsx("color_frame.xlsx",1,header = TRUE, stringsAsFactors = FALSE)
color_palette = color_frame %>%
        filter(palette == 1)
      
colors = c(unique(color_palette$color_code))

#Set the parameters for the simulations below.  
set.seed(10)
simulations = 1000
rand_exponentials = 40
lambda = .2

#This simulation produces an exponentional distribution for a 1000 random exponentials.  
exponential_dist = data.frame(distribution = rexp(simulations, rate = lambda))

#Denisty plot of exponential distribution
exponential_graph = ggplot(exponential_dist, aes(x = distribution, fill = distribution)) +
        geom_density(aes(y =..density.., fill = "Distribution"),alpha = .2) +
        theme(panel.background = element_rect(fill = "white"),
              panel.grid.minor = element_line(color = NA),
              panel.grid.major.x = element_line(color = NA),
              panel.grid.major.y = element_line(color = NA),
              panel.border = element_rect(color = "black", fill = NA, size = 1),
              plot.title = element_text(hjust = 0.5),
              text = element_text(size = 10, color = "black"),
              plot.caption = element_text(hjust = 0.5)) +
        scale_x_continuous(breaks = seq(0, max(exponential_dist$distribution), by = 5)) 
        labs(y = "Density",
             x = "Time Between Events",
             title = "Exponential Distribution")+
        geom_vline(aes(xintercept= mean(distribution, na.rm = TRUE), color = "Mean"),  
        linetype = "dashed", size = 1) +
        scale_fill_manual(name = NULL, values = c(Distribution = color_palette$color_code[6])) +
        scale_color_manual(name = NULL, values = c(Mean = color_palette$color_code[9])) 
exponential_graph
#This simulation produces a sample population of the average means and variances of 1000 sets of 40 random exponentials.  
simulation_output = data.frame()
        for (i in 1 : simulations){
        output = data.frame(output = rexp(rand_exponentials, rate = lambda))
        output = summarize(output, means = mean(output), variance = var(output))
        simulation_output = rbind(simulation_output, output)
        }
        sample_stats = summarize(simulation_output, mean = mean(means), variance = mean(variance))
        sample_stats
        
#Denisty plot of the mean of the sample population.          
        sample_mean_graph = ggplot(simulation_output, aes(x = means, fill = means)) +
                geom_density(aes(y =..density.., fill = "Distribution"),alpha = .2) +
                theme(panel.background = element_rect(fill = "white"),
                      panel.grid.minor = element_line(color = NA),
                      panel.grid.major.x = element_line(color = NA),
                      panel.grid.major.y = element_line(color = NA),
                      panel.border = element_rect(color = "black", fill = NA, size = 1),
                      plot.title = element_text(hjust = 0.5),
                      text = element_text(size = 10, color = "black"),
                      plot.caption = element_text(hjust = 0.5)) +
                scale_x_continuous(breaks = seq(0, 10, by = 1)) +
                labs(y = "Density",
                     x = "Average of Sample Means",
                     title = "Distribution of Sample Mean") +
                geom_vline(aes(xintercept= mean(simulation_output$means, na.rm = TRUE), color = "Mean"),  
                           linetype = "dashed", size = 1) +
                scale_color_manual(name = NULL, values = c(Mean = color_palette$color_code[10])) +
                scale_fill_manual(name = NULL, values = c(Distribution = color_palette$color_code[8])) 
                
        
#Denisty plot of the variance of the means in the sample population.         
        sample_var_graph = ggplot(simulation_output, aes(x = variance)) +
                geom_density(aes(y =..density.., fill = "Distribution"),alpha = .2) +
                theme(panel.background = element_rect(fill = "white"),
                      panel.grid.minor = element_line(color = NA),
                      panel.grid.major.x = element_line(color = NA),
                      panel.grid.major.y = element_line(color = NA),
                      panel.border = element_rect(color = "black", fill = NA, size = 1),
                      plot.title = element_text(hjust = 0.5),
                      text = element_text(size = 10, color = "black"),
                      plot.caption = element_text(hjust = 0.5)) +
                scale_x_continuous(breaks = seq(0, max(simulation_output$variance), by = 10)) +
                labs(y = "Density",
                     x = "Average of Sample Variances",
                     title = "Distribution of Sample Variance") +
                geom_vline(aes(xintercept= mean(simulation_output$variance, na.rm = TRUE), color = "Mean"),  
                          linetype = "dashed", size = 1) +
                scale_color_manual(name = NULL, values = c(Mean = color_palette$color_code[10])) +
                scale_fill_manual(name = NULL, values = c(Distribution = color_palette$color_code[4])) 
        
#Density plot comparing the simulated exponential distribution and the distribution of the sample means.        
exponential_dist_2 = data.frame(means = rexp(simulations, rate = lambda))
combined_graph = ggplot(simulation_output, aes(x = means, fill = means)) +
                geom_density(aes(y =..density.., fill = "Sample"),alpha = .2) +
                geom_density(data = exponential_dist_2, alpha = .2, aes(fill = "Exponential"))+
                theme(panel.background = element_rect(fill = "white"),
                      panel.grid.minor = element_line(color = NA),
                      panel.grid.major.x = element_line(color = NA),
                      panel.grid.major.y = element_line(color = NA),
                      panel.border = element_rect(color = "black", fill = NA, size = 1),
                      plot.title = element_text(hjust = 0.5),
                      text = element_text(size = 10, color = "black"),
                      plot.caption = element_text(hjust = 0.5)) +
                scale_x_continuous(breaks = seq(0, 10, by = 2), limits = c(0,10)) +
                labs(y = "Density",
                     x = "Time Between Events",
                     title = "Comparison of Exponential Distribution and Sample Mean") +
                geom_vline(aes(xintercept= mean(exponential_dist_2$means, na.rm = TRUE)),  
                           color = color_palette$color_code[4], linetype = "dashed", size = 1) +
                geom_vline(aes(xintercept= mean(simulation_output$means, na.rm = TRUE)),  
                           color = color_palette$color_code[10], linetype = "dashed", size = 1) +
                annotate(geom = "text", x = 8, y = .4, label = "The means of the of the 
                        exponential distributon and the sample means
                         are approximately equal at 1/lambda.") +
        #scale_color_manual(name = NULL, values = c(Mean = color_palette$color_code[10])) +
        scale_fill_manual(name = NULL, values = c(Sample = color_palette$color_code[8], Exponential = color_palette$color_code[9]))
        
#Produces a QQplot of the sample population distribution in order to check for normality.          
        qq = ggplot(simulation_output) + 
        stat_qq(aes(sample = means, color = "Mean"), distribution = stats :: qnorm) +
                theme(panel.background = element_rect(fill = "white"),
                      panel.grid.minor = element_line(color = NA),
                      panel.grid.major.x = element_line(color = NA),
                      panel.grid.major.y = element_line(color = NA),
                      panel.border = element_rect(color = "black", fill = NA, size = 1),
                      plot.title = element_text(hjust = 0.5),
                      text = element_text(size = 10, color = "black"),
                      plot.caption = element_text(hjust = 0.5)) +
               labs(y = "Sample Means",
                    x = "Theoretical Quantiles" ,
                     title = "QQ Plot of the Sample Distribution") +
        scale_color_manual(values=c(color_palette$color_code[8]))


        c_i_mean = (mean(simulation_output$means)+c(-1,1)+qnorm(0.995) * sd(simulation_output$means)/sqrt(length(simulation_output$means)))
        c_i_var = (mean(simulation_output$variance)+c(-1,1)+qnorm(0.995) * sd(simulation_output$variance)/sqrt(length(simulation_output$variance)))
        

        output_2 = data.frame(output_2 = rexp(simulations, rate = lambda))
        
        
        hist(output_2$output_2)
        
        mean(output_2$output_2)
        