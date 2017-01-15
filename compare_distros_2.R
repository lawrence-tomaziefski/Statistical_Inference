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
        scale_x_continuous(breaks = seq(0, max(exponential_dist$distribution), by = 5)) +
        labs(y = "Density",
             x = "Time Between Events",
             title = "Exponential Distribution") +
        geom_vline(aes(xintercept= mean(distribution, na.rm = TRUE), color = "Mean"),  
        linetype = "dashed", size = 1) +
        scale_fill_manual(name = NULL, values = c(Distribution = color_palette$color_code[6])) +
        scale_color_manual(name = NULL, values = c(Mean = color_palette$color_code[9])) 

#This simulation produces a sample population of the average means and variances of 1000 sets of 40 random exponentials.  
simulation_output = data.frame()
        for (i in 1 : simulations){
        output = data.frame(output = rexp(rand_exponentials, rate = lambda))
        output = summarize(output, means = mean(output), variance = var(output))
        simulation_output = rbind(simulation_output, output)
        }
        
expected_mean = 1/lambda
standard_dev = 1/lambda
std_error = standard_dev/sqrt(rand_exponentials) 

simulation_output  = mutate(simulation_output, normal_mean = (means - expected_mean)/std_error)
sample_stats = simulation_output %>%
                summarize(mean = mean(means), variance = mean(variance), normalized_mean = mean(normal_mean), normal_var = mean(var(normal_mean)))
        
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
        


#Density plot of the normalized means
        normal_mean_graph2 = ggplot(simulation_output, aes(x = normal_mean, fill = normal_mean)) +
                geom_density(aes(y =..density.., fill = "Distribution"),alpha = .2) +
                theme(panel.background = element_rect(fill = "white"),
                      panel.grid.minor = element_line(color = NA),
                      panel.grid.major.x = element_line(color = NA),
                      panel.grid.major.y = element_line(color = NA),
                      panel.border = element_rect(color = "black", fill = NA, size = 1),
                      plot.title = element_text(hjust = 0.5),
                      text = element_text(size = 10, color = "black"),
                      plot.caption = element_text(hjust = 0.5)) +
                scale_x_continuous(breaks = seq(-3, 3, by = .5)) +
                labs(y = "Density",
                     x = "Normalized Means",
                     title = "Distribution of Normalized Mean") +
                geom_vline(aes(xintercept= mean(simulation_output$normal_mean, na.rm = TRUE), color = "Mean"),  
                           linetype = "dashed", size = 1) +
                scale_color_manual(name = NULL, values = c(Mean = color_palette$color_code[10])) +
                scale_fill_manual(name = NULL, values = c(Distribution = color_palette$color_code[8])) 

