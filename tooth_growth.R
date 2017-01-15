#Statistical Inference Course Project
#tooth_growth.R
#by Lawrence Tomaziefski
#2017-01-14
#_______________________________________________________________________________
#Script Begins
#-------------------------------------------------------------------------------------
require(dplyr)
require(ggthemes)
require(ggplot2)
require(datasets)
require(knitr)

#Read the ToothGrowth data into a data frame.  
tooth_growth = ToothGrowth

colnames(tooth_growth) = c("Length", "Supplement", "Dose")



growth_summary = tooth_growth %>%
        group_by(Supplement, Dose) %>%
        summarize('Median Length' = median(Length),
                  'Mean Length' = mean(Length))

growth_box = ggplot(tooth_growth, aes(x = interaction(Supplement, Dose),y = Length, color = Supplement)) + 
        geom_boxplot(aes(fill = Supplement), color = "black", alpha = .5, width = .5, outlier.size=1.5, outlier.shape=21, notch = FALSE) + 
        scale_fill_manual(values = c(color_palette$color_code[2],color_palette$color_code[4])) + 
        stat_summary(fun.y="mean", geom="point", shape=23, size=3, fill="white", color = "black") +
        theme(panel.background = element_rect(fill = "white"),
              panel.grid.minor = element_line(color = NA),
              panel.grid.major.x = element_line(color = NA),
              panel.grid.major.y = element_line(color = NA),
              panel.border = element_rect(color = "black", fill = NA, size = 1),
              plot.title = element_text(hjust = 0.5),
              text = element_text(size = 10, color = "black"),
              plot.caption = element_text(hjust = 0.5)) +
        labs(x = "Interaction Between Supplement and Dose",
             title = "Supplement and Dose Effect on Guinea Pig Tooth Growth")

#Test that Ho: mu_OJ = mu_VC
t.test(Length ~ Supplement, paired = FALSE, var.equal = TRUE, data = tooth_growth)

#Test that Ho: mu_1 = mu_2
tooth_growth_dose = tooth_growth %>%
        filter(Dose >= 1)
t.test(Length ~ Dose, paired = FALSE, var.equal = TRUE, data = tooth_growth_dose)
        
#Test that Ho: mu_OJ2 = mu_VC2
tooth_growth_supplement = tooth_growth %>%
        filter(Dose == 2)
t.test(Length ~ Supplement, paired = FALSE, var.equal = TRUE, data = tooth_growth_supplement)

#Test that Ho: muOJ1 = muOJ2
tooth_growth_OJ = tooth_growth %>%
        filter(Dose >= 1 & Supplement == "OJ")
t.test(Length ~ Dose, paired = FALSE, var.equal = TRUE, data = tooth_growth_OJ)$conf.int

coplot(Length ~ Dose | Supplement, data = tooth_growth, panel = panel.smooth,
       xlab = "ToothGrowth data: length vs dose, given type of supplement")