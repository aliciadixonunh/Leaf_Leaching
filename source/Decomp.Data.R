## type the path to where your files are saved
setwd("C:/Users/dixon/OneDrive - USNH/Research/Leaf Leaching")

##upload csv file
decomp.data <- read.csv("Decomp.Rates.csv")


##loading needed packages
library(ggplot2)
library(dplyr)
library(cowplot)
library(ggpubr)
library(rstatix)
library(stats)
library(agricolae)

## Make a new row
#create a row called leaf type
##ifelse statement= if brown column values equal 1, assign "brown" as new value, if not, assign it green
decomp.data$leaftype <- ifelse(decomp.data$brown == 1, "brown","green")

##Creating new variable column based on leaf type saying "if my leaf type is equal to brown then call my new variable senesced, if its not equal to brown, call it fresh"
decomp.data$Leaf.Stage= ifelse(decomp.data$leaftype=="brown","Senesced", "Fresh")

###make a boxplot for decomp (C loss) rates
decomp.data$species <- as.character(decomp.data$species)
ggplot(decomp.data, aes(x=species, y=rate.C.loss)) + geom_bar(aes(fill = Leaf.Stage), stat = "identity", position = position_dodge(0.8), width = 0.7) + labs(x= "Species", y= "Rate of Carbon Loss", fill= "Leaf Type") + scale_fill_manual(values=c('saddlebrown','seagreen')) + theme_bw() 



###make a boxplot for DOC rates
decomp.data$species <- as.character(decomp.data$species)
ggplot(decomp.data, aes(x=species, y=DOC_rate)) + geom_bar(aes(fill = Leaf.Stage), stat = "identity", position = position_dodge(0.8), width = 0.7) + labs(x= "Species", y= "DOC Rate (mg C / g leaf / day)", fill= "Leaf Type") + scale_fill_manual(values=c('seagreen','saddlebrown')) + theme_bw()+ 
  theme(legend.text = element_text(size = 20),
        axis.text = element_text(size = 24),
        axis.title = element_text(size = 24),
        legend.title = element_text(size = 20)) ###text size for agu




###make a bar for TML rates
decomp.data$species <- as.character(decomp.data$species)
ggplot(decomp.data, aes(x=species, y=TML_rate)) + geom_bar(aes(fill = Leaf.Stage), stat = "identity", position = position_dodge(0.8), width = 0.7) + labs(x= "Species", y= "Total Mass Loss Rate (% mass of leaf / day)", fill= "Leaf Type") + scale_fill_manual(values=c('seagreen','saddlebrown')) + theme_bw() +
  theme(legend.text = element_text(size = 20),
        axis.text = element_text(size = 24),
        axis.title = element_text(size = 24),
        legend.title = element_text(size = 20)) ###text size for agu


###########RUN A T-TEST????



####################NEED HELP
##ANCOVA stats???

#load required libraries
library(tidyverse)
library(ggpubr)
library(rstatix)
library(broom)


#need to verify that the covariate and the treatment are independent, so run an ANOVA 
#fit anova model
anova_model <- aov(TML_rate ~ leaftype, data = decomp.data)
#view summary of anova model
summary(anova_model)
##p value is not significant

#need to verify that there is homogeneity of variance among the groups, we conduct Levene’s Test
#load car library to conduct Levene's Test
library(car)
#conduct Levene's Test
leveneTest(TML_rate~leaftype, data = decomp.data)
  #not significant, so we can assume homogeneity of the residual variances for all groups.


#fit ANCOVA model
ancova_model <- lm(TML_rate ~ leaftype + species, data = decomp.data)

#view summary of model
Anova(ancova_model, type="II")
##not significant






##DOC ANCOVA stats???

#load required libraries
library(tidyverse)
library(ggpubr)
library(rstatix)
library(broom)


#need to verify that the covariate and the treatment are independent, so run an ANOVA 
#fit anova model
anova_model <- aov(DOC_rate ~ leaftype, data = decomp.data)
#view summary of anova model
summary(anova_model)
##p value is not significant

#need to verify that there is homogeneity of variance among the groups, we conduct Levene’s Test
#load car library to conduct Levene's Test
library(car)
#conduct Levene's Test
leveneTest(DOC_rate~leaftype, data = decomp.data)
#not significant, so we can assume homogeneity of the residual variances for all groups.


#fit ANCOVA model
ancova_model <- lm(DOC_rate ~ leaftype + species, data = decomp.data)

#view summary of model
Anova(ancova_model, type="II")
##not significant




#load the multcomp library
library(multcomp)
# Assuming 'leaftype' is a factor variable in your dataset
# Convert 'leaftype' to a factor if it's not already
decomp.data$leaftype <- as.factor(decomp.data$leaftype)


#view summary of model
Anova(ancova_model, type="II")
#define the post hoc comparisons to make
postHocs <- glht(ancova_model, linfct = mcp(leaftype = "Tukey"))

#view a summary of the post hoc comparisons
summary(postHocs)

