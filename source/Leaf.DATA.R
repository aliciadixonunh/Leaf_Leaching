## type the path to where your files are saved
setwd("C:/Users/dixon/Dropbox (Personal)/My PC (DESKTOP-U34IQV9)/Documents/Grad School/Lab/Leaf Leaching")

##upload csv file
leachate.data <- read.csv("./data/LeafDATA.csv")


##loading needed packages
library(ggplot2)
library(dplyr)
library(cowplot)
library(ggpubr)
library(rstatix)
library(stats)
library(agricolae)
library(flextable)




## Make a new row
#create a row called leaf type
##ifelse statement= if green column values equal 1, assign "green" as new value, if not, assign it brown
leachate.data$leaftype <- ifelse(leachate.data$Green == 1, "green","brown")


#convert integer to character
leachate.data$Days_Leached <- as.character(leachate.data$Days_Leached)
str(leachate.data$Days_Leached)

#convert integer to character
leachate.data$Species <- as.character(leachate.data$Species)
str(leachate.data$Species)

##Creating new variable column based on leaf type saying "if my leaf type is equal to brown then call my new variable senesced, if its not equal to brown, call it fresh"
leachate.data$Leaf.Stage= ifelse(leachate.data$leaftype=="brown","Senesced", "Fresh")


####Making new rows to fix for detection limits
#Filter by detection limit and assign 1/2 value

    #TDN = 0.05 	? = 0.025
    leachate.data$TDN.dl = as.numeric(ifelse(leachate.data$TDN_mgN_g <= 0.05, 0.025, leachate.data$TDN_mgN_g))
    
    #NO3 = 0.004	? = 0.002
    leachate.data$NO3.dl = as.numeric(ifelse(leachate.data$NO3_mgN_g <= 0.005, 0.0025, leachate.data$NO3_mgN_g))
    
    #NH4 = 0.004	? = 0.002
    leachate.data$NH4.dl = as.numeric(ifelse(leachate.data$NH4_mgN_g <= 0.004, 0.002, leachate.data$NH4_mgN_g))
    
    #calculating DIN
    leachate.data$DIN_calc = leachate.data$NH4.dl+leachate.data$NO3.dl
    
    #calculating DON
    leachate.data$DON_calc = leachate.data$TDN.dl-(leachate.data$NO3.dl + leachate.data$NH4.dl)
    
    #DON = 0.01 	? = 0.005
    leachate.data$DON.dl = as.numeric(ifelse(leachate.data$DON_calc <= 0.01, 0.005, leachate.data$DON_calc))
    
    #DON/TDN percentage
    leachate.data$DON_percentage = as.numeric(leachate.data$DON.dl/leachate.data$TDN.dl)*100
    
    #PO4 = 0.002 	? = 0.001
    leachate.data$PO4.dl = as.numeric(ifelse(leachate.data$PO4_mgP_g <= 0.002, 0.001, leachate.data$PO4_mgP_g))
    
    #Ca = 0.1 	? = 0.05
    leachate.data$Ca.dl = as.numeric(ifelse(leachate.data$Ca_mgCa_g <= 0.1, 0.05, leachate.data$Ca_mgCa_g))
    

#create a new dataframe that just has green leaves or brown leaves
    green <- leachate.data %>%
      filter(leaftype == "green")
    
    
    brown <- leachate.data %>%
      filter(leaftype == "brown")

####Fixing mass loss now
    ##renaming old percent TML column
    leachate.data <- leachate.data %>% 
      rename("TML_old" = "percentTotalMassLost")
    
    ##renaming old Mass loss column
    leachate.data <- leachate.data %>% 
      rename("ML_mg_old" = "MassLost_mglost_gleaf")
    
    #New Mass Loss (mg lost per g of leaf)
    leachate.data <- leachate.data %>% 
      mutate(MassLoss = rowSums(leachate.data[ ,c(8, 19, 21:24, 39:41, 44:45)]))
    
    #New % Total Mass Loss
    leachate.data <- leachate.data %>% 
      mutate(perTML = leachate.data$MassLoss/leachate.data$DryWeight_g/1000*100)
    



####cation and anion averages on day 7####
  #filter by day 7
    day7 <- leachate.data %>%
      filter(Days_Leached == 7)
    
  #filter brown
    day7B <- leachate.data %>%
      filter(Days_Leached == 7) %>%
      filter(Leaf.Stage == "Senesced") %>%
      group_by(Species) %>%
      summarize(meanCa = mean(Ca.dl), meanCl = mean(Cl_mgCl_g), meanNa = mean(Na_mgNa_g), meanK = mean(K_mgK_g), meanMg = mean(Mg_mgMg_g), meanPO4 = mean(PO4.dl))
    View(day7B)
    as.table(day7B)
    
    
  #filter green
    day7G <- leachate.data %>%
      filter(Days_Leached == 7) %>%
      filter(Leaf.Stage == "Fresh") %>%
      group_by(Species) %>%
      summarize(meanCa = mean(Ca.dl), meanCl = mean(Cl_mgCl_g), meanNa = mean(Na_mgNa_g), meanK = mean(K_mgK_g), meanMg = mean(Mg_mgMg_g), meanPO4 = mean(PO4.dl))
    View(day7G)
    
    
####Mass loss due to C####

###make a boxplot for percent mass lost due to C
ggplot(leachate.data, aes(x=leaftype,  y=percent_MassLost_C)) + geom_boxplot() + labs(x= "Leaf Type", y= "% Mass Lost that is C")
##ANOVA for green leaves
MLCBvG <- lm(percent_MassLost_C ~ leaftype, leachate.data[!(leachate.data$Species == 8 & leachate.data$Days_Leached == 7),])
anova(MLCBvG)
#not Significant



###make a boxplot
#facet_wrap groups by leaves 
ggplot(leachate.data, aes(x=Days_Leached, y=percent_MassLost_C)) + geom_boxplot() + facet_wrap(~leaftype) + theme_cowplot() +labs(x= "Days Leached", y= "% Mass Lost that is C")

#create a new dataframe that just has green leaves or brown leaves
green <- leachate.data %>%
  filter(leaftype == "green")


brown <- leachate.data %>%
  filter(leaftype == "brown")

##ANOVA for green leaves
##excluding species 8
MassLossC.DaysG <- lm(percent_MassLost_C ~ Days_Leached, green[!(green$Species == 8 & green$Days_Leached == 7),])
anova(MassLossC.DaysG)
##days leached significant differnce but dont know which days, so do HSD test--means separation
MassLossC.DaysG_HSD <- HSD.test(MassLossC.DaysG, "Days_Leached", group = T)
MassLossC.DaysG_HSD 

##ANOVA for brown leaves
#excluding species 8
MassLossC.DaysB <- lm(percent_MassLost_C ~ Days_Leached, brown[!(brown$Species == 8 & brown$Days_Leached == 7),])
anova(MassLossC.DaysB)
##not significant



##Creating new variable column based on leaf type saying "if my leaf type is equal to brown then call my new variable senesced, if its not equal to brown, call it fresh"
leachate.data$Leaf.Stage= ifelse(leachate.data$leaftype=="brown","Senesced", "Fresh")

#convert integer to character
leachate.data$Species <- as.character(leachate.data$Species)
str(leachate.data$Species)
###make a boxplot
ggplot(leachate.data, aes(x=Species, y=percent_MassLost_C, fill=Leaf.Stage)) + geom_boxplot() + theme_cowplot() +labs(x= "Species", y= "% Mass Loss that is C") + scale_fill_manual(values=c('seagreen','saddlebrown')) + theme(legend.position="none")
#facet_wrap groups by species and leaf type 
ggplot(leachate.data, aes(x=Species, y=percent_MassLost_C, fill=Leaf.Stage)) + geom_boxplot() + facet_wrap(~Leaf.Stage) + theme_cowplot() +labs(x= "Species", y= "% Mass Loss that is C") + scale_fill_manual(values=c('seagreen','saddlebrown')) + theme(legend.position="none")

#facetwrap by days
#facet_wrap groups by days 
ggplot(leachate.data, aes(x=leaftype, y=percent_MassLost_C)) + geom_boxplot() + facet_wrap(~Days_Leached) + theme_cowplot() +labs(x= "Days Leached", y= "% Mass Lost that is C")


###make a boxplot for green vs brown leaves
#facet_wrap groups by species 
ggplot(leachate.data, aes(x=leaftype, y=percent_MassLost_C)) + geom_boxplot() + facet_wrap(~Species) + theme_cowplot() +labs(x= "Leaf Type", y= "% Mass Lost that is C")


##ANOVA for species 
##need to filter by species
s1 <- leachate.data %>%
  filter(Species == 1)
s2 <- leachate.data %>%
  filter(Species == 2)
s3 <- leachate.data %>%
  filter(Species == 3)
s4 <- leachate.data %>%
  filter(Species == 4)
s5 <- leachate.data %>%
  filter(Species == 5)
s6 <- leachate.data %>%
  filter(Species == 6)
s7 <- leachate.data %>%
  filter(Species == 7)
s8 <- leachate.data %>%
  filter(Species == 8)
s9 <- leachate.data %>%
  filter(Species == 9)


#ANOVA for s1
MLC.leaftypes1 <- lm(percent_MassLost_C ~ leaftype, s1)
anova(MLC.leaftypes1)
#not significant
#ANOVA for s2
MLC.leaftypes2 <- lm(percent_MassLost_C ~ leaftype, s2)
anova(MLC.leaftypes2)
# significant!!!
#ANOVA for s3
MLC.leaftypes3 <- lm(percent_MassLost_C ~ leaftype, s3)
anova(MLC.leaftypes3)
#Significant!!
#ANOVA for s4
MLC.leaftypes4 <- lm(percent_MassLost_C ~ leaftype, s4)
anova(MLC.leaftypes4)
#Not significant
#ANOVA for s5
MLC.leaftypes5 <- lm(percent_MassLost_C ~ leaftype, s5)
anova(MLC.leaftypes5)
#Significant!!
#ANOVA for s6
MLC.leaftypes6 <- lm(percent_MassLost_C ~ leaftype, s6)
anova(MLC.leaftypes6)
#Not significant!!
#ANOVA for s7
MLC.leaftypes7 <- lm(percent_MassLost_C ~ leaftype, s7)
anova(MLC.leaftypes7)
#Significant!!
#####exclude species 8??
#ANOVA for s8
MLC.leaftypes8 <- lm(percent_MassLost_C ~ leaftype, s8)
anova(MLC.leaftypes8)
#Significant!!
#ANOVA for s9
MLC.leaftypes9 <- lm(percent_MassLost_C ~ leaftype, s9)
anova(MLC.leaftypes9)
#Not significant!!




####Mass loss that is C-----Filter by day 1
day1 <- leachate.data %>%
  filter(Days_Leached == 1)

###make a boxplot for percent mass lost due to C after 1 day
ggplot(day1, aes(x=leaftype, y=percent_MassLost_C)) + geom_boxplot() +labs(x= "Leaf Type", y= "% Mass Lost that is C after 1 day")
##ANOVA for day 1
MassLossC.Day1 <- lm(percent_MassLost_C ~ leaftype, day1)
anova(MassLossC.Day1)
##notsignificant

###make a boxplot for day 1 green vs brown leaves
#facet_wrap groups by species and leaf type 
ggplot(day1, aes(x=Species, y=percent_MassLost_C, fill=Leaf.Stage)) + geom_boxplot() + facet_wrap(~Leaf.Stage) + theme_cowplot() +labs(x= "Species", y= "% Mass Loss that is C") + scale_fill_manual(values=c('seagreen','saddlebrown')) + theme(legend.position="none")

#facet_wrap groups by species 
ggplot(day1, aes(x=leaftype, y=percent_MassLost_C)) + geom_boxplot() + facet_wrap(~Species) + theme_cowplot() +labs(x= "Leaf Type", y= "% Mass Lost that is C after 1 days")


##ANOVA for species 
##need to filter by species by DAY1
s1d1 <- day1 %>%
  filter(Species == 1)
s2d1 <- day1 %>%
  filter(Species == 2)
s3d1 <- day1 %>%
  filter(Species == 3)
s4d1 <- day1 %>%
  filter(Species == 4)
s5d1 <- day1 %>%
  filter(Species == 5)
s6d1 <- day1 %>%
  filter(Species == 6)
s7d1 <- day1 %>%
  filter(Species == 7)
s8d1 <- day1 %>%
  filter(Species == 8)
s9d1 <- day1 %>%
  filter(Species == 9)


#ANOVA for s1
MLCday1.leaftypes1 <- lm(percent_MassLost_C ~ leaftype, s1d1)
anova(MLCday1.leaftypes1)
#not significant
#ANOVA for s2
MLCday1.leaftypes2 <- lm(percent_MassLost_C ~ leaftype, s2d1)
anova(MLCday1.leaftypes2)
# significant!!!
#ANOVA for s3
MLC.leaftypes3 <- lm(percent_MassLost_C ~ leaftype, s3d1)
anova(MLC.leaftypes3)
#Not significant!!
#ANOVA for s4
MLC.leaftypes4 <- lm(percent_MassLost_C ~ leaftype, s4d1)
anova(MLC.leaftypes4)
#Not significant!!
#ANOVA for s5
MLC.leaftypes5 <- lm(percent_MassLost_C ~ leaftype, s5d1)
anova(MLC.leaftypes5)
#Significant!!
#ANOVA for s6
MLC.leaftypes6 <- lm(percent_MassLost_C ~ leaftype, s6d1)
anova(MLC.leaftypes6)
#Not significant!!
#ANOVA for s7
MLC.leaftypes7 <- lm(percent_MassLost_C ~ leaftype, s7d1)
anova(MLC.leaftypes7)
#NOT significant!!
#ANOVA for s8
MLC.leaftypes8 <- lm(percent_MassLost_C ~ leaftype, s8d1)
anova(MLC.leaftypes8)
#Not Significant!!
#ANOVA for s9
MLC.leaftypes9 <- lm(percent_MassLost_C ~ leaftype, s9d1)
anova(MLC.leaftypes9)
#Significant!!





#filter by day 3
day3 <- leachate.data %>%
  filter(Days_Leached == 3)

###make a boxplot for percent mass lost due to C after 3 days
ggplot(day3, aes(x=leaftype, y=percent_MassLost_C)) + geom_boxplot() +labs(x= "Leaf Type", y= "% Mass Lost that is C after 3 days")
##ANOVA for day 3
MassLossC.Day3 <- lm(percent_MassLost_C ~ leaftype, day3)
anova(MassLossC.Day3)
##notsignificant

###make a boxplot for day 3 green vs brown leaves
#facet_wrap groups by species and leaf type 
ggplot(day3, aes(x=Species, y=percent_MassLost_C, fill=Leaf.Stage)) + geom_boxplot() + facet_wrap(~Leaf.Stage) + theme_cowplot() +labs(x= "Species", y= "% Mass Loss that is C") + scale_fill_manual(values=c('seagreen','saddlebrown')) + theme(legend.position="none")
#facet_wrap groups by species 
ggplot(day3, aes(x=leaftype, y=percent_MassLost_C)) + geom_boxplot() + facet_wrap(~Species) + theme_cowplot() +labs(x= "Leaf Type", y= "% Mass Lost that is C after 3 days")


##ANOVA for species 
##need to filter by species by DAY3
s1d3 <- day3 %>%
  filter(Species == 1)
s2d3 <- day3 %>%
  filter(Species == 2)
s3d3 <- day3 %>%
  filter(Species == 3)
s4d3 <- day3 %>%
  filter(Species == 4)
s5d3 <- day3 %>%
  filter(Species == 5)
s6d3 <- day3 %>%
  filter(Species == 6)
s7d3 <- day3 %>%
  filter(Species == 7)
s8d3 <- day3 %>%
  filter(Species == 8)
s9d3 <- day3 %>%
  filter(Species == 9)

#ANOVA for s1
MLCday1.leaftypes1 <- lm(percent_MassLost_C ~ leaftype, s1d3)
anova(MLCday1.leaftypes1)
#Significant!
#ANOVA for s2
MLCday1.leaftypes2 <- lm(percent_MassLost_C ~ leaftype, s2d3)
anova(MLCday1.leaftypes2)
#significant!!!
#ANOVA for s3
MLC.leaftypes3 <- lm(percent_MassLost_C ~ leaftype, s3d3)
anova(MLC.leaftypes3)
#Significant!!
#ANOVA for s4
MLC.leaftypes4 <- lm(percent_MassLost_C ~ leaftype, s4d3)
anova(MLC.leaftypes4)
#Significant!!
#ANOVA for s5
MLC.leaftypes5 <- lm(percent_MassLost_C ~ leaftype, s5d3)
anova(MLC.leaftypes5)
#Significant!!
#ANOVA for s6
MLC.leaftypes6 <- lm(percent_MassLost_C ~ leaftype, s6d3)
anova(MLC.leaftypes6)
#Not significant!!
#ANOVA for s7
MLC.leaftypes7 <- lm(percent_MassLost_C ~ leaftype, s7d3)
anova(MLC.leaftypes7)
#Significant!!
#ANOVA for s8
MLC.leaftypes8 <- lm(percent_MassLost_C ~ leaftype, s8d3)
anova(MLC.leaftypes8)
#Significant!!
#ANOVA for s9
MLC.leaftypes9 <- lm(percent_MassLost_C ~ leaftype, s9d3)
anova(MLC.leaftypes9)
#NOT Significant



#filter by day 7
day7 <- leachate.data %>%
  filter(Days_Leached == 7)

###make a boxplot for percent mass lost due to C after 7 days
ggplot(day7, aes(x=leaftype, y=percent_MassLost_C)) + geom_boxplot() +labs(x= "Leaf Type", y= "% Mass Lost that is C after 7 days")
##ANOVA for day 7
MassLossC.Day7 <- lm(percent_MassLost_C ~ leaftype, day7[day7$Species != 8,])
anova(MassLossC.Day7)
#Not Signifciant


####Mass loss that is C is not significantly different for any day leached

###make a boxplot for day 7 green vs brown leaves
#facet_wrap groups by species and leaf type 
ggplot(day7, aes(x=Species, y=percent_MassLost_C, fill=Leaf.Stage)) + geom_boxplot() + facet_wrap(~Leaf.Stage) + theme_cowplot() +labs(x= "Species", y= "% Mass Loss that is C") + scale_fill_manual(values=c('seagreen','saddlebrown')) + theme(legend.position="none")
#facet_wrap groups by species 
ggplot(day7, aes(x=leaftype, y=percent_MassLost_C)) + geom_boxplot() + facet_wrap(~Species) + theme_cowplot() +labs(x= "Leaf Type", y= "% Mass Lost that is C after 7 days")


##ANOVA for species 
##need to filter by species by DAY7
s1d7 <- day7 %>%
  filter(Species == 1)
s2d7 <- day7 %>%
  filter(Species == 2)
s3d7 <- day7 %>%
  filter(Species == 3)
s4d7 <- day7 %>%
  filter(Species == 4)
s5d7 <- day7 %>%
  filter(Species == 5)
s6d7 <- day7 %>%
  filter(Species == 6)
s7d7 <- day7 %>%
  filter(Species == 7)
s8d7 <- day7 %>%
  filter(Species == 8)
s9d7 <- day7 %>%
  filter(Species == 9)

#ANOVA for s1
MLCday7.leaftypes1 <- lm(percent_MassLost_C ~ leaftype, s1d7)
anova(MLCday7.leaftypes1)
#Significant!
#ANOVA for s2
MLCday7.leaftypes2 <- lm(percent_MassLost_C ~ leaftype, s2d7)
anova(MLCday7.leaftypes2)
#Not significant
#ANOVA for s3
MLCday7.leaftypes3 <- lm(percent_MassLost_C ~ leaftype, s3d7)
anova(MLCday7.leaftypes3)
#Significant!!
#ANOVA for s4
MLCday7.leaftypes4 <- lm(percent_MassLost_C ~ leaftype, s4d7)
anova(MLCday7.leaftypes4)
#Not Significant
#ANOVA for s5
MLCday7.leaftypes5 <- lm(percent_MassLost_C ~ leaftype, s5d7)
anova(MLCday7.leaftypes5)
#Significant!!
#ANOVA for s6
MLCday7.leaftypes6 <- lm(percent_MassLost_C ~ leaftype, s6d7)
anova(MLCday7.leaftypes6)
#Not significant
#ANOVA for s7
MLCday7.leaftypes7 <- lm(percent_MassLost_C ~ leaftype, s7d7)
anova(MLC.leaftypes7)
#Significant!!
#######Exclude species 8 for day 7
#ANOVA for s8
MLCday7.leaftypes8 <- lm(percent_MassLost_C ~ leaftype, s8d7)
anova(MLCday7.leaftypes8)
#Significant!!
#ANOVA for s9
MLCday7.leaftypes9 <- lm(percent_MassLost_C ~ leaftype, s9d7)
anova(MLCday7.leaftypes9)
#Significant!



###LINE GRAPH FOR PERCENT MASS LOSS THAT IS C


# Load the required libraries
library(ggplot2)
library(tidyr)

# Check if columns are numeric or character and convert if necessary
leachate.data <- mutate_if(leachate.data, is.factor, as.character)


##Creating new variable column based on leaf type saying "if my leaf type is equal to brown then call my new variable senesced, if its not equal to brown, call it fresh"
leachate.data$Leaf.Stage= ifelse(leachate.data$leaftype=="brown","Senesced", "Fresh")

##THIS GRAPH##
##trying to facet by species
ggplot(leachate.data, aes(x = as.numeric(leachate.data$Days_Leached), y = percent_MassLost_C, color=Leaf.Stage)) +
  geom_point() + 
  scale_color_manual(values=c('seagreen','saddlebrown')) +
  guides(color = guide_legend(title = "Leaf Type")) +
  geom_smooth(method= "lm", se=FALSE) + 
  scale_x_continuous(breaks= c(0, 1, 3, 7)) +
  theme_bw() +
  labs(x= "Days Leached", y= "% Mass Lost that is C") +
  facet_wrap(~Species) 
  

####Linear regression stats

##checking if Days Leached is a number or character
str(leachate.data$Days_Leached)

##chnaging Days Lreached to be numeric
leachate.data$Days_Leached <- as.numeric(leachate.data$Days_Leached)
str(leachate.data$Days_Leached)

##need to filter by species and leaf type
s1 <- leachate.data %>%
  filter(Species == 1)
s2 <- leachate.data %>%
  filter(Species == 2)
s3 <- leachate.data %>%
  filter(Species == 3)
s4 <- leachate.data %>%
  filter(Species == 4)
s5 <- leachate.data %>%
  filter(Species == 5)
s6 <- leachate.data %>%
  filter(Species == 6)
s7 <- leachate.data %>%
  filter(Species == 7)
s8 <- leachate.data %>%
  filter(Species == 8)
s9 <- leachate.data %>%
  filter(Species == 9)

#filtering each species brown only
s1B <- s1 %>%
  filter(leaftype == "brown")
s2B <- s2 %>%
  filter(leaftype == "brown")
s3B <- s3 %>%
  filter(leaftype == "brown")
s4B <- s4 %>%
  filter(leaftype == "brown")
s5B <- s5 %>%
  filter(leaftype == "brown")
s6B <- s6 %>%
  filter(leaftype == "brown")
s7B <- s7 %>%
  filter(leaftype == "brown")
s8B <- s8 %>%
  filter(leaftype == "brown")
s9B <- s9 %>%
  filter(leaftype == "brown")

#filtering each species green only
s1G <- s1 %>%
  filter(leaftype == "green")
s2G <- s2 %>%
  filter(leaftype == "green")
s3G <- s3 %>%
  filter(leaftype == "green")
s4G <- s4 %>%
  filter(leaftype == "green")
s5G <- s5 %>%
  filter(leaftype == "green")
s6G <- s6 %>%
  filter(leaftype == "green")
s7G <- s7 %>%
  filter(leaftype == "green")
s8G <- s8 %>%
  filter(leaftype == "green")
s9G <- s9 %>%
  filter(leaftype == "green")




##linear regression for species 1 Brown
lm_s1B_MLC = lm(formula = percent_MassLost_C ~ Days_Leached, data =s1B)
summary(lm_s1B_MLC)
##linear regression for species 1 Green
lm_s1G_MLC = lm(formula = percent_MassLost_C ~ Days_Leached, data =s1G)
summary(lm_s1G_MLC)

##linear regression for species 2 Brown
lm_s2B_MLC = lm(formula = percent_MassLost_C ~ Days_Leached, data =s2B)
summary(lm_s2B_MLC)
##linear regression for species 2 Green
lm_s2G_MLC = lm(formula = percent_MassLost_C ~ Days_Leached, data =s2G)
summary(lm_s2G_MLC)

##linear regression for species 3 Brown
lm_s3B_MLC = lm(formula = percent_MassLost_C ~ Days_Leached, data =s3B)
summary(lm_s3B_MLC)
##linear regression for species 3 Green
lm_s3G_MLC = lm(formula = percent_MassLost_C ~ Days_Leached, data =s3G)
summary(lm_s3G_MLC)

##linear regression for species 4 Brown
lm_s4B_MLC = lm(formula = percent_MassLost_C ~ Days_Leached, data =s4B)
summary(lm_s4B_MLC)
##linear regression for species 4 Green
lm_s4G_MLC = lm(formula = percent_MassLost_C ~ Days_Leached, data =s4G)
summary(lm_s4G_MLC)

##linear regression for species 5 Brown
lm_s5B_MLC = lm(formula = percent_MassLost_C ~ Days_Leached, data =s5B)
summary(lm_s5B_MLC)
##linear regression for species 5 Green
lm_s5G_MLC = lm(formula = percent_MassLost_C ~ Days_Leached, data =s5G)
summary(lm_s5G_MLC)

##linear regression for species 6 Brown
lm_s6B_MLC = lm(formula = percent_MassLost_C ~ Days_Leached, data =s6B)
summary(lm_s6B_MLC)
##linear regression for species 6 Green
lm_s6G_MLC = lm(formula = percent_MassLost_C ~ Days_Leached, data =s6G)
summary(lm_s6G_MLC)

##linear regression for species 7 Brown
lm_s7B_MLC = lm(formula = percent_MassLost_C ~ Days_Leached, data =s7B)
summary(lm_s7B_MLC)
##linear regression for species 7 Green
lm_s7G_MLC = lm(formula = percent_MassLost_C ~ Days_Leached, data =s7G)
summary(lm_s7G_MLC)

##linear regression for species 8 Brown
lm_s8B_MLC = lm(formula = percent_MassLost_C ~ Days_Leached, data =s8B)
summary(lm_s8B_MLC)
##linear regression for species 8 Green
lm_s8G_MLC = lm(formula = percent_MassLost_C ~ Days_Leached, data =s8G)
summary(lm_s8G_MLC)

##linear regression for species 9 Brown
lm_s9B_MLC = lm(formula = percent_MassLost_C ~ Days_Leached, data =s9B)
summary(lm_s9B_MLC)
##linear regression for species 9 Green
lm_s9G_MLC = lm(formula = percent_MassLost_C ~ Days_Leached, data =s9G)
summary(lm_s9G_MLC)





####################NEED HELP
##ANCOVA stats???

#need to verify that the covariate and the treatment are independent, so run an ANOVA 
#fit anova model
anova_model <- aov(percent_MassLost_C ~ leaftype, data = leachate.data[!(leachate.data$Species == 8 & leachate.data$Days_Leached == 7),])
#view summary of anova model
summary(anova_model)

#need to verify that there is homogeneity of variance among the groups, we conduct Levene’s Test
#load car library to conduct Levene's Test
library(car)
#conduct Levene's Test
leveneTest(percent_MassLost_C~leaftype, data = leachate.data[!(leachate.data$Species == 8 & leachate.data$Days_Leached == 7),])


#fit ANCOVA model
ancova_model <- lm(percent_MassLost_C ~ Days_Leached + leaftype, data = leachate.data[!(leachate.data$Species == 8 & leachate.data$Days_Leached == 7),])

#view summary of model
Anova(ancova_model, type="II")



#load the multcomp library
library(multcomp)
# Assuming 'leaftype' is a factor variable in your dataset
# Convert 'leaftype' to a factor if it's not already
leachate.data$leaftype <- as.factor(leachate.data$leaftype)



##do i even need post hocs?
#define the post hoc comparisons to make
postHocs <- glht(ancova_model, linfct = mcp(leaftype = "Tukey"))

#view a summary of the post hoc comparisons
summary(postHocs)


##By species?
##SPECIES 2
#fit ANCOVA model
ancova_modelMLCS2 <- lm(percent_MassLost_C ~ leaftype + Days_Leached + Species, data = s2)

#view summary of model
Anova(ancova_modelMLCS2, type="II")

#load the multcomp library
library(multcomp)
# Assuming 'leaftype' is a factor variable in your dataset
# Convert 'leaftype' to a factor if it's not already
leachate.data$leaftype <- factor(leachate.data$leaftype)

#fit ANCOVA model
ancova_modelMLCS2 <- aov(percent_MassLost_C ~ leaftype + Days_Leached, data = s2)

#view summary of model
Anova(ancova_modelMLCS2, type="II")
#define the post hoc comparisons to make
postHocsMLCS2 <- glht(ancova_modelMLCS2, linfct = mcp(leaftype = "Tukey"))

#view a summary of the post hoc comparisons
summary(postHocsMLCS2)

###need to find stats r squared for each species
    ##run a regression model to get r squared and p value for each line to look at the difference of slopes for each species



####hard to have a curve with only three points-- not enough data
      ##maybe with the median?

###brown versus green two way anova??
      #days leached 
  








#########C:N (using TDN) #############

##calculating TDN mol with detection limits 
leachate.data$TDN.dl_mol = leachate.data$TDN.dl/14

##renaming old CN column
leachate.data <- leachate.data %>% 
  rename("CN_old" = "CNratio")

##cacluating C:N with correct TDN value
leachate.data$CNratio = leachate.data$DOC..mol./leachate.data$TDN.dl_mol


###make a boxplot for C:N ratio
ggplot(leachate.data, aes(x=leaftype, y=CNratio)) + geom_boxplot()

##check structure of the relationship/variable
str(leachate.data$CNratio)
##came out as characters and we want integer
#converting character to integer
leachate.data$CNratio <- as.numeric(leachate.data$CNratio)
str(leachate.data$CNratio)

#converting interger to chracter
leachate.data$Days_Leached <- as.factor(leachate.data$Days_Leached)
str(leachate.data$Days_Leached)

##check structure of the relationship/variable
str(leachate.data$Species)
##came out as int and we want character
#converting interger to chracter
leachate.data$Species <- as.factor(leachate.data$Species)
str(leachate.data$Species)

##try again
###make a boxplot for C:N ratio
ggplot(leachate.data, aes(x=leaftype, y=CNratio)) + geom_boxplot()+labs(x= "Leaf Type", y= "C:N ratio")

##ANOVA for green leaves
CNBvG <- lm(CNratio ~ leaftype, leachate.data[!(leachate.data$Species == 8 & leachate.data$Days_Leached == 7),])
anova(CNBvG)
#Significant

#facet_wrap groups by leaves 
ggplot(leachate.data, aes(x=Days_Leached, y=CNratio)) + geom_boxplot() + facet_wrap(~leaftype) + theme_cowplot() +labs(x= "Days Leached", y= "C:N ratio")

##ANOVA for green leaves
#excluding species 8
CN.DaysG <- lm(CNratio ~ Days_Leached, green[!(green$Species == 8 & green$Days_Leached == 7),])
anova(CN.DaysG)
##Not Significant

##ANOVA for brown leaves
#excluding species 8
CN.DaysB <- lm(CNratio ~ Days_Leached, brown[!(brown$Species == 8 & brown$Days_Leached == 7),])
anova(CN.DaysB)
##not significant 


###make a boxplot
#facet_wrap groups by species and leaf type 
ggplot(leachate.data, aes(x=Species, y=CNratio, fill=Leaf.Stage)) + geom_boxplot(outlier.shape = NA) + facet_wrap(~Leaf.Stage) + theme_cowplot() +labs(x= "Species", y= "C:N ratio") + scale_fill_manual(values=c('seagreen','saddlebrown')) + geom_point(aes(shape=Days_Leached), position = position_dodge(width=0.5), alpha = 0.3, size=3) + guides(shape = guide_legend(title = "Days Leached")) + guides(fill = guide_legend(title = "Leaf Stage")) 

#not facet wrapped
ggplot(leachate.data, aes(x = Species, y = CNratio, fill = Leaf.Stage)) +
  geom_boxplot(outlier.shape = NA,  width = 0.75) + 
  theme_cowplot() +labs(x= "Species", y= "C:N ratio") + scale_fill_manual(values=c('seagreen','saddlebrown')) +
  guides(fill = guide_legend(title = "Leaf Stage")) +
  geom_point(aes(shape=Days_Leached, group = Leaf.Stage), position = position_dodge(1), alpha = 0.4, size=2) + 
  guides(shape = guide_legend(title = "Days Leached")) 

###make a boxplot
#facet_wrap groups by species and leaf type 
ggplot(leachate.data, aes(x=Species, y=CNratio)) + geom_boxplot() + facet_wrap(~leaftype) + theme_cowplot() +labs(x= "Species", y= "C:N Ratio")

###make a boxplot for green vs brown leaves
#facet_wrap groups by species 
ggplot(leachate.data, aes(x=leaftype, y=CNratio)) + geom_boxplot() + facet_wrap(~Species) + theme_cowplot() +labs(x= "Leaf Type", y= "C:N Ratio")



##ANOVA for species for all days
##need to filter by species
s1 <- leachate.data %>%
  filter(Species == 1)
s2 <- leachate.data %>%
  filter(Species == 2)
s3 <- leachate.data %>%
  filter(Species == 3)
s4 <- leachate.data %>%
  filter(Species == 4)
s5 <- leachate.data %>%
  filter(Species == 5)
s6 <- leachate.data %>%
  filter(Species == 6)
s7 <- leachate.data %>%
  filter(Species == 7)
s8 <- leachate.data %>%
  filter(Species == 8)
s9 <- leachate.data %>%
  filter(Species == 9)



#ANOVA for s1
CN.leaftypes1 <- lm(CNratio ~ leaftype, s1)
anova(CN.leaftypes1)
#not significant
#ANOVA for s2
CN.leaftypes2 <- lm(CNratio ~ leaftype, s2)
anova(CN.leaftypes2)
# significant!!!
#ANOVA for s3
CN.leaftypes3 <- lm(CNratio ~ leaftype, s3)
anova(CN.leaftypes3)
#significant !!
#ANOVA for s4
CN.leaftypes4 <- lm(CNratio ~ leaftype, s4)
anova(CN.leaftypes4)
#Not significant
#ANOVA for s5
CN.leaftypes5 <- lm(CNratio ~ leaftype, s5)
anova(CN.leaftypes5)
#Not significant
#ANOVA for s6
CN.leaftypes6 <- lm(CNratio ~ leaftype, s6)
anova(CN.leaftypes6)
#Significant
#ANOVA for s7
CN.leaftypes7 <- lm(CNratio ~ leaftype, s7)
anova(CN.leaftypes7)
#Significant!!
#######excluding species8
#ANOVA for s8
CN.leaftypes8 <- lm(CNratio ~ leaftype, s8)
anova(CN.leaftypes8)
#Not Significant
#ANOVA for s9
CN.leaftypes9 <- lm(CNratio ~ leaftype, s9)
anova(CN.leaftypes9)
#Not Significant




#############################Do each individual day leached separate to look for differences in time

####C:N ratio-----Filter by day 1
day1 <- leachate.data %>%
  filter(Days_Leached == 1)

###make a boxplot for percent mass lost due to C after 1 day
ggplot(day1, aes(x=leaftype, y=CNratio)) + geom_boxplot() +labs(x= "Leaf Type", y= "C:N after 1 day")
##ANOVA for day 1
CNratio.Day1 <- lm(CNratio ~ leaftype, day1)
anova(CNratio.Day1)
##notsignificant

###make a boxplot for day 1 green vs brown leaves
#facet_wrap groups by species and leaf type 
ggplot(day1, aes(x=Species, y=CNratio, fill=Leaf.Stage)) + geom_boxplot() + facet_wrap(~Leaf.Stage) + theme_cowplot() +labs(x= "Species", y= "C:N ratio") + scale_fill_manual(values=c('seagreen','saddlebrown')) + theme(legend.position="none")

#facet_wrap groups by species 
ggplot(day1, aes(x=leaftype, y=CNratio)) + geom_boxplot() + facet_wrap(~Species) + theme_cowplot() +labs(x= "Leaf Type", y= "C:N after 1 day")


######################NEED ANOVA FOR EACH SPECIES FOR ALL DAYS


##ANOVA for species --- After 1 day
##need to filter by species by DAY1
s1d1 <- day1 %>%
  filter(Species == 1)
s2d1 <- day1 %>%
  filter(Species == 2)
s3d1 <- day1 %>%
  filter(Species == 3)
s4d1 <- day1 %>%
  filter(Species == 4)
s5d1 <- day1 %>%
  filter(Species == 5)
s6d1 <- day1 %>%
  filter(Species == 6)
s7d1 <- day1 %>%
  filter(Species == 7)
s8d1 <- day1 %>%
  filter(Species == 8)
s9d1 <- day1 %>%
  filter(Species == 9)


#ANOVA for s1
CNday1.leaftypes1 <- lm(CNratio ~ leaftype, s1d1)
anova(CNday1.leaftypes1)
#significant
#ANOVA for s2
CNday1.leaftypes2 <- lm(CNratio ~ leaftype, s2d1)
anova(CNday1.leaftypes2)
# significant!!!
#ANOVA for s3
CNday1.leaftypes3 <- lm(CNratio ~ leaftype, s3d1)
anova(CNday1.leaftypes3)
#Not significant
#ANOVA for s4
CNday1.leaftypes4 <- lm(CNratio ~ leaftype, s4d1)
anova(CNday1.leaftypes4)
#Not significant
#ANOVA for s5
CNday1.leaftypes5 <- lm(CNratio ~ leaftype, s5d1)
anova(CNday1.leaftypes5)
#Not significant
#ANOVA for s6
CNday1.leaftypes6 <- lm(CNratio ~ leaftype, s6d1)
anova(CNday1.leaftypes6)
#Significant
#ANOVA for s7
CNday1.leaftypes7 <- lm(CNratio ~ leaftype, s7d1)
anova(CNday1.leaftypes7)
#Significant!!
#ANOVA for s8
CNdy1.leaftypes8 <- lm(CNratio ~ leaftype, s8d1)
anova(CNdy1.leaftypes8)
#Not Significant
#ANOVA for s9
CNday1.leaftypes9 <- lm(CNratio ~ leaftype, s9d1)
anova(CNday1.leaftypes9)
#Not Significant



####C:N ratio-----Filter by day 3
day3 <- leachate.data %>%
  filter(Days_Leached == 3)

###make a boxplot for percent mass lost due to C after 3 days
ggplot(day3, aes(x=leaftype, y=CNratio)) + geom_boxplot() +labs(x= "Leaf Type", y= "C:N after 3 days")
##ANOVA for day 3
CNratio.Day3 <- lm(CNratio ~ leaftype, day3)
anova(CNratio.Day3)
##notsignificant

###make a boxplot for day 3 green vs brown leaves
#facet_wrap groups by species and leaf type 
ggplot(day3, aes(x=Species, y=CNratio, fill=Leaf.Stage)) + geom_boxplot() + facet_wrap(~Leaf.Stage) + theme_cowplot() +labs(x= "Species", y= "C:N ratio") + scale_fill_manual(values=c('seagreen','saddlebrown')) + theme(legend.position="none")

#facet_wrap groups by species 
ggplot(day3, aes(x=leaftype, y=CNratio)) + geom_boxplot() + facet_wrap(~Species) + theme_cowplot() +labs(x= "Leaf Type", y= "C:N after 3 days")


##ANOVA for each species
##need to filter by species by DAY3
s1d3 <- day3 %>%
  filter(Species == 1)
s2d3 <- day3 %>%
  filter(Species == 2)
s3d3 <- day3 %>%
  filter(Species == 3)
s4d3 <- day3 %>%
  filter(Species == 4)
s5d3 <- day3 %>%
  filter(Species == 5)
s6d3 <- day3 %>%
  filter(Species == 6)
s7d3 <- day3 %>%
  filter(Species == 7)
s8d3 <- day3 %>%
  filter(Species == 8)
s9d3 <- day3 %>%
  filter(Species == 9)


#ANOVA for s1
CNday3.leaftypes1 <- lm(CNratio ~ leaftype, s1d3)
anova(CNday3.leaftypes1)
#not significant
#ANOVA for s2
CNday3.leaftypes2 <- lm(CNratio ~ leaftype, s2d3)
anova(CNday3.leaftypes2)
# significant!!!
#ANOVA for s3
CNday3.leaftypes3 <- lm(CNratio ~ leaftype, s3d3)
anova(CNday3.leaftypes3)
#Not significant
#ANOVA for s4
CNday3.leaftypes4 <- lm(CNratio ~ leaftype, s4d3)
anova(CNday3.leaftypes4)
#Not significant
#ANOVA for s5
CNday3.leaftypes5 <- lm(CNratio ~ leaftype, s5d3)
anova(CNday3.leaftypes5)
#Not significant
#ANOVA for s6
CNday3.leaftypes6 <- lm(CNratio ~ leaftype, s6d3)
anova(CNday3.leaftypes6)
#Significant
#ANOVA for s7
CNday3.leaftypes7 <- lm(CNratio ~ leaftype, s7d3)
anova(CNday3.leaftypes7)
#Significant!!
#ANOVA for s8
CNday3.leaftypes8 <- lm(CNratio ~ leaftype, s8d3)
anova(CNday3.leaftypes8)
#Not Significant
#ANOVA for s9
CNday3.leaftypes9 <- lm(CNratio ~ leaftype, s9d3)
anova(CNday3.leaftypes9)
#Not Significant





####C:N ratio-----Filter by day 7
day7 <- leachate.data %>%
  filter(Days_Leached == 7)

###make a boxplot for percent mass lost due to C after 7 days
ggplot(day7, aes(x=leaftype, y=CNratio)) + geom_boxplot() +labs(x= "Leaf Type", y= "C:N after 7 days")
##ANOVA for day 7
CNratio.Day7 <- lm(CNratio ~ leaftype, day7[day7$Species !=8,])
anova(CNratio.Day7)
##not significant!!!

#####C:N ratio is not significant for any days

###make a boxplot for day 7 green vs brown leaves
#facet_wrap groups by species and leaf type 
ggplot(day7, aes(x=Species, y=CNratio, fill=Leaf.Stage)) + geom_boxplot() + facet_wrap(~Leaf.Stage) + theme_cowplot() +labs(x= "Species", y= "C:N ratio") + scale_fill_manual(values=c('seagreen','saddlebrown')) + theme(legend.position="none")

#facet_wrap groups by species 
ggplot(day7, aes(x=leaftype, y=CNratio)) + geom_boxplot() + facet_wrap(~Species) + theme_cowplot() +labs(x= "Leaf Type", y= "C:N after 7 days")



##ANOVA for each species 
##need to filter by species by DAY7
s1d7 <- day7 %>%
  filter(Species == 1)
s2d7 <- day7 %>%
  filter(Species == 2)
s3d7 <- day7 %>%
  filter(Species == 3)
s4d7 <- day7 %>%
  filter(Species == 4)
s5d7 <- day7 %>%
  filter(Species == 5)
s6d7 <- day7 %>%
  filter(Species == 6)
s7d7 <- day7 %>%
  filter(Species == 7)
s8d7 <- day7 %>%
  filter(Species == 8)
s9d7 <- day7 %>%
  filter(Species == 9)

#ANOVA for s1
CNday7.leaftypes1 <- lm(CNratio ~ leaftype, s1d7)
anova(CNday7.leaftypes1)
#significant!!
#ANOVA for s2
CNday7.leaftypes2 <- lm(CNratio ~ leaftype, s2d7)
anova(CNday7.leaftypes2)
#NOT significant
#ANOVA for s3
CNday7.leaftypes3 <- lm(CNratio ~ leaftype, s3d7)
anova(CNday7.leaftypes3)
#Significant!!
#ANOVA for s4
CNday7.leaftypes4 <- lm(CNratio ~ leaftype, s4d7)
anova(CNday7.leaftypes4)
#not Significant!!
#ANOVA for s5
CNday7.leaftypes5 <- lm(CNratio ~ leaftype, s5d7)
anova(CNday7.leaftypes5)
#significant!!
#ANOVA for s6
CNday7.leaftypes6 <- lm(CNratio ~ leaftype, s6d7)
anova(CNday7.leaftypes6)
#NOT Significant
#ANOVA for s7
CNday7.leaftypes7 <- lm(CNratio ~ leaftype, s7d7)
anova(CNday7.leaftypes7)
#Significant!!
##exclude species 8 for day 7
#ANOVA for s8
CNday7.leaftypes8 <- lm(CNratio ~ leaftype, s8d7)
anova(CNday7.leaftypes8)
#Not Significant
#ANOVA for s9
CNday7.leaftypes9 <- lm(CNratio ~ leaftype, s9d7)
anova(CNday7.leaftypes9)
#Not Significant




###LINE GRAPH FOR C:N


# Load the required libraries
library(ggplot2)
library(tidyr)

# Check if columns are numeric or character and convert if necessary
leachate.data <- mutate_if(leachate.data, is.factor, as.character)

# Reshape the data from wide to long format
leachate.data_long <- gather(leachate.data, key = "LeafType", value = "Value", percent_MassLost_C:Percent_MassLost_N)

# Convert the 'Value' column to numeric
leachate.data_long$Value <- as.numeric(leachate.data_long$Value)

# Plot the lines using ggplot with the reshaped data
ggplot(leachate.data, aes(x = as.numeric(leachate.data$Days_Leached), y = CNratio, color = as.factor(leachate.data$Species))) +
  geom_point()


##Creating new variable column based on leaf type saying "if my leaf type is equal to brown then call my new variable senesced, if its not equal to brown, call it fresh"
leachate.data$Leaf.Stage= ifelse(leachate.data$leaftype=="brown","Senesced", "Fresh")

##THIS GRAPH##
##trying to facet by species
ggplot(leachate.data, aes(x = as.numeric(leachate.data$Days_Leached), y = CNratio, color=Leaf.Stage)) +
  geom_point() + 
  scale_color_manual(values=c('seagreen','saddlebrown')) +
  guides(color = guide_legend(title = "Leaf Type")) +
  geom_smooth(method= "lm", se=FALSE) + 
  scale_x_continuous(breaks=c(0,1,3,7)) +
  theme_bw() +
  labs(x= "Days Leached", y= "C:N") +
  facet_wrap(~Species) 


######Linear regression stats

##checking if Days Leached is a number or character
str(leachate.data$Days_Leached)

##chnaging Days Lreached to be numeric
leachate.data$Days_Leached <- as.numeric(leachate.data$Days_Leached)
str(leachate.data$Days_Leached)

##need to filter by species and leaf type
s1 <- leachate.data %>%
  filter(Species == 1)
s2 <- leachate.data %>%
  filter(Species == 2)
s3 <- leachate.data %>%
  filter(Species == 3)
s4 <- leachate.data %>%
  filter(Species == 4)
s5 <- leachate.data %>%
  filter(Species == 5)
s6 <- leachate.data %>%
  filter(Species == 6)
s7 <- leachate.data %>%
  filter(Species == 7)
s8 <- leachate.data %>%
  filter(Species == 8)
s9 <- leachate.data %>%
  filter(Species == 9)

#filtering each species brown only
s1B <- s1 %>%
  filter(leaftype == "brown")
s2B <- s2 %>%
  filter(leaftype == "brown")
s3B <- s3 %>%
  filter(leaftype == "brown")
s4B <- s4 %>%
  filter(leaftype == "brown")
s5B <- s5 %>%
  filter(leaftype == "brown")
s6B <- s6 %>%
  filter(leaftype == "brown")
s7B <- s7 %>%
  filter(leaftype == "brown")
s8B <- s8 %>%
  filter(leaftype == "brown")
s9B <- s9 %>%
  filter(leaftype == "brown")

#filtering each species green only
s1G <- s1 %>%
  filter(leaftype == "green")
s2G <- s2 %>%
  filter(leaftype == "green")
s3G <- s3 %>%
  filter(leaftype == "green")
s4G <- s4 %>%
  filter(leaftype == "green")
s5G <- s5 %>%
  filter(leaftype == "green")
s6G <- s6 %>%
  filter(leaftype == "green")
s7G <- s7 %>%
  filter(leaftype == "green")
s8G <- s8 %>%
  filter(leaftype == "green")
s9G <- s9 %>%
  filter(leaftype == "green")




##linear regression for species 1 Brown
lm_s1B_CN = lm(formula = CNratio ~ Days_Leached, data =s1B)
summary(lm_s1B_CN)
##linear regression for species 1 Green
lm_s1G_CN = lm(formula = CNratio ~ Days_Leached, data =s1G)
summary(lm_s1G_CN)

##linear regression for species 2 Brown
lm_s2B_CN = lm(formula = CNratio ~ Days_Leached, data =s2B)
summary(lm_s2B_CN)
##linear regression for species 2 Green
lm_s2G_CN = lm(formula = CNratio ~ Days_Leached, data =s2G)
summary(lm_s2G_CN)

##linear regression for species 3 Brown
lm_s3B_CN = lm(formula = CNratio ~ Days_Leached, data =s3B)
summary(lm_s3B_CN)
##linear regression for species 3 Green
lm_s3G_CN = lm(formula = CNratio ~ Days_Leached, data =s3G)
summary(lm_s3G_CN)

##linear regression for species 4 Brown
lm_s4B_CN = lm(formula = CNratio ~ Days_Leached, data =s4B)
summary(lm_s4B_CN)
##linear regression for species 4 Green
lm_s4G_CN = lm(formula = CNratio ~ Days_Leached, data =s4G)
summary(lm_s4G_CN)

##linear regression for species 5 Brown
lm_s5B_CN = lm(formula = CNratio ~ Days_Leached, data =s5B)
summary(lm_s5B_CN)
##linear regression for species 5 Green
lm_s5G_CN = lm(formula = CNratio ~ Days_Leached, data =s5G)
summary(lm_s5G_CN)

##linear regression for species 6 Brown
lm_s6B_CN = lm(formula = CNratio ~ Days_Leached, data =s6B)
summary(lm_s6B_CN)
##linear regression for species 6 Green
lm_s6G_CN = lm(formula = CNratio ~ Days_Leached, data =s6G)
summary(lm_s6G_CN)

##linear regression for species 7 Brown
lm_s7B_CN = lm(formula = CNratio ~ Days_Leached, data =s7B)
summary(lm_s7B_CN)
##linear regression for species 7 Green
lm_s7G_CN = lm(formula = CNratio ~ Days_Leached, data =s7G)
summary(lm_s7G_CN)

##linear regression for species 8 Brown
lm_s8B_CN = lm(formula = CNratio ~ Days_Leached, data =s8B)
summary(lm_s8B_CN)
##linear regression for species 8 Green
lm_s8G_CN = lm(formula = CNratio ~ Days_Leached, data =s8G)
summary(lm_s8G_CN)

##linear regression for species 9 Brown
lm_s9B_CN = lm(formula = CNratio ~ Days_Leached, data =s9B)
summary(lm_s9B_CN)
##linear regression for species 9 Green
lm_s9G_CN = lm(formula = CNratio ~ Days_Leached, data =s9G)
summary(lm_s9G_CN)



####################NEED HELP
##ANCOVA stats???

#need to verify that the covariate and the treatment are independent, so run an ANOVA 
#fit anova model
anova_modelCN <- aov(CNratio ~ leaftype, data = leachate.data[!(leachate.data$Species == 8 & leachate.data$Days_Leached == 7),])
#view summary of anova model
summary(anova_modelCN)

#need to verify that there is homogeneity of variance among the groups, we conduct Levene’s Test
#load car library to conduct Levene's Test
library(car)
#conduct Levene's Test
leveneTest(CNratio ~leaftype, data = leachate.data[!(leachate.data$Species == 8 & leachate.data$Days_Leached == 7),])


#fit ANCOVA model
ancova_modelCN <- lm(CNratio ~ Days_Leached + leaftype, data = leachate.data[!(leachate.data$Species == 8 & leachate.data$Days_Leached == 7),])

#view summary of model
Anova(ancova_modelCN, type="II")



#load the multcomp library
library(multcomp)
# Assuming 'leaftype' is a factor variable in your dataset
# Convert 'leaftype' to a factor if it's not already
leachate.data$leaftype <- as.factor(leachate.data$leaftype)


## look up change legend tite ggplot
###need to find stats squared for each species
##run a regression model to get r squared and p value for each line to look at the difference of slopes for     each species



####hard to have a curve with only three points-- not enough data
##maybe with the median?

###brown versus green two way anova
#days leached 








####DON####


##making a boxplot for all species and leaf types
ggplot(leachate.data, aes(x = Species, y = DON_percentage, fill = Leaf.Stage)) +
  geom_boxplot(outlier.shape = NA,  width = 0.75) + 
  theme_cowplot() +labs(x= "Species", y= "DON/TDN percentage") + scale_fill_manual(values=c('seagreen','saddlebrown')) +
  guides(fill = guide_legend(title = "Leaf Stage")) +
  geom_point(aes(shape=Days_Leached, group = Leaf.Stage), position = position_dodge(1), alpha = 0.4, size=2) + 
  guides(shape = guide_legend(title = "Days Leached")) 

########DOC######

###make a boxplot for DOC
ggplot(leachate.data, aes(x=Leaf.Stage, y=NPOC_mgC_g, fill=Leaf.Stage)) + geom_boxplot() + theme_cowplot() +labs(x= "Leaf Type", y= "DOC mg C/g leaf") + scale_fill_manual(values=c('seagreen','saddlebrown'))

##ANOVA for green leaves
DOCBvG <- lm(NPOC_mgC_g ~ leaftype, leachate.data[!(leachate.data$Species == 8 & leachate.data$Days_Leached == 7),])
anova(DOCBvG)
#Significant!

###make a boxplot for DOC
#facet_wrap groups by leaves 
ggplot(leachate.data, aes(x=Days_Leached, y=NPOC_mgC_g)) + geom_boxplot() + facet_wrap(~leaftype) + theme_cowplot() +labs(x= "Days Leached", y= "DOC mg C/g leaf")

##ANOVA for green leaves
DOC.DaysLeachedsG <- lm(NPOC_mgC_g ~ Days_Leached, green[!(green$Species == 8 & green$Days_Leached == 7),])
anova(DOC.DaysLeachedsG)
#not significant

##ANOVA for brown leaves
DOC.DaysLeachedsB <- lm(NPOC_mgC_g ~ Days_Leached, brown[!(brown$Species == 8 & brown$Days_Leached == 7),])
anova(DOC.DaysLeachedsB)
##not significant

#convert integer to character
leachate.data$Species <- as.character(leachate.data$Species)
str(leachate.data$Species)


###make a boxplot
ggplot(leachate.data, aes(x = Species, y = NPOC_mgC_g, fill = Leaf.Stage)) +
  geom_boxplot(outlier.shape = NA,  width = 0.75) + 
  theme_cowplot() +labs(x= "Species", y= "Dissolved organic carbon (DOC) mg C/g leaf") + scale_fill_manual(values=c('seagreen','saddlebrown')) +
   guides(fill = guide_legend(title = "Leaf Stage")) +
  geom_point(aes(shape=Days_Leached, group = Leaf.Stage), position = position_dodge(1), alpha = 0.4, size=2) + 
  guides(shape = guide_legend(title = "Days Leached")) +
  theme(legend.text = element_text(size = 20),
        axis.text = element_text(size = 24),
        axis.title = element_text(size = 24),
        legend.title = element_text(size = 20)) ###text size for agu


#facet_wrap groups by species and Days Leached 
ggplot(leachate.data, aes(x=Species, y=NPOC_mgC_g, fill=Leaf.Stage)) + geom_boxplot(outlier.shape = NA) + facet_wrap(~Days_Leached) + theme_cowplot() +labs(x= "Species", y= "DOC mg C/g leaf") + scale_fill_manual(values=c('seagreen','saddlebrown')) + geom_point(aes(shape=Days_Leached), position = position_dodge(width=0.5), alpha = 0.3, size=2) + guides(shape = guide_legend(title = "Days Leached")) + guides(fill = guide_legend(title = "Leaf Stage")) 

#facet_wrap groups by species and Days Leached for just green leaves
ggplot(green, aes(x=Species, y=NPOC_mgC_g, fill=Days_Leached)) +
  geom_boxplot(outlier.shape = NA) +
  theme_cowplot() +labs(x= "Species", y= "DOC mg C/g leaf") + 
  scale_fill_manual(values=c('palegreen','mediumseagreen', 'darkgreen')) +
  geom_point(aes(shape=Days_Leached), position = position_dodge(width=0.5), alpha = 0.3, size=2) + 
  guides(shape = guide_legend(title = "Days Leached")) 

#facet_wrap groups by species and Days Leached for just brown leaves
ggplot(brown, aes(x=Species, y=NPOC_mgC_g, fill=Days_Leached)) +
  geom_boxplot(outlier.shape = NA) + 
  theme_cowplot() +labs(x= "Species", y= "DOC mg C/g leaf") + 
  scale_fill_manual(values=c('tan','peru', 'chocolate4')) +
  geom_point(aes(shape=Days_Leached), position = position_dodge(width=0.5), alpha = 0.3, size=2) + 
  guides(shape = guide_legend(title = "Days Leached")) 


#facet_wrap groups by species 
ggplot(leachate.data, aes(x=leaftype, y=NPOC_mgC_g)) + geom_boxplot() + facet_wrap(~Species) + theme_cowplot() +labs(x= "Leaf Type", y= "DOC mg C/g leaf")



##ANOVA for species 
##need to filter by species
s1 <- leachate.data %>%
  filter(Species == 1)
s2 <- leachate.data %>%
  filter(Species == 2)
s3 <- leachate.data %>%
  filter(Species == 3)
s4 <- leachate.data %>%
  filter(Species == 4)
s5 <- leachate.data %>%
  filter(Species == 5)
s6 <- leachate.data %>%
  filter(Species == 6)
s7 <- leachate.data %>%
  filter(Species == 7)
s8 <- leachate.data %>%
  filter(Species == 8)
s9 <- leachate.data %>%
  filter(Species == 9)


#ANOVA for s1
DOC.leaftypes1 <- lm(NPOC_mgC_g ~ leaftype, s1)
anova(DOC.leaftypes1)
#not significant
#ANOVA for s2
DOC.leaftypes2 <- lm(NPOC_mgC_g ~ leaftype, s2)
anova(DOC.leaftypes2)
#not significant
#ANOVA for s3
DOC.leaftypes3 <- lm(NPOC_mgC_g ~ leaftype, s3)
anova(DOC.leaftypes3)
#Significant!!
#ANOVA for s4
DOC.leaftypes4 <- lm(NPOC_mgC_g ~ leaftype, s4)
anova(DOC.leaftypes4)
#Significant!!
#ANOVA for s5
DOC.leaftypes5 <- lm(NPOC_mgC_g ~ leaftype, s5)
anova(DOC.leaftypes5)
#Significant!!
#ANOVA for s6
DOC.leaftypes6 <- lm(NPOC_mgC_g ~ leaftype, s6)
anova(DOC.leaftypes6)
#Significant!!
#ANOVA for s7
DOC.leaftypes7 <- lm(NPOC_mgC_g ~ leaftype, s7)
anova(DOC.leaftypes7)
#Significant!!
####exclude species 8 because of day 7 data
#ANOVA for s8
DOC.leaftypes8 <- lm(NPOC_mgC_g ~ leaftype, s8[s8$Days_Leached !=7,])
anova(DOC.leaftypes8)
#Significant!!
#ANOVA for s9
DOC.leaftypes9 <- lm(NPOC_mgC_g ~ leaftype, s9)
anova(DOC.leaftypes9)
#Significant!!




#############Graphs and Analyses by individual days

###make a boxplot for DOC after 1 day
ggplot(day1, aes(x=leaftype, y=NPOC_mgC_g)) + geom_boxplot() +labs(x= "Leaf Type", y= "DOC after 1 day")
##ANOVA for day 1
DOC.Day1 <- lm(NPOC_mgC_g ~ leaftype, day1)
anova(DOC.Day1)
##Significant!

###make a boxplot for day 1 green vs brown leaves
#facet_wrap groups by species and leaf type 
ggplot(day1, aes(x=Species, y=NPOC_mgC_g, fill=Leaf.Stage)) + geom_boxplot() + facet_wrap(~Leaf.Stage) + theme_cowplot() +labs(x= "Species", y= "DOC mg C/g leaf after 1 day") + scale_fill_manual(values=c('seagreen','saddlebrown')) + theme(legend.position="none")

#facet_wrap groups by species 
ggplot(day1, aes(x=leaftype, y=NPOC_mgC_g)) + geom_boxplot() + facet_wrap(~Species) + theme_cowplot() +labs(x= "Leaf Type", y= "DOC day 1")


##ANOVA for species --- After 1 day
##need to filter by species by DAY1
s1d1 <- day1 %>%
  filter(Species == 1)
s2d1 <- day1 %>%
  filter(Species == 2)
s3d1 <- day1 %>%
  filter(Species == 3)
s4d1 <- day1 %>%
  filter(Species == 4)
s5d1 <- day1 %>%
  filter(Species == 5)
s6d1 <- day1 %>%
  filter(Species == 6)
s7d1 <- day1 %>%
  filter(Species == 7)
s8d1 <- day1 %>%
  filter(Species == 8)
s9d1 <- day1 %>%
  filter(Species == 9)

#ANOVA for s1
DOCday1.leaftypes1 <- lm(NPOC_mgC_g ~ leaftype, s1d1)
anova(DOCday1.leaftypes1)
#not significant
#ANOVA for s2
DOCday1.leaftypes2 <- lm(NPOC_mgC_g ~ leaftype, s2d1)
anova(DOCday1.leaftypes2)
#not significant
#ANOVA for s3
DOCday1.leaftypes3 <- lm(NPOC_mgC_g ~ leaftype, s3d1)
anova(DOCday1.leaftypes3)
#not Significant
#ANOVA for s4
DOCday1.leaftypes4 <- lm(NPOC_mgC_g ~ leaftype, s4d1)
anova(DOCday1.leaftypes4)
#Significant!!
#ANOVA for s5
DOCday1.leaftypes5 <- lm(NPOC_mgC_g ~ leaftype, s5d1)
anova(DOCday1.leaftypes5)
#Significant!!
#ANOVA for s6
DOCday1.leaftypes6 <- lm(NPOC_mgC_g ~ leaftype, s6d1)
anova(DOCday1.leaftypes6)
#Significant!!
#ANOVA for s7
DOCday1.leaftypes7 <- lm(NPOC_mgC_g ~ leaftype, s7d1)
anova(DOCday1.leaftypes7)
#Significant!!
#ANOVA for s8
DOCday1.leaftypes8 <- lm(NPOC_mgC_g ~ leaftype, s8d1)
anova(DOCday1.leaftypes8)
#Significant!!
#ANOVA for s9
DOCday1.leaftypes9 <- lm(NPOC_mgC_g ~ leaftype, s9d1)
anova(DOCday1.leaftypes9)
#Significant!!







###make a boxplot for DOC after 3 days
ggplot(day3, aes(x=leaftype, y=NPOC_mgC_g)) + geom_boxplot() +labs(x= "Leaf Type", y= "DOC after 3 days")
##ANOVA for day 3
DOC.Day3 <- lm(NPOC_mgC_g ~ leaftype, day3)
anova(DOC.Day3)
##Significant!

###make a boxplot for day 3 green vs brown leaves
#facet_wrap groups by species and leaf type 
ggplot(day3, aes(x=Species, y=NPOC_mgC_g, fill=Leaf.Stage)) + geom_boxplot() + facet_wrap(~Leaf.Stage) + theme_cowplot() +labs(x= "Species", y= "DOC mg C/g leaf after 3 days") + scale_fill_manual(values=c('seagreen','saddlebrown')) + theme(legend.position="none")

#facet_wrap groups by species 
ggplot(day3, aes(x=leaftype, y=NPOC_mgC_g)) + geom_boxplot() + facet_wrap(~Species) + theme_cowplot() +labs(x= "Leaf Type", y= "DOC day 3")


##ANOVA for each species DAY 3
##need to filter by species by DAY3
s1d3 <- day3 %>%
  filter(Species == 1)
s2d3 <- day3 %>%
  filter(Species == 2)
s3d3 <- day3 %>%
  filter(Species == 3)
s4d3 <- day3 %>%
  filter(Species == 4)
s5d3 <- day3 %>%
  filter(Species == 5)
s6d3 <- day3 %>%
  filter(Species == 6)
s7d3 <- day3 %>%
  filter(Species == 7)
s8d3 <- day3 %>%
  filter(Species == 8)
s9d3 <- day3 %>%
  filter(Species == 9)

#ANOVA for s1
DOCday3.leaftypes1 <- lm(NPOC_mgC_g ~ leaftype, s1d3)
anova(DOCday3.leaftypes1)
# significant!
#ANOVA for s2
DOCday3.leaftypes2 <- lm(NPOC_mgC_g ~ leaftype, s2d3)
anova(DOCday3.leaftypes2)
#significant!
#ANOVA for s3
DOCday3.leaftypes3 <- lm(NPOC_mgC_g ~ leaftype, s3d3)
anova(DOCday3.leaftypes3)
#not Significant
#ANOVA for s4
DOCday3.leaftypes4 <- lm(NPOC_mgC_g ~ leaftype, s4d3)
anova(DOCday3.leaftypes4)
#NOT Significant
#ANOVA for s5
DOCday3.leaftypes5 <- lm(NPOC_mgC_g ~ leaftype, s5d3)
anova(DOCday3.leaftypes5)
#Significant!!
#ANOVA for s6
DOCday3.leaftypes6 <- lm(NPOC_mgC_g ~ leaftype, s6d3)
anova(DOCday3.leaftypes6)
#NOT Significant
#ANOVA for s7
DOCday3.leaftypes7 <- lm(NPOC_mgC_g ~ leaftype, s7d3)
anova(DOCday3.leaftypes7)
#Significant!!
#ANOVA for s8
DOCday3.leaftypes8 <- lm(NPOC_mgC_g ~ leaftype, s8d3)
anova(DOCday3.leaftypes8)
#Significant!!
#ANOVA for s9
DOCday3.leaftypes9 <- lm(NPOC_mgC_g ~ leaftype, s9d3)
anova(DOCday3.leaftypes9)
#Significant!!




###make a boxplot for DOC after 7 days
ggplot(day7, aes(x=leaftype, y=NPOC_mgC_g)) + geom_boxplot() +labs(x= "Leaf Type", y= "DOC after 7 days")
##ANOVA for day 7
DOC.Day7 <- lm(NPOC_mgC_g ~ leaftype, day7[day7$Species !=8,])
anova(DOC.Day7)
##Significant!

###make a boxplot for day 7 green vs brown leaves
#facet_wrap groups by species and leaf type 
ggplot(day7, aes(x=Species, y=NPOC_mgC_g, fill=Leaf.Stage)) + geom_boxplot() + facet_wrap(~Leaf.Stage) + theme_cowplot() +labs(x= "Species", y= "DOC mg C/g leaf after 7 days") + scale_fill_manual(values=c('seagreen','saddlebrown')) + theme(legend.position="none")

#facet_wrap groups by species 
ggplot(day7, aes(x=leaftype, y=NPOC_mgC_g)) + geom_boxplot() + facet_wrap(~Species) + theme_cowplot() +labs(x= "Leaf Type", y= "DOC day 7")


##ANOVA for each species DAY 7
##need to filter by species by DAY7
s1d7 <- day7 %>%
  filter(Species == 1)
s2d7 <- day7 %>%
  filter(Species == 2)
s3d7 <- day7 %>%
  filter(Species == 3)
s4d7 <- day7 %>%
  filter(Species == 4)
s5d7 <- day7 %>%
  filter(Species == 5)
s6d7 <- day7 %>%
  filter(Species == 6)
s7d7 <- day7 %>%
  filter(Species == 7)
s8d7 <- day7 %>%
  filter(Species == 8)
s9d7 <- day7 %>%
  filter(Species == 9)

#ANOVA for s1
DOCday7.leaftypes1 <- lm(NPOC_mgC_g ~ leaftype, s1d7)
anova(DOCday7.leaftypes1)
# significant!
#ANOVA for s2
DOCday7.leaftypes2 <- lm(NPOC_mgC_g ~ leaftype, s2d7)
anova(DOCday7.leaftypes2)
#NOT significant
#ANOVA for s3
DOCday7.leaftypes3 <- lm(NPOC_mgC_g ~ leaftype, s3d7)
anova(DOCday7.leaftypes3)
#not Significant
#ANOVA for s4
DOCday7.leaftypes4 <- lm(NPOC_mgC_g ~ leaftype, s4d7)
anova(DOCday7.leaftypes4)
#NOT Significant
#ANOVA for s5
DOCday7.leaftypes5 <- lm(NPOC_mgC_g ~ leaftype, s5d7)
anova(DOCday7.leaftypes5)
#Significant!!
#ANOVA for s6
DOCday7.leaftypes6 <- lm(NPOC_mgC_g ~ leaftype, s6d7)
anova(DOCday7.leaftypes6)
#Significant!!
#ANOVA for s7
DOCday7.leaftypes7 <- lm(NPOC_mgC_g ~ leaftype, s7d7)
anova(DOCday7.leaftypes7)
#Significant!!
###Excluding species 8 because of day 7 data
#ANOVA for s8
DOCday7.leaftypes8 <- lm(NPOC_mgC_g ~ leaftype, s8d7)
anova(DOCday7.leaftypes8)
#Significant!!
#ANOVA for s9
DOCday7.leaftypes9 <- lm(NPOC_mgC_g ~ leaftype, s9d7)
anova(DOCday7.leaftypes9)
#Significant!!



######LINE GRAPH FOR DOC

# Load the required libraries
library(ggplot2)
library(tidyr)

# Check if columns are numeric or character and convert if necessary
leachate.data <- mutate_if(leachate.data, is.factor, as.character)

##Creating new variable column based on leaf type saying "if my leaf type is equal to brown then call my new variable senesced, if its not equal to brown, call it fresh"
leachate.data$Leaf.Stage= ifelse(leachate.data$leaftype=="brown","Senesced", "Fresh")

##THIS GRAPH##
##trying to facet by species
ggplot(leachate.data, aes(x = as.numeric(leachate.data$Days_Leached), y = NPOC_mgC_g, color=Leaf.Stage)) +
  geom_point() + 
  scale_color_manual(values=c('seagreen','saddlebrown')) +
  guides(color = guide_legend(title = "Leaf Type")) +
  geom_smooth(method= "lm", se=FALSE) + 
  scale_x_continuous(breaks=c(0,1,3,7)) +
  theme_bw() +
  labs(x= "Days Leached", y= "DOC mg C/g leaf") +
  facet_wrap(~Species) 



###only species 3 
s3 <- leachate.data %>%
  filter(Species == 3)


ggplot(s3, aes(x = as.numeric(Days_Leached), y = NPOC_mgC_g, color=Leaf.Stage)) +
  geom_point() + 
  scale_color_manual(values=c('seagreen','saddlebrown')) +
  ggtitle("Species 3") +
  guides(color = guide_legend(title = "Leaf Type")) +
  geom_smooth(method= "lm", se=FALSE) + 
  scale_x_continuous(breaks=c(0,1,3,7)) +
  theme_bw() +
  labs(x= "Days Leached", y= "DOC mg C/g leaf") +
  theme(legend.text = element_text(size = 20),
        axis.text = element_text(size = 24),
        axis.title = element_text(size = 24),
        legend.title = element_text(size = 20),
        title = element_text(size=24)) ###text size for agu





##checking if Days Leached is a number or character
str(leachate.data$Days_Leached)

##chnaging Days Lreached to be numeric
leachate.data$Days_Leached <- as.numeric(leachate.data$Days_Leached)
str(leachate.data$Days_Leached)

##need to filter by species and leaf type
s1 <- leachate.data %>%
  filter(Species == 1)
s2 <- leachate.data %>%
  filter(Species == 2)
s3 <- leachate.data %>%
  filter(Species == 3)
s4 <- leachate.data %>%
  filter(Species == 4)
s5 <- leachate.data %>%
  filter(Species == 5)
s6 <- leachate.data %>%
  filter(Species == 6)
s7 <- leachate.data %>%
  filter(Species == 7)
s8 <- leachate.data %>%
  filter(Species == 8)
s9 <- leachate.data %>%
  filter(Species == 9)

#filtering each species brown only
s1B <- s1 %>%
  filter(leaftype == "brown")
s2B <- s2 %>%
  filter(leaftype == "brown")
s3B <- s3 %>%
  filter(leaftype == "brown")
s4B <- s4 %>%
  filter(leaftype == "brown")
s5B <- s5 %>%
  filter(leaftype == "brown")
s6B <- s6 %>%
  filter(leaftype == "brown")
s7B <- s7 %>%
  filter(leaftype == "brown")
s8B <- s8 %>%
  filter(leaftype == "brown")
s9B <- s9 %>%
  filter(leaftype == "brown")

#filtering each species green only
s1G <- s1 %>%
  filter(leaftype == "green")
s2G <- s2 %>%
  filter(leaftype == "green")
s3G <- s3 %>%
  filter(leaftype == "green")
s4G <- s4 %>%
  filter(leaftype == "green")
s5G <- s5 %>%
  filter(leaftype == "green")
s6G <- s6 %>%
  filter(leaftype == "green")
s7G <- s7 %>%
  filter(leaftype == "green")
s8G <- s8 %>%
  filter(leaftype == "green")
s9G <- s9 %>%
  filter(leaftype == "green")




##linear regression for species 1 Brown
lm_s1B_DOC = lm(formula = NPOC_mgC_g ~ Days_Leached, data =s1B)
summary(lm_s1B_DOC)
##linear regression for species 1 Green
lm_s1G_DOC = lm(formula = NPOC_mgC_g ~ Days_Leached, data =s1G)
summary(lm_s1G_DOC)

##linear regression for species 2 Brown
lm_s2B_DOC = lm(formula = NPOC_mgC_g ~ Days_Leached, data =s2B)
summary(lm_s2B_DOC)
##linear regression for species 2 Green
lm_s2G_DOC = lm(formula = NPOC_mgC_g ~ Days_Leached, data =s2G)
summary(lm_s2G_DOC)

##linear regression for species 3 Brown
lm_s3B_DOC = lm(formula = NPOC_mgC_g ~ Days_Leached, data =s3B)
summary(lm_s3B_DOC)
6.9##linear regression for species 3 Green
lm_s3G_DOC = lm(formula = NPOC_mgC_g ~ Days_Leached, data =s3G)
summary(lm_s3G_DOC)

##linear regression for species 4 Brown
lm_s4B_DOC = lm(formula = NPOC_mgC_g ~ Days_Leached, data =s4B)
summary(lm_s4B_DOC)
##linear regression for species 4 Green
lm_s4G_DOC = lm(formula = NPOC_mgC_g ~ Days_Leached, data =s4G)
summary(lm_s4G_DOC)

##linear regression for species 5 Brown
lm_s5B_DOC = lm(formula = NPOC_mgC_g ~ Days_Leached, data =s5B)
summary(lm_s5B_DOC)
##linear regression for species 5 Green
lm_s5G_DOC = lm(formula = NPOC_mgC_g ~ Days_Leached, data =s5G)
summary(lm_s5G_DOC)

##linear regression for species 6 Brown
lm_s6B_DOC = lm(formula = NPOC_mgC_g ~ Days_Leached, data =s6B)
summary(lm_s6B_DOC)
##linear regression for species 6 Green
lm_s6G_DOC = lm(formula = NPOC_mgC_g ~ Days_Leached, data =s6G)
summary(lm_s6G_DOC)

##linear regression for species 7 Brown
lm_s7B_DOC = lm(formula = NPOC_mgC_g ~ Days_Leached, data =s7B)
summary(lm_s7B_DOC)
##linear regression for species 7 Green
lm_s7G_DOC = lm(formula = NPOC_mgC_g ~ Days_Leached, data =s7G)
summary(lm_s7G_DOC)

##linear regression for species 8 Brown
lm_s8B_DOC = lm(formula = NPOC_mgC_g ~ Days_Leached, data =s8B)
summary(lm_s8B_DOC)
##linear regression for species 8 Green
lm_s8G_DOC = lm(formula = NPOC_mgC_g ~ Days_Leached, data =s8G)
summary(lm_s8G_DOC)

##linear regression for species 9 Brown
lm_s9B_DOC = lm(formula = NPOC_mgC_g ~ Days_Leached, data =s9B)
summary(lm_s9B_DOC)
##linear regression for species 9 Green
lm_s9G_DOC = lm(formula = NPOC_mgC_g ~ Days_Leached, data =s9G)
summary(lm_s9G_DOC)



####################NEED HELP
##ANCOVA stats???

#need to verify that the covariate and the treatment are independent, so run an ANOVA 
#fit anova model
anova_modelDOC <- aov(NPOC_mgC_g ~ leaftype, data = leachate.data[!(leachate.data$Species == 8 & leachate.data$Days_Leached == 7),])
#view summary of anova model
summary(anova_modelDOC)
###signifciant so we cant run Ancova?


#need to verify that there is homogeneity of variance among the groups, we conduct Levene’s Test
#load car library to conduct Levene's Test
library(car)
#conduct Levene's Test
leveneTest(NPOC_mgC_g ~leaftype, data = leachate.data[!(leachate.data$Species == 8 & leachate.data$Days_Leached == 7),])
###signifciant so we cant run Ancova?


#fit ANCOVA model
ancova_modelDOC <- lm(NPOC_mgC_g ~ Days_Leached + leaftype, data = leachate.data[!(leachate.data$Species == 8 & leachate.data$Days_Leached == 7),])

#view summary of model
Anova(ancova_modelDOC, type="II")
####SIGNIFICANT!!

########INTERPRETATION OF ANCOVA
###type two error-- getting significant results based on how the data is distributed
###it is different, but it is due to  chance





#load the multcomp library
library(multcomp)
# Assuming 'leaftype' is a factor variable in your dataset
# Convert 'leaftype' to a factor if it's not already
leachate.data$leaftype <- as.factor(leachate.data$leaftype)


## look up change legend tite ggplot
###need to find stats squared for each species
##run a regression model to get r squared and p value for each line to look at the difference of slopes for     each species



####hard to have a curve with only three points-- not enough data
##maybe with the median?

###brown versus green two way anova
#days leached 










#######total mass lost######
###make a boxplot for percent total mass loss
ggplot(leachate.data, aes(x=leaftype, y=perTML)) + geom_boxplot()+labs(x= "Days Leached", y= "% total mass lost")

##ANOVA for green leaves
MassLossBvG <- lm(perTML ~ leaftype, leachate.data[leachate.data$Species !=8,])
anova(MassLossBvG)
##Significant

###make a boxplot for % mass lost
#facet_wrap groups by leaves 
ggplot(leachate.data, aes(x=Days_Leached, y=perTML)) + geom_boxplot() + facet_wrap(~leaftype) + theme_cowplot() +labs(x= "Days Leached", y= "% total mass lost")

##ANOVA for green leaves
TML.DaysLeachedsG <- lm(perTML ~ Days_Leached, green[green$Species !=8,])
anova(TML.DaysLeachedsG)
#not significant

##ANOVA for brown leaves
TML.DaysLeachedsB <- lm(perTML ~ Days_Leached, brown[brown$Species !=8,])
anova(TML.DaysLeachedsB)
##Significant
##brown days leached significant differnce but dont know which days, so do HSD test--means separation
TML.DaysB_HSD <- HSD.test(TML.DaysLeachedsB, "Days_Leached", group = T)
TML.DaysB_HSD 




###make a boxplot
#convert integer to character
leachate.data$Species <- as.character(leachate.data$Species)
str(leachate.data$Species)

ggplot(leachate.data, aes(x = Species, y = perTML, fill = Leaf.Stage)) +
  geom_boxplot(outlier.shape = NA,  width = 0.75) + 
  theme_cowplot() +labs(x= "Species", y= "% Total Mass Lost") + scale_fill_manual(values=c('seagreen','saddlebrown')) +
  guides(fill = guide_legend(title = "Leaf Stage")) +
  geom_point(aes(shape=Days_Leached, group = Leaf.Stage), position = position_dodge(1), alpha = 0.4, size=2) + 
  guides(shape = guide_legend(title = "Days Leached")) +
  theme(legend.text = element_text(size = 20),
        axis.text = element_text(size = 24),
        axis.title = element_text(size = 24),
        legend.title = element_text(size = 20)) ###text size for agu
  

#facet_wrap groups by species and leaf type 
ggplot(leachate.data, aes(x=Species, y=perTML, fill=Leaf.Stage)) + geom_boxplot(outlier.shape = NA) + facet_wrap(~Leaf.Stage) + theme_cowplot() +labs(x= "Species", y= "% Total Mass Lost") + scale_fill_manual(values=c('seagreen','saddlebrown')) + geom_point(aes(shape=Days_Leached), position = position_dodge(width=1), alpha = 0.3, size=3) + guides(shape = guide_legend(title = "Days Leached")) + guides(fill = guide_legend(title = "Leaf Stage")) 

#facet_wrap groups by species 
ggplot(leachate.data, aes(x=leaftype, y=perTML)) + geom_boxplot() + facet_wrap(~Species) + theme_cowplot() +labs(x= "Leaf Type", y= "% total mass lost")


##ANOVA for species 
##need to filter by species
s1 <- leachate.data %>%
  filter(Species == 1)
s2 <- leachate.data %>%
  filter(Species == 2)
s3 <- leachate.data %>%
  filter(Species == 3)
s4 <- leachate.data %>%
  filter(Species == 4)
s5 <- leachate.data %>%
  filter(Species == 5)
s6 <- leachate.data %>%
  filter(Species == 6)
s7 <- leachate.data %>%
  filter(Species == 7)
s8 <- leachate.data %>%
  filter(Species == 8)
s9 <- leachate.data %>%
  filter(Species == 9)


#ANOVA for s1
TML.leaftypes1 <- lm(perTML ~ leaftype, s1)
anova(TML.leaftypes1)
#not significant
#ANOVA for s2
TML.leaftypes2 <- lm(perTML ~ leaftype, s2)
anova(TML.leaftypes2)
#not significant
#ANOVA for s3
TML.leaftypes3 <- lm(perTML ~ leaftype, s3)
anova(TML.leaftypes3)
#Significant!!
#ANOVA for s4
TML.leaftypes4 <- lm(perTML ~ leaftype, s4)
anova(TML.leaftypes4)
#significant!!
#ANOVA for s5
TML.leaftypes5 <- lm(perTML ~ leaftype, s5)
anova(TML.leaftypes5)
#Significant!!
#ANOVA for s6
TML.leaftypes6 <- lm(perTML ~ leaftype, s6)
anova(TML.leaftypes6)
#Not significant
#ANOVA for s7
TML.leaftypes7 <- lm(perTML ~ leaftype, s7)
anova(TML.leaftypes7)
#Significant!!
#####exclude species 8??
#ANOVA for s8
TML.leaftypes8 <- lm(perTML ~ leaftype, s8[s8$Days_Leached !=7,])
anova(TML.leaftypes8)
#Significant!!
#ANOVA for s9
TML.leaftypes9 <- lm(perTML ~ leaftype, s9)
anova(TML.leaftypes9)
#significant!!







#############Graphs and Analyses by individual days

####Filter by day 1
day1 <- leachate.data %>%
  filter(Days_Leached == 1)
###make a boxplot for total mass loss after 1 day
ggplot(day1, aes(x=leaftype, y=perTML)) + geom_boxplot() +labs(x= "Leaf Type", y= "% Total Mass Loss after 1 day")
##ANOVA for day 1
TML.Day1 <- lm(perTML ~ leaftype, day1)
anova(TML.Day1)
##Significant!

###make a boxplot for day 1 green vs brown leaves
#facet_wrap groups by species 
ggplot(day1, aes(x=leaftype, y=perTML)) + geom_boxplot() + facet_wrap(~Species) + theme_cowplot() +labs(x= "Leaf Type", y= "% Total Mass Loss after 1 day")


##ANOVA for species --- After 1 day
##need to filter by species by DAY1
s1d1 <- day1 %>%
  filter(Species == 1)
s2d1 <- day1 %>%
  filter(Species == 2)
s3d1 <- day1 %>%
  filter(Species == 3)
s4d1 <- day1 %>%
  filter(Species == 4)
s5d1 <- day1 %>%
  filter(Species == 5)
s6d1 <- day1 %>%
  filter(Species == 6)
s7d1 <- day1 %>%
  filter(Species == 7)
s8d1 <- day1 %>%
  filter(Species == 8)
s9d1 <- day1 %>%
  filter(Species == 9)

#ANOVA for s1
TMLday1.leaftypes1 <- lm(perTML ~ leaftype, s1d1)
anova(TMLday1.leaftypes1)
#not significant
#ANOVA for s2
TMLday1.leaftypes2 <- lm(perTML ~ leaftype, s2d1)
anova(TMLday1.leaftypes2)
#significant!
#ANOVA for s3
TMLday1.leaftypes3 <- lm(perTML ~ leaftype, s3d1)
anova(TMLday1.leaftypes3)
#not Significant
#ANOVA for s4
TMLday1.leaftypes4 <- lm(perTML ~ leaftype, s4d1)
anova(TMLday1.leaftypes4)
#Significant!!
#ANOVA for s5
TMLday1.leaftypes5 <- lm(perTML ~ leaftype, s5d1)
anova(TMLday1.leaftypes5)
#Significant!!
#ANOVA for s6
TMLday1.leaftypes6 <- lm(perTML ~ leaftype, s6d1)
anova(TMLday1.leaftypes6)
#Significant!!
#ANOVA for s7
TMLday1.leaftypes7 <- lm(perTML ~ leaftype, s7d1)
anova(TMLday1.leaftypes7)
#Significant!!
#ANOVA for s8
TMLday1.leaftypes8 <- lm(perTML ~ leaftype, s8d1)
anova(TMLday1.leaftypes8)
#Significant!!
#ANOVA for s9
TMLday1.leaftypes9 <- lm(perTML ~ leaftype, s9d1)
anova(TMLday1.leaftypes9)
#Significant!!




###########DAY 3
day3 <- leachate.data %>%
  filter(Days_Leached == 3)
###make a boxplot for DOC after 3 days
ggplot(day3, aes(x=leaftype, y=perTML)) + geom_boxplot() +labs(x= "Leaf Type", y= "% Total Mass Loss after 3 days")
##ANOVA for day 3
TML.Day3 <- lm(perTML ~ leaftype, day3)
anova(TML.Day3)
## NOT Significant

###make a boxplot for day 3 green vs brown leaves
#facet_wrap groups by species 
ggplot(day3, aes(x=leaftype, y=NPOC_mgC_g)) + geom_boxplot() + facet_wrap(~Species) + theme_cowplot() +labs(x= "Leaf Type", y= "% Total Mass Loss after day 3")


##ANOVA for each species DAY 3
##need to filter by species by DAY3
s1d3 <- day3 %>%
  filter(Species == 1)
s2d3 <- day3 %>%
  filter(Species == 2)
s3d3 <- day3 %>%
  filter(Species == 3)
s4d3 <- day3 %>%
  filter(Species == 4)
s5d3 <- day3 %>%
  filter(Species == 5)
s6d3 <- day3 %>%
  filter(Species == 6)
s7d3 <- day3 %>%
  filter(Species == 7)
s8d3 <- day3 %>%
  filter(Species == 8)
s9d3 <- day3 %>%
  filter(Species == 9)


#ANOVA for s1
TMLday3.leaftypes1 <- lm(perTML ~ leaftype, s1d3)
anova(TMLday3.leaftypes1)
#Significant
#ANOVA for s2
TMLday3.leaftypes2 <- lm(perTML ~ leaftype, s2d3)
anova(TMLday3.leaftypes2)
#significant!
#ANOVA for s3
TMLday3.leaftypes3 <- lm(perTML ~ leaftype, s3d3)
anova(TMLday3.leaftypes3)
#Significant!
#ANOVA for s4
TMLday3.leaftypes4 <- lm(perTML ~ leaftype, s4d3)
anova(TMLday3.leaftypes4)
#NOT Significant
#ANOVA for s5
TMLday3.leaftypes5 <- lm(perTML ~ leaftype, s5d3)
anova(TMLday3.leaftypes5)
#Significant!!
#ANOVA for s6
TMLday3.leaftypes6 <- lm(perTML ~ leaftype, s6d3)
anova(TMLday3.leaftypes6)
#NOT Significant
#ANOVA for s7
TMLday3.leaftypes7 <- lm(perTML ~ leaftype, s7d3)
anova(TMLday3.leaftypes7)
#NOT Significant
#ANOVA for s8
TMLday3.leaftypes8 <- lm(perTML ~ leaftype, s8d3)
anova(TMLday3.leaftypes8)
#Significant!!
#ANOVA for s9
TMLday3.leaftypes9 <- lm(perTML ~ leaftype, s9d3)
anova(TMLday3.leaftypes9)
#Significant!!


#######DAY 7
day7 <- leachate.data %>%
  filter(Days_Leached == 7)

###make a boxplot for DOC after 7 days
ggplot(day7, aes(x=leaftype, y=perTML)) + geom_boxplot() +labs(x= "Leaf Type", y= "% Total Mass Loss after 7 days")
##ANOVA for day 7
TML.Day7 <- lm(perTML ~ leaftype, day7[day7$Species !=8,])
anova(TML.Day7)
##Significant!

###make a boxplot for day 7 green vs brown leaves
#facet_wrap groups by species 
ggplot(day7, aes(x=leaftype, y=perTML)) + geom_boxplot() + facet_wrap(~Species) + theme_cowplot() +labs(x= "Leaf Type", y= "% Total Mass Loss day 7")


##ANOVA for each species DAY 7
##need to filter by species by DAY7
s1d7 <- day7 %>%
  filter(Species == 1)
s2d7 <- day7 %>%
  filter(Species == 2)
s3d7 <- day7 %>%
  filter(Species == 3)
s4d7 <- day7 %>%
  filter(Species == 4)
s5d7 <- day7 %>%
  filter(Species == 5)
s6d7 <- day7 %>%
  filter(Species == 6)
s7d7 <- day7 %>%
  filter(Species == 7)
s8d7 <- day7 %>%
  filter(Species == 8)
s9d7 <- day7 %>%
  filter(Species == 9)


#ANOVA for s1
TMLday7.leaftypes1 <- lm(perTML ~ leaftype, s1d7)
anova(TMLday7.leaftypes1)
#significant!
#ANOVA for s2
TMLday7.leaftypes2 <- lm(perTML ~ leaftype, s2d7)
anova(TMLday7.leaftypes2)
#NOT significant
#ANOVA for s3
TMLday7.leaftypes3 <- lm(perTML ~ leaftype, s3d7)
anova(TMLday7.leaftypes3)
#not Significant
#ANOVA for s4
TMLday7.leaftypes4 <- lm(perTML ~ leaftype, s4d7)
anova(TMLday7.leaftypes4)
#NOT Significant
#ANOVA for s5
TMLday7.leaftypes5 <- lm(perTML ~ leaftype, s5d7)
anova(TMLday7.leaftypes5)
#NOT significant!!
#ANOVA for s6
TMLday7.leaftypes6 <- lm(perTML ~ leaftype, s6d7)
anova(TMLday7.leaftypes6)
#Significant
#ANOVA for s7
TMLday7.leaftypes7 <- lm(perTML ~ leaftype, s7d7)
anova(TMLday7.leaftypes7)
#Significant!!
####excluding species 8 because of day 7 data
##Excluding species 8
#ANOVA for s8
TMLday7.leaftypes8 <- lm(perTML ~ leaftype, s8d7)
anova(TMLday7.leaftypes8)
#Significant!!
#ANOVA for s9
TMLday7.leaftypes9 <- lm(perTML ~ leaftype, s9d7)
anova(TMLday7.leaftypes9)
#Significant!!








###LINE GRAPH FOR TMS

# Load the required libraries
library(ggplot2)
library(tidyr)

# Check if columns are numeric or character and convert if necessary
leachate.data <- mutate_if(leachate.data, is.factor, as.character)


##Creating new variable column based on leaf type saying "if my leaf type is equal to brown then call my new variable senesced, if its not equal to brown, call it fresh"
leachate.data$Leaf.Stage= ifelse(leachate.data$leaftype=="brown","Senesced", "Fresh")

##THIS GRAPH##
ggplot(leachate.data, aes(x = as.numeric(leachate.data$Days_Leached), y = perTML, color=Leaf.Stage)) +
  geom_point() + 
  scale_color_manual(values=c('seagreen','saddlebrown')) +
  guides(color = guide_legend(title = "Leaf Type")) +
  geom_smooth(method= "lm", se=FALSE) + 
  scale_x_continuous(breaks=c(0,1,3,7)) +
  theme_bw() +
  labs(x= "Days Leached", y= "Total Mass Lost (%)")

##trying to facet by species
ggplot(leachate.data, aes(x = as.numeric(leachate.data$Days_Leached), y = perTML, color=Leaf.Stage)) +
  geom_point() + 
  scale_color_manual(values=c('seagreen','saddlebrown')) +
  guides(color = guide_legend(title = "Leaf Type")) +
  geom_smooth(method= "lm", se=FALSE) + 
  scale_x_continuous(breaks=c(0,1,3,7)) +
  theme_bw() +
  labs(x= "Days Leached", y= "Total Mass Lost (%)") +
  facet_wrap(~Species) 


##checking if Days Leached is a number or character
str(leachate.data$Days_Leached)

##chnaging Days Lreached to be numeric
leachate.data$Days_Leached <- as.numeric(leachate.data$Days_Leached)
str(leachate.data$Days_Leached)

##need to filter by species and leaf type
s1 <- leachate.data %>%
  filter(Species == 1)
s2 <- leachate.data %>%
  filter(Species == 2)
s3 <- leachate.data %>%
  filter(Species == 3)
s4 <- leachate.data %>%
  filter(Species == 4)
s5 <- leachate.data %>%
  filter(Species == 5)
s6 <- leachate.data %>%
  filter(Species == 6)
s7 <- leachate.data %>%
  filter(Species == 7)
s8 <- leachate.data %>%
  filter(Species == 8)
s9 <- leachate.data %>%
  filter(Species == 9)

#filtering each species brown only
s1B <- s1 %>%
  filter(leaftype == "brown")
s2B <- s2 %>%
  filter(leaftype == "brown")
s3B <- s3 %>%
  filter(leaftype == "brown")
s4B <- s4 %>%
  filter(leaftype == "brown")
s5B <- s5 %>%
  filter(leaftype == "brown")
s6B <- s6 %>%
  filter(leaftype == "brown")
s7B <- s7 %>%
  filter(leaftype == "brown")
s8B <- s8 %>%
  filter(leaftype == "brown")
s9B <- s9 %>%
  filter(leaftype == "brown")

#filtering each species green only
s1G <- s1 %>%
  filter(leaftype == "green")
s2G <- s2 %>%
  filter(leaftype == "green")
s3G <- s3 %>%
  filter(leaftype == "green")
s4G <- s4 %>%
  filter(leaftype == "green")
s5G <- s5 %>%
  filter(leaftype == "green")
s6G <- s6 %>%
  filter(leaftype == "green")
s7G <- s7 %>%
  filter(leaftype == "green")
s8G <- s8 %>%
  filter(leaftype == "green")
s9G <- s9 %>%
  filter(leaftype == "green")




##linear regression for species 1 Brown
lm_s1B_TML = lm(formula = perTML ~ Days_Leached, data =s1B)
summary(lm_s1B_TML)
##linear regression for species 1 Green
lm_s1G_TML = lm(formula = perTML ~ Days_Leached, data =s1G)
summary(lm_s1G_TML)

##linear regression for species 2 Brown
lm_s2B_TML = lm(formula = perTML ~ Days_Leached, data =s2B)
summary(lm_s2B_TML)
##linear regression for species 2 Green
lm_s2G_TML = lm(formula = perTML ~ Days_Leached, data =s2G)
summary(lm_s2G_TML)

##linear regression for species 3 Brown
lm_s3B_TML = lm(formula = perTML ~ Days_Leached, data =s3B)
summary(lm_s3B_TML)
##linear regression for species 3 Green
lm_s3G_TML = lm(formula = perTML ~ Days_Leached, data =s3G)
summary(lm_s3G_TML)

##linear regression for species 4 Brown
lm_s4B_TML = lm(formula = perTML ~ Days_Leached, data =s4B)
summary(lm_s4B_TML)
##linear regression for species 4 Green
lm_s4G_TML = lm(formula = perTML ~ Days_Leached, data =s4G)
summary(lm_s4G_TML)

##linear regression for species 5 Brown
lm_s5B_TML = lm(formula = perTML ~ Days_Leached, data =s5B)
summary(lm_s5B_TML)
##linear regression for species 5 Green
lm_s5G_TML = lm(formula = perTML ~ Days_Leached, data =s5G)
summary(lm_s5G_TML)

##linear regression for species 6 Brown
lm_s6B_TML = lm(formula = perTML ~ Days_Leached, data =s6B)
summary(lm_s6B_TML)
##linear regression for species 6 Green
lm_s6G_TML = lm(formula = perTML ~ Days_Leached, data =s6G)
summary(lm_s6G_TML)

##linear regression for species 7 Brown
lm_s7B_TML = lm(formula = perTML ~ Days_Leached, data =s7B)
summary(lm_s7B_TML)
##linear regression for species 7 Green
lm_s7G_TML = lm(formula = perTML ~ Days_Leached, data =s7G)
summary(lm_s7G_TML)


####EXCLUDING SPECIES 8
##linear regression for species 8 Brown
lm_s8B_TML = lm(formula = perTML ~ Days_Leached, data =s8B)
summary(lm_s8B_TML)
##linear regression for species 8 Green
lm_s8G_TML = lm(formula = perTML ~ Days_Leached, data =s8G)
summary(lm_s8G_TML)

##linear regression for species 9 Brown
lm_s9B_TML = lm(formula = perTML ~ Days_Leached, data =s9B)
summary(lm_s9B_TML)
##linear regression for species 9 Green
lm_s9G_TML = lm(formula = perTML ~ Days_Leached, data =s9G)
summary(lm_s9G_TML)




##ANCOVA stats???

#need to verify that the covariate and the treatment are independent, so run an ANOVA 
#fit anova model
anova_model <- aov(perTML ~ leaftype, data = leachate.data[leachate.data$Species !=8,])
#view summary of anova model
summary(anova_model)
###signifciant so we cant run Ancova?

#need to verify that there is homogeneity of variance among the groups, we conduct Levene’s Test
#load car library to conduct Levene's Test
library(car)
#conduct Levene's Test
leveneTest(perTML~leaftype, data = leachate.data[leachate.data$Species !=8,])
###signifciant so we cant run Ancova?


#fit ANCOVA model
ancova_model <- lm(perTML ~ Days_Leached + leaftype, data = leachate.data[leachate.data$Species !=8,])

#view summary of model
Anova(ancova_model, type="II")



###DAY 1 ANCVOA###
#fit ANCOVA model
ancova_modelD1 <- lm(perTML ~ leaftype, data = day1)
#view summary of model
Anova(ancova_modelD1, type="II")

###DAY 3 ANCVOA###
#fit ANCOVA model
ancova_modelD3 <- lm(perTML ~ leaftype, data = day3)
#view summary of model
Anova(ancova_modelD3, type="II")

###DAY 7 ANCVOA###
#fit ANCOVA model
ancova_modelD7 <- lm(perTML ~ leaftype, data = day7[day7$Species !=8,])

#view summary of model
Anova(ancova_modelD7, type="II")













########TDN############
##using TDN.dl because those are values corrected to account for detection limits

###make a boxplot for TDN
ggplot(leachate.data, aes(x=leaftype, y=TDN.dl)) + geom_boxplot() +labs(x= "Leaf Type", y= "Total Dissolved Nitrogen")
##ANOVA for green leaves
TDN <- lm(TDN.dl ~ leaftype, leachate.data[leachate.data$Species !=8,])
anova(TDN)
##NOT significant


##checking if Days Leached is a number or character
str(leachate.data$Days_Leached)

##chnaging Days Lreached to be character
leachate.data$Days_Leached <- as.factor(leachate.data$Days_Leached)
str(leachate.data$Days_Leached)

###make a boxplot for TDN
#facet_wrap groups by leaves 
ggplot(leachate.data, aes(x=Days_Leached, y=TDN.dl)) + geom_boxplot() + facet_wrap(~leaftype) + theme_cowplot() +labs(x= "Days Leached", y= "TDN mgN/g leaf")

##ANOVA for green leaves
TDN.DaysLeachedsG <- lm(TDN.dl ~ Days_Leached, green[green$Species !=8,])
anova(TDN.DaysLeachedsG)
#not significant

##ANOVA for brown leaves
TDN.DaysLeachedsB <- lm(TDN.dl ~ Days_Leached, brown[brown$Species !=8,])
anova(TDN.DaysLeachedsB)
##not significant

TDNdata <- subset(leachate.data, Species != 1)

###make a boxplot
ggplot(leachate.data, aes(x = Species, y = TDN.dl, fill = Leaf.Stage)) +
  geom_boxplot(outlier.shape = NA,  width = 0.75) + 
  theme_cowplot() +labs(x= "Species", y= "TDN mg N/g leaf") + scale_fill_manual(values=c('seagreen','saddlebrown')) +
  guides(fill = guide_legend(title = "Leaf Stage")) +
  geom_point(aes(shape=Days_Leached, group = Leaf.Stage), position = position_dodge(1), alpha = 0.4, size=2) + 
  guides(shape = guide_legend(title = "Days Leached")) +
  theme(legend.text = element_text(size = 20),
        axis.text = element_text(size = 24),
        axis.title = element_text(size = 24),
        legend.title = element_text(size = 20)) ###text size for agu


##without species 1
ggplot(TDNdata, aes(x = Species, y = TDN.dl, fill = Leaf.Stage)) +
  geom_boxplot(outlier.shape = NA,  width = 0.75) + 
  theme_cowplot() +labs(x= "Species", y= "TDN mg N/g leaf") + scale_fill_manual(values=c('seagreen','saddlebrown')) +
  guides(fill = guide_legend(title = "Leaf Stage")) +
  geom_point(aes(shape=Days_Leached, group = Leaf.Stage), position = position_dodge(1), alpha = 0.4, size=2) + 
  guides(shape = guide_legend(title = "Days Leached")) +
  theme(legend.text = element_text(size = 20),
        axis.text = element_text(size = 24),
        axis.title = element_text(size = 24),
        legend.title = element_text(size = 20)) ###text size for agu


#facet_wrap groups by species 
ggplot(leachate.data, aes(x=leaftype, y=TDN.dl)) + geom_boxplot() + facet_wrap(~Species) + theme_cowplot() +labs(x= "Leaf Type", y= "TDN mg N/g leaf")





##ANOVA for species 
##need to filter by species
s1 <- leachate.data %>%
  filter(Species == 1)
s2 <- leachate.data %>%
  filter(Species == 2)
s3 <- leachate.data %>%
  filter(Species == 3)
s4 <- leachate.data %>%
  filter(Species == 4)
s5 <- leachate.data %>%
  filter(Species == 5)
s6 <- leachate.data %>%
  filter(Species == 6)
s7 <- leachate.data %>%
  filter(Species == 7)
s8 <- leachate.data %>%
  filter(Species == 8)
s9 <- leachate.data %>%
  filter(Species == 9)


#ANOVA for s1
TDN.leaftypes1 <- lm(TDN.dl ~ leaftype, s1)
anova(TDN.leaftypes1)
#not significant
#ANOVA for s2
TDN.leaftypes2 <- lm(TDN.dl ~ leaftype, s2)
anova(TDN.leaftypes2)
# significant!!
#ANOVA for s3
TDN.leaftypes3 <- lm(TDN.dl ~ leaftype, s3)
anova(TDN.leaftypes3)
#Significant!!
#ANOVA for s4
TDN.leaftypes4 <- lm(TDN.dl ~ leaftype, s4)
anova(TDN.leaftypes4)
#Not Significant
#ANOVA for s5
TDN.leaftypes5 <- lm(TDN.dl ~ leaftype, s5)
anova(TDN.leaftypes5)
#Significant!!
#ANOVA for s6
TDN.leaftypes6 <- lm(TDN.dl ~ leaftype, s6)
anova(TDN.leaftypes6)
#Significant!!
#ANOVA for s7
TDN.leaftypes7 <- lm(TDN.dl ~ leaftype, s7)
anova(TDN.leaftypes7)
#Significant!!
######EXCLUDING SPECIES 8
#ANOVA for s8
TDN.leaftypes8 <- lm(TDN.dl ~ leaftype, s8)
anova(TDN.leaftypes8)
#Significant!!
#ANOVA for s9
TDN.leaftypes9 <- lm(TDN.dl ~ leaftype, s9)
anova(TDN.leaftypes9)
#Significant!!




###make a boxplot for TDN after 1 day
ggplot(day1, aes(x=leaftype, y=TDN.dl)) + geom_boxplot() +labs(x= "Leaf Type", y= "TDN after 1 day")
##ANOVA for day 1
TDN.Day1 <- lm(TDN.dl ~ leaftype, day1[day1$Species !=8,])
anova(TDN.Day1)
## not Significant

###make a boxplot for day 1 green vs brown leaves
#facet_wrap groups by species 
ggplot(day1, aes(x=leaftype, y=TDN.dl)) + geom_boxplot() + facet_wrap(~Species) + theme_cowplot() +labs(x= "Leaf Type", y= "TDN day 1")


##ANOVA for species --- After 1 day
##need to filter by species by DAY1
s1d1 <- day1 %>%
  filter(Species == 1)
s2d1 <- day1 %>%
  filter(Species == 2)
s3d1 <- day1 %>%
  filter(Species == 3)
s4d1 <- day1 %>%
  filter(Species == 4)
s5d1 <- day1 %>%
  filter(Species == 5)
s6d1 <- day1 %>%
  filter(Species == 6)
s7d1 <- day1 %>%
  filter(Species == 7)
s8d1 <- day1 %>%
  filter(Species == 8)
s9d1 <- day1 %>%
  filter(Species == 9)



#ANOVA for s1
TDNday1.leaftypes1 <- lm(TDN.dl ~ leaftype, s1d1)
anova(TDNday1.leaftypes1)
#Significant!
#ANOVA for s2
TDNday1.leaftypes2 <- lm(TDN.dl ~ leaftype, s2d1)
anova(TDNday1.leaftypes2)
# significant!!
#ANOVA for s3
TDNday1.leaftypes3 <- lm(TDN.dl ~ leaftype, s3d1)
anova(TDNday1.leaftypes3)
#NOT Significant
#ANOVA for s4
TDNday1.leaftypes4 <- lm(TDN.dl ~ leaftype, s4d1)
anova(TDNday1.leaftypes4)
#Significant!
#ANOVA for s5
TDNday1.leaftypes5 <- lm(TDN.dl ~ leaftype, s5d1)
anova(TDNday1.leaftypes5)
#Significant!!
#ANOVA for s6
TDNday1.leaftypes6 <- lm(TDN.dl ~ leaftype, s6d1)
anova(TDNday1.leaftypes6)
#Significant!!
#ANOVA for s7
TDNday1.leaftypes7 <- lm(TDN.dl ~ leaftype, s7d1)
anova(TDNday1.leaftypes7)
#Significant!!
#ANOVA for s8
TDNday1.leaftypes8 <- lm(TDN.dl ~ leaftype, s8d1)
anova(TDNday1.leaftypes8)
#Significant!!
#ANOVA for s9
TDNday1.leaftypes9 <- lm(TDN.dl ~ leaftype, s9d1)
anova(TDNday1.leaftypes9)
#Significant!!


###make a boxplot for TDN after 3 days
ggplot(day3, aes(x=leaftype, y=TDN.dl)) + geom_boxplot() +labs(x= "Leaf Type", y= "TDN after 3 days")
##ANOVA for day 3
TDN.Day3 <- lm(TDN.dl ~ leaftype, day3[day3$Species !=8,])
anova(TDN.Day3)
##NOT Significant

###make a boxplot for day 3 green vs brown leaves
#facet_wrap groups by species 
ggplot(day3, aes(x=leaftype, y=TDN.dl)) + geom_boxplot() + facet_wrap(~Species) + theme_cowplot() +labs(x= "Leaf Type", y= "TDN day 3")


##ANOVA for each species DAY 3
##need to filter by species by DAY3
s1d3 <- day3 %>%
  filter(Species == 1)
s2d3 <- day3 %>%
  filter(Species == 2)
s3d3 <- day3 %>%
  filter(Species == 3)
s4d3 <- day3 %>%
  filter(Species == 4)
s5d3 <- day3 %>%
  filter(Species == 5)
s6d3 <- day3 %>%
  filter(Species == 6)
s7d3 <- day3 %>%
  filter(Species == 7)
s8d3 <- day3 %>%
  filter(Species == 8)
s9d3 <- day3 %>%
  filter(Species == 9)


#ANOVA for s1
TDNday3.leaftypes1 <- lm(TDN.dl ~ leaftype, s1d3)
anova(TDNday3.leaftypes1)
#Significant!
#ANOVA for s2
TDNday3.leaftypes2 <- lm(TDN.dl ~ leaftype, s2d3)
anova(TDNday3.leaftypes2)
#NOT significant
#ANOVA for s3
TDNday3.leaftypes3 <- lm(TDN.dl ~ leaftype, s3d3)
anova(TDNday3.leaftypes3)
#Significant!!
#ANOVA for s4
TDNday3.leaftypes4 <- lm(TDN.dl ~ leaftype, s4d3)
anova(TDNday3.leaftypes4)
#Significant!
#ANOVA for s5
TDNday3.leaftypes5 <- lm(TDN.dl ~ leaftype, s5d3)
anova(TDNday3.leaftypes5)
#Significant!!
#ANOVA for s6
TDNday3.leaftypes6 <- lm(TDN.dl ~ leaftype, s6d3)
anova(TDNday3.leaftypes6)
#NOT Significant
#ANOVA for s7
TDNday3.leaftypes7 <- lm(TDN.dl ~ leaftype, s7d3)
anova(TDNday3.leaftypes7)
#Significant!!
#ANOVA for s8
TDNday3.leaftypes8 <- lm(TDN.dl ~ leaftype, s8d3)
anova(TDNday3.leaftypes8)
#Significant!!
#ANOVA for s9
TDNday3.leaftypes9 <- lm(TDN.dl ~ leaftype, s9d3)
anova(TDNday3.leaftypes9)
#Significant!!







###make a boxplot for TDN after 7 days
ggplot(day7, aes(x=leaftype, y=TDN.dl)) + geom_boxplot() +labs(x= "Leaf Type", y= "TDN after 7 days")
##ANOVA for day 7
DOC.Day7 <- lm(TDN.dl ~ leaftype, day7[day7$Species !=8,])
anova(DOC.Day7)
##Significant!

###make a boxplot for day 7 green vs brown leaves
#facet_wrap groups by species 
ggplot(day7, aes(x=leaftype, y=TDN.dl)) + geom_boxplot() + facet_wrap(~Species) + theme_cowplot() +labs(x= "Leaf Type", y= "TDN day 7")



##ANOVA for each species DAY 7
##need to filter by species by DAY7
s1d7 <- day7 %>%
  filter(Species == 1)
s2d7 <- day7 %>%
  filter(Species == 2)
s3d7 <- day7 %>%
  filter(Species == 3)
s4d7 <- day7 %>%
  filter(Species == 4)
s5d7 <- day7 %>%
  filter(Species == 5)
s6d7 <- day7 %>%
  filter(Species == 6)
s7d7 <- day7 %>%
  filter(Species == 7)
s8d7 <- day7 %>%
  filter(Species == 8)
s9d7 <- day7 %>%
  filter(Species == 9)

#ANOVA for s1
TDNday7.leaftypes1 <- lm(TDN.dl ~ leaftype, s1d7)
anova(TDNday7.leaftypes1)
#NOT Significant
#ANOVA for s2
TDNday7.leaftypes2 <- lm(TDN.dl ~ leaftype, s2d7)
anova(TDNday7.leaftypes2)
# significant!!
#ANOVA for s3
TDNday7.leaftypes3 <- lm(TDN.dl ~ leaftype, s3d7)
anova(TDNday7.leaftypes3)
#Significant
#ANOVA for s4
TDNday7.leaftypes4 <- lm(TDN.dl ~ leaftype, s4d7)
anova(TDNday7.leaftypes4)
#Significant!
#ANOVA for s5
TDNday7.leaftypes5 <- lm(TDN.dl ~ leaftype, s5d7)
anova(TDNday7.leaftypes5)
#Significant!!
#ANOVA for s6
TDNday7.leaftypes6 <- lm(TDN.dl ~ leaftype, s6d7)
anova(TDNday7.leaftypes6)
#Significant!!
#ANOVA for s7
TDNday7.leaftypes7 <- lm(TDN.dl ~ leaftype, s7d7)
anova(TDNday7.leaftypes7)
#Significant!!
#####EXCLUDING SPECIES 8
#ANOVA for s8
TDNday7.leaftypes8 <- lm(TDN.dl ~ leaftype, s8d7)
anova(TDNday7.leaftypes8)
#ANOVA for s9
TDNday7.leaftypes9 <- lm(TDN.dl ~ leaftype, s9d7)
anova(TDNday7.leaftypes9)
#Significant!!



#######Nitrogen linear regression
###LINE GRAPH FOR TDN

# Load the required libraries
library(ggplot2)
library(tidyr)

# Check if columns are numeric or character and convert if necessary
leachate.data <- mutate_if(leachate.data, is.factor, as.character)


##Creating new variable column based on leaf type saying "if my leaf type is equal to brown then call my new variable senesced, if its not equal to brown, call it fresh"
leachate.data$Leaf.Stage= ifelse(leachate.data$leaftype=="brown","Senesced", "Fresh")

##THIS GRAPH##
##trying to facet by species
ggplot(leachate.data, aes(x = as.numeric(leachate.data$Days_Leached), y = TDN.dl, color=Leaf.Stage)) +
  geom_point() + 
  scale_color_manual(values=c('seagreen','saddlebrown')) +
  guides(color = guide_legend(title = "Leaf Type")) +
  geom_smooth(method= "lm", se=FALSE) + 
  scale_x_continuous(breaks=c(0,1,3,7)) +
  theme_bw() +
  labs(x= "Days Leached", y= "TDN mgN/g leaf") +
  facet_wrap(~Species) 










########DON########
##using DON.dl because those are values corrected to account for detection limits

###make a boxplot
ggplot(leachate.data, aes(x = Species, y = DON.dl, fill = Leaf.Stage)) +
  geom_boxplot(outlier.shape = NA,  width = 0.75) + 
  theme_cowplot() +labs(x= "Species", y= "DON mg N/g leaf") + scale_fill_manual(values=c('seagreen','saddlebrown')) +
  guides(fill = guide_legend(title = "Leaf Stage")) +
  geom_point(aes(shape=Days_Leached, group = Leaf.Stage), position = position_dodge(1), alpha = 0.4, size=2) + 
  guides(shape = guide_legend(title = "Days Leached")) 

##Line GRAPH##
##trying to facet by species
ggplot(leachate.data, aes(x = as.numeric(leachate.data$Days_Leached), y = DON.dl, color=Leaf.Stage)) +
  geom_point() + 
  scale_color_manual(values=c('seagreen','saddlebrown')) +
  guides(color = guide_legend(title = "Leaf Type")) +
  geom_smooth(method= "lm", se=FALSE) + 
  scale_x_continuous(breaks=c(0,1,3,7)) +
  theme_bw() +
  labs(x= "Days Leached", y= "DON mg N/g leaf") +
  facet_wrap(~Species) 

DIN2 <- subset(leachate.data, Species != 1)

#DIN line graph
ggplot(leachate.data, aes(x = as.numeric(leachate.data$Days_Leached), y = DIN_calc, color=Leaf.Stage)) +
  geom_point() + 
  scale_color_manual(values=c('seagreen','saddlebrown')) +
  guides(color = guide_legend(title = "Leaf Type")) +
  geom_smooth(method= "lm", se=FALSE) + 
  scale_x_continuous(breaks=c(0,1,3,7)) +
  theme_bw() +
  labs(x= "Days Leached", y= "DIN mg N/g leaf") +
  facet_wrap(~Species) 

ggplot(DIN2, aes(x = as.numeric(DIN2$Days_Leached), y = DIN_calc, color=Leaf.Stage)) +
  geom_point() + 
  scale_color_manual(values=c('seagreen','saddlebrown')) +
  guides(color = guide_legend(title = "Leaf Type")) +
  geom_smooth(method= "lm", se=FALSE) + 
  scale_x_continuous(breaks=c(0,1,3,7)) +
  theme_bw() +
  labs(x= "Days Leached", y= "DIN mg N/g leaf") +
  facet_wrap(~Species) 

#NO3 line graph
ggplot(leachate.data, aes(x = as.numeric(leachate.data$Days_Leached), y = NO3.dl, color=Leaf.Stage)) +
  geom_point() + 
  scale_color_manual(values=c('seagreen','saddlebrown')) +
  guides(color = guide_legend(title = "Leaf Type")) +
  geom_smooth(method= "lm", se=FALSE) + 
  scale_x_continuous(breaks=c(0,1,3,7)) +
  theme_bw() +
  labs(x= "Days Leached", y= "DIN mg N/g leaf") +
  facet_wrap(~Species) 


ggplot(DIN2, aes(x = as.numeric(DIN2$Days_Leached), y = NO3.dl, color=Leaf.Stage)) +
  geom_point() + 
  scale_color_manual(values=c('seagreen','saddlebrown')) +
  guides(color = guide_legend(title = "Leaf Type")) +
  geom_smooth(method= "lm", se=FALSE) + 
  scale_x_continuous(breaks=c(0,1,3,7)) +
  theme_bw() +
  labs(x= "Days Leached", y= "NO3 mg N/g leaf") +
  facet_wrap(~Species) 


#NH4 line graph
ggplot(leachate.data, aes(x = as.numeric(leachate.data$Days_Leached), y = NH4.dl, color=Leaf.Stage)) +
  geom_point() + 
  scale_color_manual(values=c('seagreen','saddlebrown')) +
  guides(color = guide_legend(title = "Leaf Type")) +
  geom_smooth(method= "lm", se=FALSE) + 
  scale_x_continuous(breaks=c(0,1,3,7)) +
  theme_bw() +
  labs(x= "Days Leached", y= "DIN mg N/g leaf") +
  facet_wrap(~Species) 

ggplot(DIN2, aes(x = as.numeric(DIN2$Days_Leached), y = NH4.dl, color=Leaf.Stage)) +
  geom_point() + 
  scale_color_manual(values=c('seagreen','saddlebrown')) +
  guides(color = guide_legend(title = "Leaf Type")) +
  geom_smooth(method= "lm", se=FALSE) + 
  scale_x_continuous(breaks=c(0,1,3,7)) +
  theme_bw() +
  labs(x= "Days Leached", y= "NH4 mg N/g leaf") +
  facet_wrap(~Species) 




#####DOC:DON######

##calculating DON mol with detection limits 
leachate.data$DON.dl_mol = leachate.data$DON.dl/14

##caclcuating DOC:DON 
leachate.data$DOC.DON_ratio = leachate.data$DOC..mol./leachate.data$DON.dl_mol


###make a boxplot
#facet_wrap groups by species and leaf type 
ggplot(leachate.data, aes(x=Species, y=DOC.DON_ratio, fill=Leaf.Stage)) + geom_boxplot(outlier.shape = NA) + facet_wrap(~Leaf.Stage) + theme_cowplot() +labs(x= "Species", y= "DOC:DON") + scale_fill_manual(values=c('seagreen','saddlebrown')) + geom_point(aes(shape=Days_Leached), position = position_dodge(width=0.5), alpha = 0.3, size=3) + guides(shape = guide_legend(title = "Days Leached")) + guides(fill = guide_legend(title = "Leaf Stage")) 

#not facet wrapped
ggplot(leachate.data, aes(x = Species, y = DOC.DON_ratio, fill = Leaf.Stage)) +
  geom_boxplot(outlier.shape = NA,  width = 0.75) + 
  theme_cowplot() +labs(x= "Species", y= "DOC:DON") + scale_fill_manual(values=c('seagreen','saddlebrown')) +
  guides(fill = guide_legend(title = "Leaf Stage")) +
  geom_point(aes(shape=Days_Leached, group = Leaf.Stage), position = position_dodge(1), alpha = 0.4, size=2) + 
  guides(shape = guide_legend(title = "Days Leached")) 

#######POTASSIUM############



## Make a new row
#create a row called leaf type
##ifelse statement= if green column values equal 1, assign "green" as new value, if not, assign it brown
leachate.data$leaftype <- ifelse(leachate.data$Green == 1, "green","brown")


#convert integer to character
leachate.data$Days_Leached <- as.character(leachate.data$Days_Leached)
str(leachate.data$Days_Leached)

###make a boxplot for Potassium conc
ggplot(leachate.data, aes(x=leaftype,  y=K_mgK_g)) + geom_boxplot() + labs(x= "Leaf Type", y= "Potassium (mg K per g leaf)")




###make a boxplot
#facet_wrap groups by leaves 
ggplot(leachate.data, aes(x=Days_Leached, y=K_mgK_g)) + geom_boxplot() + facet_wrap(~leaftype) + theme_cowplot() +labs(x= "Days Leached", y= "Potassium (mg K per g leaf)")

#create a new dataframe that just has green leaves or brown leaves
green <- leachate.data %>%
  filter(leaftype == "green")


brown <- leachate.data %>%
  filter(leaftype == "brown")

##ANOVA for green leaves
Potassium.DaysG <- lm(K_mgK_g ~ Days_Leached, green[green$Species !=8,])
anova(Potassium.DaysG)
##days leached significant differnce but dont know which days, so do HSD test--means separation
Potassium.DaysG_HSD <- HSD.test(Potassium.DaysG, "Days_Leached", group = T)
Potassium.DaysG_HSD 


##ANOVA for brown leaves
Potassium.DaysB <- lm(K_mgK_g ~ Days_Leached, brown[brown$Species !=8,])
anova(Potassium.DaysB)
##days leached significant differnce but dont know which days, so do HSD test--means separation
Potassium.DaysB_HSD <- HSD.test(Potassium.DaysB, "Days_Leached", group = T)
Potassium.DaysB_HSD 


#facetwrap by days
#facet_wrap groups by days 
ggplot(leachate.data, aes(x=leaftype, y=K_mgK_g)) + geom_boxplot() + facet_wrap(~Days_Leached) + theme_cowplot() +labs(x= "Days Leached", y= "Potassium (mg K per g leaf)")

###make a boxplot
#facet_wrap groups by species and leaf type 
ggplot(leachate.data, aes(x=Species, y=K_mgK_g, fill=Leaf.Stage)) + geom_boxplot()  + theme_cowplot() +labs(x= "Species", y= "Ptassium mg K/g leaf") + scale_fill_manual(values=c('seagreen','saddlebrown')) + theme(legend.position="none")

#facet_wrap groups by species and leaf type 
ggplot(leachate.data, aes(x=Species, y=K_mgK_g, fill=Leaf.Stage)) + geom_boxplot() + facet_wrap(~Leaf.Stage) + theme_cowplot() +labs(x= "Species", y= "Ptassium mg K/g leaf") + scale_fill_manual(values=c('seagreen','saddlebrown')) + theme(legend.position="none")


###make a boxplot for green vs brown leaves
#facet_wrap groups by species 
ggplot(leachate.data, aes(x=leaftype, y=K_mgK_g)) + geom_boxplot() + facet_wrap(~Species) + theme_cowplot() +labs(x= "Leaf Type", y= "Potassium (mg K per g leaf)")


##ANOVA for species 
##need to filter by species
s1 <- leachate.data %>%
  filter(Species == 1)
s2 <- leachate.data %>%
  filter(Species == 2)
s3 <- leachate.data %>%
  filter(Species == 3)
s4 <- leachate.data %>%
  filter(Species == 4)
s5 <- leachate.data %>%
  filter(Species == 5)
s6 <- leachate.data %>%
  filter(Species == 6)
s7 <- leachate.data %>%
  filter(Species == 7)
s8 <- leachate.data %>%
  filter(Species == 8)
s9 <- leachate.data %>%
  filter(Species == 9)


#ANOVA for s1
K.leaftypes1 <- lm(K_mgK_g ~ leaftype, s1)
anova(K.leaftypes1)
#significant!!
#ANOVA for s2
K.leaftypes2 <- lm(K_mgK_g ~ leaftype, s2)
anova(K.leaftypes2)
# not significant
#ANOVA for s3
K.leaftypes3 <- lm(K_mgK_g ~ leaftype, s3)
anova(K.leaftypes3)
# not Significant
#ANOVA for s4
K.leaftypes4 <- lm(K_mgK_g ~ leaftype, s4)
anova(K.leaftypes4)
#Not significant
#ANOVA for s5
K.leaftypes5 <- lm(K_mgK_g ~ leaftype, s5)
anova(K.leaftypes5)
#Significant!!
#ANOVA for s6
K.leaftypes6 <- lm(K_mgK_g ~ leaftype, s6)
anova(K.leaftypes6)
# significant!!
#ANOVA for s7
K.leaftypes7 <- lm(K_mgK_g ~ leaftype, s7)
anova(K.leaftypes7)
#not Significant
#ANOVA for s8
K.leaftypes8 <- lm(K_mgK_g ~ leaftype, s8)
anova(K.leaftypes8)
#Significant!!
#ANOVA for s9
K.leaftypes9 <- lm(K_mgK_g ~ leaftype, s9)
anova(K.leaftypes9)
#Not significant




####Potassium Conc-----Filter by day 1
day1 <- leachate.data %>%
  filter(Days_Leached == 1)

###make a boxplot for potassium conc after 1 day
ggplot(day1, aes(x=leaftype, y=K_mgK_g)) + geom_boxplot() +labs(x= "Leaf Type", y= "Potassium (mg K per g leaf) after 1 day")
##ANOVA for day 1
K.Day1 <- lm(K_mgK_g ~ leaftype, day1[day1$Species !=8,])
anova(K.Day1)
##notsignificant


###make a boxplot for day 1 green vs brown leaves
#facet_wrap groups by species 
ggplot(day1, aes(x=leaftype, y=K_mgK_g)) + geom_boxplot() + facet_wrap(~Species) + theme_cowplot() +labs(x= "Leaf Type", y= "Potassium (mg K per g leaf) after 1 day")


##ANOVA for species 
##need to filter by species by DAY1
s1d1 <- day1 %>%
  filter(Species == 1)
s2d1 <- day1 %>%
  filter(Species == 2)
s3d1 <- day1 %>%
  filter(Species == 3)
s4d1 <- day1 %>%
  filter(Species == 4)
s5d1 <- day1 %>%
  filter(Species == 5)
s6d1 <- day1 %>%
  filter(Species == 6)
s7d1 <- day1 %>%
  filter(Species == 7)
s8d1 <- day1 %>%
  filter(Species == 8)
s9d1 <- day1 %>%
  filter(Species == 9)


#ANOVA for s1
Kday1.leaftypes1 <- lm(K_mgK_g ~ leaftype, s1d1)
anova(Kday1.leaftypes1)
#not significant
#ANOVA for s2
Kday1.leaftypes2 <- lm(K_mgK_g ~ leaftype, s2d1)
anova(Kday1.leaftypes2)
# significant!!!
#ANOVA for s3
Kday1.leaftypes3 <- lm(K_mgK_g ~ leaftype, s3d1)
anova(Kday1.leaftypes3)
#Not significant
#ANOVA for s4
Kday1.leaftypes4 <- lm(K_mgK_g ~ leaftype, s4d1)
anova(Kday1.leaftypes4)
#Not significant
#ANOVA for s5
Kday1.leaftypes5 <- lm(K_mgK_g ~ leaftype, s5d1)
anova(Kday1.leaftypes5)
#Significant!!
#ANOVA for s6
Kday1.leaftypes6 <- lm(K_mgK_g ~ leaftype, s6d1)
anova(Kday1.leaftypes6)
# significant!!
#ANOVA for s7
Kday1.leaftypes7 <- lm(K_mgK_g ~ leaftype, s7d1)
anova(Kday1.leaftypes7)
# significant!!
##EXCLUDE 8??
#ANOVA for s8
Kday1.leaftypes8 <- lm(K_mgK_g ~ leaftype, s8d1)
anova(Kday1.leaftypes8)
# Significant!!
#ANOVA for s9
Kday1.leaftypes9 <- lm(K_mgK_g ~ leaftype, s9d1)
anova(Kday1.leaftypes9)
#Not Significant



######Day 3 Potassium conc
day3 <- leachate.data %>%
  filter(Days_Leached == 3)

###make a boxplot for potassium conc after 3 days
ggplot(day3, aes(x=leaftype, y=K_mgK_g)) + geom_boxplot() +labs(x= "Leaf Type", y= "Potassium (mg K per g leaf) after 3 days")
##ANOVA for day 3
K.Day3 <- lm(K_mgK_g ~ leaftype, day3[day3$Species !=8,])
anova(K.Day3)
##notsignificant


###make a boxplot for day 3 green vs brown leaves
#facet_wrap groups by species 
ggplot(day3, aes(x=leaftype, y=K_mgK_g)) + geom_boxplot() + facet_wrap(~Species) + theme_cowplot() +labs(x= "Leaf Type", y= "Potassium (mg K per g leaf) after 3 days")


##ANOVA for species 
##need to filter by species by DAY3
s1d3 <- day3 %>%
  filter(Species == 1)
s2d3 <- day3 %>%
  filter(Species == 2)
s3d3 <- day3 %>%
  filter(Species == 3)
s4d3 <- day3 %>%
  filter(Species == 4)
s5d3 <- day3 %>%
  filter(Species == 5)
s6d3 <- day3 %>%
  filter(Species == 6)
s7d3 <- day3 %>%
  filter(Species == 7)
s8d3 <- day3 %>%
  filter(Species == 8)
s9d3 <- day3 %>%
  filter(Species == 9)


#ANOVA for s1
Kday3.leaftypes1 <- lm(K_mgK_g ~ leaftype, s1d3)
anova(Kday3.leaftypes1)
# significant!!!
#ANOVA for s2
Kday3.leaftypes2 <- lm(K_mgK_g ~ leaftype, s2d3)
anova(Kday3.leaftypes2)
# significant!!!
#ANOVA for s3
Kday3.leaftypes3 <- lm(K_mgK_g ~ leaftype, s3d3)
anova(Kday3.leaftypes3)
#significant!
#ANOVA for s4
Kday3.leaftypes4 <- lm(K_mgK_g ~ leaftype, s4d3)
anova(Kday3.leaftypes4)
#Not significant
#ANOVA for s5
Kday3.leaftypes5 <- lm(K_mgK_g ~ leaftype, s5d3)
anova(Kday3.leaftypes5)
#Significant!!
#ANOVA for s6
Kday3.leaftypes6 <- lm(K_mgK_g ~ leaftype, s6d3)
anova(Kday3.leaftypes6)
#not significant
#ANOVA for s7
Kday3.leaftypes7 <- lm(K_mgK_g ~ leaftype, s7d3)
anova(Kday3.leaftypes7)
# significant!!
#EXCLUDING SPECIES 8???
#ANOVA for s8
Kday3.leaftypes8 <- lm(K_mgK_g ~ leaftype, s8d3)
anova(Kday3.leaftypes8)
#not Significant
#ANOVA for s9
Kday3.leaftypes9 <- lm(K_mgK_g ~ leaftype, s9d3)
anova(Kday3.leaftypes9)
#Not Significant




##########Day 7 postassium conc
day7 <- leachate.data %>%
  filter(Days_Leached == 7)

###make a boxplot for potassium conc after 7 days
ggplot(day7, aes(x=leaftype, y=K_mgK_g)) + geom_boxplot() +labs(x= "Leaf Type", y= "Potassium (mg K per g leaf) after 7 days")
##ANOVA for day 7
K.Day7 <- lm(K_mgK_g ~ leaftype, day7[day7$Species !=8,])
anova(K.Day7)
##notsignificant


###make a boxplot for day 7 green vs brown leaves
#facet_wrap groups by species 
ggplot(day7, aes(x=leaftype, y=K_mgK_g)) + geom_boxplot() + facet_wrap(~Species) + theme_cowplot() +labs(x= "Leaf Type", y= "Potassium (mg K per g leaf) after 7 days")


##ANOVA for species 
##need to filter by species by DAY7
s1d7 <- day7 %>%
  filter(Species == 1)
s2d7 <- day7 %>%
  filter(Species == 2)
s3d7 <- day7 %>%
  filter(Species == 3)
s4d7 <- day7 %>%
  filter(Species == 4)
s5d7 <- day7 %>%
  filter(Species == 5)
s6d7 <- day7 %>%
  filter(Species == 6)
s7d7 <- day7 %>%
  filter(Species == 7)
s8d7 <- day7 %>%
  filter(Species == 8)
s9d7 <- day7 %>%
  filter(Species == 9)


#ANOVA for s1
Kday7.leaftypes1 <- lm(K_mgK_g ~ leaftype, s1d7)
anova(Kday7.leaftypes1)
# significant!!!
#ANOVA for s2
Kday7.leaftypes2 <- lm(K_mgK_g ~ leaftype, s2d7)
anova(Kday7.leaftypes2)
#not significant
#ANOVA for s3
Kday7.leaftypes3 <- lm(K_mgK_g ~ leaftype, s3d7)
anova(Kday7.leaftypes3)
#not significant
#ANOVA for s4
Kday7.leaftypes4 <- lm(K_mgK_g ~ leaftype, s4d7)
anova(Kday7.leaftypes4)
# significant!
#ANOVA for s5
Kday7.leaftypes5 <- lm(K_mgK_g ~ leaftype, s5d7)
anova(Kday7.leaftypes5)
#Significant!!
#ANOVA for s6
Kday7.leaftypes6 <- lm(K_mgK_g ~ leaftype, s6d7)
anova(Kday7.leaftypes6)
#significant!
#ANOVA for s7
Kday7.leaftypes7 <- lm(K_mgK_g ~ leaftype, s7d7)
anova(Kday7.leaftypes7)
# significant!!
#EXCLUDING SPECIES 8???
#ANOVA for s8
Kday7.leaftypes8 <- lm(K_mgK_g ~ leaftype, s8d7)
anova(Kday7.leaftypes8)
#not Significant
#ANOVA for s9
Kday7.leaftypes9 <- lm(K_mgK_g ~ leaftype, s9d7)
anova(Kday7.leaftypes9)
#Not Significant





############ Potassium linear regression
###LINE GRAPH FOR K

# Load the required libraries
library(ggplot2)
library(tidyr)

# Check if columns are numeric or character and convert if necessary
leachate.data <- mutate_if(leachate.data, is.factor, as.character)



##Creating new variable column based on leaf type saying "if my leaf type is equal to brown then call my new variable senesced, if its not equal to brown, call it fresh"
leachate.data$Leaf.Stage= ifelse(leachate.data$leaftype=="brown","Senesced", "Fresh")

##THIS GRAPH##
##trying to facet by species
ggplot(leachate.data, aes(x = as.numeric(leachate.data$Days_Leached), y = K_mgK_g, color=Leaf.Stage)) +
  geom_point() + 
  scale_color_manual(values=c('seagreen','saddlebrown')) +
  guides(color = guide_legend(title = "Leaf Type")) +
  geom_smooth(method= "lm", se=FALSE) + 
  scale_x_continuous(breaks=c(0,1,3,7)) +
  theme_bw() +
  labs(x= "Days Leached", y= "Potassium (mg K per g leaf)") +
  facet_wrap(~Species) 



##checking if Days Leached is a number or character
str(leachate.data$Days_Leached)

##chnaging Days Lreached to be numeric
leachate.data$Days_Leached <- as.numeric(leachate.data$Days_Leached)
str(leachate.data$Days_Leached)

##need to filter by species and leaf type
s1 <- leachate.data %>%
  filter(Species == 1)
s2 <- leachate.data %>%
  filter(Species == 2)
s3 <- leachate.data %>%
  filter(Species == 3)
s4 <- leachate.data %>%
  filter(Species == 4)
s5 <- leachate.data %>%
  filter(Species == 5)
s6 <- leachate.data %>%
  filter(Species == 6)
s7 <- leachate.data %>%
  filter(Species == 7)
s8 <- leachate.data %>%
  filter(Species == 8)
s9 <- leachate.data %>%
  filter(Species == 9)

#filtering each species brown only
s1B <- s1 %>%
  filter(leaftype == "brown")
s2B <- s2 %>%
  filter(leaftype == "brown")
s3B <- s3 %>%
  filter(leaftype == "brown")
s4B <- s4 %>%
  filter(leaftype == "brown")
s5B <- s5 %>%
  filter(leaftype == "brown")
s6B <- s6 %>%
  filter(leaftype == "brown")
s7B <- s7 %>%
  filter(leaftype == "brown")
s8B <- s8 %>%
  filter(leaftype == "brown")
s9B <- s9 %>%
  filter(leaftype == "brown")

#filtering each species green only
s1G <- s1 %>%
  filter(leaftype == "green")
s2G <- s2 %>%
  filter(leaftype == "green")
s3G <- s3 %>%
  filter(leaftype == "green")
s4G <- s4 %>%
  filter(leaftype == "green")
s5G <- s5 %>%
  filter(leaftype == "green")
s6G <- s6 %>%
  filter(leaftype == "green")
s7G <- s7 %>%
  filter(leaftype == "green")
s8G <- s8 %>%
  filter(leaftype == "green")
s9G <- s9 %>%
  filter(leaftype == "green")


##STATS
##linear regression for species 1 Brown
lm_s1B_K = lm(formula = K_mgK_g ~ Days_Leached, data =s1B)
summary(lm_s1B_K)
##linear regression for species 1 Green
lm_s1G_K = lm(formula = K_mgK_g ~ Days_Leached, data =s1G)
summary(lm_s1G_K)

##linear regression for species 2 Brown
lm_s2B_K = lm(formula = K_mgK_g ~ Days_Leached, data =s2B)
summary(lm_s2B_K)
##linear regression for species 2 Green
lm_s2G_K = lm(formula = K_mgK_g ~ Days_Leached, data =s2G)
summary(lm_s2G_K)

##linear regression for species 3 Brown
lm_s3B_K = lm(formula = K_mgK_g ~ Days_Leached, data =s3B)
summary(lm_s3B_K)
##linear regression for species 3 Green
lm_s3G_K = lm(formula = K_mgK_g ~ Days_Leached, data =s3G)
summary(lm_s3G_K)

##linear regression for species 4 Brown
lm_s4B_K = lm(formula = K_mgK_g ~ Days_Leached, data =s4B)
summary(lm_s4B_K)
##linear regression for species 4 Green
lm_s4G_K = lm(formula = K_mgK_g ~ Days_Leached, data =s4G)
summary(lm_s4G_K)

##linear regression for species 5 Brown
lm_s5B_K = lm(formula = K_mgK_g ~ Days_Leached, data =s5B)
summary(lm_s5B_K)
##linear regression for species 5 Green
lm_s5G_K = lm(formula = K_mgK_g ~ Days_Leached, data =s5G)
summary(lm_s5G_K)

##linear regression for species 6 Brown
lm_s6B_K = lm(formula = K_mgK_g ~ Days_Leached, data =s6B)
summary(lm_s6B_K)
##linear regression for species 6 Green
lm_s6G_K = lm(formula = K_mgK_g ~ Days_Leached, data =s6G)
summary(lm_s6G_K)

##linear regression for species 7 Brown
lm_s7B_K = lm(formula = K_mgK_g ~ Days_Leached, data =s7B)
summary(lm_s7B_K)
##linear regression for species 7 Green
lm_s7G_K = lm(formula = K_mgK_g ~ Days_Leached, data =s7G)
summary(lm_s7G_K)

##linear regression for species 8 Brown
lm_s8B_K = lm(formula = K_mgK_g ~ Days_Leached, data =s8B)
summary(lm_s8B_K)
##linear regression for species 8 Green
lm_s8G_K = lm(formula = K_mgK_g ~ Days_Leached, data =s8G)
summary(lm_s8G_K)

##linear regression for species 9 Brown
lm_s9B_K = lm(formula = K_mgK_g ~ Days_Leached, data =s9B)
summary(lm_s9B_K)
##linear regression for species 9 Green
lm_s9G_K = lm(formula = K_mgK_g ~ Days_Leached, data =s9G)
summary(lm_s9G_K)




##########NEED TO SWAP OUT CN CODE FO K CODE
####################NEED HELP
##ANCOVA stats???

#need to verify that the covariate and the treatment are independent, so run an ANOVA 
#fit anova model
anova_modelK <- aov(K_mgK_g ~ leaftype, data = leachate.data[leachate.data$Species !=8,])
#view summary of anova model
summary(anova_modelK)

#need to verify that there is homogeneity of variance among the groups, we conduct Levene’s Test
#load car library to conduct Levene's Test
library(car)
#conduct Levene's Test
leveneTest(K_mgK_g ~leaftype, data = leachate.data[leachate.data$Species !=8,])


#fit ANCOVA model
ancova_modelK <- lm(K_mgK_g ~ Days_Leached + leaftype, data = leachate.data[leachate.data$Species !=8,])

#view summary of model
Anova(ancova_modelK, type="II")






##########Potassium plotted against C



# Load the required libraries
library(ggplot2)
library(tidyr)

# Check if columns are numeric or character and convert if necessary
leachate.data <- mutate_if(leachate.data, is.factor, as.character)

# Reshape the data from wide to long format
leachate.data_long <- gather(leachate.data, key = "LeafType", value = "Value", percent_MassLost_C:Percent_MassLost_N)

# Convert the 'Value' column to numeric
leachate.data_long$Value <- as.numeric(leachate.data_long$Value)


##Creating new variable column based on leaf type saying "if my leaf type is equal to brown then call my new variable senesced, if its not equal to brown, call it fresh"
leachate.data$Leaf.Stage= ifelse(leachate.data$leaftype=="brown","Senesced", "Fresh")

##THIS GRAPH##

##DOC vs K
ggplot(leachate.data, aes(x = as.numeric(leachate.data$NPOC_mgC_g), y = K_mgK_g, color=Leaf.Stage)) +
  geom_point() + 
  scale_color_manual(values=c('seagreen','saddlebrown')) +
  guides(color = guide_legend(title = "Leaf Type")) +
  geom_smooth(method= "lm", se=FALSE) + 
  theme_bw() +
  labs(x= "DOC mg C/g leaf", y= "Potassium (mg K per g leaf)")


###linear regression
  #filtering by leaftype

  brownleaves <- leachate.data %>%
  filter(leaftype == "brown")
  
  greenleaves <- leachate.data %>%
    filter(leaftype == "green")
  
##linear regression for Green
lm_G_KDOC = lm(formula = K_mgK_g ~ NPOC_mgC_g, data =greenleaves[greenleaves$Species !=8,])
summary(lm_G_KDOC)

##linear regression for Brown
lm_B_KDOC = lm(formula = K_mgK_g ~ NPOC_mgC_g, data =brownleaves[brownleaves$Species !=8,])
summary(lm_B_KDOC)





##trying to facet by species
ggplot(leachate.data, aes(x = as.numeric(leachate.data$NPOC_mgC_g), y = K_mgK_g, color=Leaf.Stage)) +
  geom_point() + 
  scale_color_manual(values=c('seagreen','saddlebrown')) +
  guides(color = guide_legend(title = "Leaf Type")) +
  geom_smooth(method= "lm", se=FALSE) + 
  theme_bw() +
  labs(x= "DOC mg C/g leaf", y= "Potassium (mg K per g leaf)") +
  facet_wrap(~Species) 





##leaf loss K and C

ggplot(leachate.data, aes(x = Percent_LeafLostDueC, y = percent_LeafLostDueK, color=Leaf.Stage)) +
  geom_point() + 
  scale_color_manual(values=c('seagreen','saddlebrown')) +
  guides(color = guide_legend(title = "Leaf Type")) +
  geom_smooth(method= "lm", se=FALSE) + 
  theme_bw() +
  labs(x= "Percent leaf lost due to C", y= "Percent leaf lost due to K")

  ###linear regression
  #filtering by leaftype

  brownleaves <- leachate.data %>%
   filter(leaftype == "brown")

  greenleaves <- leachate.data %>%
    filter(leaftype == "green")

  ##linear regression for Green
    lm_G_perc.KDOC = lm(formula = percent_LeafLostDueK ~ Percent_LeafLostDueC, data =greenleaves[greenleaves$Species !=8,])
    summary(lm_G_perc.KDOC)

  ##linear regression for Brown
    lm_B_perc.KDOC = lm(formula = percent_LeafLostDueK ~ Percent_LeafLostDueC, data =brownleaves[brownleaves$Species !=8,])
    summary(lm_B_perc.KDOC)






###############CALCIUM##################
    
    ## Make a new row
    #create a row called leaf type
    ##ifelse statement= if green column values equal 1, assign "green" as new value, if not, assign it brown
    leachate.data$leaftype <- ifelse(leachate.data$Green == 1, "green","brown")
    
    
    #convert integer to character
    leachate.data$Days_Leached <- as.character(leachate.data$Days_Leached)
    str(leachate.data$Days_Leached)
    
    ###make a boxplot for Calcium conc
    ggplot(leachate.data, aes(x=leaftype,  y=Ca_mgCa_g)) + geom_boxplot() + labs(x= "Leaf Type", y= "Calcium (mg Ca per g leaf")
    
    ##boxplot by leaves and species
    ggplot(leachate.data, aes(x = Species, y = Ca_mgCa_g, fill = Leaf.Stage)) +
      geom_boxplot(outlier.shape = NA,  width = 0.75) + 
      theme_cowplot() +labs(x= "Species", y= "Calcium mg Ca/g leaf") + scale_fill_manual(values=c('seagreen','saddlebrown')) +
      guides(fill = guide_legend(title = "Leaf Stage")) +
      geom_point(aes(shape=Days_Leached, group = Leaf.Stage), position = position_dodge(1), alpha = 0.4, size=2) + 
      guides(shape = guide_legend(title = "Days Leached")) 
    
    ###make a boxplot
    #facet_wrap groups by leaves 
    ggplot(leachate.data, aes(x=Days_Leached, y=Ca_mgCa_g)) + geom_boxplot() + facet_wrap(~leaftype) + theme_cowplot() +labs(x= "Days Leached", y= "Calcium (mg Ca per g leaf)")
    
    #create a new dataframe that just has green leaves or brown leaves
    green <- leachate.data %>%
      filter(leaftype == "green")
    
    
    brown <- leachate.data %>%
      filter(leaftype == "brown")
    
    ##ANOVA for green leaves
    Calcium.DaysG <- lm(Ca_mgCa_g ~ Days_Leached, green)
    anova(Calcium.DaysG)
    ##days leached significant differnce but dont know which days, so do HSD test--means separation
    Calcium.DaysG_HSD <- HSD.test(Calcium.DaysG, "Days_Leached", group = T)
    Calcium.DaysG_HSD 
    
    
    ##ANOVA for brown leaves
    Calcium.DaysB <- lm(Ca_mgCa_g ~ Days_Leached, brown)
    anova(Calcium.DaysB)
    ##days leached significant differnce but dont know which days, so do HSD test--means separation
    Calcium.DaysB_HSD <- HSD.test(Calcium.DaysB, "Days_Leached", group = T)
    Calcium.DaysB_HSD 
    
    
    #facetwrap by days
    #facet_wrap groups by days 
    ggplot(leachate.data, aes(x=leaftype, y=Ca_mgCa_g)) + geom_boxplot() + facet_wrap(~Days_Leached) + theme_cowplot() +labs(x= "Days Leached", y= "Calcium (mg Ca per g leaf)")
    
    
    ###make a boxplot for green vs brown leaves
    #facet_wrap groups by species 
    ggplot(leachate.data, aes(x=leaftype, y=Ca_mgCa_g)) + geom_boxplot() + facet_wrap(~Species) + theme_cowplot() +labs(x= "Leaf Type", y= "Calcium (mg Ca per g leaf)")
    
    
    ##ANOVA for species 
    ##need to filter by species
    s1 <- leachate.data %>%
      filter(Species == 1)
    s2 <- leachate.data %>%
      filter(Species == 2)
    s3 <- leachate.data %>%
      filter(Species == 3)
    s4 <- leachate.data %>%
      filter(Species == 4)
    s5 <- leachate.data %>%
      filter(Species == 5)
    s6 <- leachate.data %>%
      filter(Species == 6)
    s7 <- leachate.data %>%
      filter(Species == 7)
    s8 <- leachate.data %>%
      filter(Species == 8)
    s9 <- leachate.data %>%
      filter(Species == 9)
    
    
    #ANOVA for s1
    Ca.leaftypes1 <- lm(Ca_mgCa_g ~ leaftype, s1)
    anova(Ca.leaftypes1)
    #significant!!
    #ANOVA for s2
    Ca.leaftypes2 <- lm(Ca_mgCa_g ~ leaftype, s2)
    anova(Ca.leaftypes2)
    # not significant
    #ANOVA for s3
    Ca.leaftypes3 <- lm(Ca_mgCa_g ~ leaftype, s3)
    anova(Ca.leaftypes3)
    # not Significant
    #ANOVA for s4
    Ca.leaftypes4 <- lm(Ca_mgCa_g ~ leaftype, s4)
    anova(Ca.leaftypes4)
    #significant!
    #ANOVA for s5
    Ca.leaftypes5 <- lm(Ca_mgCa_g ~ leaftype, s5)
    anova(Ca.leaftypes5)
    #Not Significant
    #ANOVA for s6
    Ca.leaftypes6 <- lm(Ca_mgCa_g ~ leaftype, s6)
    anova(Ca.leaftypes6)
    # significant!!
    #ANOVA for s7
    Ca.leaftypes7 <- lm(Ca_mgCa_g ~ leaftype, s7)
    anova(Ca.leaftypes7)
    #not Significant
    #ANOVA for s8
    Ca.leaftypes8 <- lm(Ca_mgCa_g ~ leaftype, s8)
    anova(Ca.leaftypes8)
    #Significant!!
    #ANOVA for s9
    Ca.leaftypes9 <- lm(Ca_mgCa_g ~ leaftype, s9)
    anova(Ca.leaftypes9)
    #Significant!!
    
    
    
    
    ####Calcium Conc-----Filter by day 1
    day1 <- leachate.data %>%
      filter(Days_Leached == 1)
    
    ###make a boxplot for calcium conc after 1 day
    ggplot(day1, aes(x=leaftype, y=Ca_mgCa_g)) + geom_boxplot() +labs(x= "Leaf Type", y= "Calcium (mg Ca per g leaf) after 1 day")
    ##ANOVA for day 1
    Ca.Day1 <- lm(Ca_mgCa_g ~ leaftype, day1)
    anova(Ca.Day1)
    ##significant!!!!
    
    ###make a boxplot for day 1 green vs brown leaves
    #facet_wrap groups by species 
    ggplot(day1, aes(x=leaftype, y=Ca_mgCa_g)) + geom_boxplot() + facet_wrap(~Species) + theme_cowplot() +labs(x= "Leaf Type", y= "Calcium (mg Ca per g leaf) after 1 day")
    
    
    ##ANOVA for species 
    ##need to filter by species by DAY1
    s1d1 <- day1 %>%
      filter(Species == 1)
    s2d1 <- day1 %>%
      filter(Species == 2)
    s3d1 <- day1 %>%
      filter(Species == 3)
    s4d1 <- day1 %>%
      filter(Species == 4)
    s5d1 <- day1 %>%
      filter(Species == 5)
    s6d1 <- day1 %>%
      filter(Species == 6)
    s7d1 <- day1 %>%
      filter(Species == 7)
    s8d1 <- day1 %>%
      filter(Species == 8)
    s9d1 <- day1 %>%
      filter(Species == 9)
    
    
    #ANOVA for s1
    Caday1.leaftypes1 <- lm(Ca_mgCa_g ~ leaftype, s1d1)
    anova(Caday1.leaftypes1)
    # significant!!!
    #ANOVA for s2
    Caday1.leaftypes2 <- lm(Ca_mgCa_g ~ leaftype, s2d1)
    anova(Caday1.leaftypes2)
    # significant!!!
    #ANOVA for s3
    Caday1.leaftypes3 <- lm(Ca_mgCa_g ~ leaftype, s3d1)
    anova(Caday1.leaftypes3)
    #Not significant
    #ANOVA for s4
    Caday1.leaftypes4 <- lm(Ca_mgCa_g ~ leaftype, s4d1)
    anova(Caday1.leaftypes4)
    #Not significant
    #ANOVA for s5
    Caday1.leaftypes5 <- lm(Ca_mgCa_g ~ leaftype, s5d1)
    anova(Caday1.leaftypes5)
    #Not Significant
    #ANOVA for s6
    Caday1.leaftypes6 <- lm(Ca_mgCa_g ~ leaftype, s6d1)
    anova(Caday1.leaftypes6)
    # significant!!
    #ANOVA for s7
    Caday1.leaftypes7 <- lm(Ca_mgCa_g ~ leaftype, s7d1)
    anova(Caday1.leaftypes7)
    # significant!!
    #ANOVA for s8
    Caday1.leaftypes8 <- lm(Ca_mgCa_g ~ leaftype, s8d1)
    anova(Caday1.leaftypes8)
    # Significant!!
    #ANOVA for s9
    Caday1.leaftypes9 <- lm(Ca_mgCa_g ~ leaftype, s9d1)
    anova(Caday1.leaftypes9)
    #Significant!!
    
    ######Day 3 Calcium conc
    ####Calcium Conc-----Filter by day 3
    day3 <- leachate.data %>%
      filter(Days_Leached == 3)
    
    ###make a boxplot for calcium conc after 1 day
    ggplot(day3, aes(x=leaftype, y=Ca_mgCa_g)) + geom_boxplot() +labs(x= "Leaf Type", y= "Calcium (mg Ca per g leaf) after 3 days")
    ##ANOVA for day 3
    Ca.Day3 <- lm(Ca_mgCa_g ~ leaftype, day3)
    anova(Ca.Day3)
    ##significant!!!!
    
    ###make a boxplot for day 3 green vs brown leaves
    #facet_wrap groups by species 
    ggplot(day3, aes(x=leaftype, y=Ca_mgCa_g)) + geom_boxplot() + facet_wrap(~Species) + theme_cowplot() +labs(x= "Leaf Type", y= "Calcium (mg Ca per g leaf) after 3 days")
    
    #####need to do Anovas by specis for day 3
    
    
    
    ##########Day 7 Calcium conc
    ####Calcium Conc-----Filter by day 7
    day7 <- leachate.data %>%
      filter(Days_Leached == 7)
    
    
    
    ##ANOVA for species 
    ##need to filter by species by DAY1
    s1d7 <- day7 %>%
      filter(Species == 1)
    s2d7 <- day7 %>%
      filter(Species == 2)
    s3d7 <- day7 %>%
      filter(Species == 3)
    s4d7 <- day7 %>%
      filter(Species == 4)
    s5d7 <- day7 %>%
      filter(Species == 5)
    s6d7 <- day7 %>%
      filter(Species == 6)
    s7d7 <- day7 %>%
      filter(Species == 7)
    s8d7 <- day7 %>%
      filter(Species == 8)
    s9d7 <- day7 %>%
      filter(Species == 9)
    
    
    #ANOVA for s1
    Caday7.leaftypes1 <- lm(Ca_mgCa_g ~ leaftype, s1d7)
    anova(Caday7.leaftypes1)
    # significant!!!
    #ANOVA for s2
    Caday7.leaftypes2 <- lm(Ca_mgCa_g ~ leaftype, s2d7)
    anova(Caday7.leaftypes2)
    #not significant
    #ANOVA for s3
    Caday7.leaftypes3 <- lm(Ca_mgCa_g ~ leaftype, s3d7)
    anova(Caday7.leaftypes3)
    #Not significant
    #ANOVA for s4
    Caday7.leaftypes4 <- lm(Ca_mgCa_g ~ leaftype, s4d7)
    anova(Caday7.leaftypes4)
    #significant!
    #ANOVA for s5
    Caday7.leaftypes5 <- lm(Ca_mgCa_g ~ leaftype, s5d7)
    anova(Caday7.leaftypes5)
    #Not Significant
    #ANOVA for s6
    Caday7.leaftypes6 <- lm(Ca_mgCa_g ~ leaftype, s6d7)
    anova(Caday7.leaftypes6)
    # significant!!
    #ANOVA for s7
    Caday7.leaftypes7 <- lm(Ca_mgCa_g ~ leaftype, s7d7)
    anova(Caday7.leaftypes7)
    #not significant
    #ANOVA for s8
    Caday7.leaftypes8 <- lm(Ca_mgCa_g ~ leaftype, s8d7)
    anova(Caday7.leaftypes8)
    # Significant!!
    #ANOVA for s9
    Caday7.leaftypes9 <- lm(Ca_mgCa_g ~ leaftype, s9d7)
    anova(Caday7.leaftypes9)
    #Significant!!
    
    ###make a boxplot for calcium conc after 1 day
    ggplot(day7, aes(x=leaftype, y=Ca_mgCa_g)) + geom_boxplot() +labs(x= "Leaf Type", y= "Calcium (mg Ca per g leaf) after 7 days")
    ##ANOVA for day 7
    Ca.Day7 <- lm(Ca_mgCa_g ~ leaftype, day7)
    anova(Ca.Day7)
    ##significant!!!!
    
    ###make a boxplot for day 7 green vs brown leaves
    #facet_wrap groups by species 
    ggplot(day7, aes(x=leaftype, y=Ca_mgCa_g)) + geom_boxplot() + facet_wrap(~Species) + theme_cowplot() +labs(x= "Leaf Type", y= "Calcium (mg Ca per g leaf) after 7 days")
    
    ##Need  to do Anova by specieds for Ca day 7
    
    
    
    
    ############ Calcium linear regression
    ###LINE GRAPH FOR Ca
    
    # Load the required libraries
    library(ggplot2)
    library(tidyr)
    
    # Check if columns are numeric or character and convert if necessary
    leachate.data <- mutate_if(leachate.data, is.factor, as.character)
    
    
    ##Creating new variable column based on leaf type saying "if my leaf type is equal to brown then call my new variable senesced, if its not equal to brown, call it fresh"
    leachate.data$Leaf.Stage= ifelse(leachate.data$leaftype=="brown","Senesced", "Fresh")
    
    ##THIS GRAPH##
    ##trying to facet by species
    ggplot(leachate.data, aes(x = as.numeric(leachate.data$Days_Leached), y = Ca_mgCa_g, color=Leaf.Stage)) +
      geom_point() + 
      scale_color_manual(values=c('seagreen','saddlebrown')) +
      guides(color = guide_legend(title = "Leaf Type")) +
      geom_smooth(method= "lm", se=FALSE) + 
      scale_x_continuous(breaks=c(0,1,3,7)) +
      theme_bw() +
      labs(x= "Days Leached", y= "Calcium (mg Ca per g leaf)") +
      facet_wrap(~Species) 
    
    
    
    ##checking if Days Leached is a number or character
    str(leachate.data$Days_Leached)
    
    ##chnaging Days Lreached to be numeric
    leachate.data$Days_Leached <- as.numeric(leachate.data$Days_Leached)
    str(leachate.data$Days_Leached)
    
    ##need to filter by species and leaf type
    s1 <- leachate.data %>%
      filter(Species == 1)
    s2 <- leachate.data %>%
      filter(Species == 2)
    s3 <- leachate.data %>%
      filter(Species == 3)
    s4 <- leachate.data %>%
      filter(Species == 4)
    s5 <- leachate.data %>%
      filter(Species == 5)
    s6 <- leachate.data %>%
      filter(Species == 6)
    s7 <- leachate.data %>%
      filter(Species == 7)
    s8 <- leachate.data %>%
      filter(Species == 8)
    s9 <- leachate.data %>%
      filter(Species == 9)
    
    #filtering each species brown only
    s1B <- s1 %>%
      filter(leaftype == "brown")
    s2B <- s2 %>%
      filter(leaftype == "brown")
    s3B <- s3 %>%
      filter(leaftype == "brown")
    s4B <- s4 %>%
      filter(leaftype == "brown")
    s5B <- s5 %>%
      filter(leaftype == "brown")
    s6B <- s6 %>%
      filter(leaftype == "brown")
    s7B <- s7 %>%
      filter(leaftype == "brown")
    s8B <- s8 %>%
      filter(leaftype == "brown")
    s9B <- s9 %>%
      filter(leaftype == "brown")
    
    #filtering each species green only
    s1G <- s1 %>%
      filter(leaftype == "green")
    s2G <- s2 %>%
      filter(leaftype == "green")
    s3G <- s3 %>%
      filter(leaftype == "green")
    s4G <- s4 %>%
      filter(leaftype == "green")
    s5G <- s5 %>%
      filter(leaftype == "green")
    s6G <- s6 %>%
      filter(leaftype == "green")
    s7G <- s7 %>%
      filter(leaftype == "green")
    s8G <- s8 %>%
      filter(leaftype == "green")
    s9G <- s9 %>%
      filter(leaftype == "green")
    
    
    
    
    ##linear regression for species 1 Brown
    lm_s1B_Ca = lm(formula = Ca_mgCa_g ~ Days_Leached, data =s1B)
    summary(lm_s1B_Ca)
    ##linear regression for species 1 Green
    lm_s1G_Ca = lm(formula = Ca_mgCa_g ~ Days_Leached, data =s1G)
    summary(lm_s1G_Ca)
    
    ##linear regression for species 2 Brown
    lm_s2B_Ca = lm(formula = Ca_mgCa_g ~ Days_Leached, data =s2B)
    summary(lm_s2B_Ca)
    ##linear regression for species 2 Green
    lm_s2G_Ca = lm(formula = Ca_mgCa_g ~ Days_Leached, data =s2G)
    summary(lm_s2G_Ca)
    
    ##linear regression for species 3 Brown
    lm_s3B_Ca = lm(formula = Ca_mgCa_g ~ Days_Leached, data =s3B)
    summary(lm_s3B_Ca)
    ##linear regression for species 3 Green
    lm_s3G_Ca = lm(formula = Ca_mgCa_g ~ Days_Leached, data =s3G)
    summary(lm_s3G_Ca)
    
    ##linear regression for species 4 Brown
    lm_s4B_Ca = lm(formula = Ca_mgCa_g ~ Days_Leached, data =s4B)
    summary(lm_s4B_Ca)
    ##linear regression for species 4 Green
    lm_s4G_Ca = lm(formula = Ca_mgCa_g ~ Days_Leached, data =s4G)
    summary(lm_s4G_Ca)
    
    ##linear regression for species 5 Brown
    lm_s5B_Ca = lm(formula = Ca_mgCa_g ~ Days_Leached, data =s5B)
    summary(lm_s5B_Ca)
    ##linear regression for species 5 Green
    lm_s5G_Ca = lm(formula = Ca_mgCa_g ~ Days_Leached, data =s5G)
    summary(lm_s5G_Ca)
    
    ##linear regression for species 6 Brown
    lm_s6B_Ca = lm(formula = Ca_mgCa_g ~ Days_Leached, data =s6B)
    summary(lm_s6B_Ca)
    ##linear regression for species 6 Green
    lm_s6G_Ca = lm(formula = Ca_mgCa_g ~ Days_Leached, data =s6G)
    summary(lm_s6G_Ca)
    
    ##linear regression for species 7 Brown
    lm_s7B_Ca = lm(formula = Ca_mgCa_g ~ Days_Leached, data =s7B)
    summary(lm_s7B_Ca)
    ##linear regression for species 7 Green
    lm_s7G_Ca = lm(formula = Ca_mgCa_g ~ Days_Leached, data =s7G)
    summary(lm_s7G_Ca)
    
    ##linear regression for species 8 Brown
    lm_s8B_Ca = lm(formula = Ca_mgCa_g ~ Days_Leached, data =s8B)
    summary(lm_s8B_Ca)
    ##linear regression for species 8 Green
    lm_s8G_Ca = lm(formula = Ca_mgCa_g ~ Days_Leached, data =s8G)
    summary(lm_s8G_Ca)
    
    ##linear regression for species 9 Brown
    lm_s9B_Ca = lm(formula = Ca_mgCa_g ~ Days_Leached, data =s9B)
    summary(lm_s9B_Ca)
    ##linear regression for species 9 Green
    lm_s9G_Ca = lm(formula = Ca_mgCa_g ~ Days_Leached, data =s9G)
    summary(lm_s9G_Ca)
    
    
    
    
    
    ####################NEED HELP
    ##ANCOVA stats???
    
    #need to verify that the covariate and the treatment are independent, so run an ANOVA 
    #fit anova model
    anova_modelCa <- aov(Ca_mgCa_g ~ leaftype, data = leachate.data)
    #view summary of anova model
    summary(anova_modelCa)
    
    #need to verify that there is homogeneity of variance among the groups, we conduct Levene’s Test
    #load car library to conduct Levene's Test
    library(car)
    #conduct Levene's Test
    leveneTest(Ca_mgCa_g ~leaftype, data = leachate.data)
    
    
    #fit ANCOVA model
    ancova_modelCa <- lm(Ca_mgCa_g ~ Days_Leached + leaftype, data = leachate.data)
    
    #view summary of model
    Anova(ancova_modelCa, type="II")
    
    
    
    
    
    


##########Calcium plotted against C



# Load the required libraries
library(ggplot2)
library(tidyr)

# Check if columns are numeric or character and convert if necessary
leachate.data <- mutate_if(leachate.data, is.factor, as.character)


##Creating new variable column based on leaf type saying "if my leaf type is equal to brown then call my new variable senesced, if its not equal to brown, call it fresh"
leachate.data$Leaf.Stage= ifelse(leachate.data$leaftype=="brown","Senesced", "Fresh")

##THIS GRAPH##

##DOC vs Ca
ggplot(leachate.data, aes(x = as.numeric(leachate.data$NPOC_mgC_g), y = Ca_mgCa_g, color=Leaf.Stage)) +
  geom_point() + 
  scale_color_manual(values=c('seagreen','saddlebrown')) +
  guides(color = guide_legend(title = "Leaf Type")) +
  geom_smooth(method= "lm", se=FALSE) + 
  theme_bw() +
  labs(x= "DOC mg C/g leaf", y= "Calcium (mg Ca per g leaf)")


###linear regression
#filtering by leaftype

brownleaves <- leachate.data %>%
  filter(leaftype == "brown")

greenleaves <- leachate.data %>%
  filter(leaftype == "green")

##linear regression for Green
lm_G_CaDOC = lm(formula = Ca_mgCa_g ~ NPOC_mgC_g, data =greenleaves[greenleaves$Species !=8,])
summary(lm_G_CaDOC)

##linear regression for Brown
lm_B_CaDOC = lm(formula = Ca_mgCa_g ~ NPOC_mgC_g, data =brownleaves[brownleaves$Species !=8,])
summary(lm_B_CaDOC)





##trying to facet by species
ggplot(leachate.data, aes(x = as.numeric(leachate.data$NPOC_mgC_g), y = Ca_mgCa_g, color=Leaf.Stage)) +
  geom_point() + 
  scale_color_manual(values=c('seagreen','saddlebrown')) +
  guides(color = guide_legend(title = "Leaf Type")) +
  geom_smooth(method= "lm", se=FALSE) + 
  theme_bw() +
  labs(x= "DOC mg C/g leaf", y= "Calcium (mg Caper g leaf)") +
  facet_wrap(~Species) 










##############PHOSPHATE############


## Make a new row
#create a row called leaf type
##ifelse statement= if green column values equal 1, assign "green" as new value, if not, assign it brown
leachate.data$leaftype <- ifelse(leachate.data$Green == 1, "green","brown")


#convert integer to character
leachate.data$Days_Leached <- as.character(leachate.data$Days_Leached)
str(leachate.data$Days_Leached)

###make a boxplot for Phosphate conc
ggplot(leachate.data, aes(x=leaftype,  y=PO4_mgP_g)) + geom_boxplot() + labs(x= "Leaf Type", y= "Phosphate (mg P per g leaf")

##boxplot by leaves and species
ggplot(leachate.data, aes(x = Species, y = PO4.dl, fill = Leaf.Stage)) +
  geom_boxplot(outlier.shape = NA,  width = 0.75) + 
  theme_cowplot() +labs(x= "Species", y= "Phosphate mg P/g leaf") + scale_fill_manual(values=c('seagreen','saddlebrown')) +
  guides(fill = guide_legend(title = "Leaf Stage")) +
  geom_point(aes(shape=Days_Leached, group = Leaf.Stage), position = position_dodge(1), alpha = 0.4, size=2) + 
  guides(shape = guide_legend(title = "Days Leached")) 

###make a boxplot
#facet_wrap groups by leaves 
ggplot(leachate.data, aes(x=Days_Leached, y=PO4_mgP_g)) + geom_boxplot() + facet_wrap(~leaftype) + theme_cowplot() +labs(x= "Days Leached", y= "Phosphate (mg P per g leaf)")


###make a boxplot
#facet_wrap groups by leaves 
ggplot(leachate.data, aes(x=Days_Leached, y=PO4_mgP_g)) + geom_boxplot() + facet_wrap(~leaftype) + theme_cowplot() +labs(x= "Days Leached", y= "Phosphate (mg P per g leaf)")

#create a new dataframe that just has green leaves or brown leaves
green <- leachate.data %>%
  filter(leaftype == "green")


brown <- leachate.data %>%
  filter(leaftype == "brown")

##ANOVA for green leaves
Phosphorous.DaysG <- lm(PO4_mgP_g ~ Days_Leached, green)
anova(Phosphorous.DaysG)
##not significant


##ANOVA for brown leaves
Phosphorous.DaysB <- lm(PO4_mgP_g ~ Days_Leached, brown)
anova(Phosphorous.DaysB)
##days leached significant differnce but dont know which days, so do HSD test--means separation
Phosphorous.DaysB_HSD <- HSD.test(Phosphorous.DaysB, "Days_Leached", group = T)
Phosphorous.DaysB_HSD 


#facetwrap by days
#facet_wrap groups by days 
ggplot(leachate.data, aes(x=leaftype, y=PO4_mgP_g)) + geom_boxplot() + facet_wrap(~Days_Leached) + theme_cowplot() +labs(x= "Days Leached", y= "Phosphorous (mg P per g leaf)")


###make a boxplot for green vs brown leaves
#facet_wrap groups by species 
ggplot(leachate.data, aes(x=leaftype, y=PO4_mgP_g)) + geom_boxplot() + facet_wrap(~Species) + theme_cowplot() +labs(x= "Leaf Type", y= "Phosphorous (mg P per g leaf)")


##ANOVA for species 
##need to filter by species
s1 <- leachate.data %>%
  filter(Species == 1)
s2 <- leachate.data %>%
  filter(Species == 2)
s3 <- leachate.data %>%
  filter(Species == 3)
s4 <- leachate.data %>%
  filter(Species == 4)
s5 <- leachate.data %>%
  filter(Species == 5)
s6 <- leachate.data %>%
  filter(Species == 6)
s7 <- leachate.data %>%
  filter(Species == 7)
s8 <- leachate.data %>%
  filter(Species == 8)
s9 <- leachate.data %>%
  filter(Species == 9)



###################NEED TO SWAP OUR K FOR P!!!!!!!


#ANOVA for s1
K.leaftypes1 <- lm(K_mgK_g ~ leaftype, s1)
anova(K.leaftypes1)
#significant!!
#ANOVA for s2
K.leaftypes2 <- lm(K_mgK_g ~ leaftype, s2)
anova(K.leaftypes2)
# not significant
#ANOVA for s3
K.leaftypes3 <- lm(K_mgK_g ~ leaftype, s3)
anova(K.leaftypes3)
# not Significant
#ANOVA for s4
K.leaftypes4 <- lm(K_mgK_g ~ leaftype, s4)
anova(K.leaftypes4)
#Not significant
#ANOVA for s5
K.leaftypes5 <- lm(K_mgK_g ~ leaftype, s5)
anova(K.leaftypes5)
#Significant!!
#ANOVA for s6
K.leaftypes6 <- lm(K_mgK_g ~ leaftype, s6)
anova(K.leaftypes6)
# significant!!
#ANOVA for s7
K.leaftypes7 <- lm(K_mgK_g ~ leaftype, s7)
anova(K.leaftypes7)
#not Significant
#ANOVA for s8
K.leaftypes8 <- lm(K_mgK_g ~ leaftype, s8)
anova(K.leaftypes8)
#Significant!!
#ANOVA for s9
K.leaftypes9 <- lm(K_mgK_g ~ leaftype, s9)
anova(K.leaftypes9)
#Not significant




####Phosphorous Conc-----Filter by day 1
day1 <- leachate.data %>%
  filter(Days_Leached == 1)

###make a boxplot for potassium conc after 1 day
ggplot(day1, aes(x=leaftype, y=PO4_mgP_g)) + geom_boxplot() +labs(x= "Leaf Type", y= "Phosphorous (mg P per g leaf) after 1 day")
##ANOVA for day 1
P.Day1 <- lm(PO4_mgP_g ~ leaftype, day1)
anova(P.Day1)
##notsignificant

###make a boxplot for day 1 green vs brown leaves
#facet_wrap groups by species 
ggplot(day1, aes(x=leaftype, y=PO4_mgP_g)) + geom_boxplot() + facet_wrap(~Species) + theme_cowplot() +labs(x= "Leaf Type", y= "Phosphorous (mg P per g leaf) after 1 day")


##ANOVA for species 
##need to filter by species by DAY1
s1d1 <- day1 %>%
  filter(Species == 1)
s2d1 <- day1 %>%
  filter(Species == 2)
s3d1 <- day1 %>%
  filter(Species == 3)
s4d1 <- day1 %>%
  filter(Species == 4)
s5d1 <- day1 %>%
  filter(Species == 5)
s6d1 <- day1 %>%
  filter(Species == 6)
s7d1 <- day1 %>%
  filter(Species == 7)
s8d1 <- day1 %>%
  filter(Species == 8)
s9d1 <- day1 %>%
  filter(Species == 9)


################NEED TO SWAP OUR K FOR P!!!!!!!!!!!!
#ANOVA for s1
Kday1.leaftypes1 <- lm(K_mgK_g ~ leaftype, s1d1)
anova(K.leaftypes1)
# significant!!!
#ANOVA for s2
Kday1.leaftypes2 <- lm(K_mgK_g ~ leaftype, s2d1)
anova(Kday1.leaftypes2)
# significant!!!
#ANOVA for s3
Kday1.leaftypes3 <- lm(K_mgK_g ~ leaftype, s3d1)
anova(Kday1.leaftypes3)
#Not significant
#ANOVA for s4
Kday1.leaftypes4 <- lm(K_mgK_g ~ leaftype, s4d1)
anova(Kday1.leaftypes4)
#Not significant
#ANOVA for s5
Kday1.leaftypes5 <- lm(K_mgK_g ~ leaftype, s5d1)
anova(Kday1.leaftypes5)
#Significant!!
#ANOVA for s6
Kday1.leaftypes6 <- lm(K_mgK_g ~ leaftype, s6d1)
anova(Kday1.leaftypes6)
# significant!!
#ANOVA for s7
Kday1.leaftypes7 <- lm(K_mgK_g ~ leaftype, s7d1)
anova(Kday1.leaftypes7)
# significant!!
#ANOVA for s8
Kday1.leaftypes8 <- lm(K_mgK_g ~ leaftype, s8d1)
anova(Kday1.leaftypes8)
# Significant!!
#ANOVA for s9
Kday1.leaftypes9 <- lm(K_mgK_g ~ leaftype, s9d1)
anova(Kday1.leaftypes9)
#Not Significant

######Day 3 phosphorous conc





##########Day 7 phosphorous conc
##ANOVA for species 
##need to filter by species by DAY7
s1d7 <- day7 %>%
  filter(Species == 1)
s2d7 <- day7 %>%
  filter(Species == 2)
s3d7 <- day7 %>%
  filter(Species == 3)
s4d7 <- day7 %>%
  filter(Species == 4)
s5d7 <- day7 %>%
  filter(Species == 5)
s6d7 <- day7 %>%
  filter(Species == 6)
s7d7 <- day7 %>%
  filter(Species == 7)
s8d7 <- day7 %>%
  filter(Species == 8)
s9d7 <- day7 %>%
  filter(Species == 9)


#ANOVA for s1
Pday7.leaftypes1 <- lm(PO4.dl ~ leaftype, s1d7)
anova(Pday7.leaftypes1)
#NOT significant!!!
#ANOVA for s2
Pday7.leaftypes2 <- lm(PO4.dl ~ leaftype, s2d7)
anova(Pday7.leaftypes2)
# significant!
#ANOVA for s3
Pday7.leaftypes3 <- lm(PO4.dl ~ leaftype, s3d7)
anova(Pday7.leaftypes3)
#not significant
#ANOVA for s4
Pday7.leaftypes4 <- lm(PO4.dl ~ leaftype, s4d7)
anova(Pday7.leaftypes4)
#NOT significant!
#ANOVA for s5
Pday7.leaftypes5 <- lm(PO4.dl ~ leaftype, s5d7)
anova(Pday7.leaftypes5)
#NOT Significant!!
#ANOVA for s6
Pday7.leaftypes6 <- lm(PO4.dl ~ leaftype, s6d7)
anova(Pday7.leaftypes6)
#NOT significant!
#ANOVA for s7
Pday7.leaftypes7 <- lm(PO4.dl ~ leaftype, s7d7)
anova(Pday7.leaftypes7)
# significant!!
#EXCLUDING SPECIES 8???
#ANOVA for s8
Pday7.leaftypes8 <- lm(PO4.dl ~ leaftype, s8d7)
anova(Pday7.leaftypes8)
#not Significant
#ANOVA for s9
Pday7.leaftypes9 <- lm(PO4.dl ~ leaftype, s9d7)
anova(Pday7.leaftypes9)
#Not Significant



############ Phosohrous linear regression
###LINE GRAPH FOR PO4

# Load the required libraries
library(ggplot2)
library(tidyr)

# Check if columns are numeric or character and convert if necessary
leachate.data <- mutate_if(leachate.data, is.factor, as.character)


##Creating new variable column based on leaf type saying "if my leaf type is equal to brown then call my new variable senesced, if its not equal to brown, call it fresh"
leachate.data$Leaf.Stage= ifelse(leachate.data$leaftype=="brown","Senesced", "Fresh")

##THIS GRAPH##
##trying to facet by species
ggplot(leachate.data, aes(x = as.numeric(leachate.data$Days_Leached), y = PO4_mgP_g, color=Leaf.Stage)) +
  geom_point() + 
  scale_color_manual(values=c('seagreen','saddlebrown')) +
  guides(color = guide_legend(title = "Leaf Type")) +
  geom_smooth(method= "lm", se=FALSE) + 
  scale_x_continuous(breaks=c(0,1,3,7)) +
  theme_bw() +
  labs(x= "Days Leached", y= "Phosphorous (mg P per g leaf)") +
  facet_wrap(~Species) 



##checking if Days Leached is a number or character
str(leachate.data$Days_Leached)

##chnaging Days Lreached to be numeric
leachate.data$Days_Leached <- as.numeric(leachate.data$Days_Leached)
str(leachate.data$Days_Leached)

##need to filter by species and leaf type
s1 <- leachate.data %>%
  filter(Species == 1)
s2 <- leachate.data %>%
  filter(Species == 2)
s3 <- leachate.data %>%
  filter(Species == 3)
s4 <- leachate.data %>%
  filter(Species == 4)
s5 <- leachate.data %>%
  filter(Species == 5)
s6 <- leachate.data %>%
  filter(Species == 6)
s7 <- leachate.data %>%
  filter(Species == 7)
s8 <- leachate.data %>%
  filter(Species == 8)
s9 <- leachate.data %>%
  filter(Species == 9)

#filtering each species brown only
s1B <- s1 %>%
  filter(leaftype == "brown")
s2B <- s2 %>%
  filter(leaftype == "brown")
s3B <- s3 %>%
  filter(leaftype == "brown")
s4B <- s4 %>%
  filter(leaftype == "brown")
s5B <- s5 %>%
  filter(leaftype == "brown")
s6B <- s6 %>%
  filter(leaftype == "brown")
s7B <- s7 %>%
  filter(leaftype == "brown")
s8B <- s8 %>%
  filter(leaftype == "brown")
s9B <- s9 %>%
  filter(leaftype == "brown")

#filtering each species green only
s1G <- s1 %>%
  filter(leaftype == "green")
s2G <- s2 %>%
  filter(leaftype == "green")
s3G <- s3 %>%
  filter(leaftype == "green")
s4G <- s4 %>%
  filter(leaftype == "green")
s5G <- s5 %>%
  filter(leaftype == "green")
s6G <- s6 %>%
  filter(leaftype == "green")
s7G <- s7 %>%
  filter(leaftype == "green")
s8G <- s8 %>%
  filter(leaftype == "green")
s9G <- s9 %>%
  filter(leaftype == "green")



####################NEED TO SWAP OUT DOC FOR P
##linear regression for species 1 Brown
lm_s1B_DOC = lm(formula = NPOC_mgC_g ~ Days_Leached, data =s1B)
summary(lm_s1B_DOC)
##linear regression for species 1 Green
lm_s1G_DOC = lm(formula = NPOC_mgC_g ~ Days_Leached, data =s1G)
summary(lm_s1G_DOC)

##linear regression for species 2 Brown
lm_s2B_DOC = lm(formula = NPOC_mgC_g ~ Days_Leached, data =s2B)
summary(lm_s2B_DOC)
##linear regression for species 2 Green
lm_s2G_DOC = lm(formula = NPOC_mgC_g ~ Days_Leached, data =s2G)
summary(lm_s2G_DOC)

##linear regression for species 3 Brown
lm_s3B_DOC = lm(formula = NPOC_mgC_g ~ Days_Leached, data =s3B)
summary(lm_s3B_DOC)
##linear regression for species 3 Green
lm_s3G_DOC = lm(formula = NPOC_mgC_g ~ Days_Leached, data =s3G)
summary(lm_s3G_DOC)

##linear regression for species 4 Brown
lm_s4B_DOC = lm(formula = NPOC_mgC_g ~ Days_Leached, data =s4B)
summary(lm_s4B_DOC)
##linear regression for species 4 Green
lm_s4G_DOC = lm(formula = NPOC_mgC_g ~ Days_Leached, data =s4G)
summary(lm_s4G_DOC)

##linear regression for species 5 Brown
lm_s5B_DOC = lm(formula = NPOC_mgC_g ~ Days_Leached, data =s5B)
summary(lm_s5B_DOC)
##linear regression for species 5 Green
lm_s5G_DOC = lm(formula = NPOC_mgC_g ~ Days_Leached, data =s5G)
summary(lm_s5G_DOC)

##linear regression for species 6 Brown
lm_s6B_DOC = lm(formula = NPOC_mgC_g ~ Days_Leached, data =s6B)
summary(lm_s6B_DOC)
##linear regression for species 6 Green
lm_s6G_DOC = lm(formula = NPOC_mgC_g ~ Days_Leached, data =s6G)
summary(lm_s6G_DOC)

##linear regression for species 7 Brown
lm_s7B_DOC = lm(formula = NPOC_mgC_g ~ Days_Leached, data =s7B)
summary(lm_s7B_DOC)
##linear regression for species 7 Green
lm_s7G_DOC = lm(formula = NPOC_mgC_g ~ Days_Leached, data =s7G)
summary(lm_s7G_DOC)

##linear regression for species 8 Brown
lm_s8B_DOC = lm(formula = NPOC_mgC_g ~ Days_Leached, data =s8B)
summary(lm_s8B_DOC)
##linear regression for species 8 Green
lm_s8G_DOC = lm(formula = NPOC_mgC_g ~ Days_Leached, data =s8G)
summary(lm_s8G_DOC)

##linear regression for species 9 Brown
lm_s9B_DOC = lm(formula = NPOC_mgC_g ~ Days_Leached, data =s9B)
summary(lm_s9B_DOC)
##linear regression for species 9 Green
lm_s9G_DOC = lm(formula = NPOC_mgC_g ~ Days_Leached, data =s9G)
summary(lm_s9G_DOC)










###Phosphate plotted against C

# Load the required libraries
library(ggplot2)
library(tidyr)

# Check if columns are numeric or character and convert if necessary
leachate.data <- mutate_if(leachate.data, is.factor, as.character)


##Creating new variable column based on leaf type saying "if my leaf type is equal to brown then call my new variable senesced, if its not equal to brown, call it fresh"
leachate.data$Leaf.Stage= ifelse(leachate.data$leaftype=="brown","Senesced", "Fresh")

##THIS GRAPH##

##DOC vs PO4
ggplot(leachate.data, aes(x = as.numeric(leachate.data$NPOC_mgC_g), y = PO4_mgP_g, color=Leaf.Stage)) +
  geom_point() + 
  scale_color_manual(values=c('seagreen','saddlebrown')) +
  guides(color = guide_legend(title = "Leaf Type")) +
  geom_smooth(method= "lm", se=FALSE) + 
  theme_bw() +
  labs(x= "DOC mg C/g leaf", y= "Phosphate (mg P per g leaf)")


###linear regression
#filtering by leaftype

brownleaves <- leachate.data %>%
  filter(leaftype == "brown")

greenleaves <- leachate.data %>%
  filter(leaftype == "green")

##linear regression for Green
lm_G_PDOC = lm(formula = PO4_mgP_g ~ NPOC_mgC_g, data =greenleaves)
summary(lm_G_PDOC)

##linear regression for Brown
lm_B_PDOC = lm(formula = PO4_mgP_g ~ NPOC_mgC_g, data =brownleaves)
summary(lm_B_PDOC)





##trying to facet by species
ggplot(leachate.data, aes(x = as.numeric(leachate.data$NPOC_mgC_g), y = PO4_mgP_g, color=Leaf.Stage)) +
  geom_point() + 
  scale_color_manual(values=c('seagreen','saddlebrown')) +
  guides(color = guide_legend(title = "Leaf Type")) +
  geom_smooth(method= "lm", se=FALSE) + 
  theme_bw() +
  labs(x= "DOC mg C/g leaf", y= "Phosphate (mg P per g leaf)") +
  facet_wrap(~Species) 







########CHLORIDE####################

## Make a new row
#create a row called leaf type
##ifelse statement= if green column values equal 1, assign "green" as new value, if not, assign it brown
leachate.data$leaftype <- ifelse(leachate.data$Green == 1, "green","brown")


#convert integer to character
leachate.data$Days_Leached <- as.character(leachate.data$Days_Leached)
str(leachate.data$Days_Leached)

###make a boxplot for Chloride conc
ggplot(leachate.data, aes(x=leaftype,  y=Cl_mgCl_g)) + geom_boxplot() + labs(x= "Leaf Type", y= "Chloride (mg Cl per g leaf")


##boxplot by leaves and species
ggplot(leachate.data, aes(x = Species, y = Cl_mgCl_g, fill = Leaf.Stage)) +
  geom_boxplot(outlier.shape = NA,  width = 0.75) + 
  theme_cowplot() +labs(x= "Species", y= "Chloride mg Cl/g leaf") + scale_fill_manual(values=c('seagreen','saddlebrown')) +
  guides(fill = guide_legend(title = "Leaf Stage")) +
  geom_point(aes(shape=Days_Leached, group = Leaf.Stage), position = position_dodge(1), alpha = 0.4, size=2) + 
  guides(shape = guide_legend(title = "Days Leached")) +
  theme(legend.text = element_text(size = 20),
        axis.text = element_text(size = 24),
        axis.title = element_text(size = 24),
        legend.title = element_text(size = 20)) ###text size for agu

###make a boxplot
#facet_wrap groups by leaves 
ggplot(leachate.data, aes(x=Days_Leached, y=Cl_mgCl_g)) + geom_boxplot() + facet_wrap(~leaftype) + theme_cowplot() +labs(x= "Days Leached", y= "Chloride (mg Cl per g leaf)")



###make a boxplot
#facet_wrap groups by leaves 
ggplot(leachate.data, aes(x=Days_Leached, y=Cl_mgCl_g)) + geom_boxplot() + facet_wrap(~leaftype) + theme_cowplot() +labs(x= "Days Leached", y= "Chloride (mg Cl per g leaf)")

#create a new dataframe that just has green leaves or brown leaves
green <- leachate.data %>%
  filter(leaftype == "green")


brown <- leachate.data %>%
  filter(leaftype == "brown")

##ANOVA for green leaves
Cl.DaysG <- lm(Cl_mgCl_g ~ Days_Leached, green)
anova(Cl.DaysG)
##days leached significant differnce but dont know which days, so do HSD test--means separation
Cl.DaysG_HSD <- HSD.test(Cl.DaysG, "Days_Leached", group = T)
Cl.DaysG_HSD 


##ANOVA for brown leaves
Cl.DaysB <- lm(Cl_mgCl_g ~ Days_Leached, brown)
anova(Cl.DaysB)
##days leached significant differnce but dont know which days, so do HSD test--means separation
Cl.DaysB_HSD <- HSD.test(Cl.DaysB, "Days_Leached", group = T)
Cl.DaysB_HSD 


#facetwrap by days
#facet_wrap groups by days 
ggplot(leachate.data, aes(x=leaftype, y=Cl_mgCl_g)) + geom_boxplot() + facet_wrap(~Days_Leached) + theme_cowplot() +labs(x= "Days Leached", y= "Chloride (mg Cl per g leaf)")


###make a boxplot for green vs brown leaves
#facet_wrap groups by species 
ggplot(leachate.data, aes(x=leaftype, y=Cl_mgCl_g)) + geom_boxplot() + facet_wrap(~Species) + theme_cowplot() +labs(x= "Leaf Type", y= "Chloride (mg Cl per g leaf)")


##ANOVA for species 
##need to filter by species
s1 <- leachate.data %>%
  filter(Species == 1)
s2 <- leachate.data %>%
  filter(Species == 2)
s3 <- leachate.data %>%
  filter(Species == 3)
s4 <- leachate.data %>%
  filter(Species == 4)
s5 <- leachate.data %>%
  filter(Species == 5)
s6 <- leachate.data %>%
  filter(Species == 6)
s7 <- leachate.data %>%
  filter(Species == 7)
s8 <- leachate.data %>%
  filter(Species == 8)
s9 <- leachate.data %>%
  filter(Species == 9)


#ANOVA for s1
cl.leaftypes1 <- lm(Cl_mgCl_g ~ leaftype, s1)
anova(cl.leaftypes1)
#significant!!
#ANOVA for s2
cl.leaftypes2 <- lm(Cl_mgCl_g ~ leaftype, s2)
anova(cl.leaftypes2)
#significant
#ANOVA for s3
cl.leaftypes3 <- lm(Cl_mgCl_g ~ leaftype, s3)
anova(cl.leaftypes3)
#Significant
#ANOVA for s4
cl.leaftypes4 <- lm(Cl_mgCl_g ~ leaftype, s4)
anova(cl.leaftypes4)
#Not significant
#ANOVA for s5
cl.leaftypes5 <- lm(Cl_mgCl_g ~ leaftype, s5)
anova(cl.leaftypes5)
#Significant!!
#ANOVA for s6
cl.leaftypes6 <- lm(Cl_mgCl_g ~ leaftype, s6)
anova(cl.leaftypes6)
# significant!!
#ANOVA for s7
cl.leaftypes7 <- lm(Cl_mgCl_g ~ leaftype, s7)
anova(cl.leaftypes7)
#not Significant
#ANOVA for s8
cl.leaftypes8 <- lm(Cl_mgCl_g ~ leaftype, s8)
anova(cl.leaftypes8)
#Significant!!
#ANOVA for s9
cl.leaftypes9 <- lm(Cl_mgCl_g ~ leaftype, s9)
anova(cl.leaftypes9)
#Not significant




####chloride Conc-----Filter by day 1
day1 <- leachate.data %>%
  filter(Days_Leached == 1)

###make a boxplot for chloride conc after 1 day
ggplot(day1, aes(x=leaftype, y=Cl_mgCl_g)) + geom_boxplot() +labs(x= "Leaf Type", y= "Chloride (mg Cl per g leaf) after 1 day")
##ANOVA for day 1
cl.Day1 <- lm(Cl_mgCl_g ~ leaftype, day1)
anova(cl.Day1)
##significant!!

###make a boxplot for day 1 green vs brown leaves
#facet_wrap groups by species 
ggplot(day1, aes(x=leaftype, y=Cl_mgCl_g)) + geom_boxplot() + facet_wrap(~Species) + theme_cowplot() +labs(x= "Leaf Type", y= "Chloride (mg Cl per g leaf) after 1 day")


##ANOVA for species 
##need to filter by species by DAY1
s1d1 <- day1 %>%
  filter(Species == 1)
s2d1 <- day1 %>%
  filter(Species == 2)
s3d1 <- day1 %>%
  filter(Species == 3)
s4d1 <- day1 %>%
  filter(Species == 4)
s5d1 <- day1 %>%
  filter(Species == 5)
s6d1 <- day1 %>%
  filter(Species == 6)
s7d1 <- day1 %>%
  filter(Species == 7)
s8d1 <- day1 %>%
  filter(Species == 8)
s9d1 <- day1 %>%
  filter(Species == 9)


#ANOVA for s1
clday1.leaftypes1 <- lm(Cl_mgCl_g ~ leaftype, s1d1)
anova(cl.leaftypes1)
# significant!!!
#ANOVA for s2
clday1.leaftypes2 <- lm(Cl_mgCl_g ~ leaftype, s2d1)
anova(clday1.leaftypes2)
# significant!!!
#ANOVA for s3
clday1.leaftypes3 <- lm(Cl_mgCl_g ~ leaftype, s3d1)
anova(clday1.leaftypes3)
#significant!
#ANOVA for s4
clday1.leaftypes4 <- lm(Cl_mgCl_g ~ leaftype, s4d1)
anova(clday1.leaftypes4)
#Significant!
#ANOVA for s5
clday1.leaftypes5 <- lm(Cl_mgCl_g ~ leaftype, s5d1)
anova(clday1.leaftypes5)
#Significant!!
#ANOVA for s6
clday1.leaftypes6 <- lm(Cl_mgCl_g ~ leaftype, s6d1)
anova(clday1.leaftypes6)
#not  significant
#ANOVA for s7
clday1.leaftypes7 <- lm(Cl_mgCl_g ~ leaftype, s7d1)
anova(clday1.leaftypes7)
# significant!!
#ANOVA for s8
clday1.leaftypes8 <- lm(Cl_mgCl_g ~ leaftype, s8d1)
anova(clday1.leaftypes8)
# Significant!!
#ANOVA for s9
clday1.leaftypes9 <- lm(Cl_mgCl_g ~ leaftype, s9d1)
anova(clday1.leaftypes9)
#Significant



######Day 3 chloride conc
####Cl Conc-----Filter by day 3
day3 <- leachate.data %>%
  filter(Days_Leached == 3)

###make a boxplot for chloride conc after day 3
ggplot(day3, aes(x=leaftype, y=Cl_mgCl_g)) + geom_boxplot() +labs(x= "Leaf Type", y= "Chloride (mg Cl per g leaf) after 3 days")
##ANOVA for day 3
cl.Day3 <- lm(Cl_mgCl_g ~ leaftype, day3)
anova(cl.Day3)
##notsignificant

###make a boxplot for day 3 green vs brown leaves
#facet_wrap groups by species 
ggplot(day3, aes(x=leaftype, y=Cl_mgCl_g)) + geom_boxplot() + facet_wrap(~Species) + theme_cowplot() +labs(x= "Leaf Type", y= "Chloride (mg Cl per g leaf) after 3 days")


##ANOVA for species 
##need to filter by species by DAY3
s1d3 <- day3 %>%
  filter(Species == 1)
s2d3 <- day3 %>%
  filter(Species == 2)
s3d3 <- day3 %>%
  filter(Species == 3)
s4d3 <- day3 %>%
  filter(Species == 4)
s5d3 <- day3 %>%
  filter(Species == 5)
s6d3 <- day3 %>%
  filter(Species == 6)
s7d3 <- day3 %>%
  filter(Species == 7)
s8d3 <- day3 %>%
  filter(Species == 8)
s9d3 <- day3 %>%
  filter(Species == 9)


#ANOVA for s1
clday3.leaftypes1 <- lm(Cl_mgCl_g ~ leaftype, s1d3)
anova(clday3.leaftypes1)
# significant!!!
#ANOVA for s2
clday3.leaftypes2 <- lm(Cl_mgCl_g ~ leaftype, s2d3)
anova(clday3.leaftypes2)
#not significant
#ANOVA for s3
clday3.leaftypes3 <- lm(Cl_mgCl_g ~ leaftype, s3d3)
anova(clday3.leaftypes3)
#significant!
#ANOVA for s4
clday3.leaftypes4 <- lm(Cl_mgCl_g ~ leaftype, s4d3)
anova(clday3.leaftypes4)
#Significant!
#ANOVA for s5
clday3.leaftypes5 <- lm(Cl_mgCl_g ~ leaftype, s5d3)
anova(clday3.leaftypes5)
#Significant!!
#ANOVA for s6
clday3.leaftypes6 <- lm(Cl_mgCl_g ~ leaftype, s6d3)
anova(clday3.leaftypes6)
#not  significant
#ANOVA for s7
clday3.leaftypes7 <- lm(Cl_mgCl_g ~ leaftype, s7d3)
anova(clday3.leaftypes7)
# significant!!
#ANOVA for s8
clday3.leaftypes8 <- lm(Cl_mgCl_g ~ leaftype, s8d3)
anova(clday3.leaftypes8)
# Significant!!
#ANOVA for s9
clday3.leaftypes9 <- lm(Cl_mgCl_g ~ leaftype, s9d3)
anova(clday3.leaftypes9)
#Significant




##########Day 7 chloride conc
####Cl Conc-----Filter by day 7
day7 <- leachate.data %>%
  filter(Days_Leached == 7)

###make a boxplot for chloride conc after day 7
ggplot(day7, aes(x=leaftype, y=Cl_mgCl_g)) + geom_boxplot() +labs(x= "Leaf Type", y= "Chloride (mg Cl per g leaf) after 7 days")
##ANOVA for day 7
cl.Day7 <- lm(Cl_mgCl_g ~ leaftype, day7)
anova(cl.Day7)
##significant!!

###make a boxplot for day 7 green vs brown leaves
#facet_wrap groups by species 
ggplot(day7, aes(x=leaftype, y=Cl_mgCl_g)) + geom_boxplot() + facet_wrap(~Species) + theme_cowplot() +labs(x= "Leaf Type", y= "Chloride (mg Cl per g leaf) after 7 days")

##ANOVA for each species DAY 7
##need to filter by species by DAY7
s1d7 <- day7 %>%
  filter(Species == 1)
s2d7 <- day7 %>%
  filter(Species == 2)
s3d7 <- day7 %>%
  filter(Species == 3)
s4d7 <- day7 %>%
  filter(Species == 4)
s5d7 <- day7 %>%
  filter(Species == 5)
s6d7 <- day7 %>%
  filter(Species == 6)
s7d7 <- day7 %>%
  filter(Species == 7)
s8d7 <- day7 %>%
  filter(Species == 8)
s9d7 <- day7 %>%
  filter(Species == 9)


#ANOVA for s1
clday7.leaftypes1 <- lm(Cl_mgCl_g ~ leaftype, s1d7)
anova(clday7.leaftypes1)
# significant!!!
#ANOVA for s2
clday7.leaftypes2 <- lm(Cl_mgCl_g ~ leaftype, s2d7)
anova(clday7.leaftypes2)
#significant!!
#ANOVA for s3
clday7.leaftypes3 <- lm(Cl_mgCl_g ~ leaftype, s3d7)
anova(clday7.leaftypes3)
#not significant
#ANOVA for s4
clday7.leaftypes4 <- lm(Cl_mgCl_g ~ leaftype, s4d7)
anova(clday7.leaftypes4)
#Significant!
#ANOVA for s5
clday7.leaftypes5 <- lm(Cl_mgCl_g ~ leaftype, s5d7)
anova(clday7.leaftypes5)
#Significant!!
#ANOVA for s6
clday7.leaftypes6 <- lm(Cl_mgCl_g ~ leaftype, s6d7)
anova(clday7.leaftypes6)
#not  significant
#ANOVA for s7
clday7.leaftypes7 <- lm(Cl_mgCl_g ~ leaftype, s7d7)
anova(clday7.leaftypes7)
# significant!!
#ANOVA for s8
clday7.leaftypes8 <- lm(Cl_mgCl_g ~ leaftype, s8d7)
anova(clday7.leaftypes8)
# Significant!!
#ANOVA for s9
clday7.leaftypes9 <- lm(Cl_mgCl_g ~ leaftype, s9d7)
anova(clday7.leaftypes9)
#Significant




############ Chloride linear regression
###LINE GRAPH FOR Cl

# Load the required libraries
library(ggplot2)
library(tidyr)

# Check if columns are numeric or character and convert if necessary
leachate.data <- mutate_if(leachate.data, is.factor, as.character)


##Creating new variable column based on leaf type saying "if my leaf type is equal to brown then call my new variable senesced, if its not equal to brown, call it fresh"
leachate.data$Leaf.Stage= ifelse(leachate.data$leaftype=="brown","Senesced", "Fresh")

##THIS GRAPH##
##trying to facet by species
ggplot(leachate.data, aes(x = as.numeric(leachate.data$Days_Leached), y = Cl_mgCl_g, color=Leaf.Stage)) +
  geom_point() + 
  scale_color_manual(values=c('seagreen','saddlebrown')) +
  guides(color = guide_legend(title = "Leaf Type")) +
  geom_smooth(method= "lm", se=FALSE) + 
  scale_x_continuous(breaks=c(0,1,3,7)) +
  theme_bw() +
  labs(x= "Days Leached", y= "Chloride (mg Cl per g leaf)") +
  facet_wrap(~Species) 



##checking if Days Leached is a number or character
str(leachate.data$Days_Leached)

##chnaging Days Lreached to be numeric
leachate.data$Days_Leached <- as.numeric(leachate.data$Days_Leached)
str(leachate.data$Days_Leached)

##need to filter by species and leaf type
s1 <- leachate.data %>%
  filter(Species == 1)
s2 <- leachate.data %>%
  filter(Species == 2)
s3 <- leachate.data %>%
  filter(Species == 3)
s4 <- leachate.data %>%
  filter(Species == 4)
s5 <- leachate.data %>%
  filter(Species == 5)
s6 <- leachate.data %>%
  filter(Species == 6)
s7 <- leachate.data %>%
  filter(Species == 7)
s8 <- leachate.data %>%
  filter(Species == 8)
s9 <- leachate.data %>%
  filter(Species == 9)

#filtering each species brown only
s1B <- s1 %>%
  filter(leaftype == "brown")
s2B <- s2 %>%
  filter(leaftype == "brown")
s3B <- s3 %>%
  filter(leaftype == "brown")
s4B <- s4 %>%
  filter(leaftype == "brown")
s5B <- s5 %>%
  filter(leaftype == "brown")
s6B <- s6 %>%
  filter(leaftype == "brown")
s7B <- s7 %>%
  filter(leaftype == "brown")
s8B <- s8 %>%
  filter(leaftype == "brown")
s9B <- s9 %>%
  filter(leaftype == "brown")

#filtering each species green only
s1G <- s1 %>%
  filter(leaftype == "green")
s2G <- s2 %>%
  filter(leaftype == "green")
s3G <- s3 %>%
  filter(leaftype == "green")
s4G <- s4 %>%
  filter(leaftype == "green")
s5G <- s5 %>%
  filter(leaftype == "green")
s6G <- s6 %>%
  filter(leaftype == "green")
s7G <- s7 %>%
  filter(leaftype == "green")
s8G <- s8 %>%
  filter(leaftype == "green")
s9G <- s9 %>%
  filter(leaftype == "green")



#####################NEED TO SWAP OUT DOC WITH CL
##linear regression for species 1 Brown
lm_s1B_DOC = lm(formula = NPOC_mgC_g ~ Days_Leached, data =s1B)
summary(lm_s1B_DOC)
##linear regression for species 1 Green
lm_s1G_DOC = lm(formula = NPOC_mgC_g ~ Days_Leached, data =s1G)
summary(lm_s1G_DOC)

##linear regression for species 2 Brown
lm_s2B_DOC = lm(formula = NPOC_mgC_g ~ Days_Leached, data =s2B)
summary(lm_s2B_DOC)
##linear regression for species 2 Green
lm_s2G_DOC = lm(formula = NPOC_mgC_g ~ Days_Leached, data =s2G)
summary(lm_s2G_DOC)

##linear regression for species 3 Brown
lm_s3B_DOC = lm(formula = NPOC_mgC_g ~ Days_Leached, data =s3B)
summary(lm_s3B_DOC)
##linear regression for species 3 Green
lm_s3G_DOC = lm(formula = NPOC_mgC_g ~ Days_Leached, data =s3G)
summary(lm_s3G_DOC)

##linear regression for species 4 Brown
lm_s4B_DOC = lm(formula = NPOC_mgC_g ~ Days_Leached, data =s4B)
summary(lm_s4B_DOC)
##linear regression for species 4 Green
lm_s4G_DOC = lm(formula = NPOC_mgC_g ~ Days_Leached, data =s4G)
summary(lm_s4G_DOC)

##linear regression for species 5 Brown
lm_s5B_DOC = lm(formula = NPOC_mgC_g ~ Days_Leached, data =s5B)
summary(lm_s5B_DOC)
##linear regression for species 5 Green
lm_s5G_DOC = lm(formula = NPOC_mgC_g ~ Days_Leached, data =s5G)
summary(lm_s5G_DOC)

##linear regression for species 6 Brown
lm_s6B_DOC = lm(formula = NPOC_mgC_g ~ Days_Leached, data =s6B)
summary(lm_s6B_DOC)
##linear regression for species 6 Green
lm_s6G_DOC = lm(formula = NPOC_mgC_g ~ Days_Leached, data =s6G)
summary(lm_s6G_DOC)

##linear regression for species 7 Brown
lm_s7B_DOC = lm(formula = NPOC_mgC_g ~ Days_Leached, data =s7B)
summary(lm_s7B_DOC)
##linear regression for species 7 Green
lm_s7G_DOC = lm(formula = NPOC_mgC_g ~ Days_Leached, data =s7G)
summary(lm_s7G_DOC)

##linear regression for species 8 Brown
lm_s8B_DOC = lm(formula = NPOC_mgC_g ~ Days_Leached, data =s8B)
summary(lm_s8B_DOC)
##linear regression for species 8 Green
lm_s8G_DOC = lm(formula = NPOC_mgC_g ~ Days_Leached, data =s8G)
summary(lm_s8G_DOC)

##linear regression for species 9 Brown
lm_s9B_DOC = lm(formula = NPOC_mgC_g ~ Days_Leached, data =s9B)
summary(lm_s9B_DOC)
##linear regression for species 9 Green
lm_s9G_DOC = lm(formula = NPOC_mgC_g ~ Days_Leached, data =s9G)
summary(lm_s9G_DOC)








####Cl plotted against 


# Load the required libraries
library(ggplot2)
library(tidyr)

# Check if columns are numeric or character and convert if necessary
leachate.data <- mutate_if(leachate.data, is.factor, as.character)


##Creating new variable column based on leaf type saying "if my leaf type is equal to brown then call my new variable senesced, if its not equal to brown, call it fresh"
leachate.data$Leaf.Stage= ifelse(leachate.data$leaftype=="brown","Senesced", "Fresh")

##THIS GRAPH##

##DOC vs PO4
ggplot(leachate.data, aes(x = as.numeric(leachate.data$NPOC_mgC_g), y = Cl_mgCl_g, color=Leaf.Stage)) +
  geom_point() + 
  scale_color_manual(values=c('seagreen','saddlebrown')) +
  guides(color = guide_legend(title = "Leaf Type")) +
    geom_smooth(method= "lm", se=FALSE) + 
  theme_bw() +
  labs(x= "DOC mg C/g leaf", y= "Chlordie (mg cl per g leaf)")


###linear regression
#filtering by leaftype

brownleaves <- leachate.data %>%
  filter(leaftype == "brown")

greenleaves <- leachate.data %>%
  filter(leaftype == "green")

##linear regression for Green
lm_G_clDOC = lm(formula = Cl_mgCl_g ~ NPOC_mgC_g, data =greenleaves)
summary(lm_G_clDOC)

##linear regression for Brown
lm_B_clDOC = lm(formula = Cl_mgCl_g ~ NPOC_mgC_g, data =brownleaves)
summary(lm_B_clDOC)





##trying to facet by species
ggplot(leachate.data, aes(x = as.numeric(leachate.data$NPOC_mgC_g), y = Cl_mgCl_g, color=Leaf.Stage)) +
  geom_point() + 
  scale_color_manual(values=c('seagreen','saddlebrown')) +
  guides(color = guide_legend(title = "Leaf Type")) +
  geom_smooth(method= "lm", se=FALSE) + 
  theme_bw() +
  labs(x= "DOC mg C/g leaf", y= "Chloride (mg Cl per g leaf)") +
  facet_wrap(~Species) 

















#######MG####

##boxplot by leaves and species
ggplot(leachate.data, aes(x = Species, y = Mg_mgMg_g, fill = Leaf.Stage)) +
  geom_boxplot(outlier.shape = NA,  width = 0.75) + 
  theme_cowplot() +labs(x= "Species", y= "Magnesium mg Mg/g leaf") + scale_fill_manual(values=c('seagreen','saddlebrown')) +
  guides(fill = guide_legend(title = "Leaf Stage")) +
  geom_point(aes(shape=Days_Leached, group = Leaf.Stage), position = position_dodge(1), alpha = 0.4, size=2) + 
  guides(shape = guide_legend(title = "Days Leached")) +
  theme(legend.text = element_text(size = 20),
        axis.text = element_text(size = 24),
        axis.title = element_text(size = 24),
        legend.title = element_text(size = 20)) ###text size for agu



##ANOVA for species 
##need to filter by species
s1 <- leachate.data %>%
  filter(Species == 1)
s2 <- leachate.data %>%
  filter(Species == 2)
s3 <- leachate.data %>%
  filter(Species == 3)
s4 <- leachate.data %>%
  filter(Species == 4)
s5 <- leachate.data %>%
  filter(Species == 5)
s6 <- leachate.data %>%
  filter(Species == 6)
s7 <- leachate.data %>%
  filter(Species == 7)
s8 <- leachate.data %>%
  filter(Species == 8)
s9 <- leachate.data %>%
  filter(Species == 9)


##########Day 7 chloride conc
####Cl Conc-----Filter by day 7
day7 <- leachate.data %>%
  filter(Days_Leached == 7)


##ANOVA for each species DAY 7
##need to filter by species by DAY7
s1d7 <- day7 %>%
  filter(Species == 1)
s2d7 <- day7 %>%
  filter(Species == 2)
s3d7 <- day7 %>%
  filter(Species == 3)
s4d7 <- day7 %>%
  filter(Species == 4)
s5d7 <- day7 %>%
  filter(Species == 5)
s6d7 <- day7 %>%
  filter(Species == 6)
s7d7 <- day7 %>%
  filter(Species == 7)
s8d7 <- day7 %>%
  filter(Species == 8)
s9d7 <- day7 %>%
  filter(Species == 9)


#ANOVA for s1
mgday7.leaftypes1 <- lm(Mg_mgMg_g ~ leaftype, s1d7)
anova(mgday7.leaftypes1)
# significant!!!
#ANOVA for s2
mgday7.leaftypes2 <- lm(Mg_mgMg_g ~ leaftype, s2d7)
anova(mgday7.leaftypes2)
#not significant
#ANOVA for s3
mgday7.leaftypes3 <- lm(Mg_mgMg_g ~ leaftype, s3d7)
anova(mgday7.leaftypes3)
#not significant
#ANOVA for s4
mgday7.leaftypes4 <- lm(Mg_mgMg_g ~ leaftype, s4d7)
anova(mgday7.leaftypes4)
#Significant!
#ANOVA for s5
mgday7.leaftypes5 <- lm(Mg_mgMg_g ~ leaftype, s5d7)
anova(mgday7.leaftypes5)
#Significant!!
#ANOVA for s6
mgday7.leaftypes6 <- lm(Mg_mgMg_g ~ leaftype, s6d7)
anova(mgday7.leaftypes6)
#not  significant
#ANOVA for s7
mgday7.leaftypes7 <- lm(Mg_mgMg_g ~ leaftype, s7d7)
anova(mgday7.leaftypes7)
# significant!!
#ANOVA for s8
mgday7.leaftypes8 <- lm(Mg_mgMg_g ~ leaftype, s8d7)
anova(mgday7.leaftypes8)
# Significant!!
#ANOVA for s9
mgday7.leaftypes9 <- lm(Mg_mgMg_g ~ leaftype, s9d7)
anova(mgday7.leaftypes9)
#Significant




#####Na####
##boxplot by leaves and species
ggplot(leachate.data, aes(x = Species, y = Na_mgNa_g, fill = Leaf.Stage)) +
  geom_boxplot(outlier.shape = NA,  width = 0.75) + 
  theme_cowplot() +labs(x= "Species", y= "Sodium mg Na/g leaf") + scale_fill_manual(values=c('seagreen','saddlebrown')) +
  guides(fill = guide_legend(title = "Leaf Stage")) +
  geom_point(aes(shape=Days_Leached, group = Leaf.Stage), position = position_dodge(1), alpha = 0.4, size=2) + 
  guides(shape = guide_legend(title = "Days Leached")) +
  theme(legend.text = element_text(size = 20),
        axis.text = element_text(size = 24),
        axis.title = element_text(size = 24),
        legend.title = element_text(size = 20)) ###text size for agu



##ANOVA for species 
##need to filter by species
s1 <- leachate.data %>%
  filter(Species == 1)
s2 <- leachate.data %>%
  filter(Species == 2)
s3 <- leachate.data %>%
  filter(Species == 3)
s4 <- leachate.data %>%
  filter(Species == 4)
s5 <- leachate.data %>%
  filter(Species == 5)
s6 <- leachate.data %>%
  filter(Species == 6)
s7 <- leachate.data %>%
  filter(Species == 7)
s8 <- leachate.data %>%
  filter(Species == 8)
s9 <- leachate.data %>%
  filter(Species == 9)


##########Day 7 sodium conc
####Cl Conc-----Filter by day 7
day7 <- leachate.data %>%
  filter(Days_Leached == 7)


##ANOVA for each species DAY 7
##need to filter by species by DAY7
s1d7 <- day7 %>%
  filter(Species == 1)
s2d7 <- day7 %>%
  filter(Species == 2)
s3d7 <- day7 %>%
  filter(Species == 3)
s4d7 <- day7 %>%
  filter(Species == 4)
s5d7 <- day7 %>%
  filter(Species == 5)
s6d7 <- day7 %>%
  filter(Species == 6)
s7d7 <- day7 %>%
  filter(Species == 7)
s8d7 <- day7 %>%
  filter(Species == 8)
s9d7 <- day7 %>%
  filter(Species == 9)


#ANOVA for s1
naday7.leaftypes1 <- lm(Na_mgNa_g ~ leaftype, s1d7)
anova(naday7.leaftypes1)
#not significant
#ANOVA for s2
naday7.leaftypes2 <- lm(Na_mgNa_g ~ leaftype, s2d7)
anova(naday7.leaftypes2)
# significant!!
#ANOVA for s3
naday7.leaftypes3 <- lm(Na_mgNa_g ~ leaftype, s3d7)
anova(naday7.leaftypes3)
# significant
#ANOVA for s4
naday7.leaftypes4 <- lm(Na_mgNa_g ~ leaftype, s4d7)
anova(naday7.leaftypes4)
#Significant!
#ANOVA for s5
naday7.leaftypes5 <- lm(Na_mgNa_g ~ leaftype, s5d7)
anova(naday7.leaftypes5)
#Significant!!
#ANOVA for s6
naday7.leaftypes6 <- lm(Na_mgNa_g ~ leaftype, s6d7)
anova(naday7.leaftypes6)
#not  significant
#ANOVA for s7
naday7.leaftypes7 <- lm(Na_mgNa_g ~ leaftype, s7d7)
anova(naday7.leaftypes7)
# NOT significant
#ANOVA for s8
naday7.leaftypes8 <- lm(Na_mgNa_g ~ leaftype, s8d7)
anova(naday7.leaftypes8)
# Significant!!
#ANOVA for s9
naday7.leaftypes9 <- lm(Na_mgNa_g ~ leaftype, s9d7)
anova(naday7.leaftypes9)
#NOT Significant




########other plots#######


###make a boxplot for percent leaf lost due to N
ggplot(leachate.data, aes(x=leaftype, y=Percent_LeafLostDueN)) + geom_boxplot()
##ANOVA for green leaves
LossN <- lm(Percent_LeafLostDueN ~ leaftype, leachate.data)
anova(LossN)
##NOT significant-- also this data is werid and has gaps in the excel sheet

#facet_wrap groups by leaves 
ggplot(leachate.data, aes(x=Days_Leached, y=Percent_LeafLostDueN)) + geom_boxplot() + facet_wrap(~leaftype) + theme_cowplot() +labs(x= "Days Leached", y= "% Leaf Lost due to N")



###make a boxplot percent leaf lost due to C
ggplot(leachate.data, aes(x=leaftype, y=Percent_LeafLostDueC)) + geom_boxplot()
#facet_wrap groups by leaves 
ggplot(leachate.data, aes(x=Days_Leached, y=Percent_LeafLostDueC)) + geom_boxplot() + facet_wrap(~leaftype) + theme_cowplot() +labs(x= "Days Leached", y= "% Leaf Lost due to C")
#facet_wrap groups by species 
ggplot(leachate.data, aes(x=leaftype, y=Percent_LeafLostDueC)) + geom_boxplot() + facet_wrap(~Species) + theme_cowplot() +labs(x= "Leaf Type", y= "% Leaf Lost due to C")

