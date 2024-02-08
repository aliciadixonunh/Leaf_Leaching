#Ordination for leaf data
#going off of Kaye's example ordination from lab meeting 8/31

#find out which method(s) of ordination are appropriate for your data and questions, this website rules:
# http://ordination.okstate.edu/overview.htm

#pretty much all can be done in the package vegan and with your data in the same kind of format

library(tidyverse)
library(vegan)
library(ggplot2)
library(ggpubr)
library(ggfortify)
library(ggrepel)

## type the path to where your files are saved
setwd("C:/Users/dixon/Dropbox (Personal)/My PC (DESKTOP-U34IQV9)/Documents/Grad School/Lab/Leaf Leaching")

##upload csv file
pcadata <- read.csv("./data/pcadata.csv")





## Make a new row
#create a row called leaf type
##ifelse statement= if green column values equal 1, assign "green" as new value, if not, assign it brown
pcadata$leaftype <- ifelse(pcadata$Green == 1, "green","brown")
##Creating new variable column based on leaf type saying "if my leaf type is equal to brown then call my new variable senesced, if its not equal to brown, call it fresh"
pcadata$Leaf.Stage= ifelse(pcadata$leaftype=="brown","Senesced", "Fresh")



##detection limits

####Making new rows to fix for detection limits
#Filter by detection limit and assign 1/2 value

#TDN = 0.05 	? = 0.025
pcadata$TDN.dl = as.numeric(ifelse(pcadata$TDN_mgN_g <= 0.05, 0.025, pcadata$TDN_mgN_g))

#DOC = 0.10   ? = 0.5
pcadata$DOC.dl = as.numeric(ifelse(pcadata$NPOC_mgC_g <= 0.1, 0.5, pcadata$NPOC_mgC_g))

#NO3 = 0.004	? = 0.002
pcadata$NO3.dl = as.numeric(ifelse(pcadata$NO3_mgN_g <= 0.005, 0.0025, pcadata$NO3_mgN_g))

#NH4 = 0.004	? = 0.002
pcadata$NH4.dl = as.numeric(ifelse(pcadata$NH4_mgN_g <= 0.004, 0.002, pcadata$NH4_mgN_g))

#calculating DIN
pcadata$DIN_calc = pcadata$NH4.dl+pcadata$NO3.dl

#calculating DON
pcadata$DON_calc = pcadata$TDN.dl-(pcadata$NO3.dl + pcadata$NH4.dl)

#DON = 0.01 	? = 0.005
pcadata$DON.dl = as.numeric(ifelse(pcadata$DON_calc <= 0.01, 0.005, pcadata$DON_calc))

#DON/TDN percentage
pcadata$DON_percentage = as.numeric(pcadata$DON.dl/pcadata$TDN.dl)*100

#PO4 = 0.002 	? = 0.001
pcadata$PO4.dl = as.numeric(ifelse(pcadata$PO4_mgP_g <= 0.002, 0.001, pcadata$PO4_mgP_g))

#Ca = 0.1 	? = 0.05
pcadata$Ca.dl = as.numeric(ifelse(pcadata$Ca_mgCa_g <= 0.1, 0.05, pcadata$Ca_mgCa_g))



####Fixing mass loss now
##renaming old percent TML column
pcadata <- pcadata %>% 
  rename("TML_old" = "percentTotalMassLost")

##renaming old Mass loss column
pcadata <- pcadata %>% 
  rename("ML_mg_old" = "MassLost_mglost_gleaf")

#New Mass Loss (mg lost per g of leaf)
pcadata <- pcadata %>% 
  mutate(MassLoss = rowSums(pcadata[ ,c(19, 21:24, 39:42, 47:48)]))

#New % Total Mass Loss
pcadata <- pcadata %>% 
  mutate(perTML = pcadata$MassLoss/pcadata$DryWeight_g/1000*100)


###log transforming

#DOC log 10
pcadata$DOC_log <- log10(pcadata$DOC.dl)
#TDN log 10
pcadata$TDN_log <- log10(pcadata$TDN.dl)


#TML
pcadata$TML_asqrt <- sqrt(asin((pcadata$perTML/100)))





#create a new dataframe that just has green leaves or brown leaves
greenPCA <- pcadata %>%
  filter(leaftype == "green")


brownPCA <- pcadata %>%
  filter(leaftype == "brown")

#remove categorical data
datnocat <- pcadata[,c(1, 12:14, 19, 21:24, 47:48, 51:53)]
datnocat$ID <- as.character(datnocat$ID)
rownames(datnocat) <- datnocat$ID #renaming R row names to match ID number/names, special case because my ID column matched R row numbers anyway
#now remove ID column from workbook
datnocat <- datnocat[, c(2:ncol(datnocat))]



##skewnesss and kurtosis
library(moments)

skew_value <- skewness(datnocat)
cat("Skewness:", skew_value, "/n")

kurtosis_value <- kurtosis(datnocat)
cat("Kurtosis:", kurtosis_value, "/n")

##Alicia data in PCA

leafpca <- prcomp(datnocat, center=TRUE, scale. = TRUE) #prcomp http://127.0.0.1:43731/graphics/plot_zoom_png?width=1315&height=722uses q mode decomposition - singular value decomposition on centered and scaled data - not the covariance matrix
#both should usually produce the same result. moving on with prcomp
pca.var <- leafpca$sdev^2
pca.var.per <- round(pca.var/sum(pca.var)*100, 2)
barplot(pca.var.per, xlab="Principal Component", ylab = "Percent Variation")

pcaall <- as.data.frame(leafpca$x)
pcaall$ID <- rownames(pcaall)
pcameta <- merge(pcaall, pcadata, by = "ID")
pcameta$Species <- as.character(pcameta$Species)
pcameta$Days_Leached <- as.character(pcameta$Days_Leached)
pcameta$Brown <- as.character(pcameta$Brown)
pcameta$Green <- as.character(pcameta$Green)


ggplot(data=pcameta, aes(x=PC1, y=PC2, color=Species, shape=Leaf.Stage)) + geom_point(size=2) + xlab(paste("PC1 - ", pca.var.per[1], "%", sep="")) +
  ylab(paste("PC2 - ", pca.var.per[2], "%", sep = "")) + 
  guides(shape = guide_legend(title = "Leaf Stage")) +
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) 


##biplot loadings
biplot(leafpca)



#if you want the variables in your pca ranked by their importance/loading scores do this for each axis (1 here from rotation is for PC1, 2 for PC2, etc)
loading_scores <- leafpca$rotation[,1:2]
var_scores <- abs(loading_scores) #scores are between -1 and 1, the closer the absolute value is to 1 the more improtant var is in this axis
varscoreranked <- sort(var_scores, decreasing = TRUE)
top_10_var <- names(varscoreranked[1:10])
var_withvals <- leafpca$rotation[top_10_var,1:2]
# pc1varscores <- as.data.frame(var_withvals)
# pc1varscores$Variable <- rownames(pc1varscores)
# PC1varscores <- ggtexttable(pc1varscores, rows = NULL)


#make the laoding scores into a data frame and add a column with variable names
arrows <- as.data.frame(loading_scores)
arrows$varname <- rownames(arrows)

###adding loading vectors
#look at var_scores and manually choose which ones I want on my plot
#look at column names of pcameta and subset it to only include the columns I want to include on my plot

vars_ofinterest <- c("TDN.dl", "NPOC_mgC_g", "perTML", "Percent_LeafLostDueC", "percent_MassLoss_C", "Cl_mgCl_g", "MassLost", "Mg_mgMg_g", "percent_MassLost_C", "Ca.dl", "Na_mgMg_g", "DOC_log", "TDN_log", "TML_asqrt")

arrows2 <- subset(arrows, varname %in% vars_ofinterest)

#adding the loading variables I want (loadingvars) on top of the pca plot
ggplot(data=pcameta, aes(x=PC1, y=PC2, color=Species, shape=Leaf.Stage)) + geom_point(size=2) + xlab(paste("PC1 - ", pca.var.per[1], "%", sep="")) +
  ylab(paste("PC2 - ", pca.var.per[2], "%", sep = "")) + 
  guides(shape = guide_legend(title = "Leaf Stage")) +
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  geom_segment(data=arrows2, aes(x = 0, xend = PC1, y = 0, yend= PC2), arrow = arrow(length = unit(0.3, "cm")), color="black") +
  geom_text(data = arrows2, aes(x = PC1, y = PC2, label = varname), size = 3) + theme_classic()
  

###Kaye work
library(ggrepel)
library(ggalt)

ggplot(pcameta) +
  geom_point(mapping=aes(x=PC1, y=PC2, color=Species, shape=Leaf.Stage), size=2) + 
  geom_encircle(aes(x = PC1, y = PC2, group = Species, fill = Species), color = "black", alpha = 0.1) +
  xlab(paste("PC1 - ", pca.var.per[1], "%", sep="")) + ylab(paste("PC2 - ", pca.var.per[2], "%", sep = "")) +
  geom_segment(data=arrows2,
               aes(x = 0, xend = 10*PC1, y = 0, yend= 10*PC2), 
               arrow = arrow(length = unit(0.3, "cm")), color="black") +
  geom_text_repel(data = arrows2, aes(x = 10*PC1, y = 10*PC2, label = varname), size = 3) + 
  theme_classic() +
  guides(shape = guide_legend(title = "Leaf Stage")) +
  theme(strip.text = element_text(colour = 'black'),axis.line = element_line(colour = "black"),
        text = element_text(size=11),legend.text=element_text(size=9))




#loading significance?
library(loadings)
pca_leachate <- pca_loading(leafpca)
pca_leachate$loading$R   #PC loading
pca_leachate$loading$p.value    #p-value




#####only green leaves
#create a new dataframe that just has green leaves or brown leaves
greenPCA <- pcadata %>%
  filter(leaftype == "green")


#remove categorical data
Gdatnocat <- greenPCA[,c(1, 12:14, 19, 21:24, 47:48, 51:53)]
Gdatnocat$ID <- as.character(Gdatnocat$ID)
rownames(Gdatnocat) <- Gdatnocat$ID #renaming R row names to match ID number/names, special case because my ID column matched R row numbers anyway
#now remove ID column from workbook
Gdatnocat <- Gdatnocat[, c(2:ncol(Gdatnocat))]



##Alicia data in PCA

Gleafpca <- prcomp(Gdatnocat, center=TRUE, scale. = TRUE) #prcomp http://127.0.0.1:43731/graphics/plot_zoom_png?width=1315&height=722uses q mode decomposition - singular value decomposition on centered and scaled data - not the covariance matrix
#both should usually produce the same result. moving on with prcomp
Gpca.var <- Gleafpca$sdev^2
Gpca.var.per <- round(Gpca.var/sum(Gpca.var)*100, 2)
barplot(Gpca.var.per, xlab="Principal Component", ylab = "Percent Variation")

Gpcaall <- as.data.frame(Gleafpca$x)
Gpcaall$ID <- rownames(Gpcaall)
Gpcameta <- merge(Gpcaall, greenPCA, by = "ID")
Gpcameta$Species <- as.character(Gpcameta$Species)
Gpcameta$Days_Leached <- as.character(Gpcameta$Days_Leached)



ggplot(data=Gpcameta, aes(x=PC1, y=PC2, color=Species)) + geom_point(size=2) + xlab(paste("PC1 - ", Gpca.var.per[1], "%", sep="")) +
  ylab(paste("PC2 - ", Gpca.var.per[2], "%", sep = "")) + 
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) 


##biplot loadings
biplot(Gleafpca)



#if you want the variables in your pca ranked by their importance/loading scores do this for each axis (1 here from rotation is for PC1, 2 for PC2, etc)
Gloading_scores <- Gleafpca$rotation[,1:2]
Gvar_scores <- abs(Gloading_scores) #scores are between -1 and 1, the closer the absolute value is to 1 the more improtant var is in this axis
Gvarscoreranked <- sort(Gvar_scores, decreasing = TRUE)
Gtop_10_var <- names(Gvarscoreranked[1:10])
Gvar_withvals <- Gleafpca$rotation[top_10_var,1:2]
# pc1varscores <- as.data.frame(var_withvals)
# pc1varscores$Variable <- rownames(pc1varscores)
# PC1varscores <- ggtexttable(pc1varscores, rows = NULL)


#make the laoding scores into a data frame and add a column with variable names
Garrows <- as.data.frame(Gloading_scores)
Garrows$Gvarname <- rownames(Garrows)

###adding loading vectors
#look at var_scores and manually choose which ones I want on my plot
#look at column names of pcameta and subset it to only include the columns I want to include on my plot

Gvars_ofinterest <- c("TDN.dl", "NPOC_mgC_g", "perTML", "Percent_LeafLostDueC", "percent_MassLoss_C", "Cl_mgCl_g", "MassLost", "Mg_mgMg_g", "Na_mgNa_g", "Ca.dl", "Na_mgMg_g", "DOC_log", "TDN_log", "TML_asqrt")

Garrows2 <- subset(Garrows, Gvarname %in% Gvars_ofinterest)

###Kaye work
library(ggrepel)
library(ggalt)

ggplot(Gpcameta) +
  geom_point(mapping=aes(x=PC1, y=PC2, color=Species), size=2) + 
  geom_encircle(aes(x = PC1, y = PC2, group = Species, fill = Species), color = "black", alpha = 0.1) +
  xlab(paste("PC1 - ", Gpca.var.per[1], "%", sep="")) + ylab(paste("PC2 - ", Gpca.var.per[2], "%", sep = "")) +
  geom_segment(data=Garrows2,
               aes(x = 0, xend = 10*PC1, y = 0, yend= 10*PC2), 
               arrow = arrow(length = unit(0.3, "cm")), color="black") +
  geom_text_repel(data = Garrows2, aes(x = 10*PC1, y = 10*PC2, label = Gvarname), size = 3) + 
  theme_classic() +
  theme(strip.text = element_text(colour = 'black'),axis.line = element_line(colour = "black"),
        text = element_text(size=11),legend.text=element_text(size=9))



##only brown leaves
#####only green leaves
#create a new dataframe that just has green leaves or brown leaves
brownPCA <- pcadata %>%
  filter(leaftype == "brown")


#remove categorical data
Bdatnocat <- brownPCA[,c(1, 12:14, 19, 21:24, 47:48, 51:53)]
Bdatnocat$ID <- as.character(Bdatnocat$ID)
rownames(Bdatnocat) <- Bdatnocat$ID #renaming R row names to match ID number/names, special case because my ID column matched R row numbers anyway
#now remove ID column from workbook
Bdatnocat <- Bdatnocat[, c(2:ncol(Bdatnocat))]



##Alicia data in PCA

Bleafpca <- prcomp(Bdatnocat, center=TRUE, scale. = TRUE) #prcomp http://127.0.0.1:43731/graphics/plot_zoom_png?width=1315&height=722uses q mode decomposition - singular value decomposition on centered and scaled data - not the covariance matrix
#both should usually produce the same result. moving on with prcomp
Bpca.var <- Bleafpca$sdev^2
Bpca.var.per <- round(Bpca.var/sum(Bpca.var)*100, 2)
barplot(Bpca.var.per, xlab="Principal Component", ylab = "Percent Variation")

Bpcaall <- as.data.frame(Bleafpca$x)
Bpcaall$ID <- rownames(Bpcaall)
Bpcameta <- merge(Bpcaall, brownPCA, by = "ID")
Bpcameta$Species <- as.character(Bpcameta$Species)
Bpcameta$Days_Leached <- as.character(Bpcameta$Days_Leached)



ggplot(data=Bpcameta, aes(x=PC1, y=PC2, color=Species)) + geom_point(size=2) + xlab(paste("PC1 - ", Bpca.var.per[1], "%", sep="")) +
  ylab(paste("PC2 - ", Bpca.var.per[2], "%", sep = "")) + 
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) 


##biplot loadings
biplot(Bleafpca)



#if you want the variables in your pca ranked by their importance/loading scores do this for each axis (1 here from rotation is for PC1, 2 for PC2, etc)
Bloading_scores <- Bleafpca$rotation[,1:2]
Bvar_scores <- abs(Bloading_scores) #scores are between -1 and 1, the closer the absolute value is to 1 the more improtant var is in this axis
Bvarscoreranked <- sort(Bvar_scores, decreasing = TRUE)
Btop_10_var <- names(Bvarscoreranked[1:10])
Bvar_withvals <- Bleafpca$rotation[top_10_var,1:2]
# pc1varscores <- as.data.frame(var_withvals)
# pc1varscores$Variable <- rownames(pc1varscores)
# PC1varscores <- ggtexttable(pc1varscores, rows = NULL)


#make the laoding scores into a data frame and add a column with variable names
Barrows <- as.data.frame(Bloading_scores)
Barrows$Bvarname <- rownames(Barrows)

###adding loading vectors
#look at var_scores and manually choose which ones I want on my plot
#look at column names of pcameta and subset it to only include the columns I want to include on my plot

Bvars_ofinterest <- c("TDN.dl", "NPOC_mgC_g", "perTML", "Percent_LeafLostDueC", "percent_MassLoss_C", "Cl_mgCl_g", "MassLost", "Mg_mgMg_g", "Na_mgNa_g", "Ca.dl", "Na_mgMg_g", "DOC_log", "TDN_log", "TML_asqrt")

Barrows2 <- subset(Barrows, Bvarname %in% Bvars_ofinterest)

###Kaye work
library(ggrepel)
library(ggalt)

ggplot(Bpcameta) +
  geom_point(mapping=aes(x=PC1, y=PC2, color=Species), size=2) + 
  geom_encircle(aes(x = PC1, y = PC2, group = Species, fill = Species), color = "black", alpha = 0.1) +
  xlab(paste("PC1 - ", Bpca.var.per[1], "%", sep="")) + ylab(paste("PC2 - ", Bpca.var.per[2], "%", sep = "")) +
  geom_segment(data=Barrows2,
               aes(x = 0, xend = 10*PC1, y = 0, yend= 10*PC2), 
               arrow = arrow(length = unit(0.3, "cm")), color="black") +
  geom_text_repel(data = Barrows2, aes(x = 10*PC1, y = 10*PC2, label = Bvarname), size = 3) + 
  theme_classic() +
  theme(strip.text = element_text(colour = 'black'),axis.line = element_line(colour = "black"),
        text = element_text(size=11),legend.text=element_text(size=9))










# geom_text(data = arrows2, aes(x = 4*PC1, y = 4*PC2, label = varname), size = 3) + #commented this out bc i used ggrepel function on nnext line






#trying autoplot
#didnt work
autoplot(leafpca, data=loadingvars, color='Species', shape='Leaf.Stage', loadings=TRUE, loadings.label=TRUE)














####NMDS


#remove categorical data
leafpca <- pcadata[,c(1, 12:14, 19, 21:24, 47:48, 51:53)]
datnocat$ID <- as.character(datnocat$ID)
rownames(datnocat) <- datnocat$ID #renaming R row names to match ID number/names, special case because my ID column matched R row numbers anyway
#now remove ID column from workbook
datnocat <- datnocat[, c(2:ncol(datnocat))]


#NMDS plot
set.seed(123) #set seed for reproducibility
distancemat <- vegdist(pcadata, distance="bray") #nmdscomm is a community matrix. rownames are sample IDs and columns are species with abundance data by sample.
#vegdist just calculates our distance matrix. as you can see it's not necessary for the ordination but we will keep it as a separate object so you can use it later for other stuff

ordination <- metaMDS(nmdscomm, distance="bray", k=2) #look at metaMDS documentation to learn more about what you might want to include parameter-wise here.
#you can technically just plot(ordination) but the default is ugly as sin. the following lines get the data in the proper format for plotting w ggplot.

scrs <- as.data.frame(scores(ordination, display = "sites"))
#scrsspp <- as.data.frame(scores(ordination, display = "species")) #you can plot either by 'sites' (sample ID - more common) or by the species. read about NMDS options at the link above

scrs$SampleID <- rownames(scrs) #just getting sample IDs so i can merge with metadata for plotting
scrs1 <- merge(scrs, metadata, "SampleID")

nmdsplot = ggplot(data = scrs1, aes(x = NMDS1, y = NMDS2)) + 
  geom_point(data = scrs1, aes(colour = SeedType), size = 3, alpha = 0.5) + 
  theme(axis.title = element_text(size = 10, face = "bold", colour = "grey30"), 
        panel.background = element_blank(), panel.border = element_rect(fill = NA, colour = "grey30"), 
        axis.ticks = element_blank(), axis.text = element_blank(), legend.key = element_blank(), 
        legend.title = element_text(size = 10, face = "bold", colour = "grey30"), 
        legend.text = element_text(size = 9, colour = "grey30")) +
  labs(colour = "Seed Type")
nmdsplot


#test for statistical significance of separation of samples by variable of interest using PERMANOVA (adonis function)
set.seed(111)
adonis(distancemat ~ SeedType, data = metadata) #ok this is an incredibly simple formula. you can test multiple variables using + and itneractions between variables using *
#you can also stratify by site or something using strata=site or something. you can nest variables, etc. check ?adonis

#do we meet the assumptions of a permanova? run beta dispersion analysis to confirm homogeneity of variance
testbd <- betadisper(distancemat, metadata$SeedType)
TukeyHSD(testbd) #you can also boxplot them. if only two levels just use anova. you do NOT want a significant p value here!!! that would mean the dispersion is heterogeneous across groups, violating assumptions of a PERMANOVA

#assuming your NMDS looks nice enough and you want to see how environmental vars covary with your communities in ordination space, use envfit...
#envfit uses multiple regression to fit yout environmental variables (continuous or categorical) as vectors (or factors if categorical) to our ordination space
set.seed(123)
ordfit <- envfit(ordination, newmeta2, perm=999) #newmeta was a subset of metadata containing only the vars i want here. you can also specify a formula
#somevars_fit <- envfit(ordination ~ soilpH + soilMoisture +wssPrecipTotal+wssTempTripleMean, newmeta2, perm = 999, na.rm=TRUE) #here's an example if you want to specify
ordfit #this gives you a summary of the vectors and whether theyre significant or not

B <- as.list(ordfit$vectors) #get the vector info
arrows2<-as.data.frame(B$arrows*sqrt(B$r)) #scale the vectors by their strength of correlation
pvals2 <- as.data.frame(B$pvals) #get their p vals
D <- cbind(arrows2, pvals2) #df for plotting them
Dred <- subset(D, B$pvals<0.05) #only keep significant if you want
Dred$varname <- rownames(Dred) #

nmds_wvecs <- ggplot(scrs1) +
  geom_point(mapping=aes(x=NMDS1, y=NMDS2, color=SeedType)) + 
  coord_fixed() +
  geom_segment(data=Dred,
               aes(x = 0, xend = 2*NMDS1, y = 0, yend= 2*NMDS2), #NOTE!! i scaled the vectors to be a little bigger since the strength of correlation was small in this case. remove the 2* for normal
               arrow = arrow(length = unit(0.3, "cm")), color="black") +
  geom_text(data = Dred, aes(x = 2*NMDS1-0.1, y = (2*NMDS2-0.1), label = varname), #again, moved the text a little away from the end of the vectors here (scaled by 2, then changed coord position by 0.1)
            size = 3) + theme_classic() +
  #expand_limits(x=1.5) +
  theme(strip.text = element_text(colour = 'black'),axis.line = element_line(colour = "black"),
        text = element_text(size=11),legend.text=element_text(size=9), legend.title = element_blank()) 
#i later learned of ggrepel, which can make the labels just kinda bounce around instead of manually moving them around like i did with the 0.1 
#library(ggrepel)
#just add + geom_label_repel() as a line within the ggplot code and it should work

#you can also just make it so the vectors fit nicely in the plot area using vegan's ordiArrowMul() function. skip everything after making ordfit and jump to this:
env_coords = as.data.frame(scores(ordfit, "vectors")) * ordiArrowMul(ordfit)

nmds_wvecs2 <- ggplot(scrs1) +
  geom_point(mapping=aes(x=NMDS1, y=NMDS2, color=SeedType)) + 
  coord_fixed() +
  geom_segment(aes(x = 0, y = 0, xend = NMDS1, yend = NMDS2), 
               data = env_coords, size =1, alpha = 0.5, color = "grey30")  +
  geom_text(data = env_coords, aes(x = NMDS1, y = NMDS2), colour = "grey30", 
            fontface = "bold", label = varname) + theme_classic() + 
  #expand_limits(x=1.5) + #use this if you want more space i used it bc my labels were getting cut off 
  theme(strip.text = element_text(colour = 'black'),axis.line = element_line(colour = "black"),
        text = element_text(size=11),legend.text=element_text(size=9), legend.title = element_blank()) 

#you can facet NMDS plots too but keep in mind each time you run an nmds the ordination space can change, as the order of axes is arbitrary -- this is described well in the link above.
#read about constrained vs unconstrained ordination to understand better, too
#so think about what the difference is between ordinating your data as a whole and plotting with facets by site or something, versus ordinating individually by site and plotting. How might they differ?




###
#distance based redundancy analysis -- a different form of ordination where the axes/ordination space are built using environmental variables
#this can be used as part of a variation partitioning analysis  -- in fact they are usually paired in analysis
#reading about RDA will help understand, the only diff between RDA and dbRDA is distance-based allows use of non-euclidean distance matrices (i.e. bray curtis)
#if your question is in regards to whether environmental variables have significant effect on the dissimilarities in your community data, i.e.
# does surface water chemistry have significant effect on shifts in algal composition across samples?
#then this is appropriate :)

amf.dbRDA <- capscale(commmatrix ~ ., data=metadata[,c(6,15:30,33:34)], distance = "bray") #here i'm taking community matrix and its bray-curtis dissimilarity matrix and ordinating using dbRDA/capscale against the vars selected in my metadata
#alternatively instead of ~. you can give it an actual formula such as
saps.dbRDA <- dbrda(formula = commmatrix ~ pH + Ti + S + Si + Ba + pyc_stock + mac_stock + poc_stocks, data = metadata, distance = "bray")
#note that there's capscale and also dbrda as functions here. they differ in how they handle negative eigenvalues. 

summary(amf.dbRDA) #gives a ton of info about the model
anova(amf.dbRDA, permutations = how(nperm = 999)) #is the model significant as a whole
anova(amf.dbRDA, permutations = how(nperm = 999), by = "axis") #do the axes significantly explain variance
anova(amf.dbRDA, permutations = how(nperm = 999), by = "terms") #what abotu the different metadata vars

#if you want to know the percent variation explained by each axis this helps
library(ggord)
ggord(amf.dbRDA, amf.dbRDA.site$site)

# using ggplot2 to make figures
amf.dbRDA.scaling1 <- summary(amf.dbRDA, scaling = 1) # extract scaling 1
amf.dbRDA.site <- data.frame(amf.dbRDA.scaling1$sites)[, 1:2]
amf.dbRDA.env <- data.frame(amf.dbRDA.scaling1$biplot)[, 1:2] #if you want vectors for env vars that built ordination spacce


plotdbrda <- ggplot(amf.dbRDA.site, aes(CAP1, CAP2)) +
  geom_point(aes(color = site, shape = SeedType), size = 2) +
  geom_vline(xintercept = 0, color = "gray", size = 0.5) +
  geom_hline(yintercept = 0, color = "gray", size = 0.5) +
  geom_segment(data = amf.dbRDA.env1, 
               aes(x = 0,y = 0, xend = 2*CAP1, yend = 2*CAP2), #NOTE!! like i did with envfit i'm just scaling the vectors for visibility
               arrow = arrow(length = unit(0.1, "cm")), 
               size = 0.3, color = "blue") +
  geom_text(data = amf.dbRDA.env1, aes(CAP1 * 2.1, CAP2 * 2.1, label = group), color ="blue", size = 4) + #same here. use ggrepel if you want!
  labs(x = 'db-RDA1*** (15.75%)', y = 'db-RDA2*** (12.53%)') + #got the asterisks from my anova by axis above, and the percentages from the ggord output
  theme(panel.grid = element_blank(), 
        panel.background = element_rect(color = "black", fill ="transparent"), 
        legend.title = element_text(), 
        legend.key = element_rect(fill = 'transparent'))

p1 <- plotdbrda + stat_ellipse(aes(color = site), level = 0.95, linetype = 2) #95% confidence ellipses for clouds of points, here by my site variable.

#remember, always report the PERMANOVA (or similar statistical test like ANOSIM) results with your distance-based ordinations. 

