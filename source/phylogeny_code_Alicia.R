library(V.PhyloMaker)
library(phytools)


species = c("Bambusa vulgaris", "Cecropia schreberiana", "Dacryodes excelsa", "Homalium racemosum", "Manilkara bidentata", "Prestoea montana", "Rourea surinamensis", "Sloanea berteriana", "Syzygium jambos")
genera = c("Bambusa", "Cecropia", "Dacryodes", "Homalium", "Manilkara","Prestoea", "Rourea", "Sloanea", "Syzygium")
family = c("Poaceae", "Urticaceae", "Burseraceae", "Salicaceae", "Sapotaceae", "Arecaceae", "Connaraceae", "Elaeocarpaceae", "Myrtaceae")
example = data.frame(species = species, genus = genera, family = family)
result = phylo.maker(example, scenarios=c("S1", "S2", "S3"))
library(ape)
par(mfrow = c(1, 3))
plot.phylo(result$scenario.1, cex = 1.5, main = "scenario.1")
nodelabels(round(branching.times(result$scenario.1), 1), cex = 1)
plot.phylo(result$scenario.2[[1]], cex = 1.5, main = "scenario.2")
nodelabels(round(branching.times(result$scenario.2[[1]]), 1), cex = 1)
plot.phylo(result$scenario.3, cex = 1.5, main = "scenario.3")
nodelabels(round(branching.times(result$scenario.3), 1), cex = 1)

#play around with the different plotting abilities and you can take a look at the edge lengths etc for relatedness
#you can make radial phylogenies, etc.

result$scenario.3$edge.length

#basically this package takes the huge known phylogeny of vascular plants and pulls out the guys you wanna see
#check out the vignettes/info for the packages we loaded above for cool plots and stuff
#http://blog.phytools.org/2016/02/extracting-terminal-edge-lengths-for.html

