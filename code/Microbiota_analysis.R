###############################################
#           Microbiota Analysis in R          #
###############################################

#Workshop available on: https://rstudio-pubs-static.s3.amazonaws.com/268156_d3ea37937f4f4469839ab6fa2c483842.html#other_visualizations

library(ape)
library(dplyr)
library(tidyverse)
library(ggplot2)
library(gplots)
library(lme4)
library(phangorn)
library(plotly)
library(tidyr)
library(vegan)
library(VennDiagram)
library(venneuler)    #Is not installed
library(phyloseq)


##########LOAD DATA##########

#OTU table (shared file)
OTU = read.table("data/example.final.an.unique_list.0.03.norm.shared", header=TRUE, sep="\t")

#Taxonomy of each OTU
tax = read.table("data/example.final.an.unique_list.0.03.cons.taxonomy", header=TRUE, sep="\t")

#Metadata. Since we made this in Excel, not mothur, we can use the "row.names" modifier to automatically name the rows by the values in the first column (sample names)
meta = read.table("data/example.metadata.txt", header=TRUE, row.names=1, sep="\t")

#SCFA data
SCFA = read.table("data/example.SCFA.txt", header=TRUE, row.names=1, sep="\t")


##########Clean up the data##########
##OTU table
#Use the group column as the row names
row.names(OTU) = OTU$Group
#Remove some variables
OTU.clean = OTU[,-which(names(OTU) %in% c("label", "numOtus", "Group"))]

##Taxonomy table
#Name the rows as by the OTU
row.names(tax) = tax$OTU
#Remove the OTUs that dont occur in our OTU.clean data set
tax.clean = tax[row.names(tax) %in% colnames(OTU.clean),]
#Separate the taxonomy column
tax.clean = separate(tax.clean, Taxonomy, into = c("Domain", "Phylum", "Class", "Order", "Family", "Genus", "Species", "Strain"), sep=";")
#remove some columns
tax.clean = tax.clean[,-which(names(tax.clean) %in% c("Size", "Strain", "OTU"))]


##########Order the data##########
OTU.clean = OTU.clean[order(row.names(OTU.clean)),]
meta = meta[order(row.names(meta)),]
SCFA = SCFA[order(row.names(SCFA)),]


##########Set seed##########
set.seed(8765)

#########################################
##########Explore alpha metrics##########
#########################################

#Create 2x2 plot environment so that we can see all 4 metrics at once. 
par(mfrow = c(2, 2))

#Then plot each metric.
hist(meta$shannon, main="Shannon diversity", xlab="", breaks=10)
hist(meta$simpson, main="Simpson diversity", xlab="", breaks=10)
hist(meta$chao, main="Chao richness", xlab="", breaks=15)
hist(meta$ace, main="ACE richness", xlab="", breaks=15)

#Create 2x2 plot environment 
par(mfrow = c(2, 2))

#Plots
hist(meta$shannon, main="Shannon diversity", xlab="", breaks=10)
hist(1/meta$simpson, main="Inverse Simpson diversity", xlab="", breaks=10)
hist(meta$chao, main="Chao richness", xlab="", breaks=15)
hist(meta$ace, main="ACE richness", xlab="", breaks=15)

###########Test for normalcy statistically##########
shapiro.test(meta$shannon)
shapiro.test(1/meta$simpson)
shapiro.test(meta$chao)
shapiro.test(meta$ace)


###########Categorical variables############
##Normally distributed metrics
#Run the ANOVA and save it as an object
aov.shannon.age = aov(shannon ~ AgeGroup, data=meta)
#Call for the summary of that ANOVA, which will include P-values
summary(aov.shannon.age)
TukeyHSD(aov.shannon.age)
#Re-order the groups because the default is 1yr-2w-8w
meta$AgeGroup.ord = factor(meta$AgeGroup, c("2w","8w","1yr"))
#Return the plot area to 1x1
par(mfrow = c(1, 1))
#Plot
boxplot(shannon ~ AgeGroup.ord, data=meta, ylab="Shannon's diversity")

##Non-normally distributrred metrics
kruskal.test(chao ~ AgeGroup, data=meta)
pairwise.wilcox.test(meta$chao, meta$AgeGroup, p.adjust.method="fdr")
#Create 1x1 plot environment
par(mfrow = c(1, 1))
#Plot
boxplot(chao ~ AgeGroup.ord, data=meta, ylab="Chao richness")


##########Continuous variables###########
##Normally distributed metrics
glm.shannon.ADG = glm(shannon ~ ADGKG, data=meta)
summary(glm.shannon.ADG)
plot(shannon ~ ADGKG, data=meta)
#Add the glm best fit line
abline(glm.shannon.ADG)
##non-normally distributed metrics
gaussian.chao.ADG = glm(chao ~ ADGKG, data=meta, family="gaussian")
par(mfrow = c(1,2))
plot(gaussian.chao.ADG, which=c(1,2))

qp.chao.ADG = glm(chao ~ ADGKG, data=meta, family="quasipoisson")
par(mfrow = c(1,2))
plot(qp.chao.ADG, which=c(1,2))

###################################
##########Beta-diversity###########
###################################

#Calculate distance and save as a matrix
BC.dist=vegdist(OTU.clean, distance="bray")
#Run PERMANOVA on distances.
adonis(BC.dist ~ AgeGroup*ADGKG, data = meta, permutations = 1000)

#dot plots
BC.nmds = metaMDS(OTU.clean, distance="bray", k=2, trymax=1000)
#Plot the nMDS
par(mfrow = c(1, 1))
#Create a blank plot for the nmds
plot(BC.nmds, type="n", main="Bray-Curtis")
#Add the points colored by age
points(BC.nmds, display="sites", pch=20, col=c("blue", "green", "red")[meta$AgeGroup])
BC.nmds
#Add a legend
legend(-5.5, 2.5, legend=c("2w","8w","1yr"), col=c("green","red","blue"), pch=20)

J.nmds = metaMDS(OTU.clean, distance="jaccard", k=2, trymax=1000)
plot(J.nmds, type="n", main="Jaccard")
points(J.nmds, display="sites", pch=20, col=c("blue", "green", "red")[meta$AgeGroup])
legend(-3, 1.5, legend=c("2w","8w","1yr"), col=c("green","red","blue"), pch=20)

###########Ellipses##########
plot(BC.nmds, type="n", main="Bray-Curtis")
legend(-5.5, 2.5, legend=c("2w","8w","1yr"), col=c("green","red","blue"), pch=20)
#Add an ellipse for 2w
ordiellipse(BC.nmds, groups=meta$AgeGroup, display="sites", kind="se", conf=0.99, label=FALSE, col="green", draw="polygon", alpha=200, show.groups = c("2w"), border=FALSE)
#Add an ellipse for 8w
ordiellipse(BC.nmds, groups=meta$AgeGroup, display="sites", kind="se", conf=0.99, label=FALSE, col="red", draw="polygon", alpha=200, show.groups = c("8w"), border=FALSE)
#Add an ellipse for 1yr
ordiellipse(BC.nmds, groups=meta$AgeGroup, display="sites", kind="se", conf=0.99, label=FALSE, col="blue", draw="polygon", alpha=200, show.groups = c("1yr"), border=FALSE)

##########3D plots###########
#Calculate the Bray-Curtis nMDS for 3-axis
BC.nmds.3D = metaMDS(OTU.clean, distance="bray", k=3, trymax=1000)
BCxyz = scores(BC.nmds.3D, display="sites")
#This is a table that looks like 
BCxyz
#Make the 3D plot
plot_ly(x=BCxyz[,1], y=BCxyz[,2], z=BCxyz[,3], type="scatter3d", mode="markers", color=meta$AgeGroup, colors=c("blue", "green", "red"))

par(mfrow=c(1,2))
#Axis 1 and 2 (x and y)
plot(BCxyz[,1], BCxyz[,2], main="Bray-Curtis 1:2", pch=20, col=c("blue", "green", "red")[meta$AgeGroup])
legend(-5.4, 3, legend=c("2w","8w","1yr"), col=c("green","red","blue"), pch=20)
#Axis 1 and 3 (x and z)
plot(BCxyz[,1], BCxyz[,3], main="Bray-Curtis 1:3", pch=20, col=c("blue", "green", "red")[meta$AgeGroup])

##########Phylogentic-based metrics##########
OTU.UF = otu_table(as.matrix(OTU.clean), taxa_are_rows=FALSE)
tax.UF = tax_table(as.matrix(tax.clean))
meta.UF = sample_data(meta)
physeq = phyloseq(OTU.UF, tax.UF, meta.UF)
#Create a tree
load("data/NJ.tree.Rdata")
physeq.tree = merge_phyloseq(physeq, NJ.tree)
physeq.tree

##Dot plots
wUF.ordu = ordinate(physeq.tree, method="NMDS", distance="unifrac", weighted=TRUE)
par(mfrow=c(1,1))
plot(wUF.ordu, type="n", main="Weighted UniFrac")
points(wUF.ordu, pch=20, display="sites", col=c("blue", "green", "red")[meta$AgeGroup])
legend(0.3,0.15, legend=c("2w","8w","1yr"), col=c("green","red","blue"), pch=20)
##ggplot2
plot_ordination(physeq.tree, wUF.ordu, type="sites", color="AgeGroup") + 
  scale_colour_manual(values=c("2w"="green", "8w"="red", "1yr"="blue")) + 
  theme_bw() + 
  ggtitle("Weighted UniFrac")
#Visualize unweighted unifrac
uwUF.ordu = ordinate(physeq.tree, method="NMDS", distance="unifrac", weighted=FALSE)
plot_ordination(physeq.tree, uwUF.ordu, type="sites", color="AgeGroup") + 
  scale_colour_manual(values=c("2w"="green", "8w"="red", "1yr"="blue")) + 
  theme_bw() + 
  ggtitle("Unweighted UniFrac")

#Ellipses
plot(wUF.ordu, type="n", main="Weighted UniFrac")
legend(0.3, 0.15, legend=c("2w","8w","1yr"), col=c("green","red","blue"), pch=20)

#Add an ellipse for 2w
ordiellipse(wUF.ordu, groups=meta$AgeGroup, display="sites", kind="se", conf=0.99, label=FALSE, col="green", draw="polygon", alpha=200, show.groups = c("2w"), border=FALSE)

#Add an ellipse for 8w
ordiellipse(wUF.ordu, groups=meta$AgeGroup, display="sites", kind="se", conf=0.99, label=FALSE, col="red", draw="polygon", alpha=200, show.groups = c("8w"), border=FALSE)

#Add an ellipse for 1yr
ordiellipse(wUF.ordu, groups=meta$AgeGroup, display="sites", kind="se", conf=0.99, label=FALSE, col="blue", draw="polygon", alpha=200, show.groups = c("1yr"), border=FALSE)

#Ellipses using ggplot2
plot_ordination(physeq.tree, wUF.ordu, type="sites", color="AgeGroup") + 
  scale_colour_manual(values=c("2w"="green", "8w"="red", "1yr"="blue")) + 
  theme_bw() + 
  stat_ellipse() + 
  ggtitle("Weighted UniFrac")

##########3D Plots##########
wUF.ordu
wUF.dist = UniFrac(physeq.tree, weighted=TRUE, normalized=TRUE)
wUF.nmds.3D = metaMDS(wUF.dist, method="NMDS", k=3)
#Get the xyz values
wUFxyz = scores(wUF.nmds.3D, display="sites")
#This is a table that looks like 
wUFxyz

#Plot the data
plot_ly(x=wUFxyz[,1], y=wUFxyz[,2], z=wUFxyz[,3], type="scatter3d", mode="markers", color=meta$AgeGroup, colors=c("blue", "green", "red"))

##########Vectors for continuous variables##########
fit.BC = envfit(BC.nmds, meta) 
fit.BC
#Run envfit only on the variables you want
fit.BC = envfit(BC.nmds, meta[,c("AgeGroup", "ADGKG")])
fit.BC
#Repeat for UniFrac
fit.wUF = envfit(wUF.ordu, meta[,c("AgeGroup", "ADGKG")])
fit.wUF

#For bray-curtis
plot(BC.nmds, type="n", main="Bray-Curtis")
points(BC.nmds, pch=20, display="sites", col=c("blue", "green", "red")[meta$AgeGroup])
legend(-6, 2, legend=c("2w","8w","1yr"), col=c("green","red","blue"), pch=20)
#Add fitted variables
plot(fit.BC, col="black")

#For values with a fit P-value < 0.05
plot(BC.nmds, type="n", main="Bray-Curtis")
points(BC.nmds, pch=20, display="sites", col=c("blue", "green", "red")[meta$AgeGroup])
legend(-6, 2, legend=c("2w","8w","1yr"), col=c("green","red","blue"), pch=20)
#Add fitted variables
plot(fit.BC, col="black", p.max=0.05)

############Other visualizations##########                                                  #abundance plot
plot_bar(physeq.tree, fill="Phylum")
#Simplify by grouping the samples on age
plot_bar(physeq.tree, x="AgeGroup", fill="Phylum") 
#removing the lines between OTU's in the bars
plot_bar(physeq.tree, x="AgeGroup", fill="Phylum") + geom_bar(aes(color=Phylum, fill=Phylum), stat="identity", position="stack")
#only show the top 5 most abundant phyla
#Sort the Phyla by abundance and pick the top 5
top5P.names = sort(tapply(taxa_sums(physeq.tree), tax_table(physeq.tree)[, "Phylum"], sum), TRUE)[1:5]
#Cut down the physeq.tree data to only the top 10 Phyla
top5P = subset_taxa(physeq.tree, Phylum %in% names(top5P.names))
#Plot
plot_bar(top5P, x="AgeGroup", fill="Phylum") + geom_bar(aes(color=Phylum, fill=Phylum), stat="identity", position="stack")
#Show the bargraphs seperatly
plot_bar(top5P, x="AgeGroup", fill="Phylum", facet_grid = ~Phylum) + geom_bar(aes(color=Phylum, fill=Phylum), stat="identity", position="stack")

##########Heat Maps###########
##OTU's
#Sort the OTUs by abundance and pick the top 20
top20OTU.names = names(sort(taxa_sums(physeq.tree), TRUE)[1:20])
#Cut down the physeq.tree data to only the top 10 Phyla
top20OTU = prune_taxa(top20OTU.names, physeq.tree)
plot_heatmap(top20OTU)
plot_heatmap(top20OTU, sample.label="AgeGroup", sample.order="AgeGroup")
plot_heatmap(top20OTU, sample.label="AgeGroup", sample.order="AgeGroup", taxa.label="Genus")
plot_heatmap(top20OTU, sample.label="AgeGroup", sample.order="AgeGroup", taxa.label="Genus", taxa.order="Phylum")
#Change color to purple
plot_heatmap(top20OTU, sample.label="AgeGroup", sample.order="AgeGroup", taxa.label="Genus", taxa.order="Phylum", low="white", high="purple", na.value="grey")
#show distance between samples
#Bray-Curtis
heatmap.2(as.matrix(BC.dist))
heatmap.2(as.matrix(wUF.dist))
#Rainbow colors
rc <- rainbow(nrow(as.matrix(BC.dist)), start=0, end=0.9)
heatmap.2(as.matrix(BC.dist), col=rc)

##########Venn Diagrams##########

#Create a list of OTU's that occur in each sample.
OTU.5017.2w = colnames(OTU.clean["5017.2w.F", apply(OTU.clean["5017.2w.F",], MARGIN=2, function(x) any(x >0))])

OTU.5017.8w = colnames(OTU.clean["5017.8w.F", apply(OTU.clean["5017.8w.F",], MARGIN=2, function(x) any(x >0))])

OTU.5017.1yr = colnames(OTU.clean["5017.1yr.F",apply(OTU.clean["5017.1yr.F",], MARGIN=2, function(x) any(x >0))])
#Plot the OTU's
venn(list(OTU.5017.2w, OTU.5017.8w, OTU.5017.1yr))

#Do this for the age groups 
OTU.2w = colnames(OTU.clean[meta$AgeGroup == "2w", apply(OTU.clean[meta$AgeGroup == "2w",], MARGIN=2, function(x) any(x >0))])

OTU.8w = colnames(OTU.clean[meta$AgeGroup == "8w", apply(OTU.clean[meta$AgeGroup == "8w",], MARGIN=2, function(x) any(x >0))])

OTU.1yr = colnames(OTU.clean[meta$AgeGroup == "1yr", apply(OTU.clean[meta$AgeGroup == "1yr",], MARGIN=2, function(x) any(x >0))])
#plot again
venn(list(OTU.2w, OTU.8w, OTU.1yr))
#Make the plot look nice
draw.triple.venn(area1 = 385+58+71+320, area2 = 801+190+320+71, area3 = 3177+190+58+71, n12 = 320+71, n23 = 190+71, n13 = 58+71, n123 = 71, category = c("2w", "8w", "1yr"), lty = "blank", fill = c("green", "red", "blue"))

#Make the circles the be porportional to the total numbers of OTU's
#Create a venneuler object
#age.venn=venneuler(c('A' = 385+58+71+320, 'B' = 801+190+320+71, 'C' = 3177+190+58+71, 'A&B' = 320+71, 'B&C' = 190+71, 'A&C' = 58+71, 'A&B&C' = 71))

#Add group names
#age.venn$labels = c("2w", "8w", "1yr")

#Plot
#plot(age.venn)
###This part didnt work because the required package "venneuler" is not installed 
###The Venn diagram can also be created with the online tool: http://bioinformatics.psb.ugent.be/webtools/Venn/. 
write.table(OTU.2w, "data/OTU.2w.csv", sep=",", row.names=FALSE, col.names=FALSE)
write.table(OTU.8w, "data/OTU.8w.csv", sep=",", row.names=FALSE, col.names=FALSE)
write.table(OTU.1yr, "data/OTU.1yr.csv", sep=",", row.names=FALSE, col.names=FALSE)
#It worked this time

##########Networks##########
##OTU's
#Plot the OTU's distances as a network
prevotella = subset_taxa(physeq.tree, Genus == "g__Prevotella")
plot_net(prevotella, color="Species", type="taxa")
##Beta-diversity
#Plot the beta-diversity as a network
plot_net(physeq.tree, color="AgeGroup", distance="bray")

