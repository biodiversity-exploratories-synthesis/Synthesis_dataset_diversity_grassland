# This script collects all things to do during the next update of the dataset
# Script by: Caterina Penone

setwd("N:/")
library(data.table)
source("N:/R/SCRIPTS UTILES/BE_plots_zero.R") #some useful functions for Explos data
grl <- fread("Exploratories/Data/GRASSLANDS/210826_EP_species_diversity_GRL_BEXIS27707.txt")
tr <- fread("Exploratories/Data/GRASSLANDS/210112_EP_species_info_GRL_BEXIS.txt")

grl2 <- merge(grl, tr, by="Species")


#TODO in GRASSLAND DATASET:
# add oomycota and change protist_oomycota, cercozoa (also in OTU name)
# change data type for bats to "pass_number"
# homogenise trophic level names with forest dataset (e.g. these, but check others too)
tr[Group_broad == "bryophyte", Group_broad:= "Bryophyte"]
tr[Group_broad == "Bats", Group_broad:= "Bat"]
tr[Group_broad == "Birds", Group_broad:= "Bird"]
tr[Group_broad == "soilfungi", Group_broad:= "Soilfungi"]
#for Group_broad use Arthropod or use Araneae, Coleoptera... etc?
tr[Group_fine == "Bats", Group_fine:= "Bat"]
tr[Group_fine == "Birds", Group_fine:= "Bird"]
#check also all Fun_group_broad and fine (cf forest script 210217_ForestDivDataUPDATE.R)
#to be discussed with BExIS how to do proper taxonomic columns

############## Add oomicota ##########################
#########Based on this code from the forest dataset
#############OOMYCOTA
# Read diversity data and check dimension
pro17 <- fread("Exploratories/Data/FORESTS/Update2021/25767_2_data.txt")
length(unique(pro17$OTU)) * length(unique(pro17$EP_PlotID))

pro11 <- fread("Exploratories/Data/FORESTS/Update2021/25766_2_data.txt")
length(unique(pro11$OTU)) * length(unique(pro11$EP_PlotID))

# Read species information / traits
proinf <- fread("Exploratories/Data/FORESTS/Update2021/25768_2_data.txt")

# Change hew2 to hew51 in 2017
sort(unique(pro17$EP_PlotID))
pro17[EP_PlotID=="HEW2", EP_PlotID:="HEW51"]

# Add year, DataID and merge
pro17$My_PlotID <- NULL; pro11$MyPlotID <- NULL
pro17$DataID <- 25767; pro11$DataID <- 25766
pro17$Year <- 2017; pro11$Year <- 2011

protoo <- rbindlist((list(pro11,pro17)))
rm(pro11, pro17)

# Remove grassland plots
length(unique(protoo$EP_PlotID))
protoo <- protoo[!grepl("G", protoo$EP_PlotID)] 
length(unique(protoo[Year=="2011"]$Plot_bexis)) #150
length(unique(protoo[Year=="2017"]$Plot_bexis)) #150

#### Prepare species info table
# Add Group broad and Group fine
proinf$Group_broad <- "Protist.oomycota"
unique(proinf$Order)
setnames(proinf, "Order", "Group_fine")

# Create trophic level information
table(proinf[,.(Lifestyle, Substrate)])
proinf$Trophic_level <- "plantparasite.protist"
proinf[Lifestyle=="saprotroph", Trophic_level:="decomposer.protist"]
proinf[(!Lifestyle %in% "saprotroph" & Substrate == "Metazoa"),
       Trophic_level:="animalparasite.protist"]
proinf[(!Lifestyle %in% "saprotroph" & Substrate == "substrate_undetermined"),
       Trophic_level:="unknown.protist"]

table(proinf[,.(Lifestyle, Trophic_level)]) #check ok
table(proinf[,.(Substrate, Trophic_level)]) #check ok

# Functional group broad and fine
proinf$Fun_group_broad <- proinf$Fun_group_fine <- proinf$Trophic_level
proinf[(Lifestyle == "obligate_biotroph" & Trophic_level == "plantparasite.protist"),
       Fun_group_fine:="plant.obligate.biotroph.protist"]

proinf[(Lifestyle == "hemibiotroph" & Trophic_level == "plantparasite.protist"),
       Fun_group_fine:="plant.hemibiotroph.protist"]

proinf[(Lifestyle == "obligate_biotroph" & Trophic_level == "animalparasite.protist"),
       Fun_group_fine:="animal.obligate.biotroph.protist"]

proinf[(Lifestyle == "hemibiotroph" & Trophic_level == "animalparasite.protist"),
       Fun_group_fine:="animal.hemibiotroph.protist"]

proinf[(Trophic_level == "decomposer.protist"),
       Fun_group_fine:="saprotroph.protist"]

table(proinf[,.(Fun_group_fine, Trophic_level)]) #check ok

# Remove columns not needed
proinf$Genus <- proinf$Lifestyle <- proinf$Substrate <- NULL

## Change names
setnames(proinf, "OTUs", "Species")
setnames(protoo, c("EP_PlotID", "OTU", "abundance"), c("Plot_bexis", "Species", "value"))

# Merge diversity and species info
protoo <- merge(protoo, proinf, by = "Species")

# Add group name to otu ID (to avoid confusions with fungi or bacteria)
protoo[,Species:=paste(Species,"_protist_OOMYCOTA",sep="")]

# Add new columns: type, plot zero
setdiff(names(frs2), names(protoo))
setdiff(names(protoo), names(frs2))
protoo$type <- "OTU_number"
protoo <- data.table(BEplotZeros(protoo, "Plot_bexis", plotnam = "Plot"))

# Checks
apply(prot, 2, function(x)sum(is.na(x)))
apply(protoo, 2, function(x)sum(is.na(x)))

length(unique(prot$Plot)) #151
length(unique(protoo$Plot)) #151

length(unique(prot$Species)) #2101
length(unique(protoo$Species)) #1148

150 * 2101 * 2 #630300
150 * 1148 * 2 #344400

# Checks passed, merge with main dataset
frs2 <- rbindlist(list(frs2, prot, protoo),use.names = T)
rm(prot, protoo, proinf); gc()


#############Replace symbionts with proper AMF datasets ##########################
#adapt code from here (this is from BE.RE.analysis.Rproj):
lt <- fread("N:/Exploratories/Data/GRASSLANDS/AMF2023/27686_3_data.txt") #lookout table
a11 <- fread("N:/Exploratories/Data/GRASSLANDS/AMF2023/27687_3_data.txt") #2011
a14 <- fread("N:/Exploratories/Data/GRASSLANDS/AMF2023/27689_3_data.txt") #2014
a17 <- fread("N:/Exploratories/Data/GRASSLANDS/AMF2023/27691_3_data.txt") #2017

#for forests it would be
# lt <- fread("N:/Exploratories/Data/FORESTS/AMF2023/27686_3_data.txt") #lookout table
# a11 <- fread("N:/Exploratories/Data/FORESTS/AMF2023/27688_3_data.txt") #2011
# a14 <- fread("N:/Exploratories/Data/FORESTS/AMF2023/27690_3_data.txt") #2014
# a17 <- fread("N:/Exploratories/Data/FORESTS/AMF2023/27692_3_data.txt") #2017


#stack all years
a11$Year <- 2011
a14$Year <- 2014
a17$Year <- 2017

a11$DataID <- 27687
a14$DataID <- 27689
a17$DataID <- 27691

amf <- rbindlist(list(a11, a14, a17))
amf$Dataversion <- 3

#harmonise names and data structure with synthesis dataset
setnames(amf, c("Plotid", "AMF", "Abundance"), c("Plot_bexis", "Species", "value"))
amf <- data.table(BEplotZeros(amf, "Plot_bexis", plotnam="Plot")) #add plot name with zeros (e.g. AEG1 to AEG01)
amf$type <- "ASV_number"

#taxa table
amf$Group_broad <- "soilfungi"
amf$Group_fine <- "Glomeromycotina"
amf$Trophic_level <- "symbiont.soilfungi"
amf$Fun_group_broad <- "soilfungi.symbiont"
amf$Fun_group_fine <- "AMF"

sort(names(grl2))
sort(names(amf))

#remove amf measured in soils from synthesis dataset and add speciic AMF
unique(grl2[Trophic_level=="symbiont.soilfungi", .(Group_broad, Group_fine, Fun_group_broad, Fun_group_fine)]) #check
grl2 <- grl2[!Fun_group_fine=="AMF"]
grl2 <- rbindlist(list(grl2, amf), use.names = T)

###Birds 2018 --> change dataset ID and version
#From Cornelia: to make the dataset 25306 "Bird survey and trait data on all grassland and forest EPs 2018" public, 
#we had to upload the data in a new dataset. This dataset (31521) contains abbreviations for the observer. All other 
#data is identical. Dataset 25306 was deleted. I updated the link section of your dataset - I deleted the link to 25306 
#and added a link to  31521 in your synthesis dataset (27706, 27707, 31206) and dataset 31368."
