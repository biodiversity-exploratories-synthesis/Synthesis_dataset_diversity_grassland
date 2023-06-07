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