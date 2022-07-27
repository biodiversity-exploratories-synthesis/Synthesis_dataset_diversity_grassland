########### Update EP grassland datasets 27706 and 27707############
#This script creates datasets xx version xx and xx version xx in Bexis
#Script by Caterina Penone

#This script is not a full update but a fix to two issues:
#1. add two more plots for bacteria 2014 (after a mistake in script 210112_UpdateGRLdataset.R LINE:282)
#2. add dung beetles 2014

require(data.table)
setwd("N:/")
setwd("C:/Users/Caterina/Dropbox/")

source("R/SCRIPTS UTILES/BE_plots_zero.R") 
#this script is also here: https://github.com/biodiversity-exploratories-synthesis/Synthesis_useful_functions

#Read last version of grassland dataset
grl <- fread("Exploratories/Data/GRASSLANDS/210826_EP_species_diversity_GRL_BEXIS27707.txt")
tr <- fread("Exploratories/Data/GRASSLANDS/210112_EP_species_info_GRL_BEXIS.txt")

grl2 <- merge(grl, tr, by="Species")

#1. Add two plots for bacteria 2014 ###################################################
# There is a mistake in script 210112_UpdateGRLdataset.R LINE:282 
# Bacteria have 2 missing plots in 2011, not 2014, add these plots in 2014
# This script adds these plots and also changes type from "OTU_number" to "ASV_number"

# bacteria 2011
bac <- fread("Exploratories/Data/GRASSLANDS/TEXTfiles/190319_Update/24866.txt")
length(unique(bac$Plot_ID))*length(unique(bac$Sequence_variant)) #zeros are missing
bac$DataID <- 24866
bac$Dataversion <- "2"
bac$Year <- 2011
bac[, c("kingdom", "phylum", "class", "order", "family", "genus", "species") := tstrsplit(Taxonomy, ", ", fixed=TRUE)]
bac[,c(4,9:14):=NULL]
plotIDs <- data.frame(Plot_ID = unique(bac$Plot_ID))
plotIDs <- data.table(BEplotZeros(plotIDs,"Plot_ID",plotnam = "Plot"))
bac <- merge(bac,plotIDs,by="Plot_ID")
gc()

# bacteria 2014
bac4 <- fread("Exploratories/Data/GRASSLANDS/TEXTfiles/190319_Update/25066.txt")
length(unique(bac4$Plot_ID))*length(unique(bac4$Sequence_variant)) #zeros are missing
bac4$DataID <- 25066
bac4$Dataversion <- "3"
bac4$Year <- 2014
bac4[, c("kingdom", "phylum", "class", "order", "family", "genus", "species") := tstrsplit(Taxonomy, ", ", fixed=TRUE)]
bac4[,c(4,9:14):=NULL]
plotIDs <- data.frame(Plot_ID = unique(bac4$Plot_ID))
plotIDs <- data.table(BEplotZeros(plotIDs,"Plot_ID", plotnam = "Plot"))
bac4 <- merge(bac4, plotIDs, by="Plot_ID")
gc()

#merge taxonomy
setkey(bac4, Sequence_variant)
setkey(bac, Sequence_variant)
bac[bac4, kingdom := i.kingdom, by=.EACHI]

bac <- rbind(bac, bac4)
length(unique(bac$Plot)) #150
rm(bac4, plotIDs); gc()

#change column names
setnames(bac, c("Sequence_variant","Read_count","Plot_ID","kingdom"),
              c("Species","value","Plot_bexis","Group_fine"))

#add columns
bac$type <- "ASV_number"

#add info on trophic level etc.
names(grl2)
bac$Group_broad <- bac$Trophic_level <- bac$Fun_group_broad <- bac$Fun_group_fine <- "bacteria.RNA"

###check if two OTUs have same "name" but different taxonomy
temp <- unique(bac, by=c("Species","Group_fine"))
length(unique(bac$Species)) == nrow(temp)
rm(temp)

#remove old bact and rbind with main dataset
grl2 <- grl2[!Group_broad %in% "bacteria.RNA"]
sort(unique(grl2$Group_broad))
setdiff(names(grl2), names(bac))
grl2 <- rbind(grl2, bac, use.names=T)
rm(bac); gc()

#######################################################################################


#2. add dung beetles species###########################################################
# 21207: Dungwebs Species List 2014 & 2015 (Invertebrates, Scarabaeoidea, Dung Beetles) - downloaded 27/06/18
dung <- fread("Exploratories/Data/GRASSLANDS/TEXTfiles/190319_Update/21207.txt")
dung <- dung[!grepl("W", dung$EP)] #remove forest data
length(unique(dung$EP)) #150
summary(dung$count) #dataset has zeros
unique(dung$month) #use only august, july june 2014 (rest is VIP)
unique(dung$dungtype) #use only cow, deer, fox, horse, sheep, wildboar

#see how many are already in the dataset to decide whether to include the dataset
dungsp <- unique(dung$species)
setdiff(dungsp, unique(grl2$Species)) #none of the 34 sp is in the dataset
rm(dungsp)
dung <- dung[month %in% c("August_2014","July_2014","June_2014")]
length(unique(dung$EP)) #150

#drop species that are only in forests
dung[,temp:=sum(count), by="species"]
summary(dung)
dung <- dung[temp>0]
unique(dung$species) #17 species now
length(unique(dung$species)) * length(unique(dung$EP)) #need to aggregate months
dung$temp <- NULL

#remove non needed columns
dung$date <- dung$month <- dung$exotic <- dung$chem <- dung$dungtype<-NULL

#aggregate months
dung[,value:=sum(count), by=c("species", "EP")]
dung <- unique(dung[, c("species", "EP", "value"), with=F])
length(unique(dung$species)) * length(unique(dung$EP)) == nrow(dung)

#homogenise names
setnames(dung, c("species", "EP"), c("Species","Plot_bexis"))

#add missing columns
dung$Year <- 2014
dung$type <- "abundance"
dung$DataID <- 21207
dung$Dataversion<-"2"
dung <- data.table(BEplotZeros(dung,"Plot_bexis",plotnam="Plot"))

sort(unique(grl2$Group_broad))
dung$Group_broad<-"Coleoptera"
dung$Group_fine<-"Scarabaeidae"

sort(unique(grl2$Trophic_level))
dung$Trophic_level<-"decomposer.arthropod"

sort(unique(grl2$Fun_group_broad))
dung$Fun_group_broad<-"decomposer.dungbeetle"

sort(unique(grl2$Fun_group_fine))
dung$Fun_group_fine<-"decomposer.dungbeetle"

length(unique(dung$Plot)) #150

#add to grassland dataset
setdiff(unique(dung$Species), unique(grl2$Species))
grl2 <- rbind(grl2, dung, use.names=T)

rm(dung)
#######################################################################################

#any other update?

###########Save the diversity and characteristics tables separately ###################
grl <- grl2[,.(Plot_bexis, Plot, Species, value, type, DataID, Year, Dataversion)]
tr <- grl2[,.(Species, Group_broad, Group_fine, Trophic_level, Fun_group_broad, Fun_group_fine)]
length(unique(tr$Species))
dim(unique(tr)) #no duplicates
length(unique(grl2$Species))
tr <- unique(tr)
sum(is.na(grl))
apply(grl,2,function(x)sum(is.na(x)))
sum(is.na(tr))
apply(tr,2,function(x)sum(is.na(x)))
tr[is.na(Fun_group_broad)] #two myriapods and one plant


summary(factor(tr$Trophic_level))

#reorder column names in grl
setcolorder(grl,c("Plot_bexis","Plot","Species","value","type","Year","DataID","Dataversion"))

#all good: save
fwrite(grl,"Exploratories/Data/GRASSLANDS/220727_EP_species_diversity_GRL_Patch22.txt",row.names=F,quote=F,sep=";",na=NA)
fwrite(tr,"Exploratories/Data/GRASSLANDS/220727_EP_species_info_GRL_patch22.txt",row.names=F,quote=F,sep=";",na=NA)

