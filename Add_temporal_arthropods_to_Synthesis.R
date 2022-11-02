## Add temporal arthropods to synthesis dataset
#This script can be used to add the arthropod temporal dataset to the synthesis dataset
#Script by Caterina Penone

library(data.table)

allspecies <- fread("N:/Exploratories/Data/GRASSLANDS/221102_EP_species_diversity_GRL_Patch22.txt") 
fg.class <- fread("N:/Exploratories/Data/GRASSLANDS/221102_EP_species_info_GRL_Patch22.txt") 
source("N:/R/SCRIPTS UTILES/BE_plots_zero.R")
#this script is also here: https://github.com/biodiversity-exploratories-synthesis/Synthesis_useful_functions


## change column name "value" to "Abundance"
names(allspecies)[names(allspecies)=="value"] <- "Abundance"

## drop Plot_bexis and dataID dataversion columns
allsp2 <- allspecies[,-c(1,7,8)]

## merge the datasfiles
allsp2 <- merge(allsp2, fg.class, by ="Species")

######## add temporal arthropods ##########################################################################
arthro <- fread("N:/Exploratories/Data/GRASSLANDS/TemporalArthropods/21969_4_data.txt")
arthro$Species <- gsub(" ","_",arthro$Species)
##trophic group information has NAs if not at species level, unless no other species of the genus are there (those have NA in fun_group_fine)
arth_tr <- fread("N:/Exploratories/Data/GRASSLANDS/TemporalArthropods/Temporal_arth_trophicGroups_withNAs.csv")
arth_tr <- arth_tr[!is.na(Trophic_level)]

#remove data with no species information (order or genus)
arthro <- arthro[!is.na(Species)]

#merge with trait info, this also removes all occurrences not identified at the species level
arthro2 <- merge(arthro, arth_tr[,!names(arth_tr) %in% c("Order", "Suborder"), with=F], by="Species") 

#add missing zeros for all year X month X species combinations
length(unique(arthro2$Species))*length(unique(arthro2$PlotID))*length(unique(arthro2$CollectionYear))*length(unique(arthro2$CollectionMonth))
nrow(arthro2) #all zeros are missing
sum(is.na(arthro2$NumberAdults)) #no NAs
#remove non-target columns
arthro2 <- arthro2[,!c("TrapID", "Exploratory", "Traptype", "VIP_EP", "Order", "Suborder", "Family", "Trophic_level", "Fun_group_broad", "Fun_group_fine")]

arthro3 <- setDT(arthro2)[CJ(Species=Species, PlotID=PlotID, 
                             CollectionYear=CollectionYear,
                             CollectionMonth=CollectionMonth,
                             unique=T), on=.(Species, PlotID, CollectionYear, CollectionMonth)]

arthro3[is.na(NumberAdults), NumberAdults := 0 ]

#exclude plot and year combinations that need to be removed
arth_remove <- fread("N:/Exploratories/Data/GRASSLANDS/TemporalArthropods/26008_2_data.txt")
arth_remove$Collection <- gsub("-01-01", "", arth_remove$Collection)
arth_remove[,temp:=paste(PlotIDBexis, Collection, sep="_")]
arthro3[,temp:=paste(PlotID, CollectionYear, sep="_")]
arthro3 <- arthro3[!temp %in% arth_remove$temp]
arthro3$temp <- NULL

#average per species and plot, all yearsm, all months
#TODO: Noëlle replace this average by presence
summary(arthro3$NumberAdults)
arthro3[,value:=mean(NumberAdults), by=c("Species", "PlotID")]
summary(arthro3$value)
arthro3 <- unique(arthro3[,.(Species, PlotID, value)])
length(unique(arthro3$PlotID)) #150
arthro3[duplicated(arthro3[,.(Species, PlotID)])]

#add trophic info
arthro3 <- merge(arthro3, arth_tr, by="Species")

#homogenise column names to merge with main dataset
arthro3 <- data.table(BEplotZeros(arthro3, "PlotID", plotnam = "Plot"))
arthro3$Suborder <- NULL
setnames(arthro3, c("Order", "value"), c("Group_broad", "Abundance"))

#homogenise trophic group names
sort(unique(allsp2$Trophic_level))
sort(unique(arthro3$Trophic_level))
arthro3[Trophic_level=="decomposer", Trophic_level:="decomposer.arthropod"]
arthro3[Trophic_level=="herbivore", Trophic_level:="herbivore.arthropod"]
arthro3[Trophic_level=="omnivore", Trophic_level:="omnivore.arthropod"]
arthro3[Trophic_level=="pollinator", Trophic_level:="pollinator.arthropod"]
arthro3[Trophic_level=="secondary.consumer", Trophic_level:="secondary.consumer.arthropod"]

#remove 2008 data from main dataset and add temporal dataset for "Hemiptera"  "Coleoptera" "Araneae"    "Orthoptera"
allsp2 <- allsp2[!Group_broad %in% c("Hemiptera", "Coleoptera", "Araneae", "Orthoptera")]
#check if duplicate species
intersect(unique(arthro3$Species), unique(allsp2$Species)) #none
#all fine, rbind
allsp2 <- rbindlist(list(allsp2, arthro3), use.names = T, fill=T)
rm(arthro, arthro2, arthro3, arth_tr, arth_remove)
#####################################################################################################