########### Update EP grassland datasets 25626 and 27707 ############

# This script changes the dataset versions to match the new versions in BExIS2
# Script by: Caterina Penone

require(data.table)


######## Dataset 25626: Assembled RAW diversity from grassland EPs (2010-2017) for multidiversity synthesis - July 2019 #####

# Read dataset
grl25626 <- fread("Exploratories/Data/GRASSLANDS/190802_EP_species_diversity_GRL.txt")

# change versions
grl25626[DataID == 4140,  Dataversion := "2"]
grl25626[DataID == 5522,  Dataversion := "2"]
grl25626[DataID == 12526, Dataversion := "2"]
grl25626[DataID == 15086, Dataversion := "2"]
grl25626[DataID == 16746, Dataversion := "2"]
grl25626[DataID == 16871, Dataversion := "2"]
grl25626[DataID == 16893, Dataversion := "2"]
grl25626[DataID == 16894, Dataversion := "2"]
grl25626[DataID == 16895, Dataversion := "2"]
grl25626[DataID == 16896, Dataversion := "2"]
grl25626[DataID == 16897, Dataversion := "2"]
grl25626[DataID == 16908, Dataversion := "2"]
grl25626[DataID == 18166, Dataversion := "2"]
grl25626[DataID == 18187, Dataversion := "2"]
grl25626[DataID == 18206, Dataversion := "2"]
grl25626[DataID == 18207, Dataversion := "2"]
grl25626[DataID == 18208, Dataversion := "2"]
grl25626[DataID == 18226, Dataversion := "2"]
grl25626[DataID == 18548, Dataversion := "2"]
grl25626[DataID == 20146, Dataversion := "2"]
grl25626[DataID == 21449, Dataversion := "2"]
grl25626[DataID == 23986, Dataversion := "2"]
grl25626[DataID == 24247, Dataversion := "3"]
grl25626[DataID == 24426, Dataversion := "3"]
grl25626[DataID == 24986, Dataversion := "2"]
grl25626[DataID == 24466, Dataversion := "3"]
grl25626[DataID == 24690, Dataversion := "2"]
grl25626[DataID == 24866, Dataversion := "2"]
grl25626[DataID == 25066, Dataversion := "3"]
grl25626[DataID == 24567, Dataversion := "2"]
grl25626[DataID == 24569, Dataversion := "2"]
grl25626[DataID == 24571, Dataversion := "2"]

# check
unique(grl25626[,.(DataID,Dataversion)])

# Write dataset
fwrite(grl25626,"N:/Exploratories/Data/GRASSLANDS/210826_EP_species_diversity_GRL_BEXIS25626.txt",row.names=F,quote=F,sep=";",na=NA)
rm(grl25626)
##############################################################

######## Dataset 27707: Assembled RAW diversity from grassland EPs (2008-2020) for multidiversity synthesis - November 2020 ####

# Read dataset
grl27707 <- fread("Exploratories/Data/GRASSLANDS/210112_EP_species_diversity_GRL_BEXIS.txt")

# change versions
grl27707[DataID == 4140,  Dataversion := "2"]
grl27707[DataID == 5522,  Dataversion := "2"]
grl27707[DataID == 12526, Dataversion := "2"]
grl27707[DataID == 15086, Dataversion := "2"]
grl27707[DataID == 16746, Dataversion := "2"]
grl27707[DataID == 16871, Dataversion := "2"]
grl27707[DataID == 16893, Dataversion := "2"]
grl27707[DataID == 16894, Dataversion := "2"]
grl27707[DataID == 16895, Dataversion := "2"]
grl27707[DataID == 16896, Dataversion := "2"]
grl27707[DataID == 16897, Dataversion := "2"]
grl27707[DataID == 16908, Dataversion := "2"]
grl27707[DataID == 18166, Dataversion := "2"]
grl27707[DataID == 18187, Dataversion := "2"]
grl27707[DataID == 18206, Dataversion := "2"]
grl27707[DataID == 18207, Dataversion := "2"]
grl27707[DataID == 18208, Dataversion := "2"]
grl27707[DataID == 18226, Dataversion := "2"]
grl27707[DataID == 18548, Dataversion := "2"]
grl27707[DataID == 20146, Dataversion := "2"]
grl27707[DataID == 21048, Dataversion := "2"]
grl27707[DataID == 21449, Dataversion := "2"]
grl27707[DataID == 23986, Dataversion := "2"]
grl27707[DataID == 24426, Dataversion := "3"]
grl27707[DataID == 24466, Dataversion := "3"]
grl27707[DataID == 24690, Dataversion := "2"]
grl27707[DataID == 24866, Dataversion := "2"]
grl27707[DataID == 24986, Dataversion := "2"]
grl27707[DataID == 25066, Dataversion := "3"]
grl27707[DataID == 26470, Dataversion := "2"]
grl27707[DataID == 26471, Dataversion := "2"]
grl27707[DataID == 26472, Dataversion := "2"]
grl27707[DataID == 27007, Dataversion := "2"]
grl27707[DataID == 27386, Dataversion := "2"]
grl27707[DataID == 27406, Dataversion := "2"]
grl27707[DataID == 25306, Dataversion := "2"]

# check
unique(grl27707[,.(DataID,Dataversion)])


# Write dataset
fwrite(grl27707,"N:/Exploratories/Data/GRASSLANDS/210826_EP_species_diversity_GRL_BEXIS27707.txt",row.names=F,quote=F,sep=";",na=NA)
