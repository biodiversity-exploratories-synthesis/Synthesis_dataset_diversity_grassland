# This script collects all things to do during the next update of the dataset
# Script by: Caterina Penone


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
