# This script collects all things to do during the next update of the dataset
# Script by: Caterina Penone


#TODO in GRASSLAND DATASET:
# add oomycota and change protist_oomycota, cercozoa (also in OTU name)
# change data type for bats to "pass_number"
# homogenise trophic level names with forest dataset (e.g. these, but check others too)
tr[Group_broad == "bryophyte", Group_broad:= "Bryophyte"]
tr[Group_broad == "bat", Group_broad:= "Bat"]
#for Group_broad use Arthropod or use Araneae, Coleoptera... etc?
