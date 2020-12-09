###########Update EP grassland datasets version 200205############
#Changes from version with DataID: 24606 and 21726
#1.add zeros when species*plot combination is missing for a given year (birds)
#2.change "type" of foliar pathogens to presence-absence
#3.change soil fungi plant pathogens to soilfungi.plant.pathogen (instead of plant.pathogen) 
#4.for all OTU data change "type" from abundance to "OTU_number" (to avoid confusions)
#5.fix species names with multiple underscores or underscores at the end
#6.update protist datasets: removed the one from Hartmut Arndt, added 2 new datasets, changed datasets 18166 18187 18206 18207 18208 18226
#from species to OTU (to allow rarefaction) - species information can be found when downloading the original data
#7.update bacteria datasets: removed all old datasets and added bacteria RNA sequences with illumina ID:24866, 25066
#used only taxonomy from 2014 (more up to date)
#8.update fungi datasets: removed all old datasets and added soil fungi DNA sequences with illumina ID: 26470, 26471, 26472, 26473
#9.add dataset version number
#10.add missing information on trophic levels
#11.add ants dataset (ID:23986)
#12.add snails dataset (ID:23986)
#13.change dataset IDs for birds and bats (old datasets have been archived)
#14.change Trophic_level name for myriapods (myriapod.decomposer, myriapod.secondary.consumer)
#15.update plant temporal dataset until 2018
#16.add NAs for missing combinations of plot X year in the temporal datasets (plants, birds, bats) to avoid
#mistakes when calculating richness
#17.fix issue in bird dataset (2012 included twice and 2011 not included)
#18.add Collembola dataset 27007
#19.add Acari dataset 27406
#20.add Birds 2018
#21.homogeneise trophic level names

require(data.table)
#require(tidyr)
setwd("N:/")
setwd("C:/Users/Caterina/Dropbox/")

source("R/SCRIPTS UTILES/BE_plots_zero.R")

#Read last version of grassland dataset
grl <- fread("Exploratories/Data/GRASSLANDS/190218_EP_species_diversity_GRL.txt")
tr<-fread("Exploratories/Data/GRASSLANDS/190218_EP_species_info_GRL.txt")


#1###check if some datasets are missing zeros##########################################
unique(grl$DataID)
for (i in unique(grl$DataID)){
  tt<-grl[DataID==i]
  print(paste(i,":"))
  print(length(unique(tt$Plot))*length(unique(tt$Species)))
  print(nrow(tt))
}
#check datasets 19686, 11422, 6100, 16908
tt<-grl2[DataID==19686] #plant dataset, missing information are NAs -> ok
tt<-grl2[DataID==11422] #bird dataset, check
length(unique(tt$Plot))
tt<-grl2[DataID==6100] #bird dataset, check
length(unique(tt$Plot))
tt<-grl2[DataID==16908] #pollinator dataset, missing information are NAs -> ok
rm(tt,i)

#add zeros to bird datasets (to avoid people filling it with NAs and be consistent with other multiyear datasets)
allbi<-grl[DataID %in% c(11422,6100,12386,21449,24690)]
length(unique(allbi$Plot))*length(unique(allbi$Species))*length(unique(allbi$Year)) #final number of rows 61500
#produce zeros and check if some species have zeros in all plots
allbic<-dcast.data.table(allbi,Species+type+DataID+Year~Plot,value.var="value",fill=0)
length(unique(allbi$Species))*length(unique(allbi$Year))
allbi<-melt.data.table(allbic,measure.vars=5:154,variable.name="Plot")
allbi<-data.table(BEplotNonZeros(allbi,"Plot",plotnam="Plot_bexis"))

grl<-grl[!DataID %in% c(11422,6100,12386,21449,24690)]
grl<-rbind(grl,allbi,use.names=TRUE)
rm(allbi,allbic)
####################################################################################

grl2<-merge(grl,tr,by="Species")
rm(grl,tr); gc()


#2###transform plant pathogens into presence/absence##############################
unique(grl2$Group_broad)
unique(grl2[Group_broad=="Plant.pathogen",DataID]) #18548 is foliar pathogens, 21048 is soil fungi
set(grl2[DataID==18548],j="value",i=which(grl2[DataID==18548]$value!=0),value=1)
unique(grl2[DataID==18548]$value)
grl2[DataID==18548]$type<-"presenceabsence"
####################################################################################


#3###change soil fungi plant pathogens to soilfungi.plant.pathogen##################
unique(grl2[DataID==21048]$Trophic_level)
unique(grl2[DataID==21048]$Group_broad)
unique(grl2[DataID==21048]$Group_fine)
grl2[DataID==21048 & Trophic_level=="plant.pathogen"]$Trophic_level<-"soilfungi.plant.pathogen"
unique(grl2[DataID==21048]$Trophic_level)
unique(grl2[DataID==21048]$Group_fine)

#change one species which was in the plant.pathogen dataset but now has wrong values (probably a mix with a previous soil fungi)
unique(unique(grl2[DataID==21048 & Group_broad=="Plant.pathogen"]$Species))
#restore original values (based on raw data)
grl2[Species=="Urocystis_agropyri",value:=0] #species is absent in most plots
grl2[(Species=="Urocystis_agropyri") & (Plot %in% c("HEG04","HEG16","SEG21")),value:=1] #except 3 plots
grl2[(Species=="Urocystis_agropyri") & (Plot %in% c("AEG01","AEG06","AEG14","AEG41","HEG12","SEG14")),value:=NA] #4 plots are missing
grl2[Species=="Urocystis_agropyri",Trophic_level:="plant.pathogen"]
grl2[Species=="Urocystis_agropyri",DataID:=18548]
####################################################################################


#4###change OTU "type" from abundance to "OTU_number"##############################
unique(grl2$Group_broad)
grl2[Group_broad %in% c("AMF","Bacteria","Protists","Bacteria.DNA","SoilFungi")]$type<-"OTU_number"
unique(grl2$type)
###################################################################################


#5#######remove double "_" from species names or if it is the last character######
grl2$Species<-gsub("__","_",grl2$Species)
grl2[grep("__",grl2$Species),]
grl2$Species<-gsub("__","_",grl2$Species)
grl2$Species<-sub("_$","",grl2$Species)
###################################################################################

#6###remove old protist dataset and add new ones###################################
pro<-grl2[Group_broad %in% "Protists"]
unique(pro$Group_fine)
pro[Group_fine %in% "Protists"]
rm(pro)
grl2<-grl2[!Group_fine %in% "Protists"]

#add trophic level to Myxogastria and Disclosea (mostly Acanthoamoeba)
grl2[Group_broad=="Protists",Trophic_level:="mainly.bacterivore"] #it is unknown for many species

#new datasets
pro17<- fread("Exploratories/Data/GRASSLANDS/TEXTfiles/190319_Update/24466.txt")
length(unique(pro17$variable))*length(unique(pro17$EP_PlotID)) #every row is repeated twice!
pro17<-unique(pro17)
pro11<- fread("Exploratories/Data/GRASSLANDS/TEXTfiles/190319_Update/24426.txt")
length(unique(pro11$OTUs))*length(unique(pro11$EP_PlotID))
pro11<-unique(pro11)

proinf17<- fread("Exploratories/Data/GRASSLANDS/TEXTfiles/190319_Update/24468.txt")
proinf11<- fread("Exploratories/Data/GRASSLANDS/TEXTfiles/190319_Update/24467.txt")
proinf11<-unique(proinf11)

pro17$MyPlotID<-NULL; pro11$MyPlotID<-NULL
pro17$DataID<-24466; pro11$DataID<-24426
pro17$Year<-2017; pro11$Year<-2011
setnames(pro17,"variable","OTUs")

#use OTUs or species? look at correlation
# pro11s<-merge(pro11,proinf11,by="OTUs")
# pro11s[raw_abund!=0,raw_abund:=1]
# pro11s[,otuRich:=sum(raw_abund),by=EP_PlotID]
# pro11s[,spRich:=sum(raw_abund),by=list(Species,EP_PlotID)]
# 
# rich<-unique(pro11s[,.(EP_PlotID,otuRich,spRich)])
# rich<-unique(rich,by="EP_PlotID")
# plot(rich$otuRich,rich$spRich)
# cor.test(rich$otuRich,rich$spRich) #0.86 correlated, just keep OTUs in the table (so people can do rarefaction)
# rm(pro11s,rich)

setnames(pro11,"raw_abund","value"); setnames(pro17,"raw_abund","value")
pro11<-merge(pro11,proinf11,by="OTUs")
pro17<-merge(pro17,proinf17,by="OTUs")

#add dataID to otu ID
pro11[,OTUs:=paste(OTUs,"_protist",sep="")] #2011 and 2017 are compatible, use same OTUID
pro17[,OTUs:=paste(OTUs,"_protist",sep="")] #2011 and 2017 are compatible, use same OTUID

prot<-rbind(pro11,pro17)
rm(pro11,pro17,proinf11,proinf17)

prot<-prot[,.(OTUs,EP_PlotID,value,DataID,Year,
              Phylum,Class,nutrition_bacterivore, nutrition_omnivore, nutrition_eukaryvore, nutrition_plant_parasite,
              nutrition_parasite_not_plant, nutrition_unknown)]
prot$Trophic_level<-"protist.unknown" #had to put unknown instead of NA otherwise lines below do not work

###are there species with multiple nutrition?
prot[rowSums(prot[,8:13,with=F])>1]
prot[rowSums(prot[,8:13,with=F])==0]
dim(prot[rowSums(prot[,8:13,with=F])==1])

prot[nutrition_bacterivore==1,Trophic_level:="protist.bacterivore"]
prot[nutrition_omnivore==1,Trophic_level:="protist.omnivore"]
prot[nutrition_eukaryvore==1,Trophic_level:="protist.eukaryvore"]
prot[nutrition_plant_parasite==1,Trophic_level:="protist.plant.parasite"]
prot[nutrition_parasite_not_plant==1,Trophic_level:="protist.parasite.nonplant"]
nrow(prot[Trophic_level=="protist.unknown"])
sum(prot$nutrition_unknown)

prot[,(8:13):=NULL]

#add Plot
prot<-data.table(BEplotZeros(prot,"EP_PlotID",plotnam="Plot"))
setnames(prot,"EP_PlotID","Plot_bexis")
#add "Group_broad"     "Group_fine"      "Fun_group_broad" "Fun_group_fine" 
unique(grl2$Group_broad)
prot$Group_broad<-"Protists"
unique(grl2$Group_fine)
setnames(prot,"Phylum","Group_fine")
unique(grl2$Fun_group_broad); unique(grl2$Fun_group_fine)
prot$Fun_group_broad<-prot$Fun_group_fine<-prot$Trophic_level
prot$Class<-NULL
prot$type<-"OTU_number"
setnames(prot,"OTUs","Species")
prot1<-copy(prot)

#reupload the old protists datasets to use OTUs instead of species (for consistency)
all.files<-list.files("Exploratories/Data/GRASSLANDS/TEXTfiles/Protists/",full.names=T,pattern="txt")
mylist<- lapply(all.files, function(i) fread(i))
names(mylist)<-lapply(all.files,function(i) substring(i,50,54))
#melt all datasets
mylist<-lapply(mylist, function(i) melt.data.table(i, id.vars=1:9,variable.name="Plot"))
#create dataset
prot<-rbindlist(mylist,use.names=T,fill=T,idcol="DataID")
rm(all.files,mylist)
prot$value<-as.integer(prot$value)

#change plot names and add underscores in species names
prot$Plot<-gsub("_","",prot$Plot)
prot[,OTU:=paste(OTU,"_protist",DataID,sep="")]

#Remove unwanted columns
prot<-prot[,c("OTU","Plot","value","DataID","PHYLUM","CLASS"),with=F]
setnames(prot,"OTU","Species")

#Add info columns
prot$Year<-"2011_2012"
prot$type<-"OTU_number"
prot$Trophic_level<-"protist.mainly.bacterivore"
prot<-data.table(BEplotNonZeros(prot,"Plot",plotnam="Plot_bexis"))
prot$Group_broad<-"Protists"
prot[,Group_fine:=paste("Amoebozoa",CLASS,sep="_")]
prot$PHYLUM<-prot$CLASS<-NULL
prot$Fun_group_broad<-prot$Fun_group_fine<-prot$Trophic_level

#Merge with grl data
grl2<-grl2[!Group_broad=="Protists"]
grl2<-rbindlist(list(grl2,prot1,prot),use.names = T)
rm(prot,prot1)
##################################################################################


#7###remove old bacteria dataset and add new ones (24866, 25066)##########################
unique(grl2[,.(DataID,Group_broad)])

#remove all old bacteria data
grl2<-grl2[!Group_broad %in% c("Bacteria","Bacteria.DNA")]

#read files and merge
bac<-fread("Exploratories/Data/GRASSLANDS/TEXTfiles/190319_Update/24866.txt")
length(unique(bac$Plot_ID))*length(unique(bac$Sequence_variant)) #zeros are missing
#add missing combinations
#bac<-setDT(bac)[CJ(Sequence_variant=Sequence_variant,Plot_ID=Plot_ID,unique=T), on=.(Sequence_variant, Plot_ID)]
#bac[is.na(Read_count), Read_count := 0 ]
#bactx<-na.omit(unique(bac[,.(Sequence_variant,Taxonomy)])); bac$Taxonomy<-NULL
#bac<-merge(bac,bactx,by="Sequence_variant")
bac$DataID <- 24866
bac$Year<-2011
#set(bac,j="Sequence_variant",value=paste("bac11_",bac$Sequence_variant,sep="")) #taxonomy is compatible with 2014
#split Taxonomy into several columns (separate per year - memory problems)
bac[, c("kingdom", "phylum", "class", "order", "family", "genus", "species") := tstrsplit(Taxonomy, ", ", fixed=TRUE)]
bac[,c(4,8:13):=NULL]
plotIDs<-data.frame(Plot_ID=unique(bac$Plot_ID))
plotIDs<-data.table(BEplotZeros(plotIDs,"Plot_ID",plotnam = "Plot"))
bac<-merge(bac,plotIDs,by="Plot_ID")
gc()

bac4<-fread("Exploratories/Data/GRASSLANDS/TEXTfiles/190319_Update/25066.txt")
length(unique(bac4$Plot_ID))*length(unique(bac4$Sequence_variant)) #zeros are missing
#add missing combinations
#bac4<-setDT(bac4)[CJ(Sequence_variant=Sequence_variant,Plot_ID=Plot_ID,unique=T), on=.(Sequence_variant, Plot_ID)]
#bac4[is.na(Read_count), Read_count := 0 ]
#bactx<-na.omit(unique(bac4[,.(Sequence_variant,Taxonomy)])); bac4$Taxonomy<-NULL
#bac4<-merge(bac4,bactx,by="Sequence_variant")
bac4$DataID <- 25066
bac4$Year<-2014
#set(bac4,j="Sequence_variant",value=paste("bac14_",bac4$Sequence_variant,sep=""))
#split Taxonomy into several columns
bac4[, c("kingdom", "phylum", "class", "order", "family", "genus", "species") := tstrsplit(Taxonomy, ", ", fixed=TRUE)]
bac4[,c(4,8:13):=NULL]
bac4<-merge(bac4,plotIDs,by="Plot_ID")
gc()

#merge taxonomy
setkey(bac4,Sequence_variant)
setkey(bac,Sequence_variant)
bac[bac4,kingdom := i.kingdom,by=.EACHI]

bac<-rbind(bac,bac4)
rm(bac4,plotIDs); gc()

#change column names
setnames(bac,c("Sequence_variant","Read_count","Plot_ID","kingdom"),c("Species","value","Plot_bexis","Group_fine"))

#add columns
bac$type<-"OTU_number"

#add info on trophic level etc.
names(grl2)
bac$Group_broad<-bac$Trophic_level<-bac$Fun_group_broad<-bac$Fun_group_fine<-"bacteria.RNA"

###check if two OTUs have same "name" but different taxonomy
temp<-unique(bac,by=c("Species","Group_fine"))
length(unique(bac$Species))
rm(temp)

#rbind with main dataset
grl2<-rbind(grl2,bac,use.names=T)
rm(bac); gc()
###################################################################################


#8###remove old soil fungi and update with illumina dataset (26470, 26471, 26472, 26473)########################
unique(grl2$Group_broad)
grl2<-grl2[!Group_broad %in% c("AMF","SoilFungi")]

f11<-fread("Exploratories/Data/GRASSLANDS/TEXTfiles/201022_Update/26470.txt")
length(unique(f11$Plotid))*length(unique(f11$OTU)) #miss zeros too, but too large to add
f14<-fread("Exploratories/Data/GRASSLANDS/TEXTfiles/201022_Update/26471.txt")
f17<-fread("Exploratories/Data/GRASSLANDS/TEXTfiles/201022_Update/26472.txt")
finfo<-fread("Exploratories/Data/GRASSLANDS/TEXTfiles/201022_Update/26473.txt")

f11$Year<-2011; f11$DataID<-26470
f14$Year<-2014; f14$DataID<-26471
f17$Year<-2017; f17$DataID<-26472

soilf<-rbindlist(list(f11,f14,f17))
rm(f11,f14,f17)

soilf<-data.table(BEplotZeros(soilf,"Plotid",plotnam = "Plot"))
setnames(soilf,c("Plotid","OTU","Abundance"),c("Plot_bexis","Species","value"))
soilf$type<-"ASV_number"

#prepare OTU information data
finfo$Seq<-finfo$Kingdom<-NULL
summary(as.factor(finfo$Guild))
finfo<-finfo[OTU %in% unique(soilf$Species)] #remove forest species
finfo$Trophic_level<-finfo$Guild
finfo[Trophic_level %in% c("AMF","EMF","Lichen","EricoidM","OrchidM"),Trophic_level:="soilfungi.symbiont"]
finfo[Trophic_level %in% c("Saprotroph"),Trophic_level:="soilfungi.decomposer"]
finfo[Trophic_level %in% c("Pathogen","Parasite","Epiphyte"),Trophic_level:="soilfungi.pathotroph"]
finfo[Trophic_level %in% c("unknown","Endophyte"),Trophic_level:="soilfungi.other"]
summary(as.factor(finfo$Trophic_level))

finfo$Fun_group_broad<-finfo$Trophic_level
setnames(finfo,"Guild","Fun_group_fine")
setnames(finfo,"Phylum","Group_fine")
finfo$Group_broad<-"soilfungi"
finfo$Class<-finfo$Order<-finfo$Family<-finfo$Genus<-finfo$Species<-NULL

#merge
soilf<-merge(soilf,finfo,by.x="Species",by.y="OTU")

#add "soilf_" before otu number (to avoid confusion with bacteria or protists)
set(soilf,j="Species",value=paste("soilf_",soilf$Species,sep=""))

soilf$Probability<-soilf$TrophicMode<-NULL

grl2<-rbind(grl2,soilf,use.names=T)
rm(soilf,finfo); gc()
###################################################################################


#9###add dataset versions##########################################################
sort(as.numeric(unique(grl2$DataID)))
grl2[DataID==4140,Dataversion:="1.2.5"]; grl2[DataID==5522,Dataversion:="1.8.10"]
grl2[DataID==6100,Dataversion:="2.1.3"]; grl2[DataID==11422,Dataversion:="1.1.2"]
grl2[DataID==12386,Dataversion:="1.1.3"]; grl2[DataID==12526,Dataversion:="1.8.18"]
grl2[DataID==13146,Dataversion:="1.1.8"]; grl2[DataID==13526,Dataversion:="1.2.4"]
grl2[DataID==15086,Dataversion:="1.1.26"]; grl2[DataID==16746,Dataversion:="1.1.3"]
grl2[DataID==16871,Dataversion:="1.1.2"]; grl2[DataID==16893,Dataversion:="1.1.1"]
grl2[DataID==16894,Dataversion:="1.1.1"]; grl2[DataID==16895,Dataversion:="1.1.1"]
grl2[DataID==16896,Dataversion:="1.1.1"]; grl2[DataID==16897,Dataversion:="1.1.1"]
grl2[DataID==16908,Dataversion:="1.1.1"]; grl2[DataID==18166,Dataversion:="2.2.7"]
grl2[DataID==18187,Dataversion:="1.1.3"]; grl2[DataID==18206,Dataversion:="1.1.1"]
grl2[DataID==18207,Dataversion:="1.1.1"]; grl2[DataID==18208,Dataversion:="1.1.1"]
grl2[DataID==18226,Dataversion:="1.1.0"]; grl2[DataID==18548,Dataversion:="1.1.0"]
grl2[DataID==19686,Dataversion:="1.6.12"]; grl2[DataID==19786,Dataversion:="1.2.3"]
grl2[DataID==20146,Dataversion:="1.1.0"]; grl2[DataID==21048,Dataversion:="1.1.3"]
grl2[DataID==21449,Dataversion:="4.1.2"]; grl2[DataID==24426,Dataversion:="1.2.10"]
grl2[DataID==24466,Dataversion:="1.2.4"]; grl2[DataID==24690,Dataversion:="4.1.2"]
grl2[DataID==24866,Dataversion:="1.1.8"]; grl2[DataID==25066,Dataversion:="1.1.4"]
grl2[DataID==26470,Dataversion:="1.1.1"]; grl2[DataID==26471,Dataversion:="1.1.1"]
grl2[DataID==26472,Dataversion:="1.1.1"]
sum(is.na(grl2$Dataversion))
###################################################################################


#10##add information on taxa is trophic.level==NA##################################
length(unique(grl2[Trophic_level=="decomposer"]$Plot))
length(unique(grl2[Group_broad=="Myriapoda"]$Plot))
unique(grl2[is.na(Trophic_level)]$Group_broad)
unique(grl2[Group_broad=="Myriapoda"]$Species)
unique(grl2[Trophic_level=="belowground.predator"]$Group_broad)
unique(grl2[Trophic_level=="decomposer"]$DataID)
unique(grl2[is.na(Trophic_level)]$Species)

#millipedes are herbivores/detritivores
#centipedes are carnivorous (Geophilus,Pachymerium, Schendyla, Strigamia)
grl2[Group_broad=="Myriapoda",Trophic_level:="detritivore"]
summary(grl2[Group_broad=="Myriapoda"]) #ok values are similar than arthropod ones, can be secondary.consumer
length(unique(grl2[Group_broad=="Myriapoda"]$Plot))
sum(is.na(grl2[Group_broad=="Myriapoda"]$Plot)) #ok 150 plots with no NAs
grl2[Species %in% c("Geophilus_flavus","Pachymerium_ferrugineum",
                    "Schendyla_nemorensis","Strigamia_crassipes"),Trophic_level:="secondary.consumer"]
###################################################################################


#11##add dataset on ants (ID:23986)################################################
ant<-fread("Exploratories/Data/GRASSLANDS/TEXTfiles/190319_Update/23986.txt")
ant<-ant[,.(Plot,Species,Presence_absence)]
ant$Species<-gsub(" ","_",ant$Species)
ant$DataID<-23986
ant$Dataversion<-"2.1.6"
ant$Year<-"2014_2015"
ant<-data.table(BEplotNonZeros(ant,"Plot",plotnam = "Plot_bexis"))
setnames(ant,"Presence_absence","value")
ant$type<-"presenceabsence"
ant$Trophic_level<-ant$Fun_group_broad<-ant$Fun_group_fine<-"omnivore.ant"
ant$Group_broad<-ant$Group_fine<-"Formicidae"
#ant[, c("Group_fine", "sp") := tstrsplit(Species, "_", fixed=TRUE)]
#ant$sp<-NULL
length(unique(ant$Species))*length(unique(ant$Plot)) #31 species and 110 plots..

#overlap with existing species?
intersect(unique(ant$Species),unique(grl2$Species)) #11 overlapping species!

#remove from ant dataset (because pollinator dataset is more complete)
ant<-ant[!Species %in% intersect(unique(ant$Species),unique(grl2$Species))]

grl2<-rbind(grl2,ant)
rm(ant); gc()
###################################################################################



#12##add dataset on snails#########################################################
snail<-fread("Exploratories/Data/GRASSLANDS/TEXTfiles/190319_Update/24986.txt")
snail<-snail[Habitat=="GRA"]
snail$Habitat<-snail$Exploratory<-NULL
length(unique(snail$Plot))*length(unique(snail$Species))*5 #131 plots and 66 species (and 5 replicates per plot)
#missing combinations are zeros or NAs? probably zeros
length(unique(snail$Plot))*5 #655
dim(unique(snail[,.(Plot,Subplot)])) #538, some combinations missing -use the mean per plot
#first include missing combinations
snail<-setDT(snail)[CJ(Species=Species,Plot=Plot,Subplot=Subplot,unique=T), on=.(Species, Plot,Subplot)]
snail[is.na(Abundance), Abundance := 0 ]
#Average per plot
snail[,value:=mean(Abundance),by=list(Plot,Species)]
snail$Abundance<-snail$Subplot<-NULL
snail<-unique(snail) #8646
#add plots with no snails at all (according to metadata: On plots AEG6, SEG36, SEG40, SEW8 and SEW24 no snail individuals have been found)
sn2<-data.table(expand.grid(Species=unique(snail$Species),Plot=c("AEG6","SEG36","SEG40","SEW8","SEW24")))
sn2$value<-0
snail<-rbind(snail,sn2); rm(sn2)
#add columns to match grl2
snail$DataID<-24986; snail$Dataversion<-"1.3.9"; snail$type<-"abundance"; snail$Year<-2017
setnames(snail,"Plot","Plot_bexis")
snail<-data.table(BEplotZeros(snail,"Plot_bexis",plotnam = "Plot"))
snail$Group_broad<-"Mollusca"; snail$Group_fine<-"Gastropoda"
snail$Species<-gsub(" ","_",snail$Species)

foods<-fread("Exploratories/Data/Traits/Snails_foodpreferences.csv")
snail<-merge(snail,foods)
rm(foods)
snail$Fun_group_broad<-snail$Fun_group_fine<-snail$Trophic_level
length(unique(snail$Species))*length(unique(snail$Plot))

grl2<-rbind(grl2,snail)
rm(snail); gc()
###################################################################################


#13##update names of bird datasets#################################################
bb<-grl2[Group_broad %in% c("Birds","Bats")]
unique(bb[,.(Year,DataID,Group_broad)])
rm(bb)

#birds (the authors stated that no changes were made, these datasets were not checked, but version set to NA)
grl2[DataID=="11422",DataID:="21446"]; grl2[DataID==21446,Dataversion:=NA]
grl2[DataID=="6100",DataID:="21447"]; grl2[DataID==21447,Dataversion:=NA]
grl2[DataID=="12386",DataID:="21448"]; grl2[DataID==21448,Dataversion:=NA]
#bats (the authors stated that no changes were made, these datasets were not checked, but version set to NA)
grl2[DataID=="13146",DataID:="19849"]; grl2[DataID==19849,Dataversion:=NA]
grl2[DataID=="13526",DataID:="19850"]; grl2[DataID==19850,Dataversion:=NA]
###################################################################################

#14##change Trophic_level name for myriapods#######################################
#from detritivore to myriapod.decomposer
unique(grl2[Group_broad=="Myriapoda"]$Trophic_level)
unique(grl2[Trophic_level=="detritivore"]$Group_broad)
grl2[Trophic_level=="detritivore",Trophic_level:="myriapod.decomposer"]
grl2[Group_broad=="Myriapoda" & Trophic_level=="secondary.consumer",Trophic_level:="myriapod.secondary.consumer"]
###################################################################################

#15##update plant temporal dataset#################################################
oldpl<-grl2[DataID %in% "19686"]
oldpl<-unique(oldpl[,.(Species,Group_broad,Group_fine,Trophic_level,Fun_group_broad,Fun_group_fine)])
pl<-fread("Exploratories/Data/GRASSLANDS/TEXTfiles/201022_Update/27386.txt")
pl$PlotID<-NULL
setnames(pl,c("EP_PlotID","Useful_EP_PlotID","Cover"),c("Plot_bexis","Plot","value"))
pl$DataID<-"27386"; pl$Dataversion<-"1.3.0"
pl$type<-"cover"
setdiff(names(grl2),names(pl))

# pl<-merge(pl,oldpl,by="Species",all.x=T)
# apply(pl,2,function(x)sum(is.na(x)))
# unique(pl[is.na(Group_broad)]$Species) #need to create table about tax or functional group
# oldpl<-rbindlist(list(oldpl,data.table(Species=unique(pl[is.na(Group_broad)]$Species))),fill=T)
#write.table(oldpl,"Exploratories/Data/GRASSLANDS/TemporalPlants/plant_tax2019.txt",row.names=F) #complete missing species in txt file directly
oldpl<-fread("Exploratories/Data/GRASSLANDS/TemporalPlants/plant_tax2019.txt")
setdiff(oldpl$Species,pl$Species)
setdiff(pl$Species,oldpl$Species) #any new species? if so add them to the txt file
pl<-merge(pl,oldpl,by="Species",all.x=T)
apply(pl,2,function(x)sum(is.na(x)))
#fix species names
pl[grep("__",pl$Species),]
pl$Species<-gsub("__","_",pl$Species) #double underscore (none in this case)
pl$Species<-sub("_$","",pl$Species) #dot at the end


grl2<-grl2[!DataID %in% "19686"] #remove old plant data from dataset
setdiff(names(grl2),names(pl))

grl2<-rbindlist(list(grl2,pl),use.names = T) #add new dataset
rm(pl,oldpl)
gc()
###################################################################################


#16##add back NAs for all non-OTU datasets########################################
apply(grl2,2,function(x)sum(is.na(x)))
#check where combinations are missing
for (i in sort(unique(grl2$DataID))){
  tt<-grl2[DataID==i]
  if(length(unique(tt$Plot))*length(unique(tt$Species))*length(unique(tt$Year))!=nrow(tt))
    print(paste(i,":",length(unique(tt$Plot))*length(unique(tt$Species)),"/",nrow(tt)))
}

#check datasets 
tt<-grl2[DataID==16908] #pollinator dataset, missing information are NAs
tt<-grl2[DataID==18548] #plant.pathogen dataset, missing information are NAs
tt<-grl2[DataID==24866] #bacteria dataset, cannot add zeros (dataset would be too large) + two plots missing AEG33 AEG34
tt<-grl2[DataID==25066] #bacteria dataset, cannot add zeros (dataset would be too large) + two plots missing AEG33 AEG34
tt<-grl2[DataID==26470] #soilfungi dataset, cannot add zeros (dataset would be too large)
tt<-grl2[DataID==26471] #soilfungi dataset, cannot add zeros (dataset would be too large)
tt<-grl2[DataID==26472] #soilfungi dataset, cannot add zeros (dataset would be too large)

setdiff(unique(grl2$Plot),unique(tt$Plot))
length(unique(tt$Plot))
length(unique(tt$Species))
summary(tt$value)
rm(tt,i)

##add back NAs in pollinators dataset
tt<-grl2[Trophic_level=="pollinator"]
unique(tt$DataID) #pollinators are from different datasets
length(unique(tt$Plot))*length(unique(tt$Species)) #146 plots and 705 species
#add missing plots (that have only NAs) 
setdiff(unique(grl2$Plot),unique(tt$Plot)) #AEG29, AEG50, HEG13, HEG25
plotstoadd<-tt[1:4,]
plotstoadd$Plot<-plotstoadd$Plot_bexis<-c("AEG29", "AEG50", "HEG13", "HEG25")
plotstoadd$value<-NA
tt<-rbindlist(list(tt,plotstoadd))

tt<-setDT(tt)[CJ(Species=Species,Plot=Plot,unique=T), on=.(Species, Plot)] #create all plot x species combinations

tt<-merge(tt[,.(Species,Plot,value)],
          unique(tt[,!c("Plot","Plot_bexis","value"),with=F],by="Species"),
          by="Species",all.x=T)
tt<-tt[is.na(value)]
tt<-data.table(BEplotNonZeros(tt,"Plot","Plot_bexis"))
grl2<-rbindlist(list(grl2,tt),use.names = T)

##add back NAs in plant.pathogen dataset
tt<-grl2[DataID==18548]
unique(grl2[Group_broad=="Plant.pathogen"]$DataID)
length(unique(tt$Plot))*length(unique(tt$Species)) #150 plots and 84 species
tt[!complete.cases(tt)]
tt[Species=="Urocystis_agropyri" & Plot=="AEG01"]
tt2<-na.omit(tt) #remove 6 plots where "Urocystis_agropyri" is NA
tt<-setDT(tt)[CJ(Species=Species,Plot=Plot,unique=T), on=.(Species, Plot)]
tt<-merge(tt[,.(Species,Plot,value)],
          unique(tt2[,!c("Plot","Plot_bexis","value"),with=F],by="Species"),
          by="Species",all.x=T)
tt<-tt[is.na(value)]
unique(tt$DataID)
tt<-data.table(BEplotNonZeros(tt,"Plot","Plot_bexis"))
#remove "Urocystis_agropyri" NA plots from the dataset
grl2<-grl2[!(Species=="Urocystis_agropyri" & is.na(value))]
grl2<-rbindlist(list(grl2,tt),use.names = T)
rm(tt,tt2,i)

##new check, this time on group broad
for (i in unique(grl2$Group_broad)){
  tt<-grl2[Group_broad==i]
  if(length(unique(tt$Plot))*length(unique(tt$Species))*length(unique(tt$Year))!=nrow(tt))
    print(paste(i,":",length(unique(tt$Plot))*length(unique(tt$Species)),"/",nrow(tt)))
}

#coleoptera and hymenoptera- this is because of the pollinator that now have NAs for missing plots
tt<-grl2[Group_broad=="Coleoptera"]

#add NAs for all arhtropods?
tt<-grl2[Trophic_level %in% c("herbivore","secondary.consumer","decomposer","omnivore")]
length(unique(tt$Plot))
length(unique(tt$Species))
#herbivores have 150 plots
tt<-grl2[Trophic_level %in% c("herbivore")]
unique(tt$DataID)
#hemiptera (16893), coleoptera (16871), hymeno (16908), orthopt (16895)
ttw<-dcast.data.table(tt,Species~Plot,value.var="value",fill=NA)
summary(ttw)
sum(apply(ttw,2,function(x)sum(is.na(x)))>0) #11 plots with NAs
sum(apply(ttw,1,function(x)sum(is.na(x)))>0) #399 species with NAs (2 without!)
#Tenthredo_arcuata and Tenthredo_notha these two species have data for 150 plots
#this is because in the original dataset there were values for the plots, but not in Eric's initial dataset (16908)
tt[Species=="Tenthredo_arcuata" & value==1]
#correct these two species
# plotsNA<-names(which(apply(ttw,2,function(x)sum(is.na(x)))>0))
# tt[Species %in% c("Tenthredo_arcuata","Tenthredo_notha") & Plot %in% plotsNA]
# which(apply(ttw,2,function(x)sum(is.na(x)))>0)
#values are different, need to reupload from Eric's dataset
d1<-fread("Exploratories/Data/GRASSLANDS/All species EPG 19.06.15.txt")
d2<-fread("Exploratories/Data/GRASSLANDS/EP species list FGs 19.06.15.txt")
d0<-merge(d1,d2,by="Species")
tent<-d0[Species %in% c("Tenthredo.arcuata","Tenthredo.notha")]
tent$Species<-gsub('\\.',"_",tent$Species)
unique(tt[Species %in% c("Tenthredo_arcuata","Tenthredo_notha"),.(Species,DataID,Group_broad,Group_fine,Trophic_level,Fun_group_broad,Fun_group_fine)])
tent$Well.dispersed<-tent$Body.size<-tent$Clade.y<-NULL
setnames(tent,names(tent),c("Species","Group_broad","Plot","value","Trophic_level","Fun_group_fine","Fun_group_broad"))
tent$DataID<-16908; tent$Dataversion<-"1.1.1"; tent$Year<-2008; tent$type<-"abundance"
tent$Group_fine<-tent$Group_broad
tent<-data.table(BEplotNonZeros(tent,"Plot","Plot_bexis"))
#remove from main dataset and add
grl2<-grl2[!Species %in% c("Tenthredo_arcuata","Tenthredo_notha")]
grl2<-rbindlist(list(grl2,tent),use.names = T)

####add NAs for all arthropods
length(unique(grl2[Trophic_level %in% c("herbivore","secondary.consumer","decomposer","pollinator","omnivore")]$Species)) #1330
tt<-grl2[Trophic_level %in% c("herbivore","secondary.consumer","decomposer","pollinator","omnivore")]
length(unique(tt$Plot))*length(unique(tt$Species)) #150 plots and 1330 species
tt<-setDT(tt)[CJ(Species=Species,Plot=Plot,unique=T), on=.(Species, Plot)]
tt<-merge(tt[,.(Species,Plot,value)],
          unique(tt[,!c("Plot_bexis","Plot","value")],by="Species"),
          by="Species",all.x=T)
tt<-data.table(BEplotNonZeros(tt,"Plot","Plot_bexis"))
#add to main dataset
grl2<-grl2[!Trophic_level %in% c("herbivore","secondary.consumer","decomposer","pollinator","omnivore")]
grl2<-rbindlist(list(grl2,tt),use.names = T)


#bats seem to have an issue
tt<-grl2[Group_broad=="Bats"]
unique(tt$DataID)
length(unique(tt$Plot))*length(unique(tt$Species))*2 #should be 3300
length(unique(tt[DataID=="19849"]$Plot)) #one plot missing
length(unique(tt[DataID=="19850"]$Plot))
setdiff(unique(tt[DataID=="19850"]$Plot),unique(tt[DataID=="19849"]$Plot)) #AEG04
pp<-tt[DataID=="19850"& Plot=="AEG04"] #use info from other dataset (2010) to create missing lines
pp$value<-NA; pp$DataID<-19849; pp$Year<-"2009"

grl2<-rbindlist(list(grl2,pp),use.names = T)
rm(tt,pp,d0,d1,d2,plotstoadd,tent,ttw)

##new check, this time on trophic level
for (i in unique(grl2$Trophic_level)){
  tt<-grl2[Group_broad==i]
  if(length(unique(tt$Plot))*length(unique(tt$Species))*length(unique(tt$Year))!=nrow(tt))
    print(paste(i,":",length(unique(tt$Plot))*length(unique(tt$Species)),"/",nrow(tt)))
}


###check if everyone has all 150 plots
for (i in sort(unique(grl2$DataID))){
  tt<-grl2[DataID==i]
  if(length(unique(tt$Plot))!=150)
    print(paste(i,":",length(unique(tt$Plot))))
}
rm(i,tt)

#many datasets have missing plots, adding NA's for all would significantly increase the dataset size
#maybe just add this info in metadata

###################################################################################


#17##fix issue in bird dataset#####################################################
#Remove 2011 (ID 21449 and add back the correct 2011 dataset)
grl2[DataID==21449]
birds<-fread("Exploratories/Data/GRASSLANDS/TEXTfiles/Bird_data_2008_2012_EA.txt")
birdsm<-melt(birds,measure.vars=3:97,variable.name="Species",value.name="value")
birdsm$type<-"abundance"
birdsm$Clade<-"Birds"
birdsm<-birdsm[year==2011]
birdsm$DataID<-21449
birdsm$Dataversion<-"4.1.2"
setnames(birdsm,"year","Year")
birdsm<-data.table(BEplotNonZeros(birdsm,"Plot",plotnam="Plot_bexis"))
birdsm$Clade<-NULL
rm(birds)

#any new species compared to other years?
allbi<-grl2[DataID %in% c(21446,21447,21448,21449,24690)]
setdiff(unique(allbi$Species),unique(birdsm$Species)) #yes
setdiff(unique(birdsm$Species),unique(allbi$Species)) #yes
rm(allbi)
#again, add zeros to bird datasets (to avoid people filling it with NAs and be consistent with other multiyear datasets)
allbi<-grl2[DataID %in% c(21446,21447,21448,24690)]
allbi<-rbindlist(list(allbi,birdsm),use.names = T,fill=T)
unique(allbi$DataID)
length(unique(allbi$Plot))*length(unique(allbi$Species))*length(unique(allbi$Year)) #final number of rows
#produce zeros and check if some species have zeros in all plots
allbic<-setDT(allbi)[CJ(Species=Species,Plot=Plot,Year=Year,unique=T), on=.(Species, Plot, Year)]
allbic[is.na(value), value := 0 ]
#merge trophic info
tr<-fread("Exploratories/Data/GRASSLANDS/170724_EP_species_info_GRL.txt")
tr2<-fread("Exploratories/Data/GRASSLANDS/190218_EP_species_info_GRL.txt")
tr<-unique(rbindlist(list(tr,tr2)),by="Species")
allbic<-merge(allbic[,.(Species,Plot,Year,value)],tr,by="Species",all.x=T)
#add missing columns
setdiff(names(grl2),names(allbic))
allbic<-data.table(BEplotNonZeros(allbic,"Plot",plotnam="Plot_bexis"))
allbic$type<-"abundance"
allbic[Year==2008,DataID:=21446]; allbic[Year==2009,DataID:=21447]; allbic[Year==2010,DataID:=21448]
allbic[Year==2011,DataID:=21449]; allbic[Year==2012,DataID:=24690]
allbic$Dataversion<-NA ; allbic[DataID==24690,Dataversion:="4.1.2"]

length(unique(allbic$Plot))*length(unique(allbic$Species))*length(unique(allbic$Year));nrow(allbic)
apply(allbic,2,function(x)sum(is.na(x)))

#remove old and add new
grl2<-grl2[!DataID %in% c(21446,21447,21448,21449,24690)]
grl2<-rbindlist(list(grl2,allbic),use.names = T)
rm(allbi,allbic,tr,tr2,birdsm)

###################################################################################


#18##add Collembola dataset 27007#################################################
coll<-fread("Exploratories/Data/GRASSLANDS/TEXTfiles/201022_Update/27007.txt")
coll$Region<-coll$abundance_total<-coll$richness<-coll$Plot<-coll$No<-NULL
coll<-data.table(BEplotZeros(coll,"Plot_ID",plotnam = "Plot")) #add plotzero
setnames(coll,names(coll),c("Plot_bexis","Species","value","Plot")) #rename to match main table
coll$type<-"abundance"; coll$DataID<-27007; coll$Year<-2019; coll$Dataversion<-"3.3.5" #add metadata 
coll$Group_broad<-"Collembola"; coll$Group_fine<-"Collembola"
#add "_" in species name
coll$Species<-gsub(" ","_",coll$Species)
#classification trophic level (after discussion with Dennis Baulechner)
coll$Trophic_level<-coll$Fun_group_broad<-coll$Fun_group_fine<-"decomposer"

grl2<-rbindlist(list(grl2,coll),use.names = T)
rm(coll)

###################################################################################

#19##add Acari dataset 27406#######################################################
aca<-fread("Exploratories/Data/GRASSLANDS/TEXTfiles/201022_Update/27406.txt")
length(unique(aca$Plot_ID))
aca$Region<-aca$No<-aca$Plot<-NULL
setnames(aca,names(aca),c("Plot_bexis","Species","value")) #rename to match main table
aca<-data.table(BEplotZeros(aca,"Plot_bexis",plotnam = "Plot")) #add plotzero
length(unique(aca$Plot))
aca$type<-"abundance"; aca$DataID<-27406; aca$Year<-2019; aca$Dataversion<-"1.4.14" #add metadata
aca$Group_broad<-"Acari"; aca$Group_fine<-"Oribatida"
#replace "." with "_" in species name
aca$Species<-gsub('\\.',"_",aca$Species)

aca[grep("__",aca$Species),]
aca$Species<-gsub("__","_",aca$Species)
aca$Species<-sub("_$","",aca$Species)
aca$Species<-sub("_$","",aca$Species)#twice
sort(unique(aca$Species))

#add trophic level
tr<-fread("Exploratories/Data/Traits/2019_Mite_fauna_trophic_groups.csv")
setnames(tr,names(tr),c("Species","Fun_group_fine"))
sort(unique(grl2$Fun_group_fine))
tr[Fun_group_fine %in% "herbivorous",Fun_group_fine:="decomposer.herbivore.acari"]
tr[Fun_group_fine %in% "fungivorous",Fun_group_fine:="decomposer.fungivore.acari"]
tr[Fun_group_fine %in% "omnivorous",Fun_group_fine:="decomposer.omnivore.acari"]
tr[Fun_group_fine %in% "herbifungivorous",Fun_group_fine:="decomposer.herbifungivore.acari"]

sort(unique(grl2$Fun_group_broad))
sort(unique(grl2$Trophic_level))
tr$Fun_group_broad<-tr$Trophic_level<-"decomposer.acari"

#homogeneise species names
tr[Species=="Ceratozetella_sellnicki",Species:="Ceratozetes_sellnicki"]
tr[Species=="Nanhermannia_nanus",Species:="Nanhermannia_nana"]
tr[Species=="Phthiracarus_globosus",Species:="Phthiracarus_globulus"]
tr[Species=="Poecilochthonius_italicus",Species:="Poeliochthonius_italicus"]
tr[Species=="Poecilochthonius_spiciger",Species:="Poeliochthonius_spiciger"]
tr[Species=="Brachychochthonius_cricoides",Species:="Sellnickochthonius_cricoides"]
tr[Species=="Brachychochthonius_immaculatus",Species:="Sellnickochthonius_immaculatus"]
tr[Species=="Protoribates_pannonicus",Species:="Liebstadia_pannonica"]
tr[Species=="Oribella_paolii",Species:="Panthelozetes_paoli"]
tr[Species=="Heminothrus_peltifer",Species:="Platynothrus_peltifer"]


aca<-merge(aca,tr,by="Species",all.x=T)
unique(aca[is.na(Trophic_level)]$Species)
sort(unique(aca$Species))

aca[Species=="Gamasidae", c("Trophic_level","Fun_group_fine","Fun_group_broad"):="carnivore.acari"]
aca[is.na(Trophic_level),Trophic_level:="unknown.acari"]
unique(aca[is.na(Fun_group_broad)]$Species)
aca[is.na(Fun_group_broad),Fun_group_broad:="unknown.acari"]
unique(aca[is.na(Fun_group_fine)]$Species)
aca[is.na(Fun_group_fine),Fun_group_fine:="unknown.acari"]
apply(aca,2,function(x)sum(is.na(x)))


setdiff(names(grl2),names(aca))

grl2<-rbindlist(list(grl2,aca),use.names = T)
rm(aca,tr)

###################################################################################

#20##add Birds 2018 dataset 25306##################################################
bird<-fread("Exploratories/Data/GRASSLANDS/TEXTfiles/201022_Update/25306.txt")
bird<-bird[type=="grassland"] #only grasslands
length(unique(bird$plot_ID))
allpl<-unique(bird$plot_ID)
bird<-bird[on_EP==1] #select only birds seen on EPs to match previous datasets
bird<-bird[,.(plot_ID,species_latin,abundance,diet)] #select only target columns
#add zeros (many plots have zero species)
birdc<-dcast.data.table(bird,species_latin+diet~plot_ID,value.var="abundance",fill=0,fun.aggregate = sum) #sum over all rounds
length(unique(bird$species_latin))*length(unique(bird$plot_ID)) 
bird<-melt.data.table(birdc,measure.vars=3:92,variable.name="Plot")
length(unique(bird$Plot)) #90 plots, add missing plots (that are zeros, because the dataset is complete)
setdiff(allpl,unique(bird$Plot))
length(unique(bird$species_latin))
#create zeros for missing plots
allpl<-data.table(Plot=setdiff(allpl,unique(bird$Plot)),species_latin=unique(bird$species_latin),value=0) #dataset of missing plots x species
allpl<-setDT(allpl)[CJ(species_latin=species_latin,Plot=Plot,unique=T), on=.(species_latin, Plot)] #all combinations
allpl[is.na(value), value := 0 ] #45*60 (25 species and 60 plots missing)
summary(allpl$value)
#merge info on trophic level
allpl<-merge(allpl,unique(bird[,.(species_latin,diet)]),by="species_latin")
#add to dataset
bird<-rbindlist(list(bird,allpl),use.names = T) #45*150

#some rounds were not recorded, e.g. cows on the plot -> remove the plots (HEG23, HEG25, HEG35, SEG17, SEG26, SEG35, SEG47)
bird[Plot %in% c("HEG23", "HEG25", "HEG35")]
bird[Plot %in% c("HEG23", "HEG25", "HEG35"), value:=NA]

#rename and arrange
setnames(bird,names(bird),c("Species","Fun_group_fine","Plot_bexis","value")) #rename to match main table
bird<-data.table(BEplotZeros(bird,"Plot_bexis",plotnam = "Plot")) #add plotzero
bird$type<-"abundance"; bird$DataID<-25306; bird$Year<-2018; bird$Dataversion<-"1.0.0" #add metadata
bird$Group_broad<-"Birds"; bird$Group_fine<-"Birds"

#Harmonise species names (keep the ones that are already in synthesis dataset)
sort(setdiff(unique(bird$Species),unique(grl2[Group_broad=="Birds"]$Species))) #new species
sort(setdiff(unique(grl2[Group_broad=="Birds"]$Species),unique(bird$Species)))

bird[Species=="Corvus_corone",Species:="Corvus_corone_corone"]
bird[Species=="Corvus_cornix",Species:="Corvus_corone_cornix"]


# bird[Species=="Sylvia_curruca",Species:="Sylvia_curucca"]#those are not in EPs
# bird[Species=="Columba_livia_",Species:="Columba_livia"]
# bird[Species=="Delichon_urbicum",Species:="Delichon_urbica"]
bird[Species=="Regulus_ignicapilla",Species:="Regulus_ignicapillus"]
bird[Species=="Miliaria_calandra",Species:="Emberiza_calandra"]
bird[Species=="Parus_ater",Species:="Periparus_ater"]
bird[Species=="Parus_caeruleus",Species:="Cyanistes_caeruleus"]
bird[Species=="Saxicola_torquatus",Species:="Saxicola_rubicola"]

#harmonize trophic level and create "traits" from new species
birdtr<-bird[,.(Species,Fun_group_fine)]
bird$Fun_group_fine<-NULL
newsp<-sort(setdiff(unique(bird$Species),unique(grl2[Group_broad=="Birds"]$Species))) #new species
birdtr<-unique(birdtr[Species %in% newsp])

unique(birdtr$Fun_group_fine)
unique(grl2[Group_broad=="Birds"]$Trophic_level)
unique(grl2[Group_broad=="Birds"]$Fun_group_broad)
unique(grl2[Group_broad=="Birds"]$Fun_group_fine)

birdtr[Fun_group_fine=="Omnivore",Fun_group_fine:="omnivore"]
birdtr[Fun_group_fine=="Invertebrate",Fun_group_fine:="insectivore"]
birdtr[Fun_group_fine=="PlantSeed",Fun_group_fine:="granivore"] #this has to be refined to grazer/granivore
birdtr[Fun_group_fine=="VertFishScav",Fun_group_fine:="carnivore"]

unique(birdtr[Fun_group_fine=="granivore"]$Species)
birdtr[Species %in% c("Columba_oenas"),Fun_group_fine:="grazer"] #Pyrrhula_pyrrhula is mainly granivore too

birdtr$Fun_group_broad<-"vert.predator"
birdtr[Fun_group_fine %in% c("grazer","granivore"),Fun_group_broad:="vert.herb"]

birdtr$Trophic_level<-"tertiary.consumer"
birdtr[Fun_group_broad=="vert.herb",Trophic_level:="vert.herb"]

#add info on traits from main table and merge
allbirdtr<-grl2[Group_broad=="Birds"]
allbirdtr<-unique(allbirdtr[,names(birdtr),with=F])
allbirdtr<-rbindlist(list(allbirdtr,birdtr))
bird<-merge(bird,allbirdtr,by="Species")

#create "zero dataset" for all years for the two species that are new in 2018
newbi<-sort(setdiff(unique(bird$Species),unique(grl2[Group_broad=="Birds"]$Species))) #those need zeros in the other years (2008 to 2012)
newbioldy<-data.table(Species=newbi,Plot_bexis=unique(bird$Plot_bexis),value=0,Year=2008:2012)
newbioldy<-setDT(newbioldy)[CJ(Species=Species,Plot_bexis=Plot_bexis,Year=Year,unique=T), on=.(Species, Plot_bexis, Year)] #all combinations 2 species * 150 plots * 5 years
newbioldy[is.na(value), value := 0 ]
newbioldy$type<-"abundance"
newbioldy$Group_broad<-newbioldy$Group_fine<-"Birds"
newbioldy$Fun_group_fine<-"insectivore"
newbioldy[Species=="Emberiza_schoeniclus",Fun_group_fine:="granivore"]
newbioldy$Fun_group_broad<-"vert.predator"
newbioldy[Species=="Emberiza_schoeniclus",Fun_group_broad:="vert.herb"]
newbioldy$Trophic_level<-"tertiary.consumer"
newbioldy[Species=="Emberiza_schoeniclus",Trophic_level:="vert.herb"]
newbioldy$Dataversion<-NA
yearver<-unique(grl2[Group_broad=="Birds",.(Year,DataID)])
yearver$Year<-as.integer(yearver$Year)
newbioldy<-merge(newbioldy,yearver,by="Year")
newbioldy<-data.table(BEplotZeros(newbioldy,"Plot_bexis",plotnam="Plot"))

#create "zero dataset" for all years for the two species that are new in 2018
oldbi<-sort(setdiff(unique(grl2[Group_broad=="Birds"]$Species),unique(bird$Species))) #those need zeros in 2018 (55 species)
oldbi<-grl2[Species %in% oldbi & Year=="2008"]
oldbi$Year<-"2018"
oldbi$value<-0
oldbi$DataID<-"25306"

#add to main dataset
setdiff(names(bird),names(grl2))
grl2<-rbindlist(list(grl2,bird,oldbi,newbioldy),use.names=TRUE)

#checks
checkbi<-grl2[Group_broad=="Birds"]
length(unique(checkbi$Species))*length(unique(checkbi$Year))*length(unique(checkbi$Plot)) #90 000
# checkbi<-setDT(checkbi)[CJ(Species=Species,Plot_bexis=Plot_bexis,Year=Year,unique=T), on=.(Species, Plot_bexis, Year)]
# checkbi[is.na(value)]
# checkbi[Species=="Acrocephalus_schoenobaenus" & Plot_bexis=="AEG1"]

rm(allbirdtr,birdtr,bird,newsp,birdc,allpl,checkbi,newbioldy,oldbi,yearver,newbi); gc()


###################################################################################

#20##Homogeneise trophic level names: first group then taxa########################
#Initial categories can become confusing, homogenise and specify names
unique(grl2$Trophic_level)
# [1] "herbivore"                   "autotroph"                   "secondary.consumer"          "decomposer"                  "pollinator"                 
# [6] "plant.pathogen"              "belowground.predator"        "tertiary.consumer"           "myriapod.decomposer"         "belowground.herbivore"      
# [11] "omnivore"                    "myriapod.secondary.consumer" "protist.omnivore"            "protist.eukaryvore"          "protist.plant.parasite"     
# [16] "protist.bacterivore"         "protist.unknown"             "protist.parasite.nonplant"   "protist.mainly.bacterivore"  "bacteria.RNA"               
# [21] "soilfungi.other"             "soilfungi.decomposer"        "soilfungi.symbiont"          "soilfungi.pathotroph"        "omnivore.ant"               
# [26] "herbivore.snail"             "omnivore.snail"              "vert.herb"                   "decomposer.acari"            NA                           
# [31] "carnivore.acari"


grl2[Trophic_level=="herbivore",Trophic_level:="herbivore.arthropod"]
grl2[Trophic_level=="secondary.consumer",Trophic_level:="secondary.consumer.arthropod"]
grl2[Trophic_level=="decomposer",Trophic_level:="decomposer.arthropod"]
grl2[Trophic_level=="pollinator",Trophic_level:="pollinator.arthropod"]
grl2[Trophic_level=="plant.pathogen",Trophic_level:="plantpathogen.fungi"]
grl2[Trophic_level=="belowground.predator",Trophic_level:="secondary.consumer.arthropodsoillarvae"]
grl2[Trophic_level=="tertiary.consumer",Trophic_level:="tertiary.consumer.birdbat"]
grl2[Trophic_level=="myriapod.decomposer",Trophic_level:="decomposer.myriapod"]
grl2[Trophic_level=="belowground.herbivore",Trophic_level:="herbivore.arthropodsoillarvae"]
grl2[Trophic_level=="omnivore",Trophic_level:="omnivore.arthropod"]
grl2[Trophic_level=="myriapod.secondary.consumer",Trophic_level:="secondary.consumer.myriapod"]
grl2[Trophic_level=="protist.omnivore",Trophic_level:="omnivore.protist"]
grl2[Trophic_level=="protist.eukaryvore",Trophic_level:="eukaryvore.protist"]
grl2[Trophic_level=="protist.plant.parasite",Trophic_level:="plantparasite.protist"]
grl2[Trophic_level=="protist.unknown",Trophic_level:="unknown.protist"]
grl2[Trophic_level=="nonplantparasite.nonplant",Trophic_level:="nonplantparasite.protist"]
grl2[Trophic_level=="protist.mainly.bacterivore",Trophic_level:="mainlybacterivore.protist"]
grl2[Trophic_level=="protist.bacterivore",Trophic_level:="bacterivore.protist"]
grl2[Trophic_level=="soilfungi.other",Trophic_level:="unknown.soilfungi"]
grl2[Trophic_level=="soilfungi.decomposer",Trophic_level:="decomposer.soilfungi"]
grl2[Trophic_level=="soilfungi.symbiont",Trophic_level:="symbiont.soilfungi"]
grl2[Trophic_level=="soilfungi.pathotroph",Trophic_level:="pathotroph.soilfungi"]
grl2[Trophic_level=="vert.herb",Trophic_level:="herbivore.bird"]

unique(grl2$Trophic_level)
# [1] "herbivore.arthropod"                    "autotroph"                              "secondary.consumer.arthropod"          
# [4] "decomposer.arthropod"                   "pollinator.arthropod"                   "plantpathogen.fungi"                   
# [7] "secondary.consumer.arthropodsoillarvae" "tertiary.consumer.birdbat"              "decomposer.myriapod"                   
# [10] "herbivore.arthropodsoillarvae"          "omnivore.arthropod"                     "secondary.consumer.myriapod"           
# [13] "omnivore.protist"                       "eukaryvore.protist"                     "plantparasite.protist"                 
# [16] "bacterivore.protist"                    "unknown.protist"                        "parasitenonplant.protist"              
# [19] "mainlybacterivore.protist"              "bacteria.RNA"                           "unknown.soilfungi"                     
# [22] "decomposer.soilfungi"                   "symbiont.soilfungi"                     "pathotroph.soilfungi"                  
# [25] "omnivore.ant"                           "herbivore.snail"                        "omnivore.snail"                        
# [28] "herbivore.bird"                         "decomposer.acari"                       NA                                      
# [31] "carnivore.acari"  
###################################################################################

######################Lasts checks before saving table
#remove potential forest plots
grl2<-grl2[!grepl("W", grl2$Plot)]
length(unique(grl2$Plot))
sort(unique(grl2$Plot))
#in case there is an issue, compare to the plots of the plant dataset
#pl<-fread("Exploratories/Data/GRASSLANDS/TEXTfiles/201022_Update/27386.txt")
#length(unique(pl$Useful_EP_PlotID))
#setdiff(unique(grl2$Plot),unique(pl$Useful_EP_PlotID)) #there is a plot=NA
#grl2[is.na(Plot)] #in acari -> fix this


#check if columns contain NAs
apply(grl2,2,function(x)sum(is.na(x))) #ok, only in functional group columns and version (for birds and bats) + value

#check if there are species with only zeros (that were maybe only in forests)
sum(is.na(grl2$value))
grl2$value<-as.numeric(as.character(grl2$value))
grl2[,temp:=sum(value,na.rm=T),by=Species]
unique(grl2[temp==0]$Species) #one insect larvae, some birds (already in original data), (several moths) and several protists -> after checks, safe to remove them
grl2<-grl2[!temp==0]
grl2$temp<-NULL

#general checks
unique(grl2$Group_broad)
unique(grl2$Group_fine)
unique(grl2$Trophic_level)
unique(grl2$Fun_group_broad)
unique(grl2$Fun_group_fine)

#check Plot/Plot_bexis mismatch
dim(unique(grl2[,.(Plot,Plot_bexis)])) #150 -> ok

###check if everyone has all 150 plots
for (i in sort(unique(grl2$DataID))){
  tt<-grl2[DataID==i]
  if(length(unique(tt$Plot))!=150)
    print(paste(i,":",length(unique(tt$Plot)),unique(tt$Trophic_level),",",unique(tt$Group_broad)))
}

#arthropods have 139 plots -> ok
#mainly bacterivore protists dataIDs have 50 plots because there is one dataset per region -> ok
#other protists have 149 plots (24426) -> ok (missing SEG16)
#ants have 110 plots (23986) -> ok
#bacteria RNA have 148 plots (24866, 25066 - two IDs for two years) -> ok
#snails have 134 plots (24986) -> ok
#bacteria miss AEG33 and AEG34         

##soil fungi should have one plot missing? yes in 2014 and 2011
sf<-grl2[Group_broad=="soilfungi"]
length(unique(sf[Year=="2011"]$Plot))

rm(i,tt,sf)


###########Save the diversity and characteristics tables separately ###########
grl<-grl2[,.(Plot_bexis,Plot,Species,value,type,DataID,Year,Dataversion)]
tr<-grl2[,.(Species,Group_broad,Group_fine,Trophic_level,Fun_group_broad,Fun_group_fine)]
length(unique(tr$Species))
dim(unique(tr)) #some species are duplicated
tr<-unique(tr)
toRemove<-tr[duplicated(tr$Species)]
#tr[Species=="Formica_cunicularia"] #11 species recorded in pollinator and ant datasets -> remove them from ant dataset
#tr[Species=="Acrocephalus_schoenobaenus"]
#tr[Species=="Emberiza_schoeniclus"]
#grl2<-grl2[!(Species %in% toRemove$Species & Trophic_level=="ant.omnivore")]

grl<-grl2[,.(Plot_bexis,Plot,Species,value,type,DataID,Year,Dataversion)]
tr<-grl2[,.(Species,Group_broad,Group_fine,Trophic_level,Fun_group_broad,Fun_group_fine)]
length(unique(tr$Species))
length(unique(grl2$Species))
dim(unique(tr))
tr<-unique(tr)
sum(is.na(grl))
apply(grl,2,function(x)sum(is.na(x)))
sum(is.na(tr))
apply(tr,2,function(x)sum(is.na(x)))
tr[is.na(Fun_group_broad)] #two myriapods and one plant + several acari
rm(toRemove)


summary(factor(tr$Trophic_level))

#reorder column names in grl
setcolorder(grl,c("Plot_bexis","Plot","Species","value","type","Year","DataID","Dataversion"))

#all good: save
fwrite(grl,"Exploratories/Data/GRASSLANDS/201127_EP_species_diversity_GRL.txt",row.names=F,quote=F,sep=";",na=NA)
fwrite(tr,"Exploratories/Data/GRASSLANDS/201127_EP_species_info_GRL.txt",row.names=F,quote=F,sep=";",na=NA)



############NOT DONE, HAS TO BE DISCUSSED WITH ARTHROPODS CORE PROJECT###################
#NOT DONE##include temporal arthropod dataset from Sebastian Seibold#####################
#the dataset includes Coleoptera, Aranaea, Hemiptera and Orthoptera
#remove these groups only from the old synthesis data
arth<-fread("Exploratories/Data/GRASSLANDS/TEXTfiles/Grassland_KEF_2008_17_forSythesis_traits.csv")

#add zeros for absent species
length(unique(arth$Plot))*length(unique(arth$Species))*length(unique(arth$Year)) #150 plots, 1379 species, 10 yrs

arthc<-dcast.data.table(arth,Species+type+DataID+Year+Group_broad+Group_fine+
                          Trophic_level+Fun_group_broad+Fun_group_fine
                        ~Plot_bexis,value.var="value",fill=0)

# The second file includes a list with all plots with problems, i.e. either one of the two months 
# is missing or a certain group is missing. These plots should be excluded from abundance-based analyses.
# 
# Please note that the data is currently not including NA if a plot was not sampled. 
# However, it contains a 0 if a plot was sampled but a certain order was not present = true zero.
# 
# It also does not include zeros for each species, only on order level.
# 
# Also note that there are entries with individuals identified not to species. Should we exclude them? 
#   If ypou want to keep the zeros on order level, you would have to check if such a row needs to be added if the specs. 
# are removed, becaused there could be cases where the only specimen of an order couldn´t be identified.
# 
# 
# add order column


###################################################################################

#NOT DONE##add dungweb species###########################################################
# 21207: Dungwebs Species List 2014 & 2015 (Invertebrates, Scarabaeoidea, Dung Beetles) - downloaded 27/06/18
dung<-fread("Exploratories/Data/GRASSLANDS/TEXTfiles/190319_Update/21207.txt")
unique(dung$month) #use only august, july june 2014 (rest is VIP)
unique(dung$dungtype) #use only cow, deer, fox, horse, sheep, wildboar
#see how many are already in the dataset to decide whether to include the dataset
dungsp<-unique(dung$species)
setdiff(dungsp,unique(grl2$Species)) #none of the 34 sp is in the dataset -> add them
rm(dungsp)
dung<-dung[month %in% c("August_2014","July_2014","June_2014")]
dung$date<-dung$month<-dung$exotic<-dung$chem<-dung$dungtype<-NULL
setnames(dung,names(dung),c("Plot_bexis","Species","value"))
dung$Year<-2014; dung$type<-"abundance"; dung$DataID<-21207; dung$Dataversion<-"1.2.2"
dung<-data.table(BEplotZeros(dung,"Plot_bexis",plotnam="Plot"))
dung$Group_broad<-"Scarabaeidae"
dung$Group_fine<-"Scarabaeidae"
dung$Trophic_level<-"decomposer"
dung<-dung[!grepl("W", dung$Plot)] #remove forest data
dungtemp<-dcast.data.table(dung,Plot~Species,value.var = "value",fun.aggregate = sum)
dungtemp<-dungtemp[,colSums(dungtemp[,-1])>0] #only 7 species left ---> do not add them
rm(dung,dungtemp,spzero)
###################################################################################


#NOT DONE##add orthoptera species########################################################
# 19826: Orthoptera Density 2014 - all Grassland EPs - using biocenometer sampling: downloaded 25/06/18
ortho<-fread("Exploratories/Data/GRASSLANDS/TEXTfiles/190319_Update/19826.txt")
ortho$species<-gsub(" ","_",ortho$species)
orthosp<-unique(ortho$species)
setdiff(orthosp,unique(grl2$Species)) #only 11 species ---> do not add them
rm(ortho,orthosp)
###################################################################################

#NOT DONE##add Auchenorrhyncha species###################################################
# 20526: Auchenorrhyncha Density 2015 - all Grassland EPs - using biocenometer sampling : downloaded 27/06/18
aur<-fread("Exploratories/Data/GRASSLANDS/TEXTfiles/190319_Update/20526.txt")
aur$species<-gsub(" ","_",aur$species)
aursp<-unique(aur$species)
setdiff(aursp,unique(grl2$Species)) #59 species --> ok to add
unique(grl2[Species %in% unique(aur$species)]$Group_broad)
unique(grl2[Species %in% unique(aur$species)]$Trophic_level)
unique(grl2[Species %in% unique(aur$species)]$Fun_group_broad)
###are there any species in the synthesis dataset that are not in this dataset?
setdiff(unique(grl2[Group_broad=="Hemiptera" & Fun_group_broad == "sucking.herbivore"]$Species),aursp) #115 species
summary(aur$total_abundance) #max is 806 individuals --> need to see how to merge with other arhtropod species, cannot add as it is
#remove "indet" species
###################################################################################


#NOT DONE##include moth dataset#########################################################
# #remove plots where sensors had problems (column include..laissez.faire. == "no)
lepi<-fread("Exploratories/Data/GRASSLANDS/TEXTfiles/Lepidoptera_synthesis.csv")
length(unique(lepi$Plot)) #2 plots missing
lepino<-fread("Exploratories/Data/GRASSLANDS/TEXTfiles/Lepidoptera_number_of_included_rounds.csv")
plotno<-unique(lepino[include..laissez.faire.=="no"]$PlotID)
lepi<-lepi[!Plot %in% plotno]
rm(lepino,plotno)

length(unique(lepi$Plot))*length(unique(lepi$Species)) #different numbers - zeros are missing - add them
sum(is.na(lepi$value))
lepic<-dcast.data.table(lepi,Species+type+DataID+Year+Group_broad+Group_fine+
                          Trophic_level+Fun_group_broad+Fun_group_fine
                        ~Plot,value.var="value",fill=0)
lepic[1:5,1:15]
lepi<-melt.data.table(lepic,measure.vars=10:ncol(lepic),variable.name="Plot")
length(unique(lepi$Plot))*length(unique(lepi$Species))
sum(is.na(lepi$value))
rm(lepic)

setdiff(names(lepi),names(grl2))
setdiff(names(grl2),names(lepi))
lepi$Dataversion<-NA
lepi<-data.table(BEplotNonZeros(lepi,"Plot","Plot_bexis"))

lepi$DataID<-26026
lepi$Dataversion<-"1.1.2"

intersect(unique(lepi$Species),unique(grl2$Species)) #some species overlap! --> need to see how to merge with other arhtropod species, cannot add as it is
grl2[Species=="Agrotis_exclamationis"]

grl2<-rbindlist(list(grl2,lepi),use.names = T)
rm(lepi); gc()
###################################################################################

#NOT DONE##add a "phylum" column#########################################################
require(taxize)
unique(grl2$Group_fine)
#splist<-unique(grl2[!Group_broad %in% c("AMF","Bacteria.RNA","Protists","SoilFungi")]$Species)
#sphylum<-tax_name(splist, get="phylum", db = 'both')
#write.table(sphylum,"Exploratories/Data/GRASSLANDS/species_phylum.txt",row.names=F)
# sphylum<-fread("Exploratories/Data/GRASSLANDS/species_phylum.txt")
# sphylum<-dcast.data.table(sphylum,query~db)
# tt<-na.omit(sphylum)#check mismatches
# sum(tt$itis!=tt$ncbi)
# tt[itis!=ncbi,]#it's an arthropod-> needs correction
# sphylum[query=="Tabanidae",itis:="Arthropoda"]
# rm(tt)
# sphylum$phylum<-sphylum$itis
# sphylum[is.na(phylum),phylum:=ncbi]
# sphylum$itis<-sphylum$ncbi<-NULL
# write.table(sphylum,"Exploratories/Data/GRASSLANDS/species_phylum.txt",row.names=F)
# add missing species manually and read again the table
###################################################################################
