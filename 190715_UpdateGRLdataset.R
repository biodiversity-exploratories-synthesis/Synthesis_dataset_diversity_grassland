###########Update EP grassland datasets version 190715############
#Changes from last version (DataID: 24606 and 21726)
#1.add zeros when species*plot combination is missing for a given year (birds)
#2.change "type" of foliar pathogens to presence-absence
#3.change soil fungi plant pathogens to soilfungi.plant.pathogen (instead of plant.pathogen) 
#4.for all OTU data change "type" from abundance to "OTU_number" (to avoid confusions)
#5.fix species names with multiple underscores or underscores at the end
#6.update protist datasets: removed the one from Hartmut Arndt, added 2 new datasets, changed datasets 18166 18187 18206 18207 18208 18226
#from species to OTU (to allow rarefaction) - species information can be found when downloading the original data
#7.update bacteria datasets: removed all old datasets and added bacteria RNA sequences with illumina ID:24866, 25066
#used only taxonomy from 2014 (more up to date)
#8.update fungi datasets: removed all old datasets and added soil fungi DNA sequences with illumina ID: 24567, 24569, 24571, 24566
#9.add dataset version number
#10.add missing information on trophic levels
#11.add ants dataset (ID:23986)
#12.add snails dataset (ID:23986)
#13.change dataset IDs for birds and bats (old datasets have been archived)
#14.change Trophic_level name for myriapods, from detritivore to myriapod.decomposer


require(data.table)
#require(tidyr)
setwd("N:/")

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
length(unique(allbi$Plot))*length(unique(allbi$Species))*length(unique(allbi$Year)) #final number of rows
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
grl2[DataID==21048 & Trophic_level=="plant.pathogen"]$Trophic_level<-"soilfungi.plant.pathogen"
unique(grl2[DataID==21048]$Trophic_level)
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
prot1<-prot

#reupload the old protists datasets to use OTUs instead of species (for consistency)
all.files<-list.files("N:/Exploratories/Data/GRASSLANDS/TEXTfiles/Protists/",full.names=T,pattern="txt")
mylist<- lapply(all.files, function(i) fread(i))
names(mylist)<-lapply(all.files,function(i) substring(i,53,57))
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


#8###remove old soil fungi and update with illumina dataset (24567 24569 24571 24566)########################
unique(grl2$Group_broad)
grl2<-grl2[!Group_broad %in% c("AMF","SoilFungi")]

f11<-fread("Exploratories/Data/GRASSLANDS/TEXTfiles/190319_Update/24567.txt")
length(unique(f11$Plotid))*length(unique(f11$OTU)) #miss zeros too, but too large to add
f14<-fread("Exploratories/Data/GRASSLANDS/TEXTfiles/190319_Update/24569.txt")
f17<-fread("Exploratories/Data/GRASSLANDS/TEXTfiles/190319_Update/24571.txt")
finfo<-fread("Exploratories/Data/GRASSLANDS/TEXTfiles/190319_Update/24566.txt")

f11$Year<-2011; f11$DataID<-24567
f14$Year<-2014; f14$DataID<-24569
f17$Year<-2017; f17$DataID<-24571

soilf<-rbindlist(list(f11,f14,f17))
rm(f11,f14,f17)

soilf<-data.table(BEplotZeros(soilf,"Plotid",plotnam = "Plot"))
setnames(soilf,c("Plotid","OTU","Abundance"),c("Plot_bexis","Species","value"))
soilf$type<-"OTU_number"

#prepare OTU information data
finfo$RepSeq<-finfo$Kingdom<-NULL
summary(as.factor(finfo$Function))
finfo<-finfo[OTU %in% unique(soilf$Species)] #remove forest species
finfo$Trophic_level<-finfo$Function
finfo[Trophic_level %in% c("AMF","ECM","Lichenized","EricoidM","OrchidM"),Trophic_level:="soilfungi.symbiont"]
finfo[Trophic_level %in% c("Saprotroph"),Trophic_level:="soilfungi.decomposer"]
finfo[Trophic_level %in% c("Pathogen","Parasite","Epiphyte"),Trophic_level:="soilfungi.pathotroph"]
finfo[Trophic_level %in% c("unknown","Endophyte"),Trophic_level:="soilfungi.other"]

finfo$Fun_group_broad<-finfo$Trophic_level
setnames(finfo,"Function","Fun_group_fine")
setnames(finfo,"Phylum","Group_fine")
finfo$Group_broad<-"soilfungi"
finfo$Class<-finfo$Order<-finfo$Family<-finfo$Genus<-finfo$Species<-NULL

#merge
soilf<-merge(soilf,finfo,by.x="Species",by.y="OTU")

#add "soilf_" before otu number (to avoid confusion with bacteria or protists)
set(soilf,j="Species",value=paste("soilf_",soilf$Species,sep=""))


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
grl2[DataID==24567,Dataversion:="1.2.4"]; grl2[DataID==24569,Dataversion:="1.2.3"]
grl2[DataID==24571,Dataversion:="1.2.2"]; grl2[DataID==24566,Dataversion:="1.2.4"]
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
ant$Trophic_level<-ant$Fun_group_broad<-ant$Fun_group_fine<-"ant.omnivore"
ant$Group_broad<-"Formicidae"
ant[, c("Group_fine", "sp") := tstrsplit(Species, "_", fixed=TRUE)]
ant$sp<-NULL
length(unique(ant$Species))*length(unique(ant$Plot))

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


######################Lasts checks before saving table
#remove potential forest plots
grl2<-grl2[!grepl("W", grl2$Plot)]
length(unique(grl2$Plot))

#check if there are zeros missing
for (i in unique(grl2$DataID)){
  tt<-grl2[DataID==i]
  print(paste(i,":"))
  print(length(unique(tt$Plot))*length(unique(tt$Species)))
  print(nrow(tt))
}

#check datasets 19686; 16908; 24866; 25066; 24567; 24569; 24571;
tt<-grl2[DataID==19686] #plant dataset, missing information are NAs -> ok
tt<-grl2[DataID==16908] #pollinator dataset, missing information are NAs -> ok
tt<-grl2[DataID==24866] #bacteria dataset, cannot add zeros (dataset would be too large) + two plots missing AEG33 AEG34
setdiff(unique(grl2$Plot),unique(tt$Plot))
tt<-grl2[DataID==25066] #bacteria dataset, cannot add zeros (dataset would be too large) + two plots missing AEG33 AEG34
tt<-grl2[DataID==24567] #soilfungi dataset, cannot add zeros (dataset would be too large)
tt<-grl2[DataID==24569] #soilfungi dataset, cannot add zeros (dataset would be too large)
tt<-grl2[DataID==24571] #soilfungi dataset, cannot add zeros (dataset would be too large)
length(unique(tt$Plot))
length(unique(tt$Species))
summary(tt$value)
rm(tt,i)

#check if columns contain NAs
apply(grl2,2,function(x)sum(is.na(x))) #ok, only in functional group columns and version (for birds and bats)

#check if there are species with only zeros (that were maybe only in forests)
grl2[,temp:=sum(value),by=Species]
unique(grl2[temp==0]$Species) #3 plants, one insect and several protists -> after checks, safe to remove them
grl2<-grl2[!temp==0]
grl2$temp<-NULL

#general checks
unique(grl2$Group_broad)
unique(grl2$Group_fine)
unique(grl2$Trophic_level)
unique(grl2$Fun_group_broad)
unique(grl2$Fun_group_fine)

###########Save the diversity and characteristics tables separately ###########
grl<-grl2[,.(Plot_bexis,Plot,Species,value,type,DataID,Year,Dataversion)]
tr<-grl2[,.(Species,Group_broad,Group_fine,Trophic_level,Fun_group_broad,Fun_group_fine)]
length(unique(tr$Species))
dim(unique(tr)) #some species are duplicated
tr<-unique(tr)
toRemove<-tr[duplicated(tr$Species)]
tr[Species=="Formica_cunicularia"] #11 species recorded in pollinator and ant datasets -> remove them from ant dataset

grl2<-grl2[!(Species %in% toRemove$Species & Trophic_level=="ant.omnivore")]
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
tr[is.na(Fun_group_broad)] #two myriapods
rm(toRemove)


summary(factor(tr$Trophic_level))
#change one species which got the wrong trophic level and dataset ID
tr[Trophic_level=="soilfungi.plant.pathogen"]
tr[Species=="Urocystis_agropyri",Trophic_level:="plant.pathogen"]
grl[Species=="Urocystis_agropyri",DataID:="18548"]
grl[Species=="Urocystis_agropyri",Dataversion:="1.1.0"]

#reorder column names in grl
setcolorder(grl,c("Plot_bexis","Plot","Species","value","type","Year","DataID","Dataversion"))

#all good: save
fwrite(grl,"N:/Exploratories/Data/GRASSLANDS/190715_EP_species_diversity_GRL.txt",row.names=F,quote=F,sep=";",na=NA)
fwrite(tr,"N:/Exploratories/Data/GRASSLANDS/190715_EP_species_info_GRL.txt",row.names=F,quote=F,sep=";",na=NA)



###########Add in metadata########
#ants, use only P/A but add note to users that they can have abundance
#in metdadata give a warning, when merging trophic levels, check if they have the same nb of plots and comparable methods!
#add ref in metadata on snail traits
#warn about the  fact that bacteria and soil fungi miss zeros! (and bacteria also miss plots AEG33 AEG34)



############NOT DONE, HAS TO BE DISCUSSED WITH ARTHROPODS CORE PROJECT###################

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
