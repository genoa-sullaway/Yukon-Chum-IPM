

library(here)
library(tidyverse)

#Read in Russ Hopcroft data

biomass_RH <-
  list.files(here("Biomass","Seward-Line-Zoop"), 
             pattern = "csv$",
             full.names = TRUE) %>% 
  map_df(~read_csv(., col_types = cols(.default = "c")))

#Do some tidying

#Rename somecolumns

biomass_RH <- rename(biomass_RH, CRUISE = Cruise, GEAR_NAME = Gear_Type, MESH = `Mesh_Size_[um]`, TAXON_NAME = Organism_Identification, STAGE_NAME = Life_Stage, EST_NUM_PERM3 = `Abundance_[#/m3]`, SIZE_UM = `Mean_size_[um]`, WWT_MG_MEAN = `Mean_WetWeight_[mg]`, BIOMASS_WWT_MG_M3 = `Biomass_[mg_WetWeight_m-3]`, IND_COUNT = Individual_Count)

#Keep only these columns

biomass_RH_recode <- biomass_RH[, c("CRUISE", "GEAR_NAME", "MESH", "TAXON_NAME", "STAGE_NAME", "EST_NUM_PERM3", "SIZE_UM", "WWT_MG_MEAN", "BIOMASS_WWT_MG_M3", "IND_COUNT")]

#Remove excess rows with no data

biomass_RH_recode <- biomass_RH_recode[rowSums(is.na(biomass_RH_recode)) != ncol(biomass_RH_recode), ]

#COnvert to numeric values

biomass_RH_recode$MESH <- as.integer(biomass_RH_recode$MESH)
biomass_RH_recode$EST_NUM_PERM3 <- as.numeric(biomass_RH_recode$EST_NUM_PERM3)
biomass_RH_recode$WWT_MG_MEAN <- as.numeric(biomass_RH_recode$WWT_MG_MEAN)
biomass_RH_recode$SIZE_UM <- as.numeric(biomass_RH_recode$SIZE_UM)
biomass_RH_recode$BIOMASS_WWT_MG_M3 <- as.numeric(biomass_RH_recode$BIOMASS_WWT_MG_M3)
biomass_RH_recode$IND_COUNT <- as.integer(biomass_RH_recode$IND_COUNT)


#Clean up some issues with columns per Genoa's code

#Remove negative values

biomass_RH_recode <- mutate(biomass_RH_recode, WWT_MG_MEAN = case_when(WWT_MG_MEAN < 0 ~ WWT_MG_MEAN * -1, TRUE ~ WWT_MG_MEAN))

biomass_RH_recode <- mutate(biomass_RH_recode, BIOMASS_WWT_MG_M3 = case_when(BIOMASS_WWT_MG_M3 < 0 ~ BIOMASS_WWT_MG_M3 * -1, TRUE ~ BIOMASS_WWT_MG_M3))

#Recode an infinite value in WWT_MG_MEAN

biomass_RH_recode$infinite <- is.infinite(biomass_RH_recode$WWT_MG_MEAN)

biomass_RH_recode <- filter(biomass_RH_recode, infinite == FALSE)

biomass_RH_recode$infinite <- NULL


#Now build a column that is indivdual weight

biomass_RH_recode <- mutate(biomass_RH_recode, WWT_MG = BIOMASS_WWT_MG_M3/EST_NUM_PERM3)

#Check that it is a linear function of combined weights

plot(biomass_RH_recode$WWT_MG, biomass_RH_recode$WWT_MG_MEAN)

#Rename WWT_MG_MEAN column since it is really ug

biomass_RH_recode <- rename(biomass_RH_recode, WWT_UG = WWT_MG_MEAN)

#Estimate individual weight for each taxa

biomass_RH_recode <- mutate(biomass_RH_recode, IND_WT_MG = WWT_MG/IND_COUNT)


###############################################################################

#Bring in the annotated file

RH_taxa_list_annotated <- read.csv(here("Taxa-Lists", "RH_Taxa_List_Annotated.csv"))


#Add column for merging

biomass_RH_recode$ROW <- 1:26299

#Now merge with the original biomass file

biomass_RH_recode <- left_join(biomass_RH_recode, RH_taxa_list_annotated, by = "ROW")

#Eliminate duplicate column and rename

biomass_RH_recode$TAXON_NAME.x <- NULL

biomass_RH_recode <- rename(biomass_RH_recode, TAXON_NAME = TAXON_NAME.y)


#Now do some filtering

#First remove taxa that are not of interest

biomass_RH_recode <- filter(biomass_RH_recode, NOTE != "Remove")

#Drop note column

biomass_RH_recode$NOTE <- NULL

#Remove rows that have individual weight = 0 or NA

biomass_RH_recode$MISSING_IND_WT <- complete.cases(biomass_RH_recode$IND_WT_MG)

biomass_RH_recode <- filter(biomass_RH_recode, MISSING_IND_WT==TRUE)

biomass_RH_recode$MISSING_IND_WT <- NULL

#Now remove individual weights that are 0

biomass_RH_recode <- filter(biomass_RH_recode, IND_WT_MG>0)

#Now recode the stages

#Read in the annotated file

RH_stage_list_annotated <- read.csv(here("Taxa-Lists", "RH_Stage_List_Annotated.csv"))

biomass_RH_recode$ROW <- 1:17874

biomass_RH_recode <- left_join(biomass_RH_recode, RH_stage_list_annotated, by = "ROW")

#Drop some columsna and rename some columns

biomass_RH_recode$STAGE_NAME.x <- NULL
biomass_RH_recode$STAGE_NAME.y <- NULL
biomass_RH_recode$ROW <- NULL

biomass_RH_recode <- rename(biomass_RH_recode, STAGE_NAME = STAGE_NAME_EcoDAAT)


##############################################################################

#First split data sets by gear

biomass_RH_recode_150 <- filter(biomass_RH_recode, MESH==150)

biomass_RH_recode_505 <- filter(biomass_RH_recode, MESH==500|MESH==505)


#Now do some recoding of particular coarse taxa

biomass_RH_recode_150$TAXA_COARSE[biomass_RH_recode_150$TAXA_COARSE=="Appendicularia"] <- "Appendicularia_small"

biomass_RH_recode_150$TAXA_COARSE[biomass_RH_recode_150$TAXA_COARSE=="Cirripedia"] <- "Cirripedia_small"

biomass_RH_recode_150$TAXA_COARSE[biomass_RH_recode_150$TAXA_COARSE=="Cnidaria"] <- "Cnidaria_small"

biomass_RH_recode_150$TAXA_COARSE[biomass_RH_recode_150$TAXA_COARSE=="Cirripedia"] <- "Cirripedia_small"

biomass_RH_recode_150$TAXA_COARSE[biomass_RH_recode_150$TAXA_COARSE=="Limacina helicina"] <- "Limacina_small"

biomass_RH_recode_150$TAXA_COARSE[biomass_RH_recode_150$TAXA_COARSE=="Polychaeta"] <- "Polychaeta_small"

#Now repeat with large net

biomass_RH_recode_505$TAXA_COARSE[biomass_RH_recode_505$TAXA_COARSE=="Appendicularia"] <- "Appendicularia_large"

biomass_RH_recode_505$TAXA_COARSE[biomass_RH_recode_505$TAXA_COARSE=="Cirripedia"] <- "Cirripedia_large"

biomass_RH_recode_505$TAXA_COARSE[biomass_RH_recode_505$TAXA_COARSE=="Cnidaria"] <- "Cnidaria_large"

biomass_RH_recode_505$TAXA_COARSE[biomass_RH_recode_505$TAXA_COARSE=="Cirripedia"] <- "Cirripedia_large"

biomass_RH_recode_505$TAXA_COARSE[biomass_RH_recode_505$TAXA_COARSE=="Limacina helicina"] <- "Limacina_large"

biomass_RH_recode_505$TAXA_COARSE[biomass_RH_recode_505$TAXA_COARSE=="Polychaeta"] <- "Polychaeta_large"

#Now recombined the datasets

biomass_RH_recode <- rbind(biomass_RH_recode_150, biomass_RH_recode_505)

rm(biomass_RH_recode_150, biomass_RH_recode_505)


#############################################################################

biomass_RH_recode_TAXA_STAGE_SEX <- group_by(biomass_RH_recode, TAXA_COARSE, STAGE_NAME, SEX_NAME)

biomass_RH_recode_TAXA_COARSE_mean <- summarise(biomass_RH_recode_TAXA_STAGE_SEX, N = n(), IND_WT_MG_mean = mean(IND_WT_MG), IND_WT_MG_sd = sd(IND_WT_MG))

ungroup(biomass_RH_recode)

#Tidy this data set


biomass_RH_recode_TAXA_COARSE_mean <- filter(biomass_RH_recode_TAXA_COARSE_mean,!is.na(STAGE_NAME))

biomass_RH_recode_TAXA_COARSE_mean <- biomass_RH_recode_TAXA_COARSE_mean[!(biomass_RH_recode_TAXA_COARSE_mean$TAXA_COARSE=="Anomura"&biomass_RH_recode_TAXA_COARSE_mean$STAGE_NAME=="JUVENILE"),]

biomass_RH_recode_TAXA_COARSE_mean <- biomass_RH_recode_TAXA_COARSE_mean[!(biomass_RH_recode_TAXA_COARSE_mean$TAXA_COARSE=="Brachyura"&biomass_RH_recode_TAXA_COARSE_mean$STAGE_NAME=="MEGALOPAE"),]

biomass_RH_recode_TAXA_COARSE_mean <- biomass_RH_recode_TAXA_COARSE_mean[!(biomass_RH_recode_TAXA_COARSE_mean$TAXA_COARSE=="Copepod_large"&biomass_RH_recode_TAXA_COARSE_mean$STAGE_NAME==""),]

biomass_RH_recode_TAXA_COARSE_mean <- biomass_RH_recode_TAXA_COARSE_mean[!(biomass_RH_recode_TAXA_COARSE_mean$TAXA_COARSE=="Copepod_small"&biomass_RH_recode_TAXA_COARSE_mean$STAGE_NAME==""),]


biomass_RH_recode_TAXA_COARSE_mean <- biomass_RH_recode_TAXA_COARSE_mean[!(biomass_RH_recode_TAXA_COARSE_mean$TAXA_COARSE=="Copepod_small"&biomass_RH_recode_TAXA_COARSE_mean$STAGE_NAME=="NAUPLIUS"),]

biomass_RH_recode_TAXA_COARSE_mean <- biomass_RH_recode_TAXA_COARSE_mean[!(biomass_RH_recode_TAXA_COARSE_mean$TAXA_COARSE=="Euphausia pacifica"&biomass_RH_recode_TAXA_COARSE_mean$STAGE_NAME=="FURCILIA"),]

biomass_RH_recode_TAXA_COARSE_mean <- biomass_RH_recode_TAXA_COARSE_mean[!(biomass_RH_recode_TAXA_COARSE_mean$TAXA_COARSE=="Euphausiacea"&biomass_RH_recode_TAXA_COARSE_mean$STAGE_NAME=="ADULT"),]

biomass_RH_recode_TAXA_COARSE_mean <- biomass_RH_recode_TAXA_COARSE_mean[!(biomass_RH_recode_TAXA_COARSE_mean$TAXA_COARSE=="Thaliacea"&biomass_RH_recode_TAXA_COARSE_mean$STAGE_NAME=="LARVA"),]

biomass_RH_recode_TAXA_COARSE_mean <- filter( biomass_RH_recode_TAXA_COARSE_mean, TAXA_COARSE!="Tortanus discaudatus")

ungroup(biomass_RH_recode_TAXA_COARSE_mean)

#Now write the summary dataset

write.csv(biomass_RH_recode_TAXA_COARSE_mean, here("Biomass", "RH_biomass.csv"), row.names = FALSE)

