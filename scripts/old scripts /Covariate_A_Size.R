library(here)
library(tidyverse)

# data load combine =====
## load older data already compiled by oke et al  ==================
asl_dat <-read.csv("data/ASL_summary_byFWSWage.csv") %>%
  filter(Species == "chum",
         !sampleYear < 1980,
         # ASLProjectType %in% c("commercial catch", "escapement"),
         SASAP.Region %in% c("Yukon"), 
         !Fresh.Water.Age == "NA", 
         !is.na(Fresh.Water.Age),
         !is.na(Salt.Water.Age),
         !Salt.Water.Age == "NA",
         Salt.Water.Age %in% c(2,3,4,5,6,7))  %>%
  dplyr::rename(Location = "LocationID",
                Length = "mean") %>%
  dplyr::select(sampleYear,ASLProjectType, Fresh.Water.Age,Salt.Water.Age, Species,Location, Length)

## load new data =========
filenames<- list.files("data/size_covariate/", pattern="*.csv", full.names=TRUE)

size_new_list <- lapply(filenames, read.csv)

size_new_df<-bind_rows(size_new_list, .id = "column_label") %>%
  dplyr::rename(sampleYear = "Sample.Year") %>%
         filter(!Fresh.Water.Age == "NA",
                !is.na(Fresh.Water.Age),
                !is.na(Salt.Water.Age),
                !Salt.Water.Age == "NA",
                Species== "Chum") %>% 
  dplyr::select(sampleYear,ASLProjectType, Fresh.Water.Age,Salt.Water.Age, Species,Location,Length) 
 
## combine =============
size <- rbind(size_new_df,asl_dat) %>%
  # unite("total_age", 3:4,sep = ".") %>% 
  filter(ASLProjectType %in% c("escapement", "Escapement","General Escapement")) %>% 
  filter(!is.na(Length),
         !Salt.Water.Age ==6) # only 1 measurement here   

# look at mean size by age and river system
size_sum <-size %>% 
  group_by(sampleYear,Location,Salt.Water.Age) %>% 
  dplyr::summarise(mean = mean(Length))

ggplot(data = size_sum, 
       aes(x=sampleYear, y=mean, color = Location, group = Location))+
#  geom_path() + 
  geom_point() + 
  facet_wrap(~Salt.Water.Age) +
  theme_classic() +
  theme(legend.position="none") +
  ylab("Length")

# make them go back to brood year?!?!
# look  at trend just mean across locations by age
brood_size <- size %>% 
  dplyr::mutate(brood_year = sampleYear-Salt.Water.Age) %>%
  group_by(brood_year,Salt.Water.Age) %>% 
  dplyr::summarise(mean = mean(Length),
                   sd =sd(Length))

# plot trend among ages 
ggplot(data = brood_size, 
       aes(x=brood_year, y=mean, color = Salt.Water.Age, group = Salt.Water.Age))+
  geom_path() + 
  geom_point() + 
  geom_smooth() +
  facet_wrap(~Salt.Water.Age) +
  theme_classic() +
  theme(legend.position="none") +
  ylab("Length")

# then consider DFA
brood_size_scale <- size %>% 
  dplyr::mutate(brood_year = sampleYear-Salt.Water.Age) %>%
  group_by(brood_year,Salt.Water.Age) %>% 
  dplyr::summarise(mean = mean(Length),
                   sd =sd(Length)) %>%
  group_by(Salt.Water.Age) %>% 
  dplyr::mutate(scale = as.numeric(scale(mean)))

# plot trend among ages 
ggplot(data = brood_size_scale, 
       aes(x=brood_year, y=scale, color = Salt.Water.Age, group = Salt.Water.Age))+
  geom_path() + 
  geom_point() + 
  geom_smooth() +
  facet_wrap(~Salt.Water.Age) +
  theme_classic() +
  theme(legend.position="none") +
  ylab("Length")

# just get mean trend? do i need a DFA? 
# use mean scaled trend for now.... ============
size_scale <- size %>%  
  dplyr::rename(cal_year = "sampleYear") %>% 
  group_by(cal_year,Salt.Water.Age) %>% 
  dplyr::summarise(mean = mean(Length),
                   sd =sd(Length)) %>%
  group_by(Salt.Water.Age) %>% 
  dplyr::mutate(scale = as.numeric(scale(mean))) %>% 
  group_by(cal_year) %>% 
  dplyr::summarise(mean = mean(scale))
 


# plot trend among ages 
ggplot(data = size_scale %>% filter(!cal_year<2000), 
       aes(x=cal_year, y=mean))+
  geom_path() + 
  geom_point() + 
  geom_smooth() +
  theme_classic() +
  theme(legend.position="none") +
  ylab("Length")

write_csv(size_scale,"data/processed_covariates/Stage_A_Size.csv")


