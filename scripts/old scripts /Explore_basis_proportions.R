# basis data is an index, a mix of stocks, and i have pulled some GSI data to get average proportions.
# definitely some variation by year for 2003- 2007
library(readxl)
library(tidyverse)
library(here)
library(viridis)

# Source for ADFG full time series, Liz Lee emailed on 4-4-2024
adfg <- read_excel("data/BeringSea_Chum_Juv_annual_2003-2023_analysis_msa.xlsx") %>% 
  janitor::row_to_names(row_number = 1) %>%
  rename(reporting_group = `Reporting Group`) %>%
  mutate(Year = as.numeric(Year),
         Mean = as.numeric(Mean),
         SD = as.numeric(SD),
         reporting_group = case_when(reporting_group == "Yukon River Fall Run"~ "Yukon_Fall",
                                     reporting_group == "Coastal Western Alaska"~ "CWAK",
                                     TRUE ~ reporting_group )) %>%
  dplyr::select(1:4) %>%
  filter(reporting_group %in% c("CWAK","Yukon_Fall"))


# Source for summer yukon specifics: 
  # https://d1wqtxts1xzle7.cloudfront.net/65805085/455.pdf?1614433913=&response-content-disposition=inline%3B+filename%3DApplying_the_Krogh_Principle_to_Find_Sho.pdf&Expires=1703878534&Signature=Q1NPEBlCHSmC~CX99BoVo1Pf0S9CoyJI6X2rKAjFXgct-DSHkNbWWyh~1vmmDfTzcjPsSnCsQiQ05I4mGmZzIfJpkLX8lZtmPyWQYA5knOvlBfW2FBZUuDMKxSsXzJsz4xSRXN5UAHuykwYQMayJhsvDcY87Xgbz8BsAX0gQda751Wa0gkTpTmpMYkyzPInpIWMJlpIowfrZcZV8q6muMlPWLewDCoi9PaEhxzS0wyYggKmk8OY15P-ztuxnG2EZbxRzNwTgMJL0bY0yT7mWjJFIuqrGzE~KH3GWzk-ML~j3uRpm9nKc9Zclky~3-d2NMSa71GOf~0Oov87ALyBBPg__&Key-Pair-Id=APKAJLOHF5GGSLRBV4ZA#page=449
  # page 439
  # table 5

# Plot full adfg timeseries

ggplot(data =df,aes(x=Year, y = Mean, group = reporting_group, 
       fill = reporting_group)) +
  geom_bar(stat = "identity", position = "dodge", color = "black") +
  geom_errorbar(aes(ymin = Mean-SD, ymax = Mean+SD), width = 0.1) +
  theme_classic()  +
  facet_wrap(~reporting_group)+
  scale_fill_viridis_d()+
  theme(legend.position = "top") +
  ylab("Mean Proportion")

ggplot(data =df,aes(x=Year, y = Mean, group = reporting_group, 
                    fill = reporting_group)) +
  geom_bar(stat = "identity", position = "stack") +
  #geom_errorbar(aes(ymin = Mean-SD, ymax = Mean+SD), width = 0.1) +
  theme_classic()  
  
#Kondeleza Yukon ===================
prop_yukon <- read_xlsx("data/BASIS_reporting_groups_comp.xlsx") %>%
  filter(reporting_group %in% c("Yukon_Fall", "Yukon_Summer")) %>%
  dplyr::rename(Year = "year")
 
# 
# proportions <- ggplot(prop_yukon, aes(x=Year,y=percent_mean,fill=reporting_group,group=reporting_group)) + 
#   geom_bar(position="stack", 
#            stat="identity", color = "black")  +
#   xlab("Year") + 
#   ylab("Percent of Total BASIS haul")+
#   theme_classic() + 
#   scale_fill_viridis_d()

proportions <- ggplot(prop_yukon, aes(x=Year,y=percent_mean,fill=reporting_group,group=reporting_group)) + 
  geom_bar(position="dodge", 
           stat="identity", color = "black")  +
  xlab("Year") + 
  ylab("Percent of Total BASIS haul")+
  theme_classic() + 
  scale_fill_viridis_d()

proportions
 
ggsave("output/Plot_BASIS_proportions_2003_2007.jpg")


prop_join <- prop_yukon %>% 
  spread(reporting_group, percent_mean)

# add in data 
juv <- read_csv("data/Juv_Index_CC_aug2023/Index2.csv") %>%
  dplyr::select(Time, Estimate) %>%
  rename(Year = "Time") %>%
  left_join(prop_join) %>% 
  mutate(prop_yukon_fall = Estimate*Yukon_Fall,
         prop_yukon_summer = Estimate*Yukon_Summer,
         summer_fall_ratio = prop_yukon_summer/prop_yukon_fall)  
  
ggplot(juv, aes( x=Year,y=summer_fall_ratio)) + 
  geom_bar(position="stack", stat="identity")  +
  xlab("Year") + 
  ylab("Summer:Fall ratio")+
  theme_classic()

ggsave("output/Plot_genetics_summerfallratio_2003_2007.jpg")

# Compare ADFG FALL WITH Kondeleza FALL ===================
fall_kondeleza <- read_xlsx("data/BASIS_reporting_groups_comp.xlsx") %>%
  filter(reporting_group %in% c("Yukon_Fall", "Yukon_Summer")) %>%
  dplyr::rename(Year = "year",
                kondeleza_mean = "percent_mean") %>%
  rename(reporting_group_actual = "reporting_group") %>%
  mutate(reporting_group=case_when(reporting_group_actual == "Yukon_Summer"~ "CWAK",
                                   TRUE ~ reporting_group_actual))

# compare fall to fall ================
yukon_comp_fall <- adfg %>%
                  dplyr::rename(adfg_mean = "Mean") %>%
  left_join(fall_kondeleza) %>%
  dplyr::select(1:3,6) %>%
  filter(reporting_group == "Yukon_Fall") %>% 
  gather(3:4, key = "key", value = "value") 
 
ggplot(data =yukon_comp_fall, aes(x=Year, y=value, group =key, fill = key)) +
  geom_bar(stat = "identity", position = "dodge", color = "black" ) +
  #facet_wrap(~reporting_group)+
  theme_classic() +
  scale_fill_viridis_d() +
  theme(legend.position = "top") +
  ylab("Mean Proportion") +
  ggtitle("Yukon Fall")

ggsave("output/Plot_compare_adfg_kondeleza_fall.jpg")

# compare summer to CWAK ============ 
yukon_comp_cwak_summer <- adfg %>%
  dplyr::rename(adfg_mean = "Mean") %>%
  left_join(fall_kondeleza) %>%
  dplyr::select(1:3,6) %>%
  filter(!reporting_group == "Yukon_Fall") %>% 
  gather(3:4, key = "key", value = "value") %>%
  mutate(key = case_when(key == "adfg_mean" ~ "CWAK (ADFG)",
                         key == "kondeleza_mean" ~ "Summer Yukon (Kondeleza)",
                         ))

ggplot(data =yukon_comp_cwak_summer, aes(x=Year, y=value, group =key, fill = key)) +
  geom_bar(stat = "identity", position = "dodge", color = "black" ) +
  #facet_wrap(~reporting_group)+
  theme_classic()+
  scale_fill_viridis_d()+
  theme(legend.position = "top") +
  ylab("Mean Proportion") +
  ggtitle("CWAK and Yukon Summer")

ggsave("output/Plot_compare_CWAK_Summer.jpg")

yukon_ratio_cwak_summer <- yukon_comp_cwak_summer%>%
                            spread(key, value) %>%
                            dplyr::mutate(ratio = `Summer Yukon (Kondeleza)`/`CWAK (ADFG)`)

ggplot(data =yukon_ratio_cwak_summer, aes(x=Year, y=ratio)) +
  geom_bar(stat = "identity", color = "black" ) +
  theme_classic()+
  scale_fill_viridis_d()+
  theme(legend.position = "top") +
  ylab("Mean Percent") +
  ggtitle("Summer Percent of CWAK")

ggsave("output/Plot_summer_CWAK_Percent.jpg")

# What does it look like if I carry forward the summer chum percent of CWAK? 
# 62-95%, just going to use 77% here

expand_adfg_summer_fall <- adfg %>%
                            dplyr::select(-SD) %>% 
                            spread(reporting_group, Mean) %>% 
                            dplyr::mutate(Yukon_Smmer= 0.77*CWAK) %>%
  gather(2:4, key = "key", value = "value")


ggplot(data =expand_adfg_summer_fall, aes(x=Year, y=value,
                                          group =key, fill = key)) +
  geom_bar(stat = "identity", position = "dodge", color = "black" ) +
  #facet_wrap(~reporting_group)+
  theme_classic()+
  scale_fill_viridis_d()+
  theme(legend.position = "top") +
  ylab("Mean Proportion") +
  ggtitle("CWAK and Yukon Summer")
ggsave("output/Plot_estimated_CWAK_summer_fall.jpg")

ggplot(data =expand_adfg_summer_fall %>% 
         filter(!key == "CWAK"), aes(x=Year, y=value,
                                          group =key, fill = key)) +
  geom_bar(stat = "identity", position = "dodge", color = "black" ) +
  #facet_wrap(~reporting_group)+
  theme_classic()+
  scale_fill_viridis_d()+
  theme(legend.position = "top") +
  ylab("Mean Proportion") +
  ggtitle("CWAK and Yukon Summer")

ggsave("output/Plot_estimated_summer_fall.jpg")

# mean proportions ==================
# to use for now
mean_prop <- read_xlsx("data/BASIS_reporting_groups_comp.xlsx") %>%
  filter(reporting_group %in% c("Yukon_Fall", "Yukon_Summer")) %>%
  dplyr::rename(Year = "year")
  group_by(reporting_group) %>%
  dplyr::summarise(mean = mean(percent_mean))

write_csv(mean_prop, "data/mean_prop_basis.csv")

 



# prop_cwak <- read_xlsx("data/BASIS_reporting_groups_comp.xlsx") %>%
#   dplyr::rename(Year = "year") %>% 
#   dplyr::mutate(larger_group =case_when(reporting_group %in% c("Yukon_Summer", "Kusko_Bristol") ~ "CWAK",
#                                         TRUE ~ reporting_group)) %>% 
#   filter(larger_group %in% c("CWAK")) %>%
#   group_by(Year) %>%
#   dplyr::mutate(cwak_sum = sum(percent_mean),
#          percent_summer = percent_mean/cwak_sum,
#          percent_cwak_other=1-percent_summer) %>%
#   filter(!reporting_group == "Kusko_Bristol") %>%
#   dplyr::select(Year, percent_summer, percent_cwak_other) %>%
#   gather(2:3, key = "key", value = "prop")
# 
# ggplot(prop_cwak, aes( x=Year,y=prop,fill=key,group=key)) + 
#   geom_bar(position="stack", stat="identity")  
#  



