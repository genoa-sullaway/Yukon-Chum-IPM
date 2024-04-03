# basis data is an index, a mix of stocks, and i have pulled some GSI data to get average proportions.
# definitely some variation by year for 2003- 2007

# Source: 
  # https://d1wqtxts1xzle7.cloudfront.net/65805085/455.pdf?1614433913=&response-content-disposition=inline%3B+filename%3DApplying_the_Krogh_Principle_to_Find_Sho.pdf&Expires=1703878534&Signature=Q1NPEBlCHSmC~CX99BoVo1Pf0S9CoyJI6X2rKAjFXgct-DSHkNbWWyh~1vmmDfTzcjPsSnCsQiQ05I4mGmZzIfJpkLX8lZtmPyWQYA5knOvlBfW2FBZUuDMKxSsXzJsz4xSRXN5UAHuykwYQMayJhsvDcY87Xgbz8BsAX0gQda751Wa0gkTpTmpMYkyzPInpIWMJlpIowfrZcZV8q6muMlPWLewDCoi9PaEhxzS0wyYggKmk8OY15P-ztuxnG2EZbxRzNwTgMJL0bY0yT7mWjJFIuqrGzE~KH3GWzk-ML~j3uRpm9nKc9Zclky~3-d2NMSa71GOf~0Oov87ALyBBPg__&Key-Pair-Id=APKAJLOHF5GGSLRBV4ZA#page=449
  # page 439
  # table 5

#Just Yukon ===================
prop_yukon <- read_xlsx("data/BASIS_reporting_groups_comp.xlsx") %>%
  filter(reporting_group %in% c("Yukon_Fall", "Yukon_Summer")) %>%
  dplyr::rename(Year = "year")
 
ggplot(prop_yukon, aes( x=reporting_group,y=percent_mean,fill=Year,group=Year)) + 
  geom_bar(position="dodge", stat="identity")  
 

proportions <- ggplot(prop_yukon, aes(x=Year,y=percent_mean,fill=reporting_group,group=reporting_group)) + 
  geom_bar(position="stack", 
           stat="identity", color = "black")  +
  xlab("Year") + 
  ylab("Percent of Total BASIS haul")+
  theme_classic() + 
  viridis::scale_fill_viridis_d()

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


# mean proportions ==================
# to use for now
mean_prop <- read_xlsx("data/BASIS_reporting_groups_comp.xlsx") %>%
  filter(reporting_group %in% c("Yukon_Fall", "Yukon_Summer")) %>%
  dplyr::rename(Year = "year")
  group_by(reporting_group) %>%
  dplyr::summarise(mean = mean(percent_mean))

write_csv(mean_prop, "data/mean_prop_basis.csv")



# CWAK and Yukon comparison ==========
# this doesn't exactly match the data of what is used - probably bc of the norton sound grouping into kotzebue... 
# what proportion of CWAK is yukon summer?? 
prop_cwak <- read_xlsx("data/BASIS_reporting_groups_comp.xlsx") %>%
  dplyr::rename(Year = "year") %>% 
  dplyr::mutate(larger_group =case_when(reporting_group %in% c("Yukon_Summer", "Kusko_Bristol") ~ "CWAK",
                                        TRUE ~ reporting_group)) %>% 
  filter(larger_group %in% c("CWAK")) %>%
  group_by(Year) %>%
  mutate(cwak_sum = sum(percent_mean),
         percent_summer = percent_mean/cwak_sum,
         percent_cwak_other=1-percent_summer
         ) %>%
  filter(!reporting_group == "Kusko_Bristol") %>%
  select(Year, percent_summer, percent_cwak_other) %>%
  gather(2:3, key = "key", value = "prop")

ggplot(prop_cwak, aes( x=Year,y=prop,fill=key,group=key)) + 
  geom_bar(position="stack", stat="identity")  
 



