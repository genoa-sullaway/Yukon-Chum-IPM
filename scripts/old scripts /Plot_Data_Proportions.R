# Plot the bethel proportions provided to the proportions in the excel sheet 


#bethel proportions:
prop_bethel <- read_csv("data/Processed_Data/Prop_V2.csv") %>%
  gather(c(2:14), key = "week", value = "value")  %>%
  mutate(id = "prop_bethel")

prop_excel<-read_csv("data/Processed_Data/OLD/OLD_Proportions_run_present_weekly.csv")[2:14] %>%
  mutate(year = 1976:2011) %>%
  gather(c(1:13), key = "week", value = "value")%>%
  mutate(id = "prop_excel")

prop_df<-rbind(prop_bethel, prop_excel)

ggplot(data = prop_df, aes(x=week, y=value, group = id, color = id)) +
  geom_point() +
  geom_line() +
  facet_wrap(~year)


ggplot(data = prop_excel, aes(x=week, y=value), color = "black") +
  geom_point() +  
  geom_line() + 
  facet_wrap(~year)+#,scales = "free")+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))


# Plot bethel catch and effort  ============================================================
catch <- read_csv("data/Processed_Data/OLD/OLD_catch_week.csv")  %>%
  gather(1:13, key = "week", value = "catch")

effort <- read_csv("data/Processed_Data/OLD/OLD_effort.csv")  

bethel_df <- effort %>%
  gather(1:13, key = "week", value = "effort") %>%
  left_join(catch) %>%
  filter(!week %in% c("5/27 - 6/2", "6/3 - 6/9")) %>%
  mutate(cpue = as.numeric(catch)/as.numeric(effort),
         cpue = replace_na(cpue,0)) %>%   
  group_by(year) %>%
  mutate(Pyj = cpue/sum(cpue),
         Pyj = replace_na(Pyj,0))

ggplot(data = bethel_df %>% 
         dplyr::select(year,week,catch,effort) %>% 
         gather(3:4, key ="id", value ="value")) +
  geom_line(aes(x=week, y=value, group =id, color = id)) +  
  facet_wrap(~year,scales = "free")

t <- bethel_df %>% 
  dplyr::select(year,week,cpue)

ggplot(data = t, aes(x=week, y=cpue ), color = "black") +
  geom_point() +  
  geom_line() + 
  facet_wrap(~year,scales = "free")+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))



t <- bethel_df %>% 
  dplyr::select(year,week,Pyj)

ggplot(data = t, aes(x=week, y=Pyj ), color = "black") +
  geom_point() +  
  geom_line() + 
  facet_wrap(~year)+#,scales = "free")+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))





