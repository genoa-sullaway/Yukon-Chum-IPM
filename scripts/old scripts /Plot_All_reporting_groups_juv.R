library(readxl)

df <- read_excel("data/BeringSea_Chum_Juv_annual_2003-2023_analysis_msa.xlsx") %>% 
  janitor::row_to_names(row_number = 1) %>%
  rename(reporting_group = `Reporting Group`) %>%
  mutate(Year = as.numeric(Year),
         Mean = as.numeric(Mean),
         SD = as.numeric(SD)) %>%
  dplyr::select(1:4) %>%
  filter(reporting_group %in% c("Coastal Western Alaska","Yukon River Fall Run"))

ggplot(data =df) +
  geom_bar(aes(x=Year, y = Mean, group = reporting_group, 
               fill = reporting_group), stat = "identity", position = "dodge") +
  geom_errorbar(aes(x=Year, ymin = Mean-SD, ymax = Mean+SD,
                    group = reporting_group, 
                    fill = reporting_group)) +
  theme_classic() +
  
