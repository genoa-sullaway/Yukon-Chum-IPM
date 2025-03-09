# Format hatchery release covariate
# Covariate final form: Asian pink salmon hatchery releases
# source: NPAFC https://www.npafc.org/statistics/ - Data download 1-8-2024
# two hatchery covariates created here 
  # one is pink salmon from the north pacfiic and the second is chum from asian continent and AK
library(tidyverse)
library(here)
library(readxl)

# Asian Pink =====================
# pink_hatchery <- read_excel("data/NPAFC_Hatchery_Stat-1952-2022.xlsx") %>%
#   janitor::row_to_names(row_number = 1) %>%
#   data.frame() %>% 
#   filter(Species == "Pink",
#          Reporting.Area == "Whole country") %>%
#   dplyr::select(c(1:5, 54:76)) %>%
#   gather(c(6:28), key = "Year", value = "Releases") %>%
#   separate(Year, into = c("delete", "Year"), sep = 1) %>%
#   dplyr::select(-delete)
# 
# 
# ggplot(data = pink_hatchery) +
#   geom_point(aes(x=Year, y = Releases, color = Country, group = Country))
# 
# 
# sum_df <- pink_hatchery %>% 
#   filter(Country %in% c("Japan", "Russia")) %>% 
#   group_by(Year) %>%
#   summarise(sum = sum(Releases)) %>%
#   mutate(id = "Japan_Russia")
# 
# ggplot(data = sum_df,
#        aes(x=Year, y = sum, group = id)) +
#   geom_point( ) +
#   geom_line( ) +
#   theme_classic() +
#   theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
#   xlab("Release Year")

# Pink AK and Asia ============= 
pink_hatchery_akNP <- read_excel("data/NPAFC_Hatchery_Stat-1952-2022.xlsx") %>%
  janitor::row_to_names(row_number = 1) %>%
  data.frame() %>% 
  filter(Species == "Pink",
         case_when(Country == "United States" ~ Whole.country.Province.State == "Alaska",
                   !Country == "United States" ~ Whole.country.Province.State == "Whole country",
                   TRUE ~ TRUE),
         Reporting.Area %in% c("Whole country", "Whole state") ) %>%
  dplyr::select(c(1:5, 54:76)) %>%
  gather(c(6:28), key = "Year", value = "Releases") %>%
  separate(Year, into = c("delete", "Year"), sep = 1) %>%
  dplyr::select(-delete)

pink_sum_df <- pink_hatchery_akNP %>% 
  group_by(Year) %>%
  summarise(sum = sum(Releases)) %>%
  mutate(id = "N Pacific") %>%
  mutate(rolling_avg = rollmean(sum, k = 3, align = "right", partial = TRUE))
 
ggplot(data = pink_sum_df,
       aes(x=Year, y = sum, group = id)) +
  geom_point( ) +
  geom_line( ) +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  xlab("Release Year")

# pink_hatchery_ak <- pink_hatchery_akNP %>%
#   filter(Whole.country.Province.State == "Alaska")
# 
# ggplot(data = pink_hatchery_ak,
#        aes(x=Year, y = Releases, group = Species)) +
#   geom_point( ) +
#   geom_line( ) +
#   theme_classic() +
#   theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
#   xlab("Release Year")

# all Chum =====================
chum_hatchery <- read_excel("data/NPAFC_Hatchery_Stat-1952-2022.xlsx") %>%
  janitor::row_to_names(row_number = 1) %>%
  data.frame() %>%
  filter(Species == "Chum",
         Reporting.Area == "Whole country") %>%
  dplyr::select(c(1:5, 54:76)) %>%
  gather(c(6:28), key = "Year", value = "Releases") %>%
  separate(Year, into = c("delete", "Year"), sep = 1) %>%
  dplyr::select(-delete)
# 
# ggplot(data = chum_hatchery) +
#   geom_point(aes(x=Year, y = Releases, color = Country, group = Country))
# 
# 
# chum_sum_df <- chum_hatchery %>% 
#  # filter(Country %in% c("Japan", "Russia")) %>% 
#   group_by(Year) %>%
#   summarise(sum = sum(Releases)) %>%
#   mutate(id = "N Pacific")
# 
# ggplot(data = sum_df,
#        aes(x=Year, y = sum, group = id)) +
#   geom_point( ) +
#   geom_line( ) +
#   theme_classic() +
#   theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
#   xlab("Release Year")
# 
# chum_sum_df <- chum_hatchery %>% 
# #  filter(Country %in% c("United States", "Russia")) %>% 
#   group_by(Year) %>%
#   summarise(sum = sum(Releases)) %>%
#   mutate(id = "N Pacific")
# 
# ggplot(data = chum_sum_df,
#        aes(x=Year, y = sum, group = id)) +
#   geom_point( ) +
#   geom_line( ) +
#   theme_classic() +
#   theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
#   xlab("Release Year")

# Chum in Ak and the asian continent 
# chum_hatchery_akNP <- read_excel("data/NPAFC_Hatchery_Stat-1952-2022.xlsx") %>%
#   janitor::row_to_names(row_number = 1) %>%
#   data.frame() %>% 
#   filter(Species == "Chum",
#          case_when(Country == "United States" ~ Whole.country.Province.State == "Alaska",
#                    !Country == "United States" ~ Whole.country.Province.State == "Whole country",
#                    TRUE ~ TRUE)) %>%
#   dplyr::select(c(1:5, 54:76)) %>%
#   gather(c(6:28), key = "Year", value = "Releases") %>%
#   separate(Year, into = c("delete", "Year"), sep = 1) %>%
#   dplyr::select(-delete)

library(zoo)
chum_sum_df <- chum_hatchery %>% 
  group_by(Year) %>%
  summarise(sum = sum(Releases)) %>%
  dplyr::mutate(id = "N Pacific",
                rolling_avg = zoo::rollmean(sum, 
                                                   k = 3, 
                                                   align = "right", 
                                                   partial = TRUE,
                                                   na.pad = TRUE))

chum_fill <- chum_hatchery %>% 
  group_by(Year) %>%
  summarise(sum = sum(Releases)) %>%
  dplyr::mutate(id = "N Pacific") %>%
  filter(Year %in% c("2000","2001"))

chum_sum_df[1,4] <- chum_fill[1,2]

chum_sum_df[2,4] <- chum_fill %>%
                        dplyr::summarise(sum = mean(sum))

ggplot(data = chum_sum_df,
       aes(x=Year, y = sum, group = id)) +
  geom_point( ) +
  geom_line( ) +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  xlab("Release Year")

write_csv(chum_sum_df, "output/hatchery_Chum_Covariate_AKandAsia.csv")

write_csv(pink_sum_df, "output/hatchery_Pink_Covariate_AKandAsia.csv")

# look at chum from individual countries and see what countries had the dip in hatchery production


# all Chum =====================
chum_hatchery <- read_excel("data/NPAFC_Hatchery_Stat-1952-2022.xlsx") %>%
  janitor::row_to_names(row_number = 1) %>%
  data.frame() %>%
  filter(Species == "Chum",
         Reporting.Area == "Whole country") %>%
  dplyr::select(c(1:5, 54:76)) %>%
  gather(c(6:28), key = "Year", value = "Releases") %>%
  separate(Year, into = c("delete", "Year"), sep = 1) %>%
  dplyr::select(-delete)
 
ggplot(data = chum_hatchery) +
  geom_point(aes(x=Year, y = Releases, color = Country, group = Country))

# 
# chum_sum_df <- chum_hatchery %>% 
#  # filter(Country %in% c("Japan", "Russia")) %>% 
#   group_by(Year) %>%
#   summarise(sum = sum(Releases)) %>%
#   mutate(id = "N Pacific")
# 
# ggplot(data = sum_df,
#        aes(x=Year, y = sum, group = id)) +
#   geom_point( ) +
#   geom_line( ) +
#   theme_classic() +
#   theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
#   xlab("Release Year")
# 
# chum_sum_df <- chum_hatchery %>% 
# #  filter(Country %in% c("United States", "Russia")) %>% 
#   group_by(Year) %>%
#   summarise(sum = sum(Releases)) %>%
#   mutate(id = "N Pacific")
# 
# ggplot(data = chum_sum_df,
#        aes(x=Year, y = sum, group = id)) +
#   geom_point( ) +
#   geom_line( ) +
#   theme_classic() +
#   theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
#   xlab("Release Year")

# Chum in Ak and the asian continent 
# chum_hatchery_akNP <- read_excel("data/NPAFC_Hatchery_Stat-1952-2022.xlsx") %>%
#   janitor::row_to_names(row_number = 1) %>%
#   data.frame() %>% 
#   filter(Species == "Chum",
#          case_when(Country == "United States" ~ Whole.country.Province.State == "Alaska",
#                    !Country == "United States" ~ Whole.country.Province.State == "Whole country",
#                    TRUE ~ TRUE)) %>%
#   dplyr::select(c(1:5, 54:76)) %>%
#   gather(c(6:28), key = "Year", value = "Releases") %>%
#   separate(Year, into = c("delete", "Year"), sep = 1) %>%
#   dplyr::select(-delete)

 
chum_sum_df <- chum_hatchery %>% 
  group_by(Year) %>%
  summarise(sum = sum(Releases)) %>%
  dplyr::mutate(id = "N Pacific",
                rolling_avg = zoo::rollmean(sum, 
                                            k = 3, 
                                            align = "right", 
                                            partial = TRUE,
                                            na.pad = TRUE))

chum_fill <- chum_hatchery %>% 
  group_by(Year) %>%
  summarise(sum = sum(Releases)) %>%
  dplyr::mutate(id = "N Pacific") %>%
  filter(Year %in% c("2000","2001"))

chum_sum_df[1,4] <- chum_fill[1,2]

chum_sum_df[2,4] <- chum_fill %>%
  dplyr::summarise(sum = mean(sum))

ggplot(data = chum_sum_df,
       aes(x=Year, y = sum, group = id)) +
  geom_point( ) +
  geom_line( ) +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  xlab("Release Year")

