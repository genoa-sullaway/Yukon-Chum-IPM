library(tidyverse)
library(here)
library(readxl)

# total run =======================
fall_recruit <- read_csv("data/processed_data/yukon_fall_recruits.csv") 

yukon_summer_df<-read_excel("data/Yukon_Escapement_ADFG/S Chum RR 2023.xlsx",sheet = 2)  

yukon_summer_recruit <-  yukon_summer_df %>%
  dplyr::select(1:4) %>%
  janitor::row_to_names(row_number = 1) %>%
  dplyr::rename(cal_year = "Year",
                total_run =`Total Run` )  %>%
  dplyr::mutate(total_run = as.numeric(total_run)) %>%
  dplyr::select(cal_year, total_run) %>%
  dplyr::mutate(cal_year = as.numeric(cal_year),
         total_run = as.numeric(total_run)) %>% 
  data.frame()

sum_recruit<- yukon_summer_recruit %>% 
  rbind(fall_recruit) %>% 
  group_by(cal_year) %>% 
  dplyr::summarise(total_run = sum(total_run))  %>%
  filter(!cal_year<1980)

total_recruit <- ggplot(data = sum_recruit, aes(x=cal_year, y = total_run/1000000 )) +
  geom_line(color = "lightblue") +
  geom_area(fill = "lightblue") +
  theme_classic() +
  ylab("Abundance (Millions)") +
  xlab("Calendar Year") +
  theme( panel.background = element_blank(), #element_rect(fill = "black", colour = NA),
         plot.background = element_blank(), #element_rect(fill = "black", colour = NA),
         legend.background = element_blank(),
         legend.position = "none",
         
         axis.text.x=element_text(colour="white"), 
         axis.title.x = element_text(colour="white"), 
         axis.ticks.x = element_line(colour="white"), 
         axis.text.y=element_text(colour="white"), 
         axis.title.y = element_text(colour="white"), 
         axis.ticks.y = element_line(colour="white"), 
         axis.line.x.bottom=element_line(color="white"),
         axis.line.y =element_line(color="white"),
         
         panel.border = element_blank(),
         panel.grid.minor = element_blank(),
         panel.grid.major = element_blank()) + 
         scale_x_continuous(expand = c(0, 0))
#  geom_hline(yintercept = mean(sum_recruit$total_run)/1000000, linetype = 2, color = "red")

total_recruit

ggsave( "output/afs_talk_recruit_totalchum.png",  bg = "transparent",
        width = 9, height = 4)


# juveniles ================
# just fall juv =========
fall_juv <- read_csv("data/processed_data/tidy_juv_fall_yukon.csv") %>%
  rename(cal_year = "Year") %>%
  data.frame()

fall_juv<- ggplot(data = fall_juv, aes(x=cal_year, y = fall_abundance/1000000 )) +
  geom_line() +
  geom_area() +
  theme_classic() +
  ylab("Abundance (Millions)") +
  xlab("Calendar Year")# +

fall_juv
ggsave( "output/afs_talk_fall_juv_abundance.png", width = 9, height = 4)


# spawners =========================
spawners <-read_csv("data/processed_data/yukon_fall_spawners.csv") %>%
  filter(!cal_year <1980)

ggplot(data = spawners, aes(x=cal_year, y = Spawners/1000000)) +
  geom_line() +
  geom_area() +
  theme_classic() +
  ylab("Abundance (Millions)") +
  xlab("Return Year")

#summer spawners ======
yukon_summer_df<-read_excel("data/Yukon_Escapement_ADFG/S Chum RR 2023.xlsx",sheet = 2)  

# spawner harvest recruit Abundances ========
# Summer ========
yukon_summer <-  yukon_summer_df %>%
  select(1:4) %>%
  janitor::row_to_names(row_number = 1) %>%
  dplyr::rename(cal_year = "Year",
                total_run =`Total Run`,
                Spawners="Escapement" )  %>%
  dplyr::mutate(Spawners = as.numeric(Spawners)) %>%
  dplyr::select(cal_year, Spawners) %>%
  mutate(cal_year = as.numeric(cal_year),
         Spawners = as.numeric(Spawners)) %>% 
  data.frame()

ggplot(data = yukon_summer, aes(x=cal_year, y = Spawners/1000000)) +
  geom_line() +
  geom_area() +
  theme_classic() +
  ylab("Abundance (Millions)") +
  xlab("Return Year")

## combo fall and summer  ===== 

sum_spawner<- yukon_summer %>% 
    rbind(spawners) %>% 
  group_by(cal_year) %>% 
  summarise(Spawners = sum(Spawners))  

total_spawners <- ggplot(data = sum_spawner, aes(x=cal_year, y = Spawners/1000000 )) +
  geom_line() +
  geom_area() +
  theme_classic() +
  ylab("Abundance (Millions)") +
  xlab("Return Year") 

total_spawners
ggsave( "output/afs_talk_spawner_totalchum.png", width = 9, height = 4)

# horizontal line 
ggplot(data = sum_spawner, aes(x=cal_year, y = Spawners/1000000 )) +
  geom_line() +
  geom_area() +
  theme_classic() +
  ylab("Abundance (Millions)") +
  xlab("Return Year") +
  geom_hline(yintercept = mean(sum_spawner$Spawners)/1000000, linetype = 2, color = "red")


## stacked fall and summer  ===== 
combo_spawner<- yukon_summer %>% 
  mutate(id = "summer") %>% 
  rbind(spawners %>% mutate(id = "fall"))

ggplot(data = combo_spawner, aes(x=cal_year, y = Spawners/1000000, fill = id)) +
  geom_area(alpha = 0.5) +
  theme_classic() +
  ylab("Abundance (Millions)") +
  xlab("Return Year")


 