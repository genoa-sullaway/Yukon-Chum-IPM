# Plot sensitivity covariate results 
library(viridis)
library(tidyverse)
library(here)
library(bayestestR)

stage_a_list <- readRDS("output/stan_sensitivity_stage_A_list.RDS")
stage_b_list <- readRDS("output/stan_sensitivity_stage_B_list.RDS")

full_mod <-readRDS("output/stan_fit_DATA.RDS") 
 
# Sensitivity with percent difference ============
plot_function<-function(input_cov_list,stage,cov_removed,n_col){

if(stage == "a"){
  if(cov_removed == "SST_CDD_NBS"){
theta_df <- as.data.frame(bh_fit, pars = c(
  "theta1[1]","theta1[2]","theta1[3]",
  "theta2[1]","theta2[2]","theta2[3]" )) %>% 
  dplyr::mutate(draw = 1:nrow(.)) %>%
  gather(1:n_col, key = "rowname", value = "value") %>% 
  dplyr::mutate(variable = case_when(rowname=="theta1[1]" ~ "Mean Return Size",   
                                     rowname=="theta1[2]" ~ "Winter Snowpack",
                                     rowname=="theta1[3]" ~ "Pollock Recruitment",  
                                     
                                     rowname=="theta2[1]" ~ "Fullness Index",
                                     rowname=="theta2[2]" ~ "Aleutian Winter Temperature", 
                                     rowname=="theta2[3]" ~  "Chum Salmon Hatchery Release Abundance"),  
                stage = case_when(variable %in% c( "NBS July/August Temperature",
                                                   "Pollock Recruitment", 
                                                   "Winter Snowpack",
                                                   "Mean Return Size") ~ "Juvenile",
                                  variable %in% c("Aleutian Winter Temperature",
                                                  "Chum Salmon Hatchery Release Abundance",
                                                  "Fullness Index") ~ "Marine")) %>% 
  group_by(stage,variable) %>% 
  dplyr::summarise(mean = median(value),
                   ci_80_low = as.numeric(ci(value, method = "HDI", ci = 0.8)$CI_low),
                   ci_80_high = as.numeric(ci(value, method = "HDI", ci = 0.8)$CI_high),
                   ci_95_low = as.numeric(ci(value, method = "HDI", ci = 0.95)$CI_low),
                   ci_95_high = as.numeric(ci(value, method = "HDI", ci = 0.95)$CI_high))
  }
  
  if(cov_removed == "pollock_recruit_scale"){
    theta_df <- as.data.frame(bh_fit, pars = c(
      "theta1[1]","theta1[2]","theta1[3]", 
      "theta2[1]","theta2[2]","theta2[3]" )) %>% 
      dplyr::mutate(draw = 1:nrow(.)) %>%
      gather(1:n_col, key = "rowname", value = "value") %>% 
      dplyr::mutate(variable = case_when(rowname=="theta1[1]" ~ "Mean Return Size",   
                                         rowname=="theta1[2]" ~ "Winter Snowpack",
                                         rowname=="theta1[3]" ~ "NBS July/August Temperature",
                                         
                                         rowname=="theta2[1]" ~ "Fullness Index",
                                         rowname=="theta2[2]" ~ "Aleutian Winter Temperature", 
                                         rowname=="theta2[3]" ~  "Chum Salmon Hatchery Release Abundance"),  
                  
        stage = case_when(variable %in% c( "NBS July/August Temperature",
                                           "Pollock Recruitment",
                                           "Winter Snowpack", 
                                           "Mean Return Size") ~ "Juvenile",
                          variable %in% c("Aleutian Winter Temperature",
                                          "Chum Salmon Hatchery Release Abundance",
                                          "Fullness Index") ~ "Marine")) %>% 
      group_by(stage,variable) %>% 
      dplyr::summarise(mean = median(value),
                       ci_80_low = as.numeric(ci(value, method = "HDI", ci = 0.8)$CI_low),
                       ci_80_high = as.numeric(ci(value, method = "HDI", ci = 0.8)$CI_high),
                       ci_95_low = as.numeric(ci(value, method = "HDI", ci = 0.95)$CI_low),
                       ci_95_high = as.numeric(ci(value, method = "HDI", ci = 0.95)$CI_high))
  }
  if(cov_removed == "mean_size"){
    theta_df <- as.data.frame(bh_fit, pars = c(
      "theta1[1]","theta1[2]","theta1[3]", 
      "theta2[1]","theta2[2]","theta2[3]" )) %>% 
      dplyr::mutate(draw = 1:nrow(.)) %>%
      gather(1:n_col, key = "rowname", value = "value") %>% 
      dplyr::mutate(variable = case_when(rowname=="theta1[1]" ~ "Winter Snowpack",
                                         rowname=="theta1[2]" ~ "NBS July/August Temperature",
                                         rowname=="theta1[3]" ~ "Pollock Recruitment",
                                         
                                         rowname=="theta2[1]" ~ "Fullness Index",
                                         rowname=="theta2[2]" ~ "Aleutian Winter Temperature", 
                                         rowname=="theta2[3]" ~ "Chum Salmon Hatchery Release Abundance"),  
                    
        stage = case_when(variable %in% c( "NBS July/August Temperature",
                                           "Pollock Recruitment",
                                           "Winter Snowpack",
                                           "Mean Return Size") ~ "Juvenile",
                          variable %in% c("Aleutian Winter Temperature",
                                          "Chum Salmon Hatchery Release Abundance",
                                          "Fullness Index") ~ "Marine")) %>% 
      group_by(stage,variable) %>% 
      dplyr::summarise(mean = median(value),
                       ci_80_low = as.numeric(ci(value, method = "HDI", ci = 0.8)$CI_low),
                       ci_80_high = as.numeric(ci(value, method = "HDI", ci = 0.8)$CI_high),
                       ci_95_low = as.numeric(ci(value, method = "HDI", ci = 0.95)$CI_low),
                       ci_95_high = as.numeric(ci(value, method = "HDI", ci = 0.95)$CI_high))
  }
  if(cov_removed == "fall_snow_cummulative"){
    theta_df <- as.data.frame(bh_fit, pars = c(
      "theta1[1]","theta1[2]","theta1[3]", 
      "theta2[1]","theta2[2]","theta2[3]" )) %>% 
      dplyr::mutate(draw = 1:nrow(.)) %>%
      gather(1:n_col, key = "rowname", value = "value") %>% 
      dplyr::mutate(variable = case_when(rowname=="theta1[1]" ~ "Mean Return Size",
                                         rowname=="theta1[2]" ~ "NBS July/August Temperature",
                                         rowname=="theta1[3]" ~ "Pollock Recruitment",
                                         
                                         rowname=="theta2[1]" ~ "Fullness Index",
                                         rowname=="theta2[2]" ~ "Aleutian Winter Temperature", 
                                         rowname=="theta2[3]" ~  "Chum Salmon Hatchery Release Abundance"),  
                    
        stage = case_when(variable %in% c( "NBS July/August Temperature",
                                           "Pollock Recruitment",
                                           "Winter Snowpack",
                                           "Mean Return Size") ~ "Juvenile",
                          variable %in% c("Aleutian Winter Temperature",
                                          "Chum Salmon Hatchery Release Abundance",
                                          "Fullness Index") ~ "Marine")) %>% 
      group_by(stage,variable) %>% 
      dplyr::summarise(mean = median(value),
                       ci_80_low = as.numeric(ci(value, method = "HDI", ci = 0.8)$CI_low),
                       ci_80_high = as.numeric(ci(value, method = "HDI", ci = 0.8)$CI_high),
                       ci_95_low = as.numeric(ci(value, method = "HDI", ci = 0.95)$CI_low),
                       ci_95_high = as.numeric(ci(value, method = "HDI", ci = 0.95)$CI_high))
  }
}
  
  if(stage == "b"){
if(cov_removed == "SST_CDD_Aleut"){
    theta_df <- as.data.frame(bh_fit, pars = c(
      "theta1[1]","theta1[2]","theta1[3]","theta1[4]", 
      "theta2[1]","theta2[2]" )) %>% 
      dplyr::mutate(draw = 1:nrow(.)) %>%
      gather(1:n_col, key = "rowname", value = "value") %>% 
      dplyr::mutate(variable = case_when(rowname=="theta1[1]" ~ "Mean Return Size",
                                         rowname=="theta1[2]" ~ "Winter Snowpack", 
                                         rowname=="theta1[3]" ~ "NBS July/August Temperature",
                                         rowname=="theta1[4]" ~ "Pollock Recruitment",
                                         
                                         rowname=="theta2[1]" ~ "Fullness Index",
                                         rowname=="theta2[2]" ~ "Chum Salmon Hatchery Release Abundance" ),  
                    stage = case_when(variable %in% c( "Winter Snowpack",
                                                       "NBS July/August Temperature",
                                                       "Pollock Recruitment", 
                                                       "Mean Return Size") ~ "Juvenile",
                                      variable %in% c("Aleutian Winter Temperature",
                                                      "Chum Salmon Hatchery Release Abundance",
                                                      "Fullness Index") ~ "Marine")) %>% 
      group_by(stage,variable) %>% 
      dplyr::summarise(mean = median(value),
                       ci_80_low = as.numeric(ci(value, method = "HDI", ci = 0.8)$CI_low),
                       ci_80_high = as.numeric(ci(value, method = "HDI", ci = 0.8)$CI_high),
                       ci_95_low = as.numeric(ci(value, method = "HDI", ci = 0.95)$CI_low),
                       ci_95_high = as.numeric(ci(value, method = "HDI", ci = 0.95)$CI_high))
}
if(cov_removed == "Chum_hatchery"){
  theta_df <- as.data.frame(bh_fit, pars = c(
    "theta1[1]","theta1[2]","theta1[3]","theta1[4]", 
    "theta2[1]","theta2[2]" )) %>% 
    mutate(draw = 1:nrow(.)) %>%
    gather(1:n_col, key = "rowname", value = "value") %>% 
    dplyr::mutate(variable = case_when(rowname=="theta1[1]" ~ "Mean Return Size",
                                       rowname=="theta1[2]" ~ "Winter Snowpack", 
                                       rowname=="theta1[3]" ~ "NBS July/August Temperature",
                                       rowname=="theta1[4]" ~ "Pollock Recruitment",
                                       
                                       rowname=="theta2[1]" ~ "Fullness Index",
                                       rowname=="theta2[2]" ~ "Aleutian Winter Temperature"),  
                  
                  stage = case_when(variable %in% c( "NBS July/August Temperature",
                                                     "Pollock Recruitment", 
                                                     "Winter Snowpack",
                                                     "Mean Return Size") ~ "Juvenile",
                                    variable %in% c("Aleutian Winter Temperature",
                                                    "Chum Salmon Hatchery Release Abundance",
                                                    "Fullness Index") ~ "Marine")) %>% 
    group_by(stage,variable) %>% 
    dplyr::summarise(mean = median(value),
                     ci_80_low = as.numeric(ci(value, method = "HDI", ci = 0.8)$CI_low),
                     ci_80_high = as.numeric(ci(value, method = "HDI", ci = 0.8)$CI_high),
                     ci_95_low = as.numeric(ci(value, method = "HDI", ci = 0.95)$CI_low),
                     ci_95_high = as.numeric(ci(value, method = "HDI", ci = 0.95)$CI_high))
}
 
if(cov_removed == "full_index"){
  theta_df <- as.data.frame(bh_fit, pars = c(
    "theta1[1]","theta1[2]","theta1[3]","theta1[4]", 
    "theta2[1]","theta2[2]" )) %>% 
    mutate(draw = 1:nrow(.)) %>%
    gather(1:n_col, key = "rowname", value = "value") %>% 
    dplyr::mutate(variable = case_when(rowname=="theta1[1]" ~ "Mean Return Size",
                                       rowname=="theta1[2]" ~ "Winter Snowpack", 
                                       rowname=="theta1[3]" ~ "NBS July/August Temperature",
                                       rowname=="theta1[4]" ~ "Pollock Recruitment",
                                       
                                       rowname=="theta2[1]" ~ "Aleutian Winter Temperature",
                                       rowname=="theta2[2]" ~  "Chum Salmon Hatchery Release Abundance"),  
                  stage = case_when(variable %in% c( "NBS July/August Temperature",
                                                     "Pollock Recruitment", 
                                                     "Winter Snowpack",
                                                     "Mean Return Size") ~ "Juvenile",
                                    variable %in% c("Aleutian Winter Temperature",
                                                    "Chum Salmon Hatchery Release Abundance"
                                                   ) ~ "Marine")) %>% 
    group_by(stage,variable) %>% 
    dplyr::summarise(mean = median(value),
                     ci_80_low = as.numeric(ci(value, method = "HDI", ci = 0.8)$CI_low),
                     ci_80_high = as.numeric(ci(value, method = "HDI", ci = 0.8)$CI_high),
                     ci_95_low = as.numeric(ci(value, method = "HDI", ci = 0.95)$CI_low),
                     ci_95_high = as.numeric(ci(value, method = "HDI", ci = 0.95)$CI_high))
}
}
  theta_df_save <- theta_df %>% 
    dplyr::mutate(covariate_removed = cov_removed) %>% 
    dplyr::rename(reduced_mod_median=mean)
  
  theta_plot <- ggplot(data = theta_df,
                         aes(x= mean, y = variable, 
                             group = variable, color = stage)) +
      geom_errorbar(aes(xmin =ci_95_low, xmax = ci_95_high),
                    width = 0, linewidth = 0.5 ) + 
      geom_point(size = 2) + 
      geom_errorbar(aes(xmin =ci_80_low, xmax = ci_80_high), linewidth = 1.5, width = 0) + 
      theme_classic() +
      scale_color_manual(values =c("#EAAA00", "#2d9d92")) +
      theme(panel.background = element_blank(), #element_rect(fill = "black", colour = NA),
            plot.background = element_blank(), #element_rect(fill = "black", colour = NA),
            legend.background = element_blank(),
            # legend.text = element_text(color = "white"),
            legend.title = element_blank(),#"none",
            strip.text = element_blank( ), 
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            strip.background = element_blank(),
            panel.border = element_rect(colour = "white", fill = NA), 
            strip.text.x = element_blank(), 
            axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
            panel.spacing.y=unit(0, "lines")) + 
      geom_vline(xintercept=0)+
      ylab("") +
      xlab("Median Covariate Coefficient Value") +
      ggtitle(paste0("Covariate Removed:",cov_removed," ")) + 
      facet_wrap(~stage, scales = "free_y", ncol = 1)  
    
    return(list(theta_plot,theta_df_save))

}

# ggsave("output/theta_plot.png", width = 7, height = 4, bg = "transparent")

## call function to bring in reduced summarised mod DFS ========== 
# create list of plots for each covariate 
plot_sensitivity_a <- list()
plot_sensitivity_b <- list()

cov_a_list<- c("SST_CDD_NBS", 
               "pollock_recruit_scale",
               "mean_size",
               "fall_snow_cummulative")

cov_b_list<-  c("SST_CDD_Aleut",
                "Chum_hatchery",
                "full_index")

for(i in 1:length(stage_a_list)){
  bh_fit <- stage_a_list[[i]]
  cov_removed <- cov_a_list[[i]]
  plot_sensitivity_a[[i]]<-plot_function(input_cov_list=bh_fit, 
                                         stage = "a",cov_removed = cov_removed, n_col = 6)
}
 
for(i in 1:length(stage_b_list)){
  bh_fit <- stage_b_list[[i]] # pull out specific model output for plot
  cov_removed <- cov_b_list[[i]] # for title, names the covariate that has been removed in that model. 
  plot_sensitivity_b[[i]]<-plot_function(input_cov_list=bh_fit, 
                                         stage = "b",cov_removed = cov_removed, n_col = 6)
}

# Calculate percent difference ======
# add that into stage DF as a full model metric
full_mod_df <- as.data.frame(full_mod, pars = c(
    "theta1[1]","theta1[2]","theta1[3]","theta1[4]", 
    "theta2[1]","theta2[2]","theta2[3]" )) %>% 
  dplyr::mutate(draw = 1:nrow(.)) %>%
  gather(1:7, key = "rowname", value = "value") %>% 
  dplyr::mutate(variable = case_when(
    rowname=="theta1[1]" ~ "Mean Return Size",
    rowname=="theta1[2]" ~ "Winter Snowpack", 
    rowname=="theta1[3]" ~ "NBS July/August Temperature",  
    rowname=="theta1[4]" ~  "Pollock Recruitment",  
    
    rowname=="theta2[1]" ~ "Fullness Index",  
    rowname=="theta2[2]" ~ "Aleutian Winter Temperature",  
    rowname=="theta2[3]" ~ "Chum Salmon Hatchery Release Abundance"), 
    stage = case_when(variable %in% c( "Yukon River Mainstem Discharge",
                                       "NBS July/August Temperature",
                                       "Pollock Recruitment",
                                       "Winter Snowpack",
                                       "Mean Return Size") ~ "Juvenile",
                      variable %in% c("Aleutian Winter Temperature",
                                      "Chum Salmon Hatchery Release Abundance", 
                                      "Fullness Index") ~ "Marine")) %>% 
  group_by(stage,variable) %>% 
  dplyr::summarise(full_mod_median = median(value), 
                   sd_full = sd(value))
 

stage_a_df <- rbind(plot_sensitivity_a[[1]][[2]],plot_sensitivity_a[[2]][[2]],plot_sensitivity_a[[3]][[2]],plot_sensitivity_a[[4]][[2]])
stage_b_df <- rbind(plot_sensitivity_b[[1]][[2]],plot_sensitivity_b[[2]][[2]],plot_sensitivity_b[[3]][[2]])

## DFs with percent and relative diff  =======
joined_stage_a <- left_join(stage_a_df,full_mod_df) %>% 
  filter(stage == "Juvenile") %>%
  dplyr::mutate(abs_diff =(reduced_mod_median-full_mod_median),
                percent_diff =(reduced_mod_median-full_mod_median)/full_mod_median,
                relative_diff = (reduced_mod_median-full_mod_median)/sd_full) %>%
  dplyr::select(stage,variable, covariate_removed,percent_diff,relative_diff,reduced_mod_median, full_mod_median,
                abs_diff) %>% 
  dplyr::mutate( 
                covariate_removed = case_when(covariate_removed== "SST_CDD_NBS" ~ "CDD SST-NBS", 
                                              covariate_removed== "pollock_recruit_scale" ~ "Pollock Recruitment",
                                              covariate_removed== "mean_size" ~ "Mean Return Size",
                                              covariate_removed== "fall_snow_cummulative" ~ "Winter Snowpack"),
                variable = case_when(variable == "NBS July/August Temperature" ~ "CDD SST-NBS",
                                     TRUE ~ variable), 
                covariate_removed = factor(covariate_removed, levels = c("Mean Return Size","Winter Snowpack",
                                                                          "CDD SST-NBS","Pollock Recruitment")), 
                variable = factor(variable, levels = c("Mean Return Size","Winter Snowpack",
                                                       "CDD SST-NBS","Pollock Recruitment")))  

joined_stage_b <- left_join(stage_b_df,full_mod_df) %>% 
  filter(stage == "Marine") %>%
  dplyr::mutate(abs_diff =(reduced_mod_median-full_mod_median),
                percent_diff =(full_mod_median-reduced_mod_median)/full_mod_median,
                relative_diff = (full_mod_median-reduced_mod_median)/sd_full) %>%
  dplyr::select(stage,variable, covariate_removed,percent_diff,relative_diff,reduced_mod_median, full_mod_median,
                abs_diff) %>%
 
  dplyr::mutate( 
                covariate_removed = case_when(covariate_removed== "SST_CDD_Aleut" ~ "CDD SST-Aleut",
                                              covariate_removed== "Chum_hatchery" ~ "Chum Hatchery Abund.",
                                              covariate_removed== "full_index" ~ "Fullness Index"),
                variable = case_when(variable == "Chum Salmon Hatchery Release Abundance" ~"Chum Hatchery Abund.",
                                     variable == "Aleutian Winter Temperature" ~ "CDD SST-Aleut",
                                      TRUE ~ variable),
                covariate_removed = factor(covariate_removed, 
                                           levels = c("Fullness Index","CDD SST-Aleut",
                                                       "Chum Hatchery Abund.")), 
                variable = factor(variable, levels = c("Fullness Index","CDD SST-Aleut",
                                                       "Chum Hatchery Abund."))) %>%
  filter(!is.na(variable))
  
## plot Relative Diff ================
plota<- ggplot(data = joined_stage_a ,
              aes(x=covariate_removed, y = relative_diff/100, color = variable#, shape = switch
                  )) +
  geom_point()+
  geom_hline(yintercept =0, linetype =2) +
  theme_classic()+
  scale_color_viridis(discrete = TRUE, name = "Covariate Sensitivity Response") +
  ylab("Relative Difference") +
  xlab("Covariate Removed") +
  ggtitle("Stage A")+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

plota
ggsave("output/Plot_StageA_percentDiff.png", width = 6, height = 3)

plotb<- ggplot(data = joined_stage_b , 
               aes(x=covariate_removed, y = percent_diff/100 , color = variable #, shape = switch
                   )) +
  geom_point()+
  geom_hline(yintercept =0, linetype =2) +
  theme_classic()+
  scale_color_viridis(discrete = TRUE,name = "Covariate Sensitivity Response") +
  ylab("Relative Difference") +
  xlab("Covariate Removed") +
  ggtitle("Stage B") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

plotb
ggsave("output/Plot_StageB_percentDiff.png", width = 8, height = 5)

# Look here for saved manuscript plot =================
ggpubr::ggarrange(plota, plotb, labels = c("A.", "B."),ncol = 2)
ggsave("output/Plot_Supplement_Covariate_Sensitivity.png", width = 10, height = 4)
  