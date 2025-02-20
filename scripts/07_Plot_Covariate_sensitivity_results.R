# Plot sensitivity covariate results 
library(viridis)
library(tidyverse)
library(here)
library(bayestestR)

stage_a_list <- readRDS("output/stan_sensitivity_stage_A_list.RDS")
stage_b_list <- readRDS("output/stan_sensitivity_stage_B_list.RDS")

full_mod <-readRDS("output/stan_fit_DATA.RDS") 

# Sensitiivty with percent difference ============
plot_function<-function(input_cov_list,stage,cov_removed){

if(stage == "a"){
  if(cov_removed == "SST_CDD_NBS"){
theta_df <- as.data.frame(bh_fit, pars = c(
  "theta1[1]","theta1[2]","theta1[3]","theta1[4]",
  "theta2[1]","theta2[2]","theta2[3]","theta2[4]")) %>% 
  dplyr::mutate(draw = 1:nrow(.)) %>%
  gather(1:8, key = "rowname", value = "value") %>% 
  dplyr::mutate(variable = case_when(
                                     rowname=="theta1[1]" ~ "Yukon River Mainstem Discharge",
                                     rowname=="theta1[2]" ~ "Pollock Recruitment", 
                                     rowname=="theta1[3]" ~ "Mean Return Size", 
                                     rowname=="theta1[4]" ~ "Winter Snowpack", 
                                     
                                     rowname=="theta2[1]" ~ "Aleutian Winter Temperature", 
                                     rowname=="theta2[2]" ~ "Chum Salmon Hatchery Release Abundance",
                                     rowname=="theta2[3]" ~ "Pink Salmon Hatchery Release Abundance",
                                     rowname=="theta2[4]" ~ "Fullness Index"), 
               stage = case_when(variable %in% c( "Yukon River Mainstem Discharge",
                                                   "NBS July/August Temperature",
                                                   "Pollock Recruitment",
                                                   "Winter Snowpack", 
                                                   "Mean Return Size") ~ "Juvenile",
                                  variable %in% c("Aleutian Winter Temperature",
                                                  "Chum Salmon Hatchery Release Abundance",
                                                  "Pink Salmon Hatchery Release Abundance",
                                                  "Fullness Index") ~ "Marine")) %>% 
  group_by(stage,variable) %>% 
  dplyr::summarise(mean = median(value),
                   ci_80_low = as.numeric(ci(value, method = "HDI", ci = 0.8)$CI_low),
                   ci_80_high = as.numeric(ci(value, method = "HDI", ci = 0.8)$CI_high),
                   ci_95_low = as.numeric(ci(value, method = "HDI", ci = 0.95)$CI_low),
                   ci_95_high = as.numeric(ci(value, method = "HDI", ci = 0.95)$CI_high))
  }
  if(cov_removed == "yukon_mean_discharge"){
    theta_df <- as.data.frame(bh_fit, pars = c(
      "theta1[1]","theta1[2]","theta1[3]","theta1[4]",
      "theta2[1]","theta2[2]","theta2[3]","theta2[4]")) %>% 
      dplyr::mutate(draw = 1:nrow(.)) %>%
      gather(1:8, key = "rowname", value = "value") %>% 
      dplyr::mutate(variable = case_when(
        rowname=="theta1[1]" ~ "NBS July/August Temperature",
        rowname=="theta1[2]" ~ "Pollock Recruitment", 
        rowname=="theta1[3]" ~ "Mean Return Size",
        rowname=="theta1[4]" ~ "Winter Snowpack",
        
        rowname=="theta2[1]" ~ "Aleutian Winter Temperature", 
        rowname=="theta2[2]" ~ "Chum Salmon Hatchery Release Abundance",
        rowname=="theta2[3]" ~ "Pink Salmon Hatchery Release Abundance",
        rowname=="theta2[4]" ~ "Fullness Index"), 
        stage = case_when(variable %in% c( "Yukon River Mainstem Discharge",
                                           "NBS July/August Temperature",
                                           "Pollock Recruitment",
                                           "Winter Snowpack",
                                           "Mean Return Size") ~ "Juvenile",
                          variable %in% c("Aleutian Winter Temperature",
                                          "Chum Salmon Hatchery Release Abundance",
                                          "Pink Salmon Hatchery Release Abundance",
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
      "theta1[1]","theta1[2]","theta1[3]","theta1[4]",
      "theta2[1]","theta2[2]","theta2[3]","theta2[4]")) %>% 
      dplyr::mutate(draw = 1:nrow(.)) %>%
      gather(1:8, key = "rowname", value = "value") %>% 
      dplyr::mutate(variable = case_when(
        rowname=="theta1[1]" ~ "NBS July/August Temperature",
        rowname=="theta1[2]" ~ "Yukon River Mainstem Discharge",
        rowname=="theta1[3]" ~ "Mean Return Size", 
        rowname=="theta1[4]" ~ "Winter Snowpack", 
        
        rowname=="theta2[1]" ~ "Aleutian Winter Temperature", 
        rowname=="theta2[2]" ~ "Chum Salmon Hatchery Release Abundance",
        rowname=="theta2[3]" ~ "Pink Salmon Hatchery Release Abundance",
        rowname=="theta2[4]" ~ "Fullness Index"), 
        stage = case_when(variable %in% c( "Yukon River Mainstem Discharge",
                                           "NBS July/August Temperature",
                                           "Pollock Recruitment",
                                           "Winter Snowpack", 
                                           "Mean Return Size") ~ "Juvenile",
                          variable %in% c("Aleutian Winter Temperature",
                                          "Chum Salmon Hatchery Release Abundance",
                                          "Pink Salmon Hatchery Release Abundance",
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
      "theta1[1]","theta1[2]","theta1[3]","theta1[4]",
      "theta2[1]","theta2[2]","theta2[3]","theta2[4]")) %>% 
      dplyr::mutate(draw = 1:nrow(.)) %>%
      gather(1:8, key = "rowname", value = "value") %>% 
      dplyr::mutate(variable = case_when(
        rowname=="theta1[1]" ~ "NBS July/August Temperature",
        rowname=="theta1[2]" ~ "Yukon River Mainstem Discharge",
        rowname=="theta1[3]" ~ "Pollock Recruitment",
        rowname=="theta1[4]" ~ "Winter Snowpack",
        
        rowname=="theta2[1]" ~ "Aleutian Winter Temperature", 
        rowname=="theta2[2]" ~ "Chum Salmon Hatchery Release Abundance",
        rowname=="theta2[3]" ~ "Pink Salmon Hatchery Release Abundance",
        rowname=="theta2[4]" ~ "Fullness Index"), 
        stage = case_when(variable %in% c( "Yukon River Mainstem Discharge",
                                           "NBS July/August Temperature",
                                           "Pollock Recruitment",
                                           "Winter Snowpack",
                                           "Mean Return Size") ~ "Juvenile",
                          variable %in% c("Aleutian Winter Temperature",
                                          "Chum Salmon Hatchery Release Abundance",
                                          "Pink Salmon Hatchery Release Abundance",
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
      "theta1[1]","theta1[2]","theta1[3]","theta1[4]",
      "theta2[1]","theta2[2]","theta2[3]","theta2[4]")) %>% 
      dplyr::mutate(draw = 1:nrow(.)) %>%
      gather(1:8, key = "rowname", value = "value") %>% 
      dplyr::mutate(variable = case_when(
        rowname=="theta1[1]" ~ "NBS July/August Temperature",
        rowname=="theta1[2]" ~ "Yukon River Mainstem Discharge",
        rowname=="theta1[3]" ~ "Pollock Recruitment",
        rowname=="theta1[4]" ~ "Mean Return Size", 
        
        rowname=="theta2[1]" ~ "Aleutian Winter Temperature", 
        rowname=="theta2[2]" ~ "Chum Salmon Hatchery Release Abundance",
        rowname=="theta2[3]" ~ "Pink Salmon Hatchery Release Abundance",
        rowname=="theta2[4]" ~ "Fullness Index"), 
        stage = case_when(variable %in% c( "Yukon River Mainstem Discharge",
                                           "NBS July/August Temperature",
                                           "Pollock Recruitment",
                                           "Winter Snowpack",
                                           "Mean Return Size") ~ "Juvenile",
                          variable %in% c("Aleutian Winter Temperature",
                                          "Chum Salmon Hatchery Release Abundance",
                                          "Pink Salmon Hatchery Release Abundance",
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
      "theta1[1]","theta1[2]","theta1[3]","theta1[4]","theta1[5]",
      "theta2[1]","theta2[2]","theta2[3]")) %>% 
      mutate(draw = 1:nrow(.)) %>%
      gather(1:8, key = "rowname", value = "value") %>% 
      dplyr::mutate(variable = case_when(rowname=="theta1[1]" ~ "NBS July/August Temperature",
                                         rowname=="theta1[2]" ~ "Yukon River Mainstem Discharge",
                                         rowname=="theta1[3]" ~ "Pollock Recruitment", 
                                         rowname=="theta1[4]" ~ "Mean Return Size",  
                                         rowname=="theta1[5]" ~ "Winter Snowpack",
                                     
                                         rowname=="theta2[1]" ~ "Chum Salmon Hatchery Release Abundance",
                                         rowname=="theta2[2]" ~ "Pink Salmon Hatchery Release Abundance",
                                         rowname=="theta2[3]" ~ "Fullness Index"),  
                    stage = case_when(variable %in% c( "Yukon River Mainstem Discharge",
                                                       "NBS July/August Temperature",
                                                       "Pollock Recruitment", 
                                                       "Mean Return Size") ~ "Juvenile",
                                      variable %in% c("Aleutian Winter Temperature",
                                                      "Chum Salmon Hatchery Release Abundance",
                                                      "Pink Salmon Hatchery Release Abundance",
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
    "theta1[1]","theta1[2]","theta1[3]","theta1[4]","theta1[5]",
    "theta2[1]","theta2[2]","theta2[3]")) %>% 
    mutate(draw = 1:nrow(.)) %>%
    gather(1:8, key = "rowname", value = "value") %>% 
    dplyr::mutate(variable = case_when(rowname=="theta1[1]" ~ "NBS July/August Temperature",
                                       rowname=="theta1[2]" ~ "Yukon River Mainstem Discharge",
                                       rowname=="theta1[3]" ~ "Pollock Recruitment", 
                                       rowname=="theta1[4]" ~ "Mean Return Size",  
                                       rowname=="theta1[5]" ~ "Winter Snowpack",
                                       
                                       rowname=="theta2[1]" ~ "Aleutian Winter Temperature",
                                       rowname=="theta2[2]" ~ "Pink Salmon Hatchery Release Abundance",
                                       rowname=="theta2[3]" ~ "Fullness Index"),  
                  stage = case_when(variable %in% c( "Yukon River Mainstem Discharge",
                                                     "NBS July/August Temperature",
                                                     "Pollock Recruitment", 
                                                     "Winter Snowpack",
                                                     "Mean Return Size") ~ "Juvenile",
                                    variable %in% c("Aleutian Winter Temperature",
                                                    "Chum Salmon Hatchery Release Abundance",
                                                    "Pink Salmon Hatchery Release Abundance",
                                                    "Fullness Index") ~ "Marine")) %>% 
    group_by(stage,variable) %>% 
    dplyr::summarise(mean = median(value),
                     ci_80_low = as.numeric(ci(value, method = "HDI", ci = 0.8)$CI_low),
                     ci_80_high = as.numeric(ci(value, method = "HDI", ci = 0.8)$CI_high),
                     ci_95_low = as.numeric(ci(value, method = "HDI", ci = 0.95)$CI_low),
                     ci_95_high = as.numeric(ci(value, method = "HDI", ci = 0.95)$CI_high))
}
if(cov_removed == "Pink_hatchery"){
  theta_df <- as.data.frame(bh_fit, pars = c(
    "theta1[1]","theta1[2]","theta1[3]","theta1[4]","theta1[5]",
    "theta2[1]","theta2[2]","theta2[3]")) %>% 
    mutate(draw = 1:nrow(.)) %>%
    gather(1:8, key = "rowname", value = "value") %>% 
    dplyr::mutate(variable = case_when(rowname=="theta1[1]" ~ "NBS July/August Temperature",
                                       rowname=="theta1[2]" ~ "Yukon River Mainstem Discharge",
                                       rowname=="theta1[3]" ~ "Pollock Recruitment", 
                                       rowname=="theta1[4]" ~ "Mean Return Size",  
                                       rowname=="theta1[5]" ~ "Winter Snowpack",
                                       
                                       rowname=="theta2[1]" ~ "Aleutian Winter Temperature",
                                       rowname=="theta2[2]" ~ "Chum Salmon Hatchery Release Abundance",
                                       rowname=="theta2[3]" ~ "Fullness Index"),  
                  stage = case_when(variable %in% c( "Yukon River Mainstem Discharge",
                                                     "NBS July/August Temperature",
                                                     "Pollock Recruitment", 
                                                     "Winter Snowpack",
                                                     "Mean Return Size") ~ "Juvenile",
                                    variable %in% c("Aleutian Winter Temperature",
                                                    "Chum Salmon Hatchery Release Abundance",
                                                    "Pink Salmon Hatchery Release Abundance",
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
    "theta1[1]","theta1[2]","theta1[3]","theta1[4]","theta1[5]",
    "theta2[1]","theta2[2]","theta2[3]")) %>% 
    mutate(draw = 1:nrow(.)) %>%
    gather(1:8, key = "rowname", value = "value") %>% 
    dplyr::mutate(variable = case_when(rowname=="theta1[1]" ~ "NBS July/August Temperature",
                                       rowname=="theta1[2]" ~ "Yukon River Mainstem Discharge",
                                       rowname=="theta1[3]" ~ "Pollock Recruitment", 
                                       rowname=="theta1[4]" ~ "Mean Return Size",  
                                       rowname=="theta1[5]" ~ "Winter Snowpack",
                                       
                                       rowname=="theta2[1]" ~ "Aleutian Winter Temperature",
                                       rowname=="theta2[2]" ~ "Chum Salmon Hatchery Release Abundance",
                                       rowname=="theta2[3]" ~  "Pink Salmon Hatchery Release Abundance"),  
                  stage = case_when(variable %in% c( "Yukon River Mainstem Discharge",
                                                     "NBS July/August Temperature",
                                                     "Pollock Recruitment", 
                                                     "Winter Snowpack",
                                                     "Mean Return Size") ~ "Juvenile",
                                    variable %in% c("Aleutian Winter Temperature",
                                                    "Chum Salmon Hatchery Release Abundance",
                                                    "Pink Salmon Hatchery Release Abundance",
                                                    "Fullness Index") ~ "Marine")) %>% 
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
               "yukon_mean_discharge",
               "pollock_recruit_scale",
               "mean_size",
               "fall_snow_cummulative")

cov_b_list<-  c("SST_CDD_Aleut",
                "Chum_hatchery",
                "Pink_hatchery",
                "full_index")
 
for(i in 1:length(stage_a_list)){
  bh_fit <- stage_a_list[[i]]
  cov_removed <- cov_a_list[[i]]
  plot_sensitivity_a[[i]]<-plot_function(input_cov_list=bh_fit, stage = "a",cov_removed = cov_removed)
}

# plot_sensitivity_a[[1]][[1]]

# ggsave(plot_sensitivity_a,"output/sensitivity_a.png")
# pdf("output/sensitivity_a.pdf")
# plot_sensitivity_a[[1]] 
# plot_sensitivity_a[[2]]
# plot_sensitivity_a[[3]] 
# plot_sensitivity_a[[4]] 
# dev.off()
 
for(i in 1:length(stage_b_list)){
  bh_fit <- stage_b_list[[i]] # pull out specific model output for plot
  cov_removed <- cov_b_list[[i]] # for title, names the covariate that has been removed in that model. 
  plot_sensitivity_b[[i]]<-plot_function(input_cov_list=bh_fit, stage = "b",cov_removed = cov_removed)
}

 

# Calculate percent difference ======
# add that into stage DF as a full model metric
full_mod_df <- as.data.frame(full_mod, pars = c(
    "theta1[1]","theta1[2]","theta1[3]","theta1[4]","theta1[5]",
    "theta2[1]","theta2[2]","theta2[3]","theta2[4]")) %>% 
  dplyr::mutate(draw = 1:nrow(.)) %>%
  gather(1:9, key = "rowname", value = "value") %>% 
  dplyr::mutate(variable = case_when(
    rowname=="theta1[1]" ~ "NBS July/August Temperature",
    rowname=="theta1[2]" ~ "Yukon River Mainstem Discharge",
    rowname=="theta1[3]" ~ "Pollock Recruitment", 
    rowname=="theta1[4]" ~ "Mean Return Size", 
    rowname=="theta1[5]" ~ "Winter Snowpack", 
    
    rowname=="theta2[1]" ~ "Aleutian Winter Temperature", 
    rowname=="theta2[2]" ~ "Chum Salmon Hatchery Release Abundance",
    rowname=="theta2[3]" ~ "Pink Salmon Hatchery Release Abundance",
    rowname=="theta2[4]" ~ "Fullness Index"), 
    stage = case_when(variable %in% c( "Yukon River Mainstem Discharge",
                                       "NBS July/August Temperature",
                                       "Pollock Recruitment",
                                       "Winter Snowpack",
                                       "Mean Return Size") ~ "Juvenile",
                      variable %in% c("Aleutian Winter Temperature",
                                      "Chum Salmon Hatchery Release Abundance",
                                      "Pink Salmon Hatchery Release Abundance",
                                      "Fullness Index") ~ "Marine")) %>% 
  group_by(stage,variable) %>% 
  dplyr::summarise(full_mod_median = median(value), 
                   sd_full = sd(value))
 

stage_a_df <- rbind(plot_sensitivity_a[[1]][[2]],plot_sensitivity_a[[2]][[2]],plot_sensitivity_a[[3]][[2]],plot_sensitivity_a[[4]][[2]],plot_sensitivity_a[[5]][[2]])
stage_b_df <- rbind(plot_sensitivity_b[[1]][[2]],plot_sensitivity_b[[2]][[2]],plot_sensitivity_b[[3]][[2]],plot_sensitivity_b[[4]][[2]])

## DFs with percent and relative diff  =======
joined_stage_a <- left_join(stage_a_df,full_mod_df) %>% 
  dplyr::mutate(abs_diff =(reduced_mod_median-full_mod_median),
                percent_diff =(reduced_mod_median-full_mod_median)/full_mod_median,
                relative_diff = (reduced_mod_median-full_mod_median)/sd_full) %>%
  dplyr::select(stage,variable, covariate_removed,percent_diff,relative_diff,reduced_mod_median, full_mod_median,
                abs_diff) %>%
  dplyr::mutate(switch = case_when(reduced_mod_median < 0 & full_mod_median <0 ~ "N",
                                   reduced_mod_median > 0 & full_mod_median >0 ~ "N",
                                   TRUE ~ "Y"),
                covariate_removed = case_when(covariate_removed== "SST_CDD_NBS" ~ "CDD SST-NBS",
                                              covariate_removed== "yukon_mean_discharge" ~ "Yukon River - Mean Flow",
                                              covariate_removed== "pollock_recruit_scale" ~ "Pollock Recruit. Index",
                                              covariate_removed== "mean_size" ~ "Mean Spawner Size",
                                              covariate_removed== "fall_snow_cummulative" ~ "Winter Snowpack"),
                variable = case_when(variable== "Aleutian Winter Temperature" ~ "CDD SST-NBS",
                                     variable== "Yukon River Mainstem Discharge" ~ "Yukon River - Mean Flow",
                                     variable== "Pollock Recruitment" ~ "Pollock Recruit. Index",
                                     variable== "Mean Return Size" ~ "Mean Spawner Size",
                                     variable== "Winter Snowpack" ~ "Winter Snowpack",
                                     TRUE ~ variable), 
                covariate_removed = factor(covariate_removed, levels = c("Mean Spawner Size","Winter Snowpack",
                                                                            "Yukon River - Mean Flow","CDD SST-NBS",
                                                                            "Pollock Recruit. Index")), 
                variable = factor(variable, levels = c("Mean Spawner Size","Winter Snowpack",
                                                          "Yukon River - Mean Flow","CDD SST-NBS",
                                                          "Pollock Recruit. Index","Chum Salmon Hatchery Release Abundance",
                                                          "Pink Salmon Hatchery Release Abundance","Aleutian Winter Temperature")))  %>%
  filter(!is.na(variable))


joined_stage_b <- left_join(stage_b_df,full_mod_df) %>% 
  dplyr::mutate(abs_diff =(reduced_mod_median-full_mod_median),
                percent_diff =(full_mod_median-reduced_mod_median)/full_mod_median,
                relative_diff = (full_mod_median-reduced_mod_median)/sd_full) %>%
  dplyr::select(stage,variable, covariate_removed,percent_diff,relative_diff,reduced_mod_median, full_mod_median,
                abs_diff) %>%
  dplyr::mutate(switch = case_when(reduced_mod_median < 0 & full_mod_median < 0 ~ "N",
                                   reduced_mod_median > 0 & full_mod_median > 0 ~ "N",
                                   TRUE ~ "Y"),
                covariate_removed = case_when(covariate_removed== "SST_CDD_Aleut" ~ "CDD SST-ALeut",
                                              covariate_removed== "Chum_hatchery" ~ "Chum Hatchery Abund.",
                                              covariate_removed== "Pink_hatchery" ~ "Pink Hatchery Abund.",
                                              covariate_removed== "full_index" ~ "Fullness Index"),
                variable = case_when(variable== "Aleutian Winter Temperature" ~ "CDD SST-ALeut",
                                     variable== "Chum Salmon Hatchery Release Abundance" ~ "Chum Hatchery Abund.",
                                     variable== "Pink Salmon Hatchery Release Abundance" ~ "Pink Hatchery Abund.", 
                                     
                                     variable== "Mean Return Size" ~ "Mean Spawner Size",
                                     variable== "Winter Snowpack" ~ "Winter Snowpack",
                                     TRUE ~ variable),  
                covariate_removed = factor(covariate_removed, levels = c("Fullness Index","CDD SST-ALeut",
                                                                            "Chum Hatchery Abund.","Pink Hatchery Abund.")),
                
                
                variable = factor(variable, levels = c("Fullness Index","CDD SST-ALeut",
                                                        "Chum Hatchery Abund.","Pink Hatchery Abund."))) %>%
  filter(!is.na(variable))
 

## plot Relative Diff ================
plota<- ggplot(data = joined_stage_a  %>% filter(stage == "Juvenile"),
              aes(x=covariate_removed, y = relative_diff, color = variable#, shape = switch
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

plotb<- ggplot(data = joined_stage_b %>% filter(stage == "Marine"), 
               aes(x=covariate_removed, y = percent_diff , color = variable #, shape = switch
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
ggpubr::ggarrange(plota, plotb, labels = c("A.", "B."),ncol = 1)
ggsave("output/Plot_Supplement_Covariate_Sensitivity.png", width = 6, height = 10)
 
# # OLD ================
# # Sensitivity with posterior correlations ========================= 
# ### reduced mod posterior =================================
# reduced_posterior_function<-function(input_cov_list,stage,cov_removed,cov_match){
#   
#   if(stage == "a"){
#     if(cov_removed == "SST_CDD_NBS"){
#       theta_df <- as.data.frame(bh_fit, pars = c(
#         "theta1[1]","theta1[2]","theta1[3]",
#         "theta2[1]","theta2[2]","theta2[3]","theta2[4]")) %>% 
#         dplyr::mutate(draw = 1:nrow(.)) %>%
#         gather(1:7, key = "rowname", value = "value") %>% 
#         dplyr::mutate(variable = case_when(
#           rowname=="theta1[1]" ~ "Yukon River Mainstem Discharge",
#           rowname=="theta1[2]" ~ "Pollock Recruitment", 
#           rowname=="theta1[3]" ~ "Mean Return Size", 
#           
#           rowname=="theta2[1]" ~ "Aleutian Winter Temperature", 
#           rowname=="theta2[2]" ~ "Chum Salmon Hatchery Release Abundance",
#           rowname=="theta2[3]" ~ "Pink Salmon Hatchery Release Abundance",
#           rowname=="theta2[4]" ~ "Fullness Index"), 
#           stage = case_when(variable %in% c( "Yukon River Mainstem Discharge",
#                                              "NBS July/August Temperature",
#                                              "Pollock Recruitment",
#                                              "Mean Return Size") ~ "Juvenile",
#                             variable %in% c("Aleutian Winter Temperature",
#                                             "Chum Salmon Hatchery Release Abundance",
#                                             "Pink Salmon Hatchery Release Abundance",
#                                             "Fullness Index") ~ "Marine")) 
#     }
#     if(cov_removed == "yukon_mean_discharge"){
#       theta_df <- as.data.frame(bh_fit, pars = c(
#         "theta1[1]","theta1[2]","theta1[3]",
#         "theta2[1]","theta2[2]","theta2[3]","theta2[4]")) %>% 
#         dplyr::mutate(draw = 1:nrow(.)) %>%
#         gather(1:7, key = "rowname", value = "value") %>% 
#         dplyr::mutate(variable = case_when(
#           rowname=="theta1[1]" ~ "NBS July/August Temperature",
#           rowname=="theta1[2]" ~ "Pollock Recruitment", 
#           rowname=="theta1[3]" ~ "Mean Return Size", 
#           
#           rowname=="theta2[1]" ~ "Aleutian Winter Temperature", 
#           rowname=="theta2[2]" ~ "Chum Salmon Hatchery Release Abundance",
#           rowname=="theta2[3]" ~ "Pink Salmon Hatchery Release Abundance",
#           rowname=="theta2[4]" ~ "Fullness Index"), 
#           stage = case_when(variable %in% c( "Yukon River Mainstem Discharge",
#                                              "NBS July/August Temperature",
#                                              "Pollock Recruitment",
#                                              "Mean Return Size") ~ "Juvenile",
#                             variable %in% c("Aleutian Winter Temperature",
#                                             "Chum Salmon Hatchery Release Abundance",
#                                             "Pink Salmon Hatchery Release Abundance",
#                                             "Fullness Index") ~ "Marine")) 
#     }
#     if(cov_removed == "pollock_recruit_scale"){
#       theta_df <- as.data.frame(bh_fit, pars = c(
#         "theta1[1]","theta1[2]","theta1[3]",
#         "theta2[1]","theta2[2]","theta2[3]","theta2[4]")) %>% 
#         dplyr::mutate(draw = 1:nrow(.)) %>%
#         gather(1:7, key = "rowname", value = "value") %>% 
#         dplyr::mutate(variable = case_when(
#           rowname=="theta1[1]" ~ "NBS July/August Temperature",
#           rowname=="theta1[2]" ~ "Yukon River Mainstem Discharge",
#           rowname=="theta1[3]" ~ "Mean Return Size", 
#           
#           rowname=="theta2[1]" ~ "Aleutian Winter Temperature", 
#           rowname=="theta2[2]" ~ "Chum Salmon Hatchery Release Abundance",
#           rowname=="theta2[3]" ~ "Pink Salmon Hatchery Release Abundance",
#           rowname=="theta2[4]" ~ "Fullness Index"), 
#           stage = case_when(variable %in% c( "Yukon River Mainstem Discharge",
#                                              "NBS July/August Temperature",
#                                              "Pollock Recruitment",
#                                              "Mean Return Size") ~ "Juvenile",
#                             variable %in% c("Aleutian Winter Temperature",
#                                             "Chum Salmon Hatchery Release Abundance",
#                                             "Pink Salmon Hatchery Release Abundance",
#                                             "Fullness Index") ~ "Marine")) 
#     }
#     if(cov_removed == "mean_size"){
#       theta_df <- as.data.frame(bh_fit, pars = c(
#         "theta1[1]","theta1[2]","theta1[3]",
#         "theta2[1]","theta2[2]","theta2[3]","theta2[4]")) %>% 
#         dplyr::mutate(draw = 1:nrow(.)) %>%
#         gather(1:7, key = "rowname", value = "value") %>% 
#         dplyr::mutate(variable = case_when(
#           rowname=="theta1[1]" ~ "NBS July/August Temperature",
#           rowname=="theta1[2]" ~ "Yukon River Mainstem Discharge",
#           rowname=="theta1[3]" ~ "Pollock Recruitment",
#           
#           rowname=="theta2[1]" ~ "Aleutian Winter Temperature", 
#           rowname=="theta2[2]" ~ "Chum Salmon Hatchery Release Abundance",
#           rowname=="theta2[3]" ~ "Pink Salmon Hatchery Release Abundance",
#           rowname=="theta2[4]" ~ "Fullness Index"), 
#           stage = case_when(variable %in% c( "Yukon River Mainstem Discharge",
#                                              "NBS July/August Temperature",
#                                              "Pollock Recruitment",
#                                              "Mean Return Size") ~ "Juvenile",
#                             variable %in% c("Aleutian Winter Temperature",
#                                             "Chum Salmon Hatchery Release Abundance",
#                                             "Pink Salmon Hatchery Release Abundance",
#                                             "Fullness Index") ~ "Marine")) 
#     }
#   }
#   
#   if(stage == "b"){
#     if(cov_removed == "SST_CDD_Aleut"){
#       theta_df <- as.data.frame(bh_fit, pars = c(
#         "theta1[1]","theta1[2]","theta1[3]","theta1[4]",
#         "theta2[1]","theta2[2]","theta2[3]")) %>% 
#         mutate(draw = 1:nrow(.)) %>%
#         gather(1:7, key = "rowname", value = "value") %>% 
#         dplyr::mutate(variable = case_when(rowname=="theta1[1]" ~ "NBS July/August Temperature",
#                                            rowname=="theta1[2]" ~ "Yukon River Mainstem Discharge",
#                                            rowname=="theta1[3]" ~ "Pollock Recruitment", 
#                                            rowname=="theta1[4]" ~ "Mean Return Size",  
#                                            
#                                            rowname=="theta2[1]" ~ "Chum Salmon Hatchery Release Abundance",
#                                            rowname=="theta2[2]" ~ "Pink Salmon Hatchery Release Abundance",
#                                            rowname=="theta2[3]" ~ "Fullness Index"),  
#                       stage = case_when(variable %in% c( "Yukon River Mainstem Discharge",
#                                                          "NBS July/August Temperature",
#                                                          "Pollock Recruitment", 
#                                                          "Mean Return Size") ~ "Juvenile",
#                                         variable %in% c("Aleutian Winter Temperature",
#                                                         "Chum Salmon Hatchery Release Abundance",
#                                                         "Pink Salmon Hatchery Release Abundance",
#                                                         "Fullness Index") ~ "Marine")) 
#     }
#     if(cov_removed == "Chum_hatchery"){
#       theta_df <- as.data.frame(bh_fit, pars = c(
#         "theta1[1]","theta1[2]","theta1[3]","theta1[4]",
#         "theta2[1]","theta2[2]","theta2[3]")) %>% 
#         mutate(draw = 1:nrow(.)) %>%
#         gather(1:7, key = "rowname", value = "value") %>% 
#         dplyr::mutate(variable = case_when(rowname=="theta1[1]" ~ "NBS July/August Temperature",
#                                            rowname=="theta1[2]" ~ "Yukon River Mainstem Discharge",
#                                            rowname=="theta1[3]" ~ "Pollock Recruitment", 
#                                            rowname=="theta1[4]" ~ "Mean Return Size",  
#                                            
#                                            rowname=="theta2[1]" ~ "Aleutian Winter Temperature",
#                                            rowname=="theta2[2]" ~ "Pink Salmon Hatchery Release Abundance",
#                                            rowname=="theta2[3]" ~ "Fullness Index"),  
#                       stage = case_when(variable %in% c( "Yukon River Mainstem Discharge",
#                                                          "NBS July/August Temperature",
#                                                          "Pollock Recruitment", 
#                                                          "Mean Return Size") ~ "Juvenile",
#                                         variable %in% c("Aleutian Winter Temperature",
#                                                         "Chum Salmon Hatchery Release Abundance",
#                                                         "Pink Salmon Hatchery Release Abundance",
#                                                         "Fullness Index") ~ "Marine")) 
#     }
#     if(cov_removed == "Pink_hatchery"){
#       theta_df <- as.data.frame(bh_fit, pars = c(
#         "theta1[1]","theta1[2]","theta1[3]","theta1[4]",
#         "theta2[1]","theta2[2]","theta2[3]")) %>% 
#         mutate(draw = 1:nrow(.)) %>%
#         gather(1:7, key = "rowname", value = "value") %>% 
#         dplyr::mutate(variable = case_when(rowname=="theta1[1]" ~ "NBS July/August Temperature",
#                                            rowname=="theta1[2]" ~ "Yukon River Mainstem Discharge",
#                                            rowname=="theta1[3]" ~ "Pollock Recruitment", 
#                                            rowname=="theta1[4]" ~ "Mean Return Size",  
#                                            
#                                            rowname=="theta2[1]" ~ "Aleutian Winter Temperature",
#                                            rowname=="theta2[2]" ~ "Chum Salmon Hatchery Release Abundance",
#                                            rowname=="theta2[3]" ~ "Fullness Index"),  
#                       stage = case_when(variable %in% c( "Yukon River Mainstem Discharge",
#                                                          "NBS July/August Temperature",
#                                                          "Pollock Recruitment", 
#                                                          "Mean Return Size") ~ "Juvenile",
#                                         variable %in% c("Aleutian Winter Temperature",
#                                                         "Chum Salmon Hatchery Release Abundance",
#                                                         "Pink Salmon Hatchery Release Abundance",
#                                                         "Fullness Index") ~ "Marine")) 
#     }
#     if(cov_removed == "full_index"){
#       theta_df <- as.data.frame(bh_fit, pars = c(
#         "theta1[1]","theta1[2]","theta1[3]","theta1[4]",
#         "theta2[1]","theta2[2]","theta2[3]")) %>% 
#         mutate(draw = 1:nrow(.)) %>%
#         gather(1:7, key = "rowname", value = "value") %>% 
#         dplyr::mutate(variable = case_when(rowname=="theta1[1]" ~ "NBS July/August Temperature",
#                                            rowname=="theta1[2]" ~ "Yukon River Mainstem Discharge",
#                                            rowname=="theta1[3]" ~ "Pollock Recruitment", 
#                                            rowname=="theta1[4]" ~ "Mean Return Size",  
#                                            
#                                            rowname=="theta2[1]" ~ "Aleutian Winter Temperature",
#                                            rowname=="theta2[2]" ~ "Chum Salmon Hatchery Release Abundance",
#                                            rowname=="theta2[3]" ~  "Pink Salmon Hatchery Release Abundance"),  
#                       stage = case_when(variable %in% c( "Yukon River Mainstem Discharge",
#                                                          "NBS July/August Temperature",
#                                                          "Pollock Recruitment", 
#                                                          "Mean Return Size") ~ "Juvenile",
#                                         variable %in% c("Aleutian Winter Temperature",
#                                                         "Chum Salmon Hatchery Release Abundance",
#                                                         "Pink Salmon Hatchery Release Abundance",
#                                                         "Fullness Index") ~ "Marine")) 
#     }
#   }
#   theta_df_save <- theta_df  %>%
#     dplyr::rename(reduced_mod_estimate = "value") %>%
#     dplyr::mutate(variable_match = paste0(cov_match))
#    
#   return(theta_df_save) # return DF with posteriors 
# }
# 
# ### full mod posterior ====================================
# full_mod_df <- as.data.frame(full_mod, pars = c(
#   "theta1[1]","theta1[2]","theta1[3]","theta1[4]",
#   "theta2[1]","theta2[2]","theta2[3]","theta2[4]")) %>% 
#   dplyr::mutate(draw = 1:nrow(.)) %>%
#   gather(1:8, key = "rowname", value = "value") %>% 
#   dplyr::mutate(variable_match = case_when(
#     rowname=="theta1[1]" ~ "NBS July/August Temperature",
#     rowname=="theta1[2]" ~ "Yukon River Mainstem Discharge",
#     rowname=="theta1[3]" ~ "Pollock Recruitment", 
#     rowname=="theta1[4]" ~ "Mean Return Size", 
#     
#     rowname=="theta2[1]" ~ "Aleutian Winter Temperature", 
#     rowname=="theta2[2]" ~ "Chum Salmon Hatchery Release Abundance",
#     rowname=="theta2[3]" ~ "Pink Salmon Hatchery Release Abundance",
#     rowname=="theta2[4]" ~ "Fullness Index"), 
#     stage = case_when(variable_match %in% c( "Yukon River Mainstem Discharge",
#                                        "NBS July/August Temperature",
#                                        "Pollock Recruitment",
#                                        "Mean Return Size") ~ "Juvenile",
#                       variable_match %in% c("Aleutian Winter Temperature",
#                                       "Chum Salmon Hatchery Release Abundance",
#                                       "Pink Salmon Hatchery Release Abundance",
#                                       "Fullness Index") ~ "Marine"))  %>%
#   dplyr::rename(full_mod_estimate = "value") %>%
#   dplyr::select(draw,variable_match,full_mod_estimate)
# 
# 
# ## call function to bring in reduced summarised mod DFS ========== 
# # create list of plots for each covariate 
# posterior_dfreduced_a <- list()
# posterior_dfreduced_b <- list()
# 
# cov_a_list<- c("SST_CDD_NBS", 
#                "yukon_mean_discharge",
#                "pollock_recruit_scale",
#                "mean_size")
# 
# cov_b_list<-  c("SST_CDD_Aleut",
#                 "Chum_hatchery",
#                 "Pink_hatchery",
#                 "full_index")
# 
# cov_matcha <- c("NBS July/August Temperature",
#                "Yukon River Mainstem Discharge",
#                "Pollock Recruitment", 
#                "Mean Return Size")
# 
# cov_matchb <- c("Aleutian Winter Temperature", 
#                "Chum Salmon Hatchery Release Abundance",
#                "Pink Salmon Hatchery Release Abundance",
#                "Fullness Index")
# 
# for(i in 1:length(stage_a_list)){
#   bh_fit <- stage_a_list[[i]]
#   cov_removed <- cov_a_list[[i]]
#   posterior_dfreduced_a[[i]]<-reduced_posterior_function(input_cov_list=bh_fit, stage = "a",
#                                                          cov_match=cov_matcha[[i]],cov_removed = cov_removed )
# }
#  
# for(i in 1:length(stage_b_list)){
#   bh_fit <- stage_b_list[[i]] # pull out specific model output for plot
#   cov_removed <- cov_b_list[[i]] # for title, names the covariate that has been removed in that model. 
#   posterior_dfreduced_b[[i]]<-reduced_posterior_function(input_cov_list=bh_fit, stage = "b",
#                                                          cov_match=cov_matchb[[i]],cov_removed = cov_removed)
# }
#  
# stage_a_reduced_posterior <- rbind(posterior_dfreduced_a[[1]],
#                                    posterior_dfreduced_a[[2]],
#                                    posterior_dfreduced_a[[3]],
#                                    posterior_dfreduced_a[[4]])
# 
# stage_b_reduced_posterior <- rbind(posterior_dfreduced_b[[1]],
#                                    posterior_dfreduced_b[[2]],
#                                    posterior_dfreduced_b[[3]],
#                                    posterior_dfreduced_b[[4]])
# 
# ### join reduced with full models dfs ====================================
# posterior_a <- left_join(stage_a_reduced_posterior,full_mod_df)
# posterior_b <- left_join(stage_b_reduced_posterior,full_mod_df)
# 
# ### Plot posterior correlations =================
# # plot posterior removed with all the other covariates in the reduced model
# 
# cov_match_a <- c("NBS July/August Temperature",
#                "Yukon River Mainstem Discharge",
#                "Pollock Recruitment", 
#                "Mean Return Size")
# 
# cov_match_b <- c("Aleutian Winter Temperature", 
#                "Chum Salmon Hatchery Release Abundance",
#                "Pink Salmon Hatchery Release Abundance",
#                "Fullness Index")
# 
# posterior_plota <- list()
# posterior_plotb <- list()
# 
# for(i in 1:length(cov_match_a)){
#   
# posterior_plota[[i]]<- ggplot(data =posterior_a %>% 
#                           filter(variable_match == cov_match_a[[i]]),
#                          aes(x= full_mod_estimate, y = reduced_mod_estimate)) +
#                     geom_point() +
#                     geom_smooth(method = "lm") + 
#                     facet_wrap(~variable,scales = "free") +
#                     ggtitle(paste0("Cov removed:",cov_match_a[[i]]," "))
# 
# }
# 
#  
# for(i in 1:length(cov_match_b)){
#   
#   posterior_plotb[[i]]<- ggplot(data =posterior_b %>% 
#                                   filter(variable_match == cov_match_b[[i]]),
#                                 aes(x= full_mod_estimate, y = reduced_mod_estimate)) +
#     geom_point() +
#     geom_smooth(method = "lm") + 
#     facet_wrap(~variable,scales = "free") +
#     ggtitle(paste0("Cov removed:",cov_match_b[[i]]," "))
# }
# 
# pdf("output/posterior_correlation_plots_a.pdf")
# posterior_plota[[1]] 
# posterior_plota[[2]] 
# posterior_plota[[3]] 
# posterior_plota[[4]] 
# dev.off()
# 
# pdf("output/posterior_correlation_plots_b.pdf")
# posterior_plotb[[1]] 
# posterior_plotb[[2]] 
# posterior_plotb[[3]] 
# posterior_plotb[[4]] 
# dev.off()
# 
# 
# 
# 
#  