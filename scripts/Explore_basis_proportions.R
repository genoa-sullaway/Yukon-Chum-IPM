# basis data is an index, a mix of stocks, and i have pulled some GSI data to get average proportions.
# definitely some variation by year for 2003- 2007

# Source: 
  # https://d1wqtxts1xzle7.cloudfront.net/65805085/455.pdf?1614433913=&response-content-disposition=inline%3B+filename%3DApplying_the_Krogh_Principle_to_Find_Sho.pdf&Expires=1703878534&Signature=Q1NPEBlCHSmC~CX99BoVo1Pf0S9CoyJI6X2rKAjFXgct-DSHkNbWWyh~1vmmDfTzcjPsSnCsQiQ05I4mGmZzIfJpkLX8lZtmPyWQYA5knOvlBfW2FBZUuDMKxSsXzJsz4xSRXN5UAHuykwYQMayJhsvDcY87Xgbz8BsAX0gQda751Wa0gkTpTmpMYkyzPInpIWMJlpIowfrZcZV8q6muMlPWLewDCoi9PaEhxzS0wyYggKmk8OY15P-ztuxnG2EZbxRzNwTgMJL0bY0yT7mWjJFIuqrGzE~KH3GWzk-ML~j3uRpm9nKc9Zclky~3-d2NMSa71GOf~0Oov87ALyBBPg__&Key-Pair-Id=APKAJLOHF5GGSLRBV4ZA#page=449
  # page 439
  # table 5

# load summary table: 
prop <- read_xlsx("data/basis_chum_age_comp_lowe_kondeleza_table5.xlsx")
 
ggplot(prop, aes( x=reporting_group,y=percent_mean,fill=year,group=year)) + 
  geom_bar(position="dodge", stat="identity")  

mean_prop <- prop %>%
  group_by(reporting_group) %>%
  dplyr::summarise(mean = mean(percent_mean))

write_csv(mean_prop, "data/mean_prop_basis.csv")




 



