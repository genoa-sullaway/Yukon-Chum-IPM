# Explore zoop data for zoop in chum diet that was prominent in Murphy et al 2016: Distribution, Diet, and Bycatch of Chum Salmon in the Eastern Bering Sea
# chrome-extension://efaidnbmnnnibpcajpcglclefindmkaj/https://meetings.npfmc.org/CommentReview/DownloadFile?p=fd891fa4-b73b-4cd6-922c-199e1d558a60.pdf&fileName=2.%20Distribution%2C%20Diet%20and%20Bycatch%20of%20chum%20salmon%20EBS%20Murphy%20et%20al%2C%202016.pdf

zoop <- read_csv(here("data", "Processed_Data", "NBS_Zoop_Process_Final.csv"))


themisto<- zoop %>% filter(stringr::str_detect(TAXA_COARSE, 'Themisto') )
