
library(here)
library(tidyverse)

Euphausiid <- read.csv(here("Literature", "Harvey_Euphausiid.csv"))

Euphausiid$STAGE <- ifelse(Euphausiid$TL_MEAN<=15, "JUVENILE", "ADULT")

Euphausiid_byTAXA_STAGE <- group_by(Euphausiid, Species, STAGE)

Euphausiid_Summary <- summarise(Euphausiid_byTAXA_STAGE, N = n(), DW_MG_mean = mean(DW_MG), DW_MG_SD = sd(DW_MG))

ungroup(Euphausiid)

write.csv(Euphausiid_Summary, here("Literature", "Harvey_Euphausiid_Means.csv"), row.names = FALSE)