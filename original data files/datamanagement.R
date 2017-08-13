library('haven')

setwd("C:/Users/mgiordan/git/Multilevel_II_Rworkshop_ICPSR2017/original data files")

# longitudinal 
chapman <- read_spss("longitudinal_chapman.sav")
saveRDS(chapman, file = "../longitudinal_chapman.rds")
#write_dta()

# binary
thai <- read_spss("binary_thai.sav")
saveRDS(thai, file = "../binary_thai.rds")
#write_dta()

# multiple membership
nurse <- read_dta("mm_nursing.dta")
saveRDS(nurse, "../mm_nursing.rds")
#write_dta()

