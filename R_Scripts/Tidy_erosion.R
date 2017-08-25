# WES PORTER
# 8/25/2017
# RUSLE2 PROJECT - Tidy_erosion

# Script Summary: Using tidy data principles to tidy AnnAGNPS summary files

library(dplyr)
library(tidyr)
library(stringr)

in_loc <-  file.path("E:","Wes","Work","Rusle2","AnnAGNPS-RUSLE2_Runs","1-simulations",
                     "wEIwPEGwBUFwoSCOUR_FLAGS_AUG25","wEIwPEGwBUFwoSCOUR_FLAGS_AUG25_scn6")

# Scan header lines
h1 <- scan(file.path(in_loc,"AnnAGNPS_TBL_Gaging_Station_Data_Hyd.csv"), nlines = 1, skip = 25, what = character(), sep = ',')

h2 <- scan(file.path(in_loc,"AnnAGNPS_TBL_Gaging_Station_Data_Hyd.csv"), nlines = 1, skip = 26, what = character(), sep = ',')

# Lengths of h1 and h2 are not the same. Pad h1 with blanks.
h1[seq(length(h1) + 1, length(h2))] <- ''

# Fill in h1 blanks with last non-missing value
h1 <- data_frame(header = h1) # have to convert to df to play with fill function

h1[h1 == ""] <-  NA 

h1 <-  fill(h1, header)

# Concatenate column names
hc <- sapply(h1, paste, h2, sep="_")

hc[which(str_sub(hc,-1) == '_')] <- str_sub(hc[which(str_sub(hc,-1) == '_')],1,-2) 
hc[which(str_sub(hc,-1) == '.')] <- str_sub(hc[which(str_sub(hc,-1) == '.')],1,-2) 

hc <-  gsub(" ", "_", hc, fixed = T) %>%  # Replace spaces with '_'
  # With pipe your data are passed as a first argument to the next function, 
  # so if you want to use it somewhere else you need to wrap the next line in {} and use . as a data "marker".
  {gsub("(", "", ., fixed = T) %>% 
  gsub(")", "", ., fixed = T) %>%
  gsub(".", "_", ., fixed = T) %>%
  tolower(.)}

# Read in data ------------------------------------------------------------------
# skipping junk at top and bottom

# Where does data begin and end?
dat_begin <- 29
dat_end <- 15367

data <- read.csv(file.path(in_loc,"AnnAGNPS_TBL_Gaging_Station_Data_Hyd.csv"), skip = (dat_begin - 1), 
                 nrow = length(dat_begin:dat_end), header = F, stringsAsFactors = F)

# Assign column names
names(data) <- hc





