# The LI-6400 portable photosynthesis system outputs a single text file
# as well as individual .xls files corresponding to every time you've appended data.
# You need to open them in Excel, then save them as .xlsx.
# After that, this script will find them all, extract the data, parse the remarks that are relevant sample ID information
# and put it all into one data frame.

library(tidyverse)
library(readxl)

#folder where the .xlsx files are
data.dir <- "/Users//Documents/relicor"

#a vector of all the paths
(paths <- list.files(data.dir, "*.xlsx") %>% 
    paste0(data.dir, "/", .))

#test one to make sure it works
read_xlsx(paths[1], skip = 7)

#read all the .xlsx files into a list
raw.list <- paths %>% map(read_xlsx, skip = 7)

#function for parsing the data frames created by reading in the .xlsx files  
parse.licor <- function(raw){
  #add column id's to keep track of rows and allow easier joining later
  raw1 <- raw %>% 
    rowid_to_column() 
  
  #this section is for getting sample ID's entered in as remarks.
  #this currently assumes labels like "h.9.a"
  #You'll have to customize based on your system for labeling.
  raw1 %>% 
    filter(Obs == "Remark=") %>% 
    
    #find the Remark lines that have your sample ID info in them. This will need to be customized
    filter(grepl("[[:lower:]]\\.[[:digit:]]\\.[[:lower:]]", .$HHMMSS)) %>% 
    
    #separate out sample ID and then further separate however you like. This will need to be customized
    separate(HHMMSS, into = c("HHMMSS", "sampleID"), sep = " ") %>% 
    separate(sampleID, into = c("plot.id", "plant.id", "leaf"), sep = ".") %>%
    separate(leaf, into = c("leaf", "trash"), sep ="\"") %>% 
    
    #the Remark line ends with a quotation mark and some other stuff you don't need. This gets rid of it
    separate(leaf, into = c("leaf", "trash"), sep ="\"") %>% 
    select(rowid, plot.id, plant.id, leaf) %>% 
    
    #after creating a data frame of just relevant sample ID info and rowid, merge with the original data
    right_join(raw1) %>% 
    
    #then fill-down all relevant sample ID data
    fill(plot.id, leaf, plant.id) %>% 
    
    #then remove unnecessary rows and columns
    filter(Obs != "Remark=" & Obs != "in") %>% 
    select(-rowid)
}

#apply tidying function to all data frames
parsed.data <- lapply(raw.list, parse.licor)