#SV Scarpino
#Apr 4 2024
#Plotting USDA cattle data
#https://quickstats.nass.usda.gov/?source_desc=CENSUS

###########
#libraries#
###########
library(tidyverse)
library(ggthemes)
library(ggplot2)
library(dplyr)
library(maps)

#########
#Globals#
#########
timestamp <- as.numeric(Sys.time())
do_plot <- FALSE
write_new <- FALSE

###########
#Acc Funcs#
###########

######
#Data#
######
census <- read.csv("../Data/986F2261-E656-368B-ADA0-2122D25ACAA1.csv")
county <- map_data("county")

county_names <- tolower(census$County)
county_names <- gsub(pattern = "[.]", replacement = "", x = county_names)
county_names[which(county_names == "lapaz")] <- "la paz"
county_names[which(county_names == "o brien")] <- "obrien"
county_names[which(county_names == "saint helena")] <- "st helena"

test_county <- which(is.na(match(county_names, county$subregion) == TRUE) & ! census$State %in% c("ALASKA", "HAWAII") & ! census$County.ANSI %in% c(550, 800))

if(length(test_county) != 0){
  stop("County matching failed")
}else{
  census$join <- paste0(tolower(census$State), county_names)
  county$join <- paste0(county$region, county$subregion)
  merged_data <- left_join(x = county, y = census, by = "join")
  
  merged_data$Value <- gsub(pattern = ",", replacement = "", x = merged_data$Value)
  merged_data$Value <- gsub(pattern = "(D)", replacement = NA, x = merged_data$Value)
  merged_data$Value <- as.numeric(merged_data$Value)
}

#############
#Saving Data#
#############
if(write_new == TRUE){
  filename <- paste0("../Data/USDA_dairy_cattle_county_merged_", timestamp, ".csv")
  write.csv(merged_data, file = filename)
}

##########
#Plotting#
##########
if(do_plot == TRUE){
  ggplot(data=merged_data, aes(x=long, y=lat, group=group, fill = Value)) + geom_polygon(color = "#4d4d4d20") + theme(legend.position = "right", legend.key = element_rect(fill = "#f0f0f0"), legend.background = element_rect(fill = "#ffffffaa", colour = "black"), panel.background = element_rect(fill = "white", colour = "black"), axis.text.y = element_text(colour = "black", size = 14), axis.text.x = element_text(colour = "black", size = 14), axis.title = element_text(colour = "black", size = 20), panel.grid.minor = element_line(colour = "#00000050",linetype = 3), panel.grid.major = element_line(colour = "#00000060", linetype = 3)) 
}

