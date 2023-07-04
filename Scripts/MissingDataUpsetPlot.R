##This script is used to get patterns of missing data for Figure S1. 
#Jessica Younger 2023-06-30

library(tidyverse)
library(naniar)
#Set the working directory to the folder with data and functions for this project
setwd("~/path/to/working/directory")

#Load the data - output of projectiLEAD_triallevelprocessing.R Script
data=read.csv(file="Data/proj-InitialPapers_asmt-AS_time-1234_type-agg_shape-long_clean-trimRTAccExOutlierRmBRTRegress_prog-R_date-2023-03-07.csv")

#Select just the pid and time point columns to work with
data=data%>%
 select(pid, Time.Point)
#Pivot to wide format to get missingness across timepoint
data=data%>%pivot_wider(id_cols="pid", names_from=Time.Point, values_from=Time.Point )
#Rename the columns
names(data)=c("pid", "Timepoint 1", "Timepoint 2", "Timepoint 3", "Timepoint 4")
#Plot the upset figure
missing=gg_miss_upset(data[2:5], order.by="freq", text.scale=2)

#Save as a file
out_path <- file.path(paste0("Results/", "FigureS1_MissingDataPatterns_UpsetPlot_", Sys.Date(), ".png"))
png(file=out_path, units="in", width=10, height=5, res=300)
missing
dev.off()  
