#Script to get processed data ready for analysis in Mplus for Confirmatory Factor Analysis
#Outputs data in format for analysis with Mplus as well as a table of the column names
#Jessica Younger 06-30-2023


#Library packages used in this script
library(tidyverse)
#Set the working directory to the folder with data and functions for this project
setwd("~/path/to/working/directory")

#Load the data - output of projectiLEAD_triallevelprocessing.R Script
data=read.csv(file="Data/proj-InitialPapers_asmt-AS_time-1234_type-agg_shape-long_clean-trimRTAccExOutlierRmBRTRegress_prog-R_date-2023-03-07.csv")

#Get just those variables that will be used in factor analyses with BRT regressed out
ace_cfa = data%>%
  dplyr::select("pid", "Cohort", "Time.Point", "grade", "gender", contains("residuals"))

names(ace_cfa)
# Change NA's to 9999 for analysis in Mplus
ace_cfa[is.na(ace_cfa)] = 9999

#Write out the long format version with no column names for Mplus
write.table(ace_cfa, file = paste0("Data/proj-InitialPapers_asmt-AS_time-1234_type-agg_shape-long_clean_trimRTAccExOutlierRMBRTRegress_prog-Mplus_date-", Sys.Date(), '.csv'), sep = ',', row.names = F, col.names = F)
#And the column names for reference
write.table(names(ace_cfa), file = paste0("Data/colnames_proj-InitialPapers_asmt-AS_time-1234_type-agg_shape-long_clean_trimRTAccExOutlierRMBRTRegress_prog-Mplus_date-", Sys.Date(), '.txt'), sep = ',', row.names = F, col.names = F)

#Reshape to wide format
ace_wide = reshape(ace_cfa, 
                   timevar = 'Time.Point',
                   idvar = c('pid', 'Cohort', 'gender'),
                   direction = 'wide')

ace_wide[is.na(ace_wide)] = 9999
#Write out the wide format version with no column names for Mplus
write.table(ace_wide, file = paste0("Data/proj-InitialPapers_asmt-AS_time-1234_type-agg_shape-wide_clean_trimRTAccExOutlierRMBRTRegress_prog-Mplus_date-", Sys.Date(), '.csv'), sep = ',', row.names = F, col.names = F)
#And the column names for reference
write.table(names(ace_wide), file = paste0("Data/colnames_proj-InitialPapers_asmt-AS_time-1234_type-agg_shape-wide_clean_trimRTAccExOutlierRMBRTRegress_prog-Mplus_date-", Sys.Date(), '.txt'), sep = ',', row.names = F, col.names = F)
