#Script to run resampling analysis on network analysis to understand how sample size might impact network analysis results. 
#The results of these analyses are what is reported in the manuscript in Table S11.
#Kristine O'Laughlin with edits from Jessica Younger 2023-06-30

# Clear environment
rm(list = ls())

# Load libraries
library(qgraph)
library(tidyverse)
library(bootnet)


#Set the working directory to the folder with data and functions for this project
setwd("~/path/to/working/directory")
setwd("~/Desktop/BetterTogether_Scripts_Deidentify")

#Source the functions used in this script
source("Functions/Resampling_SpinGlassCommunityDetectionIterations_Function.R")

#Load the data - output of projectiLEAD_triallevelprocessing.R Script
data=read.csv(file="Data/proj-InitialPapers_asmt-A_time-1234_type-agg_shape-long_clean-trimRTAccExOutlierRmBRTRegress_prog-R_date-2023-09-09.csv")

# Number of datasets to create
rep = 500

# Number of subjects in resample
N = 500

# Set starting seed
set.seed(131233)

#Select the subset of data needed for the community detection analysis 
data=data%>%
  select(pid:grade,BACKWARDSSPATIALSPAN.object_count_span.overall.residuals:STROOP.rcs.overall.residuals, BOXED.rt_mean.correct.residuals )

# Rename columns for the metrics of interest to make the results easier to read
colnames(data) = c(names(data)[1:4], 'BSS', 'FSS', 'SSUS',
                   'SIMP', 'TNT', 'FLNK', 'STRP', 'BOX')

#Order the data for easier checking
data = data[order(data$pid, data$Time.Point), ]

#Recode the modules with RT related metrics so they go in the same direction as the oter modules
data$BOX = -1*data$BOX
data$SSUS = -1*data$SSUS

#Create samples of data
samp = lapply(samp <- vector(mode = 'list', 3),function(x) 
  x = lapply(x <- vector(mode = 'list', 4), function(x) 
    x = vector(mode = 'list', rep)))

#Remove data sets that are missing all ACE module data (i.e., number os missing data sets is less than 8, the total nubmer of modules used)
tmp.data = data[-which(rowSums(is.na(data[, 5:12])) == 8), ]

# Take N resampled datasets with replacement
for (cohort in 1:length(unique(data$Cohort))) {
  for (time in 1:4) {
    for (r in 1:rep) {
      
      # Sample rows of dataset
      sub = tmp.data[which(tmp.data$Cohort == unique(tmp.data$Cohort)[cohort] &
                       tmp.data$Time.Point == time), ]
      samp[[cohort]][[time]][[r]] = sub[sample(nrow(sub), N, replace = T), ]
    
    }
  }
}

# Fit network models to each dataset

net = lapply(net <- vector(mode = 'list', 3),function(x) 
  x = lapply(x <- vector(mode = 'list', 4), function(x) 
    x = vector(mode = 'list', rep)))

for (x in 1:3) {
  for (y in 1:4) {
    for (z in 1:rep) {
      network = estimateNetwork(samp[[x]][[y]][[z]][, 5:12], default = "pcor", corMethod = "cor_auto", missing = "fiml")
      n1 = plot(network, layout = "circle", labels = colnames(network), label.cex = 3)
      net[[x]][[y]][[z]] = try(spinComRec(n1, numberIterations = 1, numberEstimations = 1000))
      print(paste('Completed resample', z, 'at time', y, 'for Cohort', unique(data$Cohort)[x]))
      
    }
  }
}
  
# Save output of resampling just in case need to load later since this script takes a long time to run
saveRDS(net, file = paste0('Results/iLEAD_NetworkAnalysis_Resampling_', Sys.Date(), '.rds'))
save.image(file = paste0('Results/iLEAD_NetworkAnalysis_Resampling_', Sys.Date(), '.RData'))

#If loading the Rdata
#load("~/Desktop/iLEAD_Scripts_Edit/ResamplingResults/iLEAD_NetworkAnalysis_Resampling_2023-03-22.RData")

###### What is the composition of each of the communities formed and their frequency across the re-samples

community.counts  = lapply(community.counts <- vector(mode = 'list', 3), function(x) x <- vector(mode = 'list', 4))

for (x in 1:3) {
  
  for (y in 1:4) {
      
    community.counts[[x]][[y]] = as.data.frame(table(sapply(lapply(net[[x]][[y]], function(z) z$uniqueMembershipVectors[, 1]), paste0, collapse = '-')))
    colnames(community.counts[[x]][[y]]) = c('Communities', 'Frequency')  
    community.counts[[x]][[y]] = community.counts[[x]][[y]][order(community.counts[[x]][[y]]$Frequency, decreasing = T), ]
    community.counts[[x]][[y]]$Proportion = community.counts[[x]][[y]]$Frequency/rep
    community.counts[[x]][[y]][, 4:11] = NA
    colnames(community.counts[[x]][[y]])[4:11] = colnames(data)[5:12]
    
    for (i in 1:nrow(community.counts[[x]][[y]])) {
    
    community.counts[[x]][[y]][i, 4:11] = str_split(community.counts[[x]][[y]]$Communities[i], '-')[[1]][1:8]
    
    }
    
    community.counts[[x]][[y]] = community.counts[[x]][[y]][order(community.counts[[x]][[y]]$Frequency, decreasing = T), ]
    community.counts[[x]][[y]] = community.counts[[x]][[y]][, c(4:11, 2, 3)]
    
  }
}


#Write out the results of the resampling for each cohort into a csv with each of the unique solutions and the frequency
#it was the top structure out of the N resamples

for (x in 1:3) {
  
  for (y in 1:4) {
    
    write.csv(community.counts[[x]][[y]], file = paste0('Results/ResamplingResults/Resampled_NetworkCommunityCompositions_C', unique(data$Cohort)[x], '_T', y, '.csv'), row.names = F)
    
  }
  
}


