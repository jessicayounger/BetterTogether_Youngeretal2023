##Script to run community detection, including the 1000 iterations to understand best fitting model
#The results of these analyses are what is reported in the manuscript. Customized script is used to generate figures for publication that improve readability of the final results derived from this script
#See NetworkAnalysisFigure_igraph.R to generate the figure
#See NetworkEdgeCorrections.R to pull edgeweights associated with the results derived from this script
#Jessica Younger 2023-06-30

# Clear environment
rm(list = ls())
#Library Packages used in this script
library("tidyverse")
library("qgraph")
library("bootnet")

#Set the working directory to the folder with data and functions for this project
setwd("~/path/to/working/directory")

#Source the functions used in this script
source("Functions/SpinGlassCommunityDetectionIterations_Function.R")

#Load the data - output of projectiLEAD_triallevelprocessing.R Script
data=read.csv(file="Data/proj-InitialPapers_asmt-AS_time-1234_type-agg_shape-long_clean-trimRTAccExOutlierRmBRTRegress_prog-R_date-2023-03-07.csv")

#Select the subset of data needed for the community detection analysis 
names(data)
data=data%>%
  select(pid:grade,BACKWARDSSPATIALSPAN.object_count_span.overall.residuals:STROOP.rcs.overall.residuals, BOXED.rt_mean.correct.residuals )

# Rename columns for the metrics of interest to make the results easier to read
colnames(data) = c(names(data)[1:4], 'BSS', 'FSS', 'SSUS',
                   'SIMP', 'TNT', 'FLNK', 'STRP', 'BOX')

#Order the data for easier checking
data = data[order(data$pid, data$Time.Point), ]

#Remove data sets that are missing all ACE module data (i.e., number os missing data sets is less than 8, the total nubmer of modules used)
data=
  data%>%    
  mutate(na_count= apply(., 1, function(x) sum(is.na(x))))%>%
  group_by(Cohort, na_count)%>%
  filter(na_count<8)
  
#Recode the modules with RT related metrics so they go in the same direction as the oter modules
data$BOX=-1*data$BOX
data$SSUS=-1*data$SSUS

#Set up the label names for the output
colnames(data) = c(names(data)[1:4], "B. Span", "F. Span", "Sust\nAttn", "Impul\n Attn", 
                   "Tap &\n Trace", "Flanker", "Stroop", "Boxed")

#### Generate data and run 1000 of iterations of community detection
#For each cohort
for (i in c(3,5,7)){
#For each Timepoint
  for (j in c(1,2,3,4)){
#Estimate the network    
network=estimateNetwork(data[data$Cohort==i & data$Time.Point==j, c(5:12)],default="pcor", corMethod="cor_auto", missing="fiml")
#Plot it
n1 <- (plot(network, layout="circle", labels=colnames(network), label.cex=3))
#Set seed so that results can be reproduced
set.seed(5)
#Run 1000 repetitions of the community detection to genereate most selected organization using SpinGlassCommunityDetectionIterations_Function
spinComRec(n1, numberIterations = 1, numberEstimations = 1000, outpath=paste0("Results/CommunityDetectionResults/C", i, "T", j, "_"))
  }
}

