##Script to generate the edgeweights of the network analyses. 
#Note this analysis is independent from community detection analyses run in CommunityDetection_withIteration.R
#The results of these analyses are what is reported in the manuscript in Table 4. 
#Jessica Younger 2023-06-30

#Library Packages used in this script
library(tidyverse)
library(bootnet)
library(qgraph)
library(igraph)
library(DescTools)
library(Hmisc)

#Set the working directory to the folder with data and functions for this project
setwd("~/path/to/working/directory")

#Load the function used in this script
source("Functions/GenerateCorrelationTable_Function.R")

#Load the data - output of projectiLEAD_triallevelprocessing.R Script
data=read.csv(file="Data/proj-InitialPapers_asmt-AS_time-1234_type-agg_shape-long_clean-trimRTAccExOutlierRmBRTRegress_prog-R_date-2023-03-07.csv")

#Select the subset of data needed for the community detection analysis 
names(data)
data=data%>%
  dplyr::select(pid:grade, BACKWARDSSPATIALSPAN.object_count_span.overall.residuals:STROOP.rcs.overall.residuals, BOXED.rt_mean.correct.residuals)

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


#Run the network analyses
#For each Cohort
for (i in c(3,5,7)){
#For each TimePoint  
  for (j in c(1,2,3,4)){
    network=estimateNetwork(data[data$Cohort==i & data$Time.Point==j, c(5:12)],default="pcor", corMethod="cor_auto", missing="fiml")
    g_net=as.igraph(plot(network, layout="circle", labels=colnames(network), label.cex=3))
    set.seed(5)
    g=cluster_spinglass(g_net)
    assign(paste0("network_c", i, "t", j), g_net)
    assign(paste0("groups_c", i, "t", j), g)
  }
}
#Get the edgeweights associated with each network and save to a variable
#For each Cohort
for (i in c(3,5,7)){
#For each Timepoint  
  for (j in c(1,2,3,4)){
    #Get the edgeweights
    assign(paste0("E_c", i, "t", j), data.frame(get.edgelist(get(paste0("network_c",i,"t", j))), E(get(paste0("network_c",i,"t", j)))$weight))
    #Set the names to be consistent
    assign(paste0("E_c", i, "t", j), setNames(get(paste0("E_c", i, "t", j)),c("V1", "V2", paste0("weight_c", i, "t", j))))
  }
}

#Put all of the separate edgeweight data into one dataframe
edge_weight_data=
  E_c3t2%>%
  full_join(E_c3t1,by=c("V1", "V2"))%>%
  full_join(E_c3t3,by=c("V1", "V2"))%>%
  full_join(E_c3t4,by=c("V1", "V2"))%>%
  full_join(E_c5t1,by=c("V1", "V2"))%>%
  full_join(E_c5t2,by=c("V1", "V2"))%>%
  full_join(E_c5t3,by=c("V1", "V2"))%>%
  full_join(E_c5t4,by=c("V1", "V2"))%>%
  full_join(E_c7t1,by=c("V1", "V2"))%>%
  full_join(E_c7t2,by=c("V1", "V2"))%>%
  full_join(E_c7t3,by=c("V1", "V2"))%>%
  full_join(E_c7t4,by=c("V1", "V2"))%>%
  arrange(V1, V2)%>%
  unite("edge", V1:V2)

#####Run correlations on the Edge Weights

#Fisher transform the correlations before doing correlations on edgeweights
edge_weight_data_z=edge_weight_data%>%
  mutate(across(matches("weight"),FisherZ))

#Create a dataframe with just the values (no labels)
corlist=edge_weight_data_z[2:13]
#Get the R, P value, and Confidence Intervals for each correlation and format into a table for Table 4 
cortable(corlist,  doc=paste0("Results/NetworkEdge_CorrelationResults_", Sys.Date()))

####Perform test to determine whether strength of correlations differ across cohort
#Start Set up
#Get just the matrix of R values
network_correlations<-rcorr(as.matrix(edge_weight_data_z[2:13]))
#Fisher transform the R values before performing tests on them 
network_correlations_z=as.data.frame(network_correlations$r)%>%
  mutate(across(matches("weight"),FisherZ))
#Update the Inf values to 1 for easier processing
network_correlations_z[network_correlations_z=="Inf"]=1

#Write to a csv to better format. There must be a way to do this in R, but easier for me to do in excel for now
write.csv(network_correlations_z, file=paste0("Results/NetworkEdge_CorrelationsZscored_", Sys.Date(), ".csv"))


#Read in the reformatted data that contains just the within cohort network correlations along with a cohort label
networkcores=read.csv(file="Data/iLEAD_WithinNetworkCorrelationsZ_2023-03-07.csv")
#Test if there is a main effect of cohort on strength of correlations
a=aov(correlation~cohort, data=networkcores)
summary(a)
EtaSq(a)
#Perform Tukey PostHoc Tests
TukeyHSD(a)
#Perform follow-up tests to confirm direction of effect
t.test(networkcores$correlation[networkcores$cohort=="c3"],networkcores$correlation[networkcores$cohort=="c5"])
t.test(networkcores$correlation[networkcores$cohort=="c7"],networkcores$correlation[networkcores$cohort=="c5"])
