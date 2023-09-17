#Script to generate the Confidence Intervals for the Network Analysis results as shown in Figure S6
#Jessica Younger 2023-06-30

#Library the functions used in this script
library(tidyverse)
library(bootnet)
library(qgraph)
library(igraph)
library(gridExtra)


#Set the working directory to the folder with data and functions for this project
setwd("~/path/to/working/directory")

#Load the data - output of projectiLEAD_triallevelprocessing.R Script
data=read.csv(file="Data/proj-InitialPapers_asmt-A_time-1234_type-agg_shape-long_clean-trimRTAccExOutlierRmBRTRegress_prog-R_date-2023-09-09.csv")

#Select the subset of data needed for the community detection analysis 
names(data)
data=data%>%
  dplyr::select(pid:grade,BACKWARDSSPATIALSPAN.object_count_span.overall.residuals:STROOP.rcs.overall.residuals, BOXED.rt_mean.correct.residuals )

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


###Run the Network Analyses
#For each cohort
for (i in c(3,5,7)){
#For each time point
  for (j in c(1,2,3,4)){
  #estimate the network
    network=estimateNetwork(data[data$Cohort==i & data$Time.Point==j, c(5:12)],default="pcor", corMethod="cor_auto", missing="fiml")
    #generaet the plot
    g_net=as.igraph(plot(network, layout="circle", labels=colnames(network), label.cex=3))
    #set seed for future replication
    set.seed(5)
    #get a community structure
    g=cluster_spinglass(g_net)
    #save the network to a variable
    assign(paste0("network_c", i, "t", j), g_net)
    #save the community structure to a variable
    assign(paste0("groups_c", i, "t", j), g)
  }
}

###Bootstrap each network analysis
#For each cohort
for (i in c(3,5,7)){
  #For each timepoint
  for (j in c(1,2,3,4)){
    #generate the network
network=estimateNetwork(data[data$Cohort==i & data$Time.Point==j, c(5:12)],default="pcor", corMethod="cor_auto", missing="fiml")
#get the bootstrap results
boot1=bootnet(network, nBoots = 1000, statistics="edge", type="parametric")
table=summary(boot1)
#Create a table with the results
table$cohort=paste0("Cohort", i, "Time", j)
table$CIband = table$CIupper - table$CIlower
assign(paste0("bootstrap_c", i, "t", j), table)
assign(paste0("bootplot_c", i, "t", j), boot1)
  }
}

#Put all of the bootstrap results together into one big table
network_accuracy = rbind(bootstrap_c3t1,bootstrap_c3t2,bootstrap_c3t3, bootstrap_c3t4,
                         bootstrap_c5t1,bootstrap_c5t2,bootstrap_c5t3, bootstrap_c5t4,
                         bootstrap_c7t1,bootstrap_c7t2,bootstrap_c7t3, bootstrap_c7t4 )
#Label the table according to Cohort
network_accuracy=network_accuracy%>%
  mutate(CohortGroup=ifelse(grepl("Cohort3", cohort), 3, ifelse(grepl("Cohort5", cohort), 5, 7)))


###Start working with Confidence Interval Bands
#Get summary stats of the  CI Bands
describe(network_accuracy$CIband)

####Plot the CI Bands as in Figure S4
#Generate Plots
bootplot_c3t1=plot(bootplot_c3t1, graph = "contemporaneous", legend=F)
bootplot_c3t2=plot(bootplot_c3t2, graph = "contemporaneous", legend=F)
bootplot_c3t3=plot(bootplot_c3t3, graph = "contemporaneous", legend=F)
bootplot_c3t4=plot(bootplot_c3t4, graph = "contemporaneous", legend=F)

bootplot_c5t1=plot(bootplot_c3t1, graph = "contemporaneous", legend=F)
bootplot_c5t2=plot(bootplot_c3t2, graph = "contemporaneous", legend=F)
bootplot_c5t3=plot(bootplot_c3t3, graph = "contemporaneous", legend=F)
bootplot_c5t4=plot(bootplot_c3t4, graph = "contemporaneous", legend=F)

bootplot_c7t1=plot(bootplot_c3t1, graph = "contemporaneous", legend=F)
bootplot_c7t2=plot(bootplot_c3t2, graph = "contemporaneous", legend=F)
bootplot_c7t3=plot(bootplot_c3t3, graph = "contemporaneous", legend=F)
bootplot_c7t4=plot(bootplot_c3t4, graph = "contemporaneous", legend=F)

#Set up the Labels
col.titles = paste("Timepoint", 1:4)
row.titles = c("3rd - 4th Grade Cohort", "5th - 6th Grade Cohort"," 7th - 8th Grade Cohort")

#Put the labels in the right place for the combined figure
bootplot_c3t1=arrangeGrob(bootplot_c3t1, left=row.titles[1])
bootplot_c5t1=arrangeGrob(bootplot_c5t1, left=row.titles[2])
bootplot_c7t1=arrangeGrob(bootplot_c7t1, left=row.titles[3])

bootplot_c3t1=arrangeGrob(bootplot_c3t1, top=col.titles[1], ncol=1)
bootplot_c3t2=arrangeGrob(bootplot_c3t2, top=col.titles[2], ncol=1)
bootplot_c3t3=arrangeGrob(bootplot_c3t3, top=col.titles[3], ncol=1)
bootplot_c3t4=arrangeGrob(bootplot_c3t4, top=col.titles[4], ncol=1)

#Set up where to save the results
pdf(paste0("Results/FigureS6_NetworkAnalysis_CIplot_landscape_", Sys.Date(),".pdf"), width=16, height=18)
#Make the combined plot
grid.arrange(
  bootplot_c3t1, bootplot_c3t2, bootplot_c3t3, bootplot_c3t4,
  bootplot_c5t1, bootplot_c5t2, bootplot_c5t3, bootplot_c5t4,
  bootplot_c7t1, bootplot_c7t2, bootplot_c7t3, bootplot_c7t4, 
  ncol=4,
  nrow=3)

dev.off()

